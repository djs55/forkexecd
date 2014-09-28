open Lwt

module Process = struct
  type t =
    | Unix of int
    | Thread of (unit Lwt.t * (unit -> unit))
    | Group of t list
  (** The current running instance is either an external process
      or one of our internal threads *)

  let rec wait = function
    | Unix pid ->
      Lwt_unix.sleep 1.
    | Thread (t, _) ->
      t
    | Group ts ->
      Lwt.join (List.map wait ts)

  let rec stop = function
    | Unix pid ->
      Lwt_unix.sleep 1.
    | Thread (_, s) ->
      s ();
      return ()
    | Group ts ->
      Lwt_list.iter_p stop ts
end

type rag = Red | Amber | Green

type t = {
  name: string;
  description: string;
  children: t list;
  rag: rag ref;
  start: unit -> Process.t;

}
(** A service which has a name and can be started/stopped/restarted *)

let rec find t name =
  if t.name = name then Some t
  else
    List.fold_left (fun acc b -> match acc with
      | Some x -> acc
      | None -> find b name
    ) None t.children

(* Rather than doing something sophisticated with events we broadcast via
   this condition variable and cause the UI to update *)
let generation_id = ref 0
let cvar : unit Lwt_condition.t = Lwt_condition.create ()

let trigger_update () =
  incr generation_id;
  Lwt_condition.broadcast cvar ()

let wait_for_update x =
  let rec loop () =
    if !generation_id > x
    then return !generation_id
    else
      Lwt_condition.wait cvar >>= fun () ->
      loop () in
  loop ()

type level = Info | Error | Action

let string_of_level = function
| Info -> "info"
| Error -> "error"
| Action -> "action"

type logmsg = {
  timestamp: float;
  level: level;
  message: string;
}

module FloatMap = Map.Make(struct type t = float let compare = compare end)
let log_messages = ref FloatMap.empty
let max_log_messages = 16 (* keep this small *)
let append level message =
  let m =
    if FloatMap.cardinal !log_messages = 16
    then FloatMap.remove (fst (FloatMap.min_binding !log_messages)) !log_messages
    else !log_messages in
  log_messages := FloatMap.add (Unix.gettimeofday ()) (level, message) m

let info (fmt : ('a, unit, string, unit) format4) = (Printf.kprintf (fun s ->
  Printf.fprintf stderr "[info] %s\n%!" s;
  append Info s
  ) fmt)
let error (fmt : ('a, unit, string, unit) format4) = (Printf.kprintf (fun s ->
  Printf.fprintf stderr "[error] %s\n%!" s;
  append Error s
  ) fmt)
let action (fmt : ('a, unit, string, unit) format4) = (Printf.kprintf (fun s ->
  Printf.fprintf stderr "[action] %s\n%!" s;
  append Action s
  ) fmt)

let _ = info "Monitor is running"

let start t =
  action "Starting %s" t.name;
  Process.wait (t.start ())

let restart ?(max=2) ?(interval=300.) t =
  let rag = ref Red in
  let rec loop stop exits p =
    let now = Unix.gettimeofday () in
    (* If exits <> [] then we are amber until it is empty *)
    let timer = Lwt_unix.sleep (interval -. now +. (List.fold_left min now exits)) in
    Lwt.choose [ Process.wait p; stop; timer ]
    >>= fun () ->
    match Lwt.state timer with
    | Return () ->
      info "%s: supervisor has recovered" t.name;
      rag := Green;
      trigger_update ();
      loop stop [] p
    | _ ->
      let now = Unix.gettimeofday () in
      let exits = now :: (List.filter (fun x -> now -. x < interval) exits) in
      if List.length exits > max then begin
        error "%s: failed more than %d times in %.0f seconds" t.name max interval;
        rag := Red;
        trigger_update ();
        return ();
      end else begin
        error "%s: failed but will restart" t.name;
        rag := Amber;
        trigger_update ();
        loop stop exits (t.start ())
      end in
  { name = t.name ^ " supervisor";
    description = Printf.sprintf "I will automatically restart %s up to %d times in %.0f seconds" t.description max interval;
    children = [ t ];
    rag;
    start = fun () ->
      info "%s: supervisor will restart up to %d times in %.0f seconds" t.name max interval;
      rag := Green;
      trigger_update ();
      let th, u = Lwt.task () in
      Process.Thread (loop th [] (t.start()), fun () -> Lwt.wakeup_later u ())
  }

let run cmd args =
  action "%s %s" cmd (String.concat " " args);
  let th, u = Lwt.task () in
  let action = Lwt.choose [ Lwt_unix.sleep 5.; th ] in
  Process.Thread (action, fun () -> Lwt.wakeup_later u ())

let init'd service_name description =
  let rag = ref Red in
  let start () =
    action "service %s start" service_name;
    info "%s: service has recovered" service_name;
    rag := Green;
    trigger_update ();
    (* Determine the pid of the service and watch it *)
    let watch_pid () =
      Lwt_unix.sleep 5. >>= fun () ->
      info "%s: service has failed" service_name;
      rag := Red;
      trigger_update ();
      return () in
    let th, u = Lwt.task () in
    let action = Lwt.choose [ watch_pid (); th ] in
    Process.Thread (action, fun () -> Lwt.wakeup_later u ()) in {
  name = "init.d/" ^ service_name;
  description; children = []; rag; start;
  }

let group name description children =
  let rag = ref Red in {
  name;
  description;
  children;
  rag;
  start = fun () ->
    rag := Green;
    trigger_update ();
    Process.Group (List.map (fun t -> t.start ()) children)
}

(* This is our specific policy: *)

let xenopsd = init'd "xenopsd" "The Xen domain manager"
let squeezed = init'd "squeezed" "The memory ballooning daemon"
let xapi = init'd "xapi" "The XenAPI interface"

let toolstack = group "toolstack" "The xapi toolstack" [
  restart xenopsd;
  restart squeezed;
  restart xapi;
]

let system = restart toolstack
