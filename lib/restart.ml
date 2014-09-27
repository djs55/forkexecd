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

type t = {
  name: string;
  description: string;
  start: unit -> Process.t;
}
(** A service which has a name and can be started/stopped/restarted *)

let info (fmt : ('a, unit, string, unit) format4) = (Printf.kprintf (fun s -> Printf.fprintf stderr "[info] %s\n%!" s) fmt)
let error (fmt : ('a, unit, string, unit) format4) = (Printf.kprintf (fun s -> Printf.fprintf stderr "[error] %s\n%!" s) fmt)
let action (fmt : ('a, unit, string, unit) format4) = (Printf.kprintf (fun s -> Printf.fprintf stderr "[action] %s\n%!" s) fmt)

let start t =
  action "Starting %s" t.name;
  Process.wait (t.start ())

let restart ?(max=2) ?(interval=300.) t =
  let rec loop stop exits p =
    Lwt.choose [ Process.wait p; stop ]
    >>= fun () ->
    let now = Unix.gettimeofday () in
    let exits = now :: (List.filter (fun x -> now -. x < interval) exits) in
    if List.length exits > max then begin
      error "%s: failed more than %d times in %.0f seconds" t.name max interval;
      return ();
    end else begin
      error "%s: failed but will restart" t.name;
      loop stop exits (t.start ())
    end in
  { name = t.name ^ " (protected)";
    description = Printf.sprintf "%s (will automatically restart up to %d times in %.0f seconds)" t.description max interval;
    start = fun () ->
      info "%s: will restart up to %d times in %.0f seconds" t.name max interval;
      let th, u = Lwt.task () in
      Process.Thread (loop th [] (t.start()), fun () -> Lwt.wakeup_later u ())
  }

let run cmd args =
  action "%s %s" cmd (String.concat " " args);
  let th, u = Lwt.task () in
  let action = Lwt.choose [ Lwt_unix.sleep 5.; th ] in
  Process.Thread (action, fun () -> Lwt.wakeup_later u ())

let init'd service_name description = {
  name = service_name ^ " (run from init.d)";
  description;
  start = fun () -> run "service" [ service_name; "start" ];
}

let group name description ts = {
  name;
  description;
  start = fun () -> Process.Group (List.map (fun t -> t.start ()) ts)
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
