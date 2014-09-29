open Lwt

type rag = Red | Amber | Green

type t = {
  name: string;
  description: string;
  children: t list;
  rag: rag ref;
  start: unit -> unit Lwt.t;
  stop: unit -> unit Lwt.t;
  failure : unit -> unit Lwt.t;
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
  t.start ()

let restart ?(max=2) ?(interval=300.) t =
  let rag = ref Red in
  let c = Lwt_condition.create () in
  let rec failure () =
    if !rag = Red then return ()
    else
      Lwt_condition.wait c >>= fun () ->
      failure () in
  let rec loop exits p =
    let now = Unix.gettimeofday () in
    (* If exits <> [] then we are amber until it is empty *)
    let timer = Lwt_unix.sleep (interval -. now +. (List.fold_left min now exits)) in
    Lwt.choose [ p; timer; failure () ]
    >>= fun () ->
    if !rag = Red then return ()
    else match Lwt.state timer with
    | Return () ->
      info "%s: supervisor has recovered" t.name;
      rag := Green;
      trigger_update ();
      loop [] p
    | _ ->
      let now = Unix.gettimeofday () in
      let exits = now :: (List.filter (fun x -> now -. x < interval) exits) in
      if List.length exits > max then begin
        error "%s: failed more than %d times in %.0f seconds" t.name max interval;
        rag := Red;
        Lwt_condition.broadcast c ();
        trigger_update ();
        return ();
      end else begin
        error "%s: failed but will restart" t.name;
        rag := Amber;
        trigger_update ();
        t.start () >>= fun () ->
        rag := Green;
        trigger_update ();
        loop exits (t.failure ())
      end in
  let start () =
    info "%s: supervisor will restart up to %d times in %.0f seconds" t.name max interval;
    rag := Amber;
    trigger_update ();
    t.start () >>= fun () ->
    rag := Green;
    trigger_update ();
    let _ = loop [] (t.failure ()) in
    return () in
  let stop () =
    rag := Red;
    trigger_update ();
    Lwt_condition.broadcast c ();
    t.stop () >>= fun () ->
    return () in
  { name = t.name ^ " supervisor";
    description = Printf.sprintf "I will automatically restart %s up to %d times in %.0f seconds" t.description max interval;
    children = [ t ];
    rag;
    start; stop; failure
  }

let init'd_real service_name description =
  let rag = ref Red in
  let c = Lwt_condition.create () in
  let m = Lwt_mutex.create () in

  let is_alive () =
    Lwt_mutex.with_lock m
      (fun () ->
        Lwt_unix.system (Printf.sprintf "service %s status 2>&1 >/dev/null" service_name)
        >>= function
        | Lwt_unix.WEXITED 0 -> return true
        | _ -> return false
      ) in

  let rec watch_process () =
    is_alive () >>= fun alive ->
    let rag' = if alive then Green else Red in
    if !rag <> rag' then begin
      rag := rag';
      Lwt_condition.broadcast c ();
      trigger_update ()
    end;
    Lwt_unix.sleep 1. >>= fun () ->
    watch_process () in
  let _ = watch_process () in

  let start () =
    Lwt_mutex.with_lock m
      (fun () ->
        rag := Amber;
        trigger_update ();
        action "service %s start" service_name;
        Lwt_unix.system (Printf.sprintf "service %s start" service_name) >>= fun _ ->
        Lwt_unix.sleep 5. >>= fun () ->
        return ()
      ) in

  let stop () =
    Lwt_mutex.with_lock m
      (fun () ->
        Lwt_unix.system (Printf.sprintf "service %s stop" service_name) >>= fun _ ->
        Lwt_unix.sleep 5. >>= fun () ->
        return ()
      ) in
  let rec failure () =
    if !rag = Red then return ()
    else
      Lwt_condition.wait c >>= fun () ->
      failure () in

 {
  name = "init.d/" ^ service_name;
  description; children = []; rag; start; stop; failure
  }

let init'd_simulated service_name description =
  let rag = ref Red in
  let c = Lwt_condition.create () in
  let start () =
    let manage_process () =
      action "service %s start" service_name;
      rag := Amber;
      trigger_update ();
      Lwt_unix.sleep (Random.float 3. +. 1.) >>= fun () ->
      info "%s: service has recovered" service_name;
      rag := Green;
      trigger_update ();
      Lwt_unix.sleep (Random.float 3. +. 5.) >>= fun () ->
      info "%s: service has failed" service_name;
      rag := Red;
      Lwt_condition.broadcast c ();
      trigger_update ();
      return () in
    let _ = manage_process () in
    return () in
  let stop () =
    return () in
  let rec failure () =
    if !rag = Red then return ()
    else
      Lwt_condition.wait c >>= fun () ->
      failure () in
 {
  name = "init.d/" ^ service_name;
  description; children = []; rag; start; stop; failure
  }

let simulate = ref false

let init'd a b = (if !simulate then init'd_simulated else init'd_real) a b

let group name description children =
  let rag = ref Red in
  let c = Lwt_condition.create () in
  let m = Lwt_mutex.create () in
  let start () =
    Lwt_mutex.with_lock m
      (fun () ->
         action "group %s start" name;
         rag := Amber;
         trigger_update ();
         Lwt_list.iter_s (fun t -> t.start()) children >>= fun () ->
         rag := Green;
         trigger_update ();
         (* When one of our children exits, go amber,
            When they all exit, go red *)
         let watch_children () =
           let ts = List.map (fun t -> t.failure ()) children in
           Lwt.choose ts >>= fun () ->
           Lwt_mutex.with_lock m
             (fun () ->
               rag := Red;
               Lwt_condition.broadcast c ();
               trigger_update ();
               Lwt_list.iter_s (fun t -> t.stop ()) children >>= fun () ->
               Lwt.join ts
             ) >>= fun () ->
           return () in
         let _ = watch_children () in
         return ()
       ) in
  let stop () =
    Lwt_mutex.with_lock m
      (fun () ->
        Lwt_list.iter_s (fun t -> t.stop ()) children >>= fun () ->
        rag := Red;
        Lwt_condition.broadcast c ();
        trigger_update ();
        return ()
      ) in
  let rec failure () =
    if !rag = Red then return ()
    else
      Lwt_condition.wait c >>= fun () ->
      failure () in
   { name; description; children; rag; start; stop; failure }
