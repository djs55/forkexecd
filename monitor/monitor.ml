open Cohttp_lwt
open Lwt

let port = 8080
let root = "./monitor"

let string_of_file path =
  Lwt_io.file_length path >>= fun len ->
  let result = String.make (Int64.to_int len) ' ' in
  Lwt_io.open_file ~mode:Lwt_io.input path >>= fun ic ->
  Lwt_io.read_into_exactly ic result 0 (String.length result) >>= fun () ->
  Lwt_io.close ic >>= fun () ->
  return result

let pre = string_of_file (root ^ "/html/pre")
let post = string_of_file (root ^ "/html/post")

(* verbs are encoded as uri prefixes *)
let _start = "/start"
let _stop = "/stop"
let _event = "/event/"

let start name = match Restart.find System.system name with
| Some t -> ignore(t.Restart.start ()); return ()
| None ->
  Restart.error "start: %s not found" name;
  return ()

let stop _ = return ()

let actions = [
  _start, start;
  _stop, stop;
]

let startswith prefix x =
  let prefix' = String.length prefix in
  let x' = String.length x in
  prefix' <= x' && (String.sub x 0 prefix' = prefix)

let trigger_actions path =
  Lwt_list.iter_s
    (fun (verb, fn) ->
      let prefix = verb ^ "/" in
      let prefix' = String.length prefix in
      if startswith prefix path
      then fn (String.sub path prefix' (String.length path - prefix'))
      else return ()
    ) actions

let generate_page () =
  pre >>= fun pre ->
  post >>= fun post ->

  let open Restart in
  let rec row_of_service { name; description; children; rag } =
    let icon = match !rag with
    | Red -> <:html< <img src="img/red.png"/> >>
    | Amber -> <:html< <img src="img/amber.png"/> >>
    | Green -> <:html< <img src="img/green.png"/> >> in
    let action_uri name verb = [ `Data (Printf.sprintf "%s/%s" verb name) ] in
    let actions = match !rag with
    | Red -> <:html< <a href=$action_uri name _start$>Start</a> >>
    | Amber
    | Green -> <:html< <a href=$action_uri name _stop$>Stop</a> >> in
    let name = [ `Data name ] in
    let description = [ `Data description ] in
    let children = List.concat (List.map row_of_service children) in
    <:html<
      <tr><td>$icon$</td><td>$name$</td><td>$description$</td><td>$actions$</td></tr>
      $children$
    >> in

  let table = <:html<
  <div class="row">
    <div class="large-12 columns">
      <form>
      <table width="100%">
        <thead><tr><th>State</th><th>Name</th><th>Description</th><th>Actions</th></tr></thead>
        <tbody>
          $row_of_service System.system$
        </tbody>
      </table>
      </form>
    </div>
  </div>
   >> in

  let row_of_logs () =
    let rec loop acc m =
      if FloatMap.is_empty m
      then acc
      else
        let time, (level, message) = FloatMap.min_binding m in
        let time_string =
          let l = Unix.localtime time in
          [ `Data (Printf.sprintf "%02d:%02d:%02d.%02d"
            l.Unix.tm_hour l.Unix.tm_min l.Unix.tm_sec (int_of_float (100. *. (time -. (floor time))))) ] in
        let level_string = [ `Data (string_of_level level) ] in
        let message_string = [ `Data message ] in
        let item = <:html<
          <tr><td>$time_string$</td><td>$level_string$</td><td>$message_string$</td></tr>
        >> in
        loop (item :: acc) (FloatMap.remove time m) in
    List.concat (loop [] !log_messages) in

  let log = <:html<
  <div class="row">
    <div class="large-12 columns">
      <table width="100%">
        <thead><tr><th width="10%">Time</th><th width="10%">Level</th><th>Details</th></tr></thead>
        <tbody>
          $row_of_logs ()$
        </tbody>
      </table>
    </div>
  </div>
  >> in

  return (pre ^ (Cow.Html.to_string table) ^ (Cow.Html.to_string log) ^ post)

let resource_prefixes = [
  "/css"; "/js"; "/img"
]

let slash = Re_str.regexp_string "/"

type event_result = {
  events: (int * string) list;
} with rpc

let callback conn_id req body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in

  if List.fold_left (||) false (List.map (fun prefix -> startswith prefix path) resource_prefixes)
  then Cohttp_lwt_unix.Server.respond_file ~fname:(root ^ path) ()
  else
    if startswith _event path then begin
      match Re_str.split_delim slash path with
      | "" :: _ :: from :: timeout ->
        Printf.fprintf stderr "path=[%s] from=[%s]\n%!" path from;
        let from = int_of_string from in
        Restart.wait_for_update from >>= fun next ->
        let result = { events = [ next, "nothing" ]} in
        let body = Jsonrpc.to_string (rpc_of_event_result result) in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
      | _ ->
        Printf.fprintf stderr "failed to parse %s\n%!" path;
        fail Not_found
    end else begin
      trigger_actions path >>= fun () ->
      if path <> "/"
      then Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path uri "/") ()
      else begin
        generate_page () >>= fun body ->
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
      end
    end

let conn_closed conn_id () = ()

let start_server port () =
  Printf.fprintf stderr "Listening for HTTP on port %d\n%!" port;

  let config = { Cohttp_lwt_unix.Server.callback; conn_closed } in
  Cohttp_lwt_unix.Server.create ~address:"0.0.0.0" ~port config

let _ = Lwt_main.run (start_server port ())
