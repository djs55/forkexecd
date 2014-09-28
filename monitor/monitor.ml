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

let generate_page () =
  pre >>= fun pre ->
  post >>= fun post ->

  let open Restart in
  let rec row_of_service { name; description; children; rag } =
    let icon = match !rag with
    | Red -> <:html< <img src="img/red.png"/> >>
    | Amber -> <:html< <img src="img/amber.png"/> >>
    | Green -> <:html< <img src="img/green.png"/> >> in
    let name = [ `Data name ] in
    let description = [ `Data description ] in
    let children = List.concat (List.map row_of_service children) in
    <:html<
      <tr><td>$icon$</td><td>$name$</td><td>$description$</td><td><a href="#">Restart</a></td></tr>
      $children$
    >> in

  let table = <:html<
  <div class="row">
    <div class="large-12 columns">
      <table width="100%">
        <thead><tr><th>State</th><th>Name</th><th>Description</th><th>Actions</th></tr></thead>
        <tbody>
          $row_of_service System.system$
        </tbody>
      </table>
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

let startswith prefix x =
  let prefix' = String.length prefix in
  let x' = String.length x in
  prefix' <= x' && (String.sub x 0 prefix' = prefix)

let callback conn_id req body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  if List.fold_left (||) false (List.map (fun prefix -> startswith prefix path) resource_prefixes)
  then Cohttp_lwt_unix.Server.respond_file ~fname:(root ^ path) ()
  else begin
    generate_page () >>= fun body ->
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
  end

let conn_closed conn_id () = ()

let start_server port () =
  Printf.fprintf stderr "Listening for HTTP on port %d\n%!" port;

  let config = { Cohttp_lwt_unix.Server.callback; conn_closed } in
  Cohttp_lwt_unix.Server.create ~address:"0.0.0.0" ~port config

let _ = Lwt_main.run (start_server port ())
