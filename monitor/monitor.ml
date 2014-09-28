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

  let row_of_service () =
    <:html<
      <tr><td><img src="img/green.png"/></td><td>xenopsd</td><td>The Xen domain manager</td><td><a href="#">Restart</a></td></tr>
    >> in

  let table = <:html<
  <div class="row">
    <div class="large-12 columns">
      <table>
        <thead><tr><th>State</th><th>Name</th><th>Description</th><th>Actions</th></tr></thead>
        <tbody>
          <tr><td><img src="img/red.png"/></td><td>xenopsd</td><td>The Xen domain manager</td><td><a href="#">Restart</a></td></tr>
          <tr><td><img src="img/amber.png"/></td><td>xenopsd</td><td>The Xen domain manager</td><td><a href="#">Restart</a></td></tr>
          <tr><td><img src="img/green.png"/></td><td>xenopsd</td><td>The Xen domain manager</td><td><a href="#">Restart</a></td></tr>
          $row_of_service ()$
        </tbody>
      </table>
    </div>
  </div>
   >> in

  return (pre ^ (Cow.Html.to_string table) ^ post)

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
