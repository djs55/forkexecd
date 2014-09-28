open Cohttp_lwt

let port = 8080

let callback conn_id req body =
  let uri = Cohttp.Request.uri req in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"HELLO" ()

let conn_closed conn_id () = ()

let start_server port () =
  Printf.fprintf stderr "Listening for HTTP on port %d\n%!" port;

  let config = { Cohttp_lwt_unix.Server.callback; conn_closed } in
  Cohttp_lwt_unix.Server.create ~address:"0.0.0.0" ~port config

let _ = Lwt_main.run (start_server port ())
