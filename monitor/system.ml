open Restart

(* This is our specific policy: *)

let xenopsd = init'd "xenopsd" "/var/run/xenopsd-xc.pid" "The Xen domain manager"
let squeezed = init'd "squeezed" "/var/run/squeezed.pid" "The memory ballooning daemon"
let xapi = init'd "xapi" "/var/run/xapi.pid" "The XenAPI interface"

let toolstack = group "toolstack" "The xapi toolstack" [
  restart xenopsd;
  restart squeezed;
  restart xapi;
]

let system = restart toolstack
