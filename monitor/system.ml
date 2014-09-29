open Restart

(* This is our specific policy: *)

let xenopsd = init'd "xenopsd-xc" "The Xen domain manager"
let squeezed = init'd "squeezed" "The memory ballooning daemon"
let xapi = init'd "xapi" "The XenAPI interface"

let toolstack = group "toolstack" "The xapi toolstack" [
  restart xenopsd;
  restart squeezed;
  restart xapi;
]

let system = restart toolstack
