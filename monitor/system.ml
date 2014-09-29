open Restart

(* This is our specific policy: *)

let xenopsd = init'd "xenopsd-xc" "The Xen domain manager"
let squeezed = init'd "squeezed" "The memory ballooning daemon"
let networkd = init'd "xcp-networkd" "The host network configuration daemon"

let toolstack = group "toolstack" "The xapi toolstack" [
(*
  restart xenopsd;
*)
  restart squeezed;
  restart networkd;
]

let system = restart toolstack
