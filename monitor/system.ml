open Restart

(* This is our specific policy: *)

let xenopsd = init'd "xenopsd-xc" "The Xen domain manager"
let squeezed = init'd "squeezed" "The memory ballooning daemon"
let networkd = init'd "xcp-networkd" "The host network configuration daemon"

let toolstack = group "toolstack" "The xapi toolstack" [
  restart squeezed;
  restart networkd;
]

let vm_start_probe =
  always_ok "VM.start" "the system can start VMs"

let dom0_disk_space =
  always_ok "dom0 disk space" "the dom0 partition has enough free space"

let system = [
  restart toolstack;
  vm_start_probe;
  dom0_disk_space;
]
