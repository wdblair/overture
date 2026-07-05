(*
** SATS/fw.sats -- the hardware seam of blink.ovt, freestanding ATS.
** The names fw_init / cycles_sense / latest / led_actuate are the
** ext# surface the generated harness links against.
*)

#include "contrib/kernelats/prelude/staloadall.hats"

%{#
#include "contrib/kernelats/ccomp/pats_ccomp.h"
#include "CATS/fw.cats"
#include "prelude/CATS/integer.cats"
#include "prelude/CATS/bool.cats"
%}

fun fw_mmio_write (addr: uint, v: uint): void = "ext#"
fun fw_mmio_read (addr: uint): uint = "ext#"
fun fw_led_toggle (): bool = "ext#"

(* the seam, implemented in ATS (DATS/fw.dats) *)
fun fw_init (): void = "ext#fw_init"
fun cycles_sense (t: int): int = "ext#cycles_sense"
fun latest (a0: int): int = "ext#latest"
fun led_actuate (t: int, v: int): void = "ext#led_actuate"

(* ****** ****** *)

(* end of [SATS/fw.sats] *)
