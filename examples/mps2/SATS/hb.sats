(*
** SATS/hb.sats -- the hardware seam of heartbeat.ovt, freestanding
** ATS on the MPS2-AN500 model: the CMSDK UART as the actuator
** surface, a software counter as the sensor, semihosting as the
** way home.
*)

#include "contrib/kernelats/prelude/staloadall.hats"

%{#
#include "contrib/kernelats/ccomp/pats_ccomp.h"
#include "CATS/hb.cats"
#include "prelude/CATS/integer.cats"
#include "prelude/CATS/bool.cats"
%}

(* the volatile floor (CATS/hb.cats) *)

fun hb_mmio_write (addr: uint, v: uint): void = "ext#"
fun hb_mmio_read (addr: uint): uint = "ext#"
fun hb_count_next (): int = "ext#"
fun hb_beat_next (): int = "ext#"
fun hb_exit (): void = "ext#"

(* the seam, implemented in ATS (DATS/hb.dats) *)
fun fw_init (): void = "ext#fw_init"
fun counter_sense (t: int): int = "ext#counter_sense"
fun latest (a0: int): int = "ext#latest"
fun fast_actuate (t: int, v: int): void = "ext#fast_actuate"
fun mid_actuate (t: int, v: int): void = "ext#mid_actuate"
fun slow_actuate (t: int, v: int): void = "ext#slow_actuate"

(* ****** ****** *)

(* end of [SATS/hb.sats] *)
