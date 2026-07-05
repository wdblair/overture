(*
** SATS/rt.sats -- the thin OS, freestanding ATS.
**
** The pacing logic is ATS; CATS/rt.cats holds the volatile MMIO
** floor. Follows the kernelats recipe.
*)

#include "contrib/kernelats/prelude/staloadall.hats"

%{#
#include "contrib/kernelats/ccomp/pats_ccomp.h"
#include "CATS/rt.cats"
#include "prelude/CATS/integer.cats"
#include "prelude/CATS/bool.cats"
%}

(* the volatile floor (CATS/rt.cats) *)

fun rt_mmio_write (addr: uint, v: uint): void = "ext#"
fun rt_mmio_read (addr: uint): uint = "ext#"
fun rt_wfi (): void = "ext#"

fun rt_ms_bump (): void = "ext#"
fun rt_ms_reached (target: uint): bool = "ext#"
fun rt_ms_now (): uint = "ext#"

fun rt_overture_step (t: int): void = "ext#"
fun rt_base_tick (): int = "ext#"
fun rt_hyperperiod (): int = "ext#"
fun rt_wrap_at (): int = "ext#"

(* the firmware's hardware bring-up (example provides) *)
fun fw_init (): void = "ext#fw_init"

(* the thin OS itself, implemented in ATS (DATS/rt.dats) *)
fun rt_main (): void = "ext#rt_main"
fun rt_systick (): void = "ext#SysTick_Handler"

(* ****** ****** *)

(* end of [SATS/rt.sats] *)
