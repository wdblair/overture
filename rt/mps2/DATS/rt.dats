(*
** DATS/rt.dats -- the thin OS in freestanding ATS.
**
** SysTick paces logical time over the generated Overture harness:
** one tick = 1 ms on the AN500's 25 MHz SYSCLK. Logical time advances
** by the program's base tick and wraps one hyperperiod past the
** maximum offset, where the firing pattern is periodic.
*)

#define ATS_DYNLOADFLAG 0

staload "SATS/rt.sats"

staload _ = "prelude/DATS/integer.dats"
staload _ = "prelude/DATS/bool.dats"

(* ****** ****** *)
(* Cortex-M core registers *)

#define SYST_CSR 0xE000E010u
#define SYST_RVR 0xE000E014u
#define SYST_CVR 0xE000E018u

#define SYSCLK_HZ 25000000u
#define TICK_HZ 1000u

(* ****** ****** *)

implement rt_systick () = rt_ms_bump ()

(* ****** ****** *)

implement rt_main () = let
//
val () = fw_init ()
//
(* SysTick: enable | interrupt | processor clock *)
val () = rt_mmio_write (SYST_RVR, SYSCLK_HZ / TICK_HZ - 1u)
val () = rt_mmio_write (SYST_CVR, 0u)
val () = rt_mmio_write (SYST_CSR, 7u)
//
val base = rt_base_tick ()
val hyper = rt_hyperperiod ()
val wrap = rt_wrap_at ()
//
fun pace (target: uint): void =
  if rt_ms_reached (target) then () else (rt_wfi (); pace (target))
//
fun loop (t: int, next: uint): void = let
  val next = next + g0int2uint_int_uint (base)
  val () = pace (next)
  val () = rt_overture_step (t)
  val t = t + base
  val t = (if t >= wrap then t - hyper else t): int
in
  loop (t, next)
end // end of [loop]
//
in
  loop (0, rt_ms_now ())
end // end of [rt_main]

(* ****** ****** *)

(* end of [DATS/rt.dats] *)
