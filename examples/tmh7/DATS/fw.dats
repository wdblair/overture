(*
** DATS/fw.dats -- blink.ovt's hardware bindings in freestanding
** ATS: GPIO E corner LEDs as the actuator, the DWT cycle counter as
** the sensor. Only the volatile loads/stores are C (CATS/fw.cats).
*)

#define ATS_DYNLOADFLAG 0

staload "SATS/fw.sats"

staload _ = "prelude/DATS/integer.dats"
staload _ = "prelude/DATS/bool.dats"

(* ****** ****** *)
(* STM32H743 registers *)

#define RCC_AHB4ENR 0x580244E0u
#define GPIOE_MODER 0x58021000u
#define GPIOE_BSRR  0x58021018u

#define DEMCR      0xE000EDFCu
#define DWT_CTRL   0xE0001000u
#define DWT_CYCCNT 0xE0001004u

#define LED_PINS 0x30u (* PE4 | PE5 *)

(* ****** ****** *)

implement fw_init () = let
//
(* clock GPIOE; PE4/PE5 as outputs *)
val () = fw_mmio_write
  (RCC_AHB4ENR, fw_mmio_read (RCC_AHB4ENR) lor 0x10u)
val _ = fw_mmio_read (RCC_AHB4ENR) (* settle *)
val moder = fw_mmio_read (GPIOE_MODER)
val moder = moder land 0xFFFFF0FFu (* clear mode[4], mode[5] *)
val moder = moder lor 0x500u       (* 01: output, both pins *)
val () = fw_mmio_write (GPIOE_MODER, moder)
//
(* enable the DWT cycle counter *)
val () = fw_mmio_write (DEMCR, fw_mmio_read (DEMCR) lor 0x1000000u)
val () = fw_mmio_write (DWT_CYCCNT, 0u)
val () = fw_mmio_write (DWT_CTRL, fw_mmio_read (DWT_CTRL) lor 1u)
//
in
  // nothing
end // end of [fw_init]

(* ****** ****** *)

implement cycles_sense (t) =
  g0uint2int_uint_int (fw_mmio_read (DWT_CYCCNT))

implement latest (a0) = a0

(* ****** ****** *)
(*
** toggle on every actuation: the 1 s cadence is the schedule's
** guarantee, not arithmetic here (state lives in the CATS floor)
*)

implement led_actuate (t, v) =
  if fw_led_toggle ()
    then fw_mmio_write (GPIOE_BSRR, 0x30u)     (* PE4|PE5 set *)
    else fw_mmio_write (GPIOE_BSRR, 0x300000u) (* PE4|PE5 reset *)
  // end of [if]

(* ****** ****** *)

(* end of [DATS/fw.dats] *)
