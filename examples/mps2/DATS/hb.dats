(*
** DATS/hb.dats -- heartbeat.ovt's bindings in freestanding ATS:
** a software counter as the sensor, the CMSDK APB UART as the
** actuator surface. Only the volatile loads/stores and the
** semihosting exit are C (CATS/hb.cats).
*)

#define ATS_DYNLOADFLAG 0

staload "SATS/hb.sats"

staload _ = "prelude/DATS/integer.dats"
staload _ = "prelude/DATS/bool.dats"

(* ****** ****** *)
(* MPS2 CMSDK APB UART 0 *)

#define UART_DATA    0x40004000u
#define UART_STATE   0x40004004u (* bit 0: TX buffer full *)
#define UART_CTRL    0x40004008u (* bit 0: TX enable *)
#define UART_BAUDDIV 0x40004010u

#define SLOW_BEATS 10

(* ****** ****** *)

fn putc (c: int): void = let
  fun wait (): void =
    if g0uint2int_uint_int (hb_mmio_read (UART_STATE) land 0x1u) = 0
      then () else wait ()
  val () = wait ()
in
  hb_mmio_write (UART_DATA, g0int2uint_int_uint (c))
end // end of [putc]

(* decimal, no allocation: high digits by recursion *)
fun put_int (n: int): void =
  if n < 0 then (putc (45(*'-'*)); put_int (~n))
  else let
    val () = if n >= 10 then put_int (n / 10)
  in
    putc (48(*'0'*) + n mod 10)
  end // end of [put_int]

(* one transcript line: <name> t=<t> v=<v> *)
fn put_line (c0: int, c1: int, c2: int, c3: int, t: int, v: int): void = let
  val () = putc (c0)
  val () = putc (c1)
  val () = putc (c2)
  val () = putc (c3)
  val () = putc (32) (* ' ' *)
  val () = putc (116) (* 't' *)
  val () = putc (61) (* '=' *)
  val () = put_int (t)
  val () = putc (32)
  val () = putc (118) (* 'v' *)
  val () = putc (61)
  val () = put_int (v)
in
  putc (10) (* '\n' *)
end // end of [put_line]

(* ****** ****** *)

implement fw_init () = let
  val () = hb_mmio_write (UART_BAUDDIV, 16u)
  val () = hb_mmio_write (UART_CTRL, 0x1u) (* TX enable *)
in
  // nothing
end // end of [fw_init]

implement counter_sense (t) = hb_count_next ()

implement latest (a0) = a0

implement fast_actuate (t, v) =
  put_line (102, 97, 115, 116, t, v) (* "fast" *)

implement mid_actuate (t, v) =
  put_line (109, 105, 100, 32, t, v) (* "mid " *)

(* the tenth slow beat ends the run: QEMU exits, make diffs *)
implement slow_actuate (t, v) = let
  val () = put_line (115, 108, 111, 119, t, v) (* "slow" *)
in
  if hb_beat_next () >= SLOW_BEATS then hb_exit () else ()
end // end of [slow_actuate]

(* ****** ****** *)

(* end of [DATS/hb.dats] *)
