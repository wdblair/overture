(*
** recdump.dats -- decode a tmh7 flight-recorder dump. Hosted ATS2;
** built by [make recdump].
**
**   dfu-util -a 0 -s 0x24070000:2048:force -U rec.bin
**   ./recdump rec.bin
**
** The dump is raw little-endian 32-bit words: a six-word header
** (magic, count, next, canary, chip id, status), then
** (t, x, y, z) gyro records from offset 0x18.
*)

#include "share/atspre_staload.hats"

#define MAGIC  1331057746 (* 0x4F565452, "RTVO" *)
#define CANARY 1611526157 (* 0x600DF00D *)
#define CHIPID 36         (* 0x24 *)
#define NMAX   126        (* (2048 - 24) / 16 *)

(* ****** ****** *)

fn die (msg: string): void =
  (prerrln! ("recdump: ", msg); $extfcall (void, "exit", 1))

fun rd_byte (inp: FILEref): int = let
  val c = fileref_getc (inp)
in
  if c < 0 then (die ("unexpected end of file"); 0) else c
end // end of [rd_byte]

(* little-endian two's complement: the top byte carries the sign,
   so fold it in signed and every intermediate fits an int *)
fn fold8 (hi: int, lo: int): int = lo + hi * 256

fun rd_i32 (inp: FILEref): int = let
  val b0 = rd_byte (inp)
  val b1 = rd_byte (inp)
  val b2 = rd_byte (inp)
  val b3 = rd_byte (inp)
  val s3 = if b3 >= 128 then b3 - 256 else b3
in
  fold8 (fold8 (fold8 (s3, b2), b1), b0)
end // end of [rd_i32]

fn dps (v: int): double = g0int2float_int_double (v) / 16.4

(* ****** ****** *)

fn dump (inp: FILEref): void = let
//
val magic = rd_i32 (inp)
val count = rd_i32 (inp)
val _next = rd_i32 (inp)
val canary = rd_i32 (inp)
val chipid = rd_i32 (inp)
val status = rd_i32 (inp)
//
val _ = $extfcall (int, "printf", "magic   0x%08x  %s\n",
  magic, (if magic = MAGIC then "ok" else "BAD"): string)
val _ = $extfcall (int, "printf", "count   %d\n", count)
val _ = $extfcall (int, "printf", "canary  0x%08x  %s\n",
  canary, (if canary = CANARY then "ok" else "CLOBBERED"): string)
val _ = $extfcall (int, "printf", "chip id %#04x      %s\n",
  chipid, (if chipid = CHIPID then "ok (BMI270)" else "BAD"): string)
val _ = $extfcall (int, "printf", "status  %#04x      %s\n",
  status,
  (if status mod 16 = 1 then "ok (init complete)" else "INIT FAILED"): string)
//
val n = if count < NMAX then count else NMAX
//
fun loop
  (inp: FILEref, i: int, n: int,
   mnx: int, mxx: int, sx: int,
   mny: int, mxy: int, sy: int,
   mnz: int, mxz: int, sz: int): void =
  if i >= n then (
    if n > 0 then let
      val nd = g0int2float_int_double (n)
      val _ = $extfcall (int, "printf",
        "%d entries: x in [%d, %d] mean %+.2f dps; y in [%d, %d] mean %+.2f dps; z in [%d, %d] mean %+.2f dps\n",
        n, mnx, mxx, g0int2float_int_double (sx) / nd / 16.4,
        mny, mxy, g0int2float_int_double (sy) / nd / 16.4,
        mnz, mxz, g0int2float_int_double (sz) / nd / 16.4)
    in end
  ) else let
    val t = rd_i32 (inp)
    val x = rd_i32 (inp)
    val y = rd_i32 (inp)
    val z = rd_i32 (inp)
    val () =
      if (i < 5) orelse (i >= n - 2) then
        ignoret ($extfcall (int, "printf",
          "[%3d] t=%6d  gyro={%6d, %6d, %6d}  (%+7.2f, %+7.2f, %+7.2f dps)\n",
          i, t, x, y, z, dps (x), dps (y), dps (z)))
      else if i = 5 then ignoret ($extfcall (int, "printf", "  ...\n"))
      else ()
    val mnx = if x < mnx then x else mnx
    val mxx = if x > mxx then x else mxx
    val mny = if y < mny then y else mny
    val mxy = if y > mxy then y else mxy
    val mnz = if z < mnz then z else mnz
    val mxz = if z > mxz then z else mxz
  in
    loop (inp, i + 1, n, mnx, mxx, sx + x,
          mny, mxy, sy + y, mnz, mxz, sz + z)
  end // end of [loop]
//
in
  loop (inp, 0, n,
    2147483647, ~2147483647, 0,
    2147483647, ~2147483647, 0,
    2147483647, ~2147483647, 0)
end // end of [dump]

(* ****** ****** *)

implement main0 (argc, argv) =
  if argc >= 2 then let
    val opt = fileref_open_opt (argv[1], file_mode_r)
  in
    case+ opt of
    | ~None_vt () => die ("cannot open the dump file")
    | ~Some_vt (inp) => (dump (inp); fileref_close (inp))
  end else
    prerrln! ("usage: recdump <dump.bin>")
  // end of [if]

(* end of [recdump.dats] *)
