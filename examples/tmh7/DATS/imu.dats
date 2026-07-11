(*
** DATS/imu.dats -- imu.ovt's hardware bindings in freestanding ATS:
** a bit-banged SPI master on the BMI270's pins, the AXI-SRAM flight
** recorder, and the corner LEDs. Only the volatile loads/stores and
** the bootloader jump are C (CATS/imu.cats).
*)

#define ATS_DYNLOADFLAG 0

staload "SATS/imu.sats"

staload _ = "prelude/DATS/integer.dats"
staload _ = "prelude/DATS/bool.dats"

(* ****** ****** *)
(* STM32H743 registers *)

#define RCC_AHB4ENR 0x580244E0u

#define GPIOA_MODER 0x58020000u
#define GPIOA_IDR   0x58020010u
#define GPIOA_BSRR  0x58020018u
#define GPIOE_MODER 0x58021000u
#define GPIOE_BSRR  0x58021018u

#define DEMCR      0xE000EDFCu
#define DWT_CTRL   0xE0001000u
#define DWT_CYCCNT 0xE0001004u

(* SPI lines: PA4 CS, PA5 SCK, PA6 MISO, PA7 MOSI (mode 0) *)
#define CS_HI   0x10u
#define CS_LO   0x100000u
#define SCK_HI  0x20u
#define SCK_LO  0x200000u
#define MOSI_HI 0x80u
#define MOSI_LO 0x800000u
#define MISO_IN 0x40u

#define LED_PINS 0x30u (* PE4 | PE5 *)

(* the flight recorder: header + (t, value) pairs in AXI SRAM *)
#define REC_MAGIC   0x24070000u
#define REC_COUNT   0x24070004u
#define REC_NEXT    0x24070008u
#define REC_CANARY  0x2407000Cu
#define REC_CHIPID  0x24070010u (* diagnostics: chip id read at boot *)
#define REC_STATUS  0x24070014u (* diagnostics: INTERNAL_STATUS after init *)
#define REC_FIRST   0x24070018u
#define MAGIC_V     0x4F565452u (* "RTVO" little-endian *)
#define CANARY_V    0x600DF00Du
#define REC_LIMIT   100

(* half a bit period, in 64 MHz cycles: ~500 kHz SCK *)
#define HALF_BIT 64

(* ****** ****** *)

fn imu_delay (n: int): void = let
  val s = g0uint2int_uint_int (imu_mmio_read (DWT_CYCCNT))
  fun loop (s: int, n: int): void = let
    val now = g0uint2int_uint_int (imu_mmio_read (DWT_CYCCNT))
  in
    if now - s < n then loop (s, n) else ()
  end // end of [loop]
in
  loop (s, n)
end // end of [imu_delay]

(* ****** ****** *)
(* one full-duplex byte, MSB first, mode 0 *)

fn spi_xfer (out: uint): int = let
  fun bit (out: uint, mask: uint, acc: int): int =
    if g0uint2int_uint_int (mask) = 0 then acc
    else let
      val () =
        if g0uint2int_uint_int (out land mask) = 0
          then imu_mmio_write (GPIOA_BSRR, MOSI_LO)
          else imu_mmio_write (GPIOA_BSRR, MOSI_HI)
      val () = imu_delay (HALF_BIT)
      val () = imu_mmio_write (GPIOA_BSRR, SCK_HI)
      val miso = imu_mmio_read (GPIOA_IDR)
      val b = g0uint2int_uint_int (miso land MISO_IN)
      val acc = if b = 0 then acc + acc else acc + acc + 1
      val () = imu_delay (HALF_BIT)
      val () = imu_mmio_write (GPIOA_BSRR, SCK_LO)
    in
      bit (out, mask >> 1, acc)
    end // end of [if]
in
  bit (out, 0x80u, 0)
end // end of [spi_xfer]

(* a BMI270 register read: address, one dummy byte, then data *)
fn bmi_read (reg: uint): int = let
  val () = imu_mmio_write (GPIOA_BSRR, CS_LO)
  val () = imu_delay (HALF_BIT)
  val _ = spi_xfer (reg lor 0x80u)
  val _ = spi_xfer (0u)
  val v = spi_xfer (0u)
  val () = imu_delay (HALF_BIT)
  val () = imu_mmio_write (GPIOA_BSRR, CS_HI)
in
  v
end // end of [bmi_read]

(* sign-extend a little-endian 16-bit pair *)
fn sx16 (lo: int, hi: int): int = let
  val v = hi * 256 + lo
in
  if v >= 32768 then v - 65536 else v
end // end of [sx16]

(* one burst over GYR_X..GYR_Z (0x12..0x17): six data bytes *)
fn bmi_read_gyr (v: &vec3? >> vec3): void = let
  val () = imu_mmio_write (GPIOA_BSRR, CS_LO)
  val () = imu_delay (HALF_BIT)
  val _ = spi_xfer (0x12u lor 0x80u)
  val _ = spi_xfer (0u) (* dummy *)
  val xl = spi_xfer (0u)
  val xh = spi_xfer (0u)
  val yl = spi_xfer (0u)
  val yh = spi_xfer (0u)
  val zl = spi_xfer (0u)
  val zh = spi_xfer (0u)
  val () = imu_delay (HALF_BIT)
  val () = imu_mmio_write (GPIOA_BSRR, CS_HI)
in
  v := @{x= sx16 (xl, xh), y= sx16 (yl, yh), z= sx16 (zl, zh)}
end // end of [bmi_read_gyr]

fn bmi_write (reg: uint, v: uint): void = let
  val () = imu_mmio_write (GPIOA_BSRR, CS_LO)
  val () = imu_delay (HALF_BIT)
  val _ = spi_xfer (reg)
  val _ = spi_xfer (v)
  val () = imu_delay (HALF_BIT)
  val () = imu_mmio_write (GPIOA_BSRR, CS_HI)
in
  // nothing
end // end of [bmi_write]

(* the configuration file, burst-written to INIT_DATA (0x5E) *)
fn bmi_load_blob (): void = let
  val () = imu_mmio_write (GPIOA_BSRR, CS_LO)
  val () = imu_delay (HALF_BIT)
  val _ = spi_xfer (0x5Eu)
  fun loop (i: int): void =
    if i < 328 then let
      val _ = spi_xfer (g0int2uint_int_uint (bmi270_blob_byte (i)))
    in
      loop (i + 1)
    end // end of [if]
  val () = loop (0)
  val () = imu_delay (HALF_BIT)
  val () = imu_mmio_write (GPIOA_BSRR, CS_HI)
in
  // nothing
end // end of [bmi_load_blob]

(* ****** ****** *)

implement fw_init () = let
//
(* clock GPIOA and GPIOE *)
val () = imu_mmio_write
  (RCC_AHB4ENR, imu_mmio_read (RCC_AHB4ENR) lor 0x11u)
val _ = imu_mmio_read (RCC_AHB4ENR) (* settle *)
//
(* enable the DWT cycle counter (the delay timebase) *)
val () = imu_mmio_write (DEMCR, imu_mmio_read (DEMCR) lor 0x1000000u)
val () = imu_mmio_write (DWT_CYCCNT, 0u)
val () = imu_mmio_write (DWT_CTRL, imu_mmio_read (DWT_CTRL) lor 1u)
//
(* idle levels first: CS high, SCK and MOSI low *)
val () = imu_mmio_write (GPIOA_BSRR, CS_HI)
val () = imu_mmio_write (GPIOA_BSRR, 0xA00000u) (* PA5|PA7 reset *)
//
(* PA4/PA5/PA7 outputs, PA6 input *)
val m = imu_mmio_read (GPIOA_MODER)
val m = m land 0xFFFF00FFu
val m = m lor 0x4500u
val () = imu_mmio_write (GPIOA_MODER, m)
//
(* PE4/PE5 as outputs (corner LEDs) *)
val moder = imu_mmio_read (GPIOE_MODER)
val moder = moder land 0xFFFFF0FFu
val moder = moder lor 0x500u
val () = imu_mmio_write (GPIOE_MODER, moder)
//
(* the flight recorder header *)
val () = imu_mmio_write (REC_MAGIC, MAGIC_V)
val () = imu_mmio_write (REC_COUNT, 0u)
val () = imu_mmio_write (REC_NEXT, REC_FIRST)
val () = imu_mmio_write (REC_CANARY, CANARY_V)
//
(* BMI270 into SPI mode: a CS pulse, then a throwaway read *)
val () = imu_delay (6400) (* 100 us *)
val () = imu_mmio_write (GPIOA_BSRR, CS_LO)
val () = imu_delay (6400)
val () = imu_mmio_write (GPIOA_BSRR, CS_HI)
val () = imu_delay (6400)
val _ = bmi_read (0u)
//
(* known state whatever ran before: soft reset (drops to I2C
   mode, so pulse CS again), then the Bosch init dance *)
val () = bmi_write (0x7Eu, 0xB6u)   (* CMD: softreset *)
val () = imu_delay (6400000)        (* 100 ms *)
val () = imu_mmio_write (GPIOA_BSRR, CS_LO)
val () = imu_delay (6400)
val () = imu_mmio_write (GPIOA_BSRR, CS_HI)
val () = imu_delay (6400)
val _ = bmi_read (0u)               (* throwaway: completes the mode switch *)
val id = bmi_read (0u)              (* chip id: 0x24 *)
val () = bmi_write (0x7Cu, 0u)      (* PWR_CONF: power save off *)
val () = imu_delay (64000)          (* 1 ms *)
val () = bmi_write (0x59u, 0u)      (* INIT_CTRL: begin *)
val () = bmi_load_blob ()
val () = bmi_write (0x59u, 1u)      (* INIT_CTRL: done *)
val () = imu_delay (9600000)        (* 150 ms *)
val st = bmi_read (0x21u)           (* INTERNAL_STATUS: 1 = init ok *)
val () = bmi_write (0x7Du, 0x0Eu)   (* PWR_CTRL: temp|gyr|acc on *)
val () = imu_delay (3200000)        (* 50 ms: first samples *)
//
(* boot diagnostics into the recorder header *)
val () = imu_mmio_write (REC_CHIPID, g0int2uint_int_uint (id))
val () = imu_mmio_write (REC_STATUS, g0int2uint_int_uint (st))
//
in
  // nothing
end // end of [fw_init]

(* ****** ****** *)

(* all three axes in one burst; +-2000 dps is 16.4 LSB/dps *)
implement gyro_sense (t, v) = bmi_read_gyr (v)

implement latest (i, r0) = r0 := i

(* running mean of everything seen; the final firing's output is
   the bias that [hold] serves forever *)
implement calibrate (i, r0) = let
  val () = imu_cal_add (i.x, i.y, i.z)
in
  r0 := @{x= imu_cal_x (), y= imu_cal_y (), z= imu_cal_z ()}
end // end of [calibrate]

implement subtract (a, b, r0) =
  r0 := @{x= a.x - b.x, y= a.y - b.y, z= a.z - b.z}

(* manhattan magnitude: cheap, monotone, sign-free *)
implement magnitude (i) = let
  val ax = (if i.x < 0 then ~(i.x) else i.x): int
  val ay = (if i.y < 0 then ~(i.y) else i.y): int
  val az = (if i.z < 0 then ~(i.z) else i.z): int
in
  ax + ay + az
end // end of [magnitude]

(*
** append (t, x, y, z); the 100th record ends the run by
** re-entering the ROM bootloader -- the actuator lands the flight
*)
implement rec_actuate (t, v) = let
  val n = g0uint2int_uint_int (imu_mmio_read (REC_COUNT))
in
  if n >= REC_LIMIT then () else let
    val a = imu_mmio_read (REC_NEXT)
    val () = imu_mmio_write (a, g0int2uint_int_uint (t))
    val () = imu_mmio_write (a + 4u, g0int2uint_int_uint (v.x))
    val () = imu_mmio_write (a + 8u, g0int2uint_int_uint (v.y))
    val () = imu_mmio_write (a + 12u, g0int2uint_int_uint (v.z))
    val () = imu_mmio_write (REC_NEXT, a + 16u)
    val n1 = n + 1
    val () = imu_mmio_write (REC_COUNT, g0int2uint_int_uint (n1))
  in
    if n1 >= REC_LIMIT then imu_dfu_jump () else ()
  end // end of [if]
end // end of [rec_actuate]

implement led_actuate (t, v) =
  if imu_led_toggle ()
    then imu_mmio_write (GPIOE_BSRR, LED_PINS)
    else imu_mmio_write (GPIOE_BSRR, 0x300000u)
  // end of [if]

(* ****** ****** *)

(* end of [DATS/imu.dats] *)
