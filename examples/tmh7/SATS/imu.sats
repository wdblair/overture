(*
** SATS/imu.sats -- the hardware seam of imu.ovt, freestanding ATS.
** Bit-banged SPI on the BMI270 pins (PA4 CS, PA5 SCK, PA6 MISO,
** PA7 MOSI), the AXI-SRAM flight recorder, and the jump back into
** the ROM bootloader that ends a recording run.
*)

#include "contrib/kernelats/prelude/staloadall.hats"

%{#
#include "contrib/kernelats/ccomp/pats_ccomp.h"
#include "CATS/imu.cats"
#include "prelude/CATS/integer.cats"
#include "prelude/CATS/bool.cats"
%}

(* the volatile floor (CATS/imu.cats) *)

fun imu_mmio_write (addr: uint, v: uint): void = "ext#"
fun imu_mmio_read (addr: uint): uint = "ext#"
fun imu_led_toggle (): bool = "ext#"
fun imu_dfu_jump (): void = "ext#"
fun bmi270_blob_byte (i: int): int = "ext#"

(* the seam, implemented in ATS (DATS/imu.dats) *)
fun fw_init (): void = "ext#fw_init"
fun gyro_sense (t: int): int = "ext#gyro_sense"
fun latest (a0: int): int = "ext#latest"
fun rec_actuate (t: int, v: int): void = "ext#rec_actuate"
fun led_actuate (t: int, v: int): void = "ext#led_actuate"

(* ****** ****** *)

(* end of [SATS/imu.sats] *)
