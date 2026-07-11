(*
** SATS/imu.sats -- the hardware seam of imu.ovt, freestanding ATS.
** Bit-banged SPI on the BMI270 pins (PA4 CS, PA5 SCK, PA6 MISO,
** PA7 MOSI), the AXI-SRAM flight recorder, and the jump back into
** the ROM bootloader that ends a recording run.
**
** vec3 mirrors the Overture typedef; records cross this seam by
** reference (ATS &), which the generated harness and the C floor
** both see as a pointer to the struct.
*)

#include "contrib/kernelats/prelude/staloadall.hats"

%{#
#include "contrib/kernelats/ccomp/pats_ccomp.h"
#include "CATS/imu.cats"
#include "prelude/CATS/integer.cats"
#include "prelude/CATS/bool.cats"
%}

typedef vec3 = @{x= int, y= int, z= int}

(* the volatile floor (CATS/imu.cats) *)

fun imu_mmio_write (addr: uint, v: uint): void = "ext#"
fun imu_mmio_read (addr: uint): uint = "ext#"
fun imu_led_toggle (): bool = "ext#"
fun imu_dfu_jump (): void = "ext#"
fun bmi270_blob_byte (i: int): int = "ext#"

(* the seam, implemented in ATS (DATS/imu.dats) *)
fun fw_init (): void = "ext#fw_init"
fun gyro_sense (t: int, v: &vec3? >> vec3): void = "ext#gyro_sense"
fun latest (i: &vec3, r0: &vec3? >> vec3): void = "ext#latest"
fun magnitude (i: &vec3): int = "ext#magnitude"
fun rec_actuate (t: int, v: &vec3): void = "ext#rec_actuate"
fun led_actuate (t: int, v: int): void = "ext#led_actuate"

(* ****** ****** *)

(* end of [SATS/imu.sats] *)
