/*
** CATS/imu.cats -- volatile floor for the IMU-recorder firmware:
** raw MMIO, the LED toggle state, and the jump into the STM32H743
** ROM bootloader that ends a recording run. Logic lives in
** DATS/imu.dats.
*/

#ifndef OVERTURE_IMU_CATS
#define OVERTURE_IMU_CATS

#define IMU_REG32(a) (*(volatile unsigned int *)(a))

ATSinline()
atstype_void
imu_mmio_write (atstype_uint addr, atstype_uint v)
{
  IMU_REG32(addr) = v;
}

ATSinline()
atstype_uint
imu_mmio_read (atstype_uint addr)
{
  return IMU_REG32(addr);
}

/* the Bosch configuration file (bmi270_blob.c) */
extern const unsigned char bmi270_blob[328];

ATSinline()
atstype_int
bmi270_blob_byte (atstype_int i)
{
  return (atstype_int)bmi270_blob[i];
}

/* the LED toggle state: flipped on every actuation */
static unsigned int imu_led_state = 0;

ATSinline()
atstype_bool
imu_led_toggle ()
{
  imu_led_state ^= 1u;
  return (imu_led_state != 0u);
}

/*
** into the ROM bootloader (system memory at 0x1FF09800) without a
** reset, so the AXI SRAM -- and the flight recorder in it -- stays
** warm for DFU readback. SysTick is silenced first; PRIMASK is left
** clear because the bootloader relies on interrupts.
*/
ATSinline()
atstype_void
imu_dfu_jump ()
{
  IMU_REG32(0xE000E010u) = 0u;          /* SysTick off */
  IMU_REG32(0xE000ED04u) = (1u << 25);  /* clear pending SysTick */
  {
    unsigned int sp = IMU_REG32(0x1FF09800u);
    unsigned int pc = IMU_REG32(0x1FF09804u);
    IMU_REG32(0xE000ED08u) = 0x1FF09800u; /* VTOR -> system memory */
    __asm volatile ("msr msp, %0 \n bx %1" : : "r"(sp), "r"(pc));
  }
  for (;;) ;
}

#endif /* OVERTURE_IMU_CATS */
