/*
** CATS/fw.cats -- volatile floor for the blink firmware:
** GPIO E (corner LEDs via the LED2/LED3 pads) and the DWT cycle
** counter. Logic lives in DATS/fw.dats.
*/

#ifndef OVERTURE_FW_CATS
#define OVERTURE_FW_CATS

#define FW_REG32(a) (*(volatile unsigned int *)(a))

ATSinline()
atstype_void
fw_mmio_write (atstype_uint addr, atstype_uint v)
{
  FW_REG32(addr) = v;
}

ATSinline()
atstype_uint
fw_mmio_read (atstype_uint addr)
{
  return FW_REG32(addr);
}

/* the LED toggle state: flipped on every actuation */
static unsigned int fw_led_state = 0;

ATSinline()
atstype_bool
fw_led_toggle ()
{
  fw_led_state ^= 1u;
  return (fw_led_state != 0u);
}

#endif /* OVERTURE_FW_CATS */
