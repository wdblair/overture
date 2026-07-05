/*
** CATS/hb.cats -- volatile floor for the heartbeat firmware: raw
** MMIO, the software counter, the slow-beat tally, and the
** semihosting exit that ends a run. Logic lives in DATS/hb.dats.
*/

#ifndef OVERTURE_HB_CATS
#define OVERTURE_HB_CATS

#define HB_REG32(a) (*(volatile unsigned int *)(a))

ATSinline()
atstype_void
hb_mmio_write (atstype_uint addr, atstype_uint v)
{
  HB_REG32(addr) = v;
}

ATSinline()
atstype_uint
hb_mmio_read (atstype_uint addr)
{
  return HB_REG32(addr);
}

/* the fast counter: bumped on every sense */
static unsigned int hb_count = 0;

ATSinline()
atstype_int
hb_count_next ()
{
  return (atstype_int)(++hb_count);
}

/* the slow-beat tally: the tenth beat ends the run */
static unsigned int hb_beats = 0;

ATSinline()
atstype_int
hb_beat_next ()
{
  return (atstype_int)(++hb_beats);
}

/* semihosting SYS_EXIT: QEMU (run with -semihosting) terminates */
ATSinline()
atstype_void
hb_exit ()
{
  register unsigned int r0 __asm__("r0") = 0x18u;
  register unsigned int r1 __asm__("r1") = 0x20026u;
  __asm__ volatile ("bkpt 0xAB" : : "r"(r0), "r"(r1));
  for (;;) ;
}

#endif /* OVERTURE_HB_CATS */
