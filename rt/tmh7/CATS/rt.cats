/*
** CATS/rt.cats -- the volatile floor of the thin OS.
** Everything here is an ATSinline MMIO or intrinsic wrapper; all
** logic above it lives in ATS (DATS/rt.dats).
*/

#ifndef OVERTURE_RT_CATS
#define OVERTURE_RT_CATS

/* ****** ****** */

#define RT_REG32(a) (*(volatile unsigned int *)(a))

ATSinline()
atstype_void
rt_mmio_write (atstype_uint addr, atstype_uint v)
{
  RT_REG32(addr) = v;
}

ATSinline()
atstype_uint
rt_mmio_read (atstype_uint addr)
{
  return RT_REG32(addr);
}

ATSinline()
atstype_void
rt_wfi ()
{
  __asm__ volatile ("wfi");
}

/* ****** ****** */
/*
** the millisecond counter: bumped by the SysTick body, consumed by
** the pacing loop; both live in this translation unit
*/

static volatile unsigned int rt_ms_c = 0;

ATSinline()
atstype_void
rt_ms_bump ()
{
  rt_ms_c++;
}

/* has wall time reached [target] (wrap-safe signed compare)? */
ATSinline()
atstype_bool
rt_ms_reached (atstype_uint target)
{
  return ((atstype_int)(rt_ms_c - target)) >= 0;
}

ATSinline()
atstype_uint
rt_ms_now ()
{
  return rt_ms_c;
}

/* ****** ****** */
/*
** the generated Overture harness (compiled with
** -DOVERTURE_FREESTANDING elsewhere in this firmware)
*/

extern void overture_step (int t);
extern const int overture_base_tick;
extern const int overture_hyperperiod;
extern const int overture_wrap_at;

ATSinline()
atstype_void
rt_overture_step (atstype_int t)
{
  overture_step (t);
}

ATSinline() atstype_int rt_base_tick () { return overture_base_tick; }
ATSinline() atstype_int rt_hyperperiod () { return overture_hyperperiod; }
ATSinline() atstype_int rt_wrap_at () { return overture_wrap_at; }

/* ****** ****** */

#endif /* OVERTURE_RT_CATS */
