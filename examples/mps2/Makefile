#
# heartbeat.ovt -> Overture -> freestanding-ATS harness ->
# arm-none-eabi -> QEMU mps2-an500 (Cortex-M7)
#
# make hil-qemu   hardware in the loop, emulated: build (the patsopt
#                 run that compiles the harness proves the schedule),
#                 boot under QEMU, diff the UART transcript against
#                 heartbeat.expected. Every printed value is a
#                 function of logical time, so the transcript is
#                 exact and the diff is the verdict.
#

OVERTURE ?= ../../src/overture
RT       := ../../rt/mps2
PATSHOME := ../../ATS2-Postiats-int-0.4.2
CONTRIB  := ../..
PATSOPT  := $(PATSHOME)/bin/patsopt

TC       ?= /Applications/ARM/bin
CC       := $(TC)/arm-none-eabi-gcc
SIZE     := $(TC)/arm-none-eabi-size
QEMU     ?= /opt/homebrew/bin/qemu-system-arm

ARCH := -mcpu=cortex-m7 -mthumb -mfloat-abi=soft

CFLAGS := $(ARCH) -std=c99 -O2 -Wall \
          -ffreestanding -fno-builtin -nostdlib

ATS_CFLAGS := $(ARCH) -O2 -Wall \
              -ffreestanding -fno-builtin -nostdlib -nostdinc \
              -D_ATS_CCOMP_PRELUDE_NONE_=1 \
              -D_ATS_CCOMP_HEADER_NONE_=1 \
              -D_ATS_CCOMP_EXCEPTION_NONE_=1 \
              -I$(CONTRIB) -I$(CONTRIB)/contrib/kernelats

ATSFLAGS := -IATS $(CONTRIB)

OBJS := boot.o rt_dats.o hb_dats.o heartbeat_bare_dats.o sched_bare_dats.o

heartbeat_bare.dats: heartbeat.ovt $(OVERTURE)
	$(OVERTURE) --codegen=ats-bare heartbeat.ovt

heartbeat_bare_dats.c: heartbeat_bare.dats
	env PATSHOME=$(PATSHOME) $(PATSOPT) $(ATSFLAGS) -o $@ -d heartbeat_bare.dats

sched_bare_dats.c: heartbeat_bare.dats
	env PATSHOME=$(PATSHOME) $(PATSOPT) $(ATSFLAGS) -o $@ -d overture_sched_bare.dats

rt_dats.c: $(RT)/DATS/rt.dats $(RT)/SATS/rt.sats
	cd $(RT) && env PATSHOME=$(PATSHOME) \
	  $(PATSOPT) $(ATSFLAGS) -o $(CURDIR)/$@ -d DATS/rt.dats

hb_dats.c: DATS/hb.dats SATS/hb.sats
	env PATSHOME=$(PATSHOME) $(PATSOPT) $(ATSFLAGS) -o $@ -d DATS/hb.dats

boot.o: $(RT)/boot.c
	$(CC) $(CFLAGS) -c $< -o $@

rt_dats.o: rt_dats.c
	$(CC) $(ATS_CFLAGS) -I$(RT) -c $< -o $@

hb_dats.o: hb_dats.c
	$(CC) $(ATS_CFLAGS) -I. -c $< -o $@

heartbeat_bare_dats.o: heartbeat_bare_dats.c heartbeat_cells.cats
	$(CC) $(ATS_CFLAGS) -I. -c $< -o $@

sched_bare_dats.o: sched_bare_dats.c
	$(CC) $(ATS_CFLAGS) -I. -c $< -o $@

heartbeat.elf: $(OBJS) $(RT)/mps2.ld
	$(CC) $(ARCH) -nostdlib -T $(RT)/mps2.ld $(OBJS) -o $@ -lgcc

hb: heartbeat.elf
	$(SIZE) heartbeat.elf

# ---- hardware in the loop, emulated; icount decouples virtual
# ---- time from the host clock, so ten virtual seconds run in
# ---- milliseconds and the transcript is reproducible

hil-qemu: heartbeat.elf
	$(QEMU) -M mps2-an500 -kernel heartbeat.elf -nographic \
	  -monitor none -semihosting \
	  -icount shift=0,align=off,sleep=off > transcript.out
	diff transcript.out heartbeat.expected
	@echo "hil-qemu: transcript matches heartbeat.expected"

clean:
	rm -f *.o *_dats.c heartbeat_bare.dats heartbeat_cells.cats \
	      heartbeat_cells.sats heartbeat.elf transcript.out \
	      overture_sched_bare.*

.PHONY: hb hil-qemu clean
