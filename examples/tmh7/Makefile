#
# blink.ovt -> Overture -> C harness + freestanding-ATS runtime and
# stubs -> arm-none-eabi -> T-Motor MINI H7
#
# make          certify (host patsopt proves the schedule) + build
# make flash    flash over USB DFU (hold BOOT, replug, then run)
# make hil      hardware in the loop: flash imu.ovt, fly the 10 s
#               recording, read the log back over DFU, decode
#
# The runtime logic and hardware stubs are ATS (kernelats recipe);
# only volatile MMIO floors (CATS/) and the boot path
# (rt/tmh7/boot.c) are C.
#

OVERTURE ?= ../../src/overture
RT       := ../../rt/tmh7
PATSHOME := ../../ATS2-Postiats-int-0.4.2
CONTRIB  := ../..
PATSOPT  := $(PATSHOME)/bin/patsopt

TC       ?= /Applications/ARM/bin
CC       := $(TC)/arm-none-eabi-gcc
OBJCOPY  := $(TC)/arm-none-eabi-objcopy
SIZE     := $(TC)/arm-none-eabi-size
DFU      ?= /opt/homebrew/bin/dfu-util

ARCH := -mcpu=cortex-m7 -mthumb -mfloat-abi=soft

# plain C: the boot path and the generated harness
CFLAGS := $(ARCH) -std=c99 -O2 -Wall \
          -ffreestanding -fno-builtin -nostdlib \
          -DOVERTURE_FREESTANDING -I$(RT)

# C generated from freestanding ATS: the kernelats recipe
ATS_CFLAGS := $(ARCH) -O2 -Wall \
              -ffreestanding -fno-builtin -nostdlib -nostdinc \
              -D_ATS_CCOMP_PRELUDE_NONE_=1 \
              -D_ATS_CCOMP_HEADER_NONE_=1 \
              -D_ATS_CCOMP_EXCEPTION_NONE_=1 \
              -I$(CONTRIB) -I$(CONTRIB)/contrib/kernelats

ATSFLAGS := -IATS $(CONTRIB)

OBJS := boot.o rt_dats.o blink_gen.o fw_dats.o

all: certify blink.bin
	$(SIZE) blink.elf

# ---- the schedule certificate, proven on the host ----
# --codegen=ats emits the harness whose typechecking (by patsopt)
# discharges the per-slot capacity proofs; the C artifact that flies
# carries the same ground numbers.

certify: blink.ovt
	env PATSHOME=$(PATSHOME) $(OVERTURE) --codegen=ats blink.ovt > /dev/null
	env PATSHOME=$(PATSHOME) $(PATSOPT) -tc -d blink_gen.dats > /dev/null
	@echo "certify: the schedule of blink.ovt is feasible"

# ---- code generation ----

blink_gen.c: blink.ovt $(OVERTURE)
	$(OVERTURE) --codegen=c blink.ovt

# rt/tmh7 and examples/tmh7 sit at equal depth, so the relative
# PATSHOME/CONTRIB paths hold from either directory
rt_dats.c: $(RT)/DATS/rt.dats $(RT)/SATS/rt.sats
	cd $(RT) && env PATSHOME=$(PATSHOME) \
	  $(PATSOPT) $(ATSFLAGS) -o $(CURDIR)/$@ -d DATS/rt.dats

fw_dats.c: DATS/fw.dats SATS/fw.sats
	env PATSHOME=$(PATSHOME) $(PATSOPT) $(ATSFLAGS) -o $@ -d DATS/fw.dats

# ---- compilation ----

boot.o: $(RT)/boot.c
	$(CC) $(CFLAGS) -c $< -o $@

blink_gen.o: blink_gen.c
	$(CC) $(CFLAGS) -c $< -o $@

rt_dats.o: rt_dats.c
	$(CC) $(ATS_CFLAGS) -I$(RT) -c $< -o $@

fw_dats.o: fw_dats.c
	$(CC) $(ATS_CFLAGS) -I. -c $< -o $@

blink.elf: $(OBJS) $(RT)/tmh7.ld
	$(CC) $(ARCH) -nostdlib -T $(RT)/tmh7.ld $(OBJS) -o $@ -lgcc

blink.bin: blink.elf
	$(OBJCOPY) -O binary $< $@

# ---- the ats-bare variant: the harness itself is freestanding ATS;
# ---- the patsopt run below IS the certificate check for this build

BARE_OBJS := boot.o rt_dats.o fw_dats.o blink_bare_dats.o sched_bare_dats.o

blink_bare.dats: blink.ovt $(OVERTURE)
	$(OVERTURE) --codegen=ats-bare blink.ovt

blink_bare_dats.c: blink_bare.dats
	env PATSHOME=$(PATSHOME) $(PATSOPT) $(ATSFLAGS) -o $@ -d blink_bare.dats

sched_bare_dats.c: blink_bare.dats
	env PATSHOME=$(PATSHOME) $(PATSOPT) $(ATSFLAGS) -o $@ -d overture_sched_bare.dats

blink_bare_dats.o: blink_bare_dats.c blink_cells.cats
	$(CC) $(ATS_CFLAGS) -I. -c $< -o $@

sched_bare_dats.o: sched_bare_dats.c
	$(CC) $(ATS_CFLAGS) -I. -c $< -o $@

blink-ats.elf: $(BARE_OBJS) $(RT)/tmh7.ld
	$(CC) $(ARCH) -nostdlib -T $(RT)/tmh7.ld $(BARE_OBJS) -o $@ -lgcc

blink-ats.bin: blink-ats.elf
	$(OBJCOPY) -O binary $< $@

ats: blink-ats.bin
	$(SIZE) blink-ats.elf

flash-ats: blink-ats.bin
	$(DFU) -a 0 -s 0x08000000:leave -D blink-ats.bin

flash: blink.bin
	$(DFU) -a 0 -s 0x08000000:leave -D blink.bin

# ---- imu.ovt: BMI270 gyro + AXI flight recorder, ATS harness;
# ---- the run ends in the ROM bootloader, then (:force lets
# ---- dfu-util read outside the alt's flash-only memory map):
# ----   dfu-util -a 0 -s 0x24070000:1024:force -U rec.bin
# ----   make recdump && ./recdump rec.bin

recdump: recdump.dats
	env PATSHOME=$(PATSHOME) PATH="$(PATSHOME)/bin:$$PATH" \
	  $(PATSHOME)/bin/patscc -O2 -DATS_MEMALLOC_LIBC -o $@ $< \
	  -L$(PATSHOME)/ccomp/atslib/lib -latslib

IMU_OBJS := boot.o rt_dats.o imu_dats.o bmi270_blob.o \
            imu_bare_dats.o imu_sched_bare_dats.o

bmi270_blob.o: bmi270_blob.c
	$(CC) $(CFLAGS) -c $< -o $@

imu_bare.dats: imu.ovt $(OVERTURE)
	$(OVERTURE) --codegen=ats-bare imu.ovt

imu_bare_dats.c: imu_bare.dats
	env PATSHOME=$(PATSHOME) $(PATSOPT) $(ATSFLAGS) -o $@ -d imu_bare.dats

imu_sched_bare_dats.c: imu_bare.dats
	env PATSHOME=$(PATSHOME) $(PATSOPT) $(ATSFLAGS) -o $@ -d overture_sched_bare.dats

imu_dats.c: DATS/imu.dats SATS/imu.sats
	env PATSHOME=$(PATSHOME) $(PATSOPT) $(ATSFLAGS) -o $@ -d DATS/imu.dats

imu_dats.o: imu_dats.c
	$(CC) $(ATS_CFLAGS) -I. -c $< -o $@

imu_bare_dats.o: imu_bare_dats.c imu_cells.cats
	$(CC) $(ATS_CFLAGS) -I. -c $< -o $@

imu_sched_bare_dats.o: imu_sched_bare_dats.c
	$(CC) $(ATS_CFLAGS) -I. -c $< -o $@

imu-ats.elf: $(IMU_OBJS) $(RT)/tmh7.ld
	$(CC) $(ARCH) -nostdlib -T $(RT)/tmh7.ld $(IMU_OBJS) -o $@ -lgcc

imu-ats.bin: imu-ats.elf
	$(OBJCOPY) -O binary $< $@

imu: imu-ats.bin
	$(SIZE) imu-ats.elf

flash-imu: imu-ats.bin
	$(DFU) -a 0 -s 0x08000000:leave -D imu-ats.bin

# ---- hardware in the loop: flash the IMU experiment, let it fly,
# ---- read the recorder back over DFU, decode. The board must
# ---- start in DFU (hold BOOT, replug); it lands back in DFU by
# ---- itself when the recording ends, so hil runs are repeatable
# ---- without touching the board. (The get_status error after
# ---- :leave is benign, hence the tolerant flash line.)

hil: imu-ats.bin recdump
	$(DFU) -a 0 -s 0x08000000:leave -D imu-ats.bin || true
	@echo "hil: flying -- 10 s of recording, 1 Hz LED heartbeat"
	sleep 20
	rm -f rec.bin
	$(DFU) -a 0 -s 0x24070000:1024:force -U rec.bin
	./recdump rec.bin

clean:
	rm -f *.o *_dats.c blink_gen.* blink_stubs.* blink_bare.dats \
	      blink_cells.cats blink_cells.sats blink.elf blink.bin \
	      blink-ats.elf blink-ats.bin imu_bare.dats imu_cells.cats \
	      imu_cells.sats imu-ats.elf imu-ats.bin recdump rec.bin \
	      overture_sched.sats overture_sched.dats overture_sched_bare.*

.PHONY: all ats certify flash flash-ats imu flash-imu hil clean
