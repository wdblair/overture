/*
 * tmh7.ld -- STM32H743 (T-Motor MINI H7 flight controller)
 *
 * Flash bank 1 at 0x08000000 (2 MB). The 128 KB DTCM holds stack,
 * data, and bss; the 512 KB AXI SRAM is reserved for the flight
 * recorder (survives the self-jump into the ROM bootloader, which
 * is how logs are read back over DFU without a debugger).
 */

OUTPUT_FORMAT("elf32-littlearm")
OUTPUT_ARCH(arm)
ENTRY(Reset_Handler)

MEMORY
{
    FLASH  (rx)  : ORIGIN = 0x08000000, LENGTH = 2M
    DTCM   (rwx) : ORIGIN = 0x20000000, LENGTH = 128K
    AXIRAM (rw)  : ORIGIN = 0x24000000, LENGTH = 512K
}

_estack = ORIGIN(DTCM) + LENGTH(DTCM);

SECTIONS
{
    .isr_vector : {
        KEEP(*(.isr_vector))
    } > FLASH

    .text : {
        *(.text*)
        *(.rodata*)
        . = ALIGN(4);
    } > FLASH

    _sidata = LOADADDR(.data);

    .data : {
        . = ALIGN(4);
        _sdata = .;
        *(.data*)
        . = ALIGN(4);
        _edata = .;
    } > DTCM AT> FLASH

    .bss : {
        . = ALIGN(4);
        _sbss = .;
        *(.bss*)
        *(COMMON)
        . = ALIGN(4);
        _ebss = .;
    } > DTCM

    /* the flight recorder: never loaded, never zeroed by us */
    .recorder (NOLOAD) : {
        _recorder_start = .;
        . += LENGTH(AXIRAM);
        _recorder_end = .;
    } > AXIRAM

    /DISCARD/ : {
        *(.comment)
        *(.note*)
        *(.ARM.exidx*)
        *(.ARM.attributes)
    }
}
