/*
 * mps2.ld -- ARM MPS2-AN500 (Cortex-M7) as modelled by QEMU.
 *
 * Code (and the reset vector table) in ZBT SSRAM1 at 0x00000000;
 * data, bss, and stack in SSRAM2/3 at 0x20000000.
 */

OUTPUT_FORMAT("elf32-littlearm")
OUTPUT_ARCH(arm)
ENTRY(Reset_Handler)

MEMORY
{
    CODE (rx)  : ORIGIN = 0x00000000, LENGTH = 4M
    RAM  (rwx) : ORIGIN = 0x20000000, LENGTH = 4M
}

_estack = ORIGIN(RAM) + LENGTH(RAM);

SECTIONS
{
    .isr_vector : {
        KEEP(*(.isr_vector))
    } > CODE

    .text : {
        *(.text*)
        *(.rodata*)
        . = ALIGN(4);
    } > CODE

    _sidata = LOADADDR(.data);

    .data : {
        . = ALIGN(4);
        _sdata = .;
        *(.data*)
        . = ALIGN(4);
        _edata = .;
    } > RAM AT> CODE

    .bss : {
        . = ALIGN(4);
        _sbss = .;
        *(.bss*)
        *(COMMON)
        . = ALIGN(4);
        _ebss = .;
    } > RAM

    /DISCARD/ : {
        *(.comment)
        *(.note*)
        *(.ARM.exidx*)
        *(.ARM.attributes)
    }
}
