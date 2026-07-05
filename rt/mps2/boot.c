/*
 * boot.c -- reset path for the MPS2-AN500 (Cortex-M7 under QEMU),
 * in plain C.
 *
 * On M-profile the vector table is the entry protocol: word 0 is the
 * initial stack pointer, word 1 the reset handler. QEMU resets from
 * the table at address zero, where the linker script places ours.
 */

#include <stdint.h>

#define REG32(a) (*(volatile uint32_t *)(a))
#define SCB_VTOR 0xE000ED08u

extern uint32_t _estack, _sidata, _sdata, _edata, _sbss, _ebss;

extern void rt_main(void);

void Reset_Handler(void);

/* any unexpected exception: park (visible as a hung run) */
void Default_Handler(void)
{
    for (;;) {}
}

void SysTick_Handler(void);

__attribute__((section(".isr_vector"), used))
const void *const g_vectors[16] = {
    (void *)&_estack,      /*  0: initial MSP        */
    Reset_Handler,         /*  1: reset              */
    Default_Handler,       /*  2: NMI                */
    Default_Handler,       /*  3: HardFault          */
    Default_Handler,       /*  4: MemManage          */
    Default_Handler,       /*  5: BusFault           */
    Default_Handler,       /*  6: UsageFault         */
    0, 0, 0, 0,            /*  7-10: reserved        */
    Default_Handler,       /* 11: SVCall             */
    Default_Handler,       /* 12: DebugMonitor       */
    0,                     /* 13: reserved           */
    Default_Handler,       /* 14: PendSV             */
    SysTick_Handler        /* 15: SysTick            */
};

void Reset_Handler(void)
{
    REG32(SCB_VTOR) = (uint32_t)(uintptr_t)g_vectors;

    uint32_t *src = &_sidata;
    for (uint32_t *dst = &_sdata; dst < &_edata; dst++) *dst = *src++;
    for (uint32_t *dst = &_sbss; dst < &_ebss; dst++) *dst = 0;

    rt_main();

    for (;;) {}
}
