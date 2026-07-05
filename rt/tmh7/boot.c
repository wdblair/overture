/*
 * boot.c -- reset path for the STM32H743, in plain C.
 *
 * On M-profile the vector table is the entry protocol: word 0 is the
 * initial stack pointer, word 1 the reset handler; there are no
 * exception levels to negotiate. This file is the part slated to be
 * replaced by proven assembly eventually; it is deliberately tiny.
 */

#include <stdint.h>
#include "mmio.h"

extern uint32_t _estack, _sidata, _sdata, _edata, _sbss, _ebss;

extern void rt_main(void);

void Reset_Handler(void);

/* any unexpected exception: fast panic blink on PE4 (LED) */
void Default_Handler(void)
{
    REG32(RCC_AHB4ENR) |= (1u << 4);
    GPIO_MODER(GPIOE_BASE) =
        (GPIO_MODER(GPIOE_BASE) & ~(3u << 8)) | (1u << 8);
    for (;;) {
        GPIO_ODR(GPIOE_BASE) ^= (1u << 4);
        for (volatile int i = 0; i < 400000; i++) {}
    }
}

void SysTick_Handler(void); /* provided by rt.c */

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
    /* point the vector table at ourselves, wherever we were booted */
    REG32(SCB_VTOR) = (uint32_t)(uintptr_t)g_vectors;

    /* copy .data from flash, zero .bss */
    uint32_t *src = &_sidata;
    for (uint32_t *dst = &_sdata; dst < &_edata; dst++) *dst = *src++;
    for (uint32_t *dst = &_sbss; dst < &_ebss; dst++) *dst = 0;

    rt_main();

    for (;;) {}
}
