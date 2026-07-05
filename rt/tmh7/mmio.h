/*
 * mmio.h -- volatile register access for the STM32H743.
 * The only unavoidable C in the runtime; everything above it can be
 * (and increasingly will be) typed code.
 */

#ifndef OVERTURE_RT_MMIO_H
#define OVERTURE_RT_MMIO_H

#include <stdint.h>

#define REG32(addr) (*(volatile uint32_t *)(uintptr_t)(addr))

/* Cortex-M7 core peripherals */
#define SYST_CSR   0xE000E010u
#define SYST_RVR   0xE000E014u
#define SYST_CVR   0xE000E018u
#define SCB_VTOR   0xE000ED08u

/* STM32H743 (D3 domain) */
#define RCC_AHB4ENR 0x580244E0u
#define GPIOA_BASE  0x58020000u
#define GPIOE_BASE  0x58021000u

#define GPIO_MODER(base) REG32((base) + 0x00u)
#define GPIO_ODR(base)   REG32((base) + 0x14u)
#define GPIO_BSRR(base)  REG32((base) + 0x18u)

#endif /* OVERTURE_RT_MMIO_H */
