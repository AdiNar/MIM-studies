#ifndef MIKRO01_BUTTONS_H
#define MIKRO01_BUTTONS_H

#include <gpio.h>
#include <stm32.h>
#include "nec_pilot.h"

#define MODE_BUTTON_IRQ EXTI0_IRQn
#define USER_IRQ EXTI15_10_IRQn

#define MODE_BUTTON_GPIO GPIOA
#define MODE_BUTTON_PIN 0

#define USER_GPIO GPIOC
#define USER_PIN 13

void enableButtonInterrupt(GPIO_TypeDef *gpio, uint8_t pin, int num) {
    GPIOinConfigure(gpio, pin, GPIO_PuPd_UP,
                    EXTI_Mode_Interrupt,
                    EXTI_Trigger_Rising_Falling);

    NVIC_EnableIRQ(num);
}

int initButtonInterrupts() {
    RCC->APB2ENR |= RCC_APB2ENR_SYSCFGEN;

    __NOP();
    __NOP();

    enableButtonInterrupt(MODE_BUTTON_GPIO, MODE_BUTTON_PIN, MODE_BUTTON_IRQ);
    enableButtonInterrupt(USER_GPIO, USER_PIN, USER_IRQ);

    return 0;
}

void initButtonsClock() {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN | RCC_AHB1ENR_GPIOCEN;

    __NOP();
    __NOP();
}

void initButtons() {
    initButtonsClock();
    initButtonInterrupts();
}

uint8_t isOn(GPIO_TypeDef *gpio, uint8_t pin) {
    return (gpio->IDR & (1 << pin)) ? 1 : 0;
}

void handleIrqs();
void EXTI0_IRQHandler(void) { handleIrqs(); }
void EXTI15_10_IRQHandler(void) { handleIrqs(); }

void check(uint32_t mr, uint32_t pr, uint16_t msg, GPIO_TypeDef *gpio, int pin, int rev) {
    if( (EXTI->IMR & mr) && (EXTI->PR & pr)) {
        EXTI->PR = pr;
        if (isDmaFree() && isOn(gpio, pin) ^ rev) {
            sendCommand(msg);
        }
    }
}

void handleIrqs() {
    check(EXTI_IMR_MR13, EXTI_PR_PR13, ON_CMD, USER_GPIO, USER_PIN, 1);
    check(EXTI_IMR_MR0, EXTI_PR_PR0, OFF_CMD, MODE_BUTTON_GPIO, MODE_BUTTON_PIN, 0);
}

#endif //MIKRO01_BUTTONS_H