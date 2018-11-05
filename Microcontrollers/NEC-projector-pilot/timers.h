#ifndef ZALB_TIMERS_H
#define ZALB_TIMERS_H

#include <gpio.h>
#include <stm32.h>
#include "nec_pilot.h"

#define EnableTim(TIMx)               (TIMx->CR1 |= TIM_CR1_CEN)

// Values from manual
#define TIM_OutputState_Enable             ((uint16_t)0x0001)

#define TIM_OCMode_PWM1                    ((uint16_t)0x0060)
#define TIM_MasterSlaveMode_Enable ((uint16_t)0x0080)
#define TIM_TRGOSource_Update      ((uint16_t)0x0040)
#define TIM_TS_ITR2 0x20

#define SMCR_SLAVE_SMS ((uint16_t)0x0005) // strona ~617
#define SMCR_SLAVE_TS ((uint16_t)0x0500)
#define CCMR1_SLAVE ((uint16_t)0x0001)
// END

#define PRESCALER 20

void enableIntInTimer(TIM_TypeDef* TIMx) {
    TIMx->SR = ~TIM_SR_UIF;
    TIMx->DIER |= TIM_DIER_UIE;
}

void enableTimerInterrupts() {
    enableIntInTimer(TIM3);
    NVIC_EnableIRQ(TIM3_IRQn);
}

void enableTimers() {
    EnableTim(TIM2);
    EnableTim(TIM3);
}

void withDMA(TIM_TypeDef* TIMx) {
    TIMx->CR2 |= TIM_CR2_CCDS;
    TIMx->DIER |= TIM_DIER_UDE;
    TIMx->CR1 |= TIM_CR1_URS;
}

void initOutput(TIM_TypeDef* TIMx) {
    TIMx->CCMR1 =
            TIM_CCMR1_OC1M_2 | TIM_CCMR1_OC1M_1 |
            TIM_CCMR1_OC1PE;
    TIMx->CCER = TIM_CCER_CC1E | TIM_CCER_CC1P;
}

void initSlaveValues(TIM_TypeDef* TIMx) {
    TIMx->PSC = PRESCALER;
    TIMx->ARR = 19;
    TIMx->EGR = TIM_EGR_UG;
    TIMx->CCR1 = 10;
}

void initGatedSlave(TIM_TypeDef* TIMx) {
    TIMx->SMCR &= ~TIM_SMCR_SMS;
    TIMx->SMCR |= SMCR_SLAVE_SMS;
}

void initSlaveTrigger(TIM_TypeDef* TIMx) {
    TIMx->SMCR &= ~TIM_SMCR_TS;
    TIMx->SMCR |= TIM_TS_ITR2; // strona 630, itr2 dla TIM2 to TIM3_TRGO
}

void initSlave(TIM_TypeDef* TIMx) {
    initOutput(TIMx);
    initSlaveValues(TIMx);
    initGatedSlave(TIMx);
    initSlaveTrigger(TIMx);
}

void initMasterValues(TIM_TypeDef* TIMx) {
    TIMx->PSC = PRESCALER;
    TIMx->ARR = PERIOD;
    TIMx->EGR = TIM_EGR_UG;
    TIMx->CCR1 = 0;
}

void initMasterRegisters(TIM_TypeDef* TIMx) {
    TIMx->SMCR |= TIM_MasterSlaveMode_Enable;
    TIMx->CR2 |= TIM_TRGOSource_Update;
}

void initMaster(TIM_TypeDef* TIMx) {
    initOutput(TIMx);
    initMasterValues(TIMx);
    withDMA(TIMx);
    initMasterRegisters(TIMx);
}


void initTimerClocks() {
    RCC->APB1ENR |= RCC_APB1ENR_TIM3EN | RCC_APB1ENR_TIM2EN;
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN;

    __NOP();
    __NOP();
}

void initPins() {
    GPIOafConfigure(GPIOA, 5, GPIO_OType_PP,
                    GPIO_Low_Speed,
                    GPIO_PuPd_NOPULL, GPIO_AF_TIM2);
}

void initTimers() {
    initTimerClocks();
    initPins();

    initSlave(TIM2);
    initMaster(TIM3);
    enableTimerInterrupts();
    enableTimers();
}

// Zgaś diodę slave'a gdy slave jest wyłączony
void TIM3_IRQHandler(void) {
    uint32_t it_status = TIM3->SR & TIM3->DIER;

    if (it_status & TIM_SR_UIF) {
        TIM3->SR = ~TIM_SR_UIF;
    }

    TIM2->CNT = 0;
}

#endif //ZALB_TIMERS_H
