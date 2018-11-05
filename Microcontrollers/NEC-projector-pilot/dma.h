#ifndef ZALB_DMA_H
#define ZALB_DMA_H

#include <stm32.h>
#include "buttons.h"
#include "nec_pilot.h"

void enableDMAInterrupts() {
    DMA1->LIFCR = DMA_LIFCR_CTCIF2;
    NVIC_EnableIRQ(DMA1_Stream2_IRQn);
}

void initDMAClock() {
    RCC->AHB1ENR |= RCC_AHB1ENR_DMA1EN;

    __NOP();
    __NOP();
}

void initDma() {
    initDMAClock();

    DMA_STREAM->CR |=
            DMA_SxCR_CHSEL_0 | DMA_SxCR_CHSEL_2		    //Wybor 5 kanalu DMA
            | DMA_SxCR_MSIZE_0							//Transfer 16 bitow danych
            | DMA_SxCR_PSIZE_0							//Transfer 16 bitow danych
            | DMA_SxCR_MINC    							//Inkrementacja adresu pamięci
            | DMA_SxCR_DIR_0	                        // pamiec -> peryferia
            | DMA_SxCR_TCIE;

    DMA_STREAM->PAR = (uint32_t)&TIM3->CCR1;

    enableDMAInterrupts();
}

void DMA1_Stream2_IRQHandler() {
    uint32_t isr = DMA1->LISR;
    if (isr & DMA_LISR_TCIF2) {
        DMA1->LIFCR = DMA_LIFCR_CTCIF2;
        if (!isOn(USER_GPIO, USER_PIN) || isOn(MODE_BUTTON_GPIO, MODE_BUTTON_PIN)) {
            sendRepeatCommand();
        }
    }
}

#endif //ZALB_DMA_H
