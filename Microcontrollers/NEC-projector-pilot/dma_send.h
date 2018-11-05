#ifndef ZALB_DMA_SEND_H
#define ZALB_DMA_SEND_H

#define DMA_STREAM DMA1_Stream2

uint8_t dmaSize;
uint16_t dmaData[100];

void sendRawTimerData(uint16_t val, uint8_t times) {
    for (uint8_t i=0; i<times; i++) {
        dmaData[dmaSize++] = val;
    }
}

int isDmaFree() {
    return !(DMA_STREAM->CR & DMA_SxCR_EN);
}

void dmaSend() {
    DMA_STREAM->NDTR = dmaSize;
    dmaSize = 0;
    DMA_STREAM->M0AR = (uint32_t)&dmaData;
    DMA_STREAM->CR |= DMA_SxCR_EN;
}

#endif //ZALB_DMA_SEND_H
