#ifndef ZALB_NEC_PILOT_H
#define ZALB_NEC_PILOT_H

#include "dma_send.h"

#define DEFAULT_ADDRESS 0xE918
#define ON_CMD 0x08
#define OFF_CMD 0x14

#define PERIOD 849
#define FULL 850
#define HALF 429
#define EMPTY 0

void sendValue(uint16_t v, uint8_t bits) {
    for (uint8_t bit=0; bit<bits; bit++) {
        if (v & (((uint16_t) 1) << bit)) {
            sendRawTimerData(HALF, 1);
            sendRawTimerData(EMPTY, 1);
        } else {
            sendRawTimerData(HALF, 1);
        }
    }
}

void sendRepeatCommand() {
    sendRawTimerData(FULL, 8);
    sendRawTimerData(EMPTY, 2);
    sendRawTimerData(HALF, 1);
    sendRawTimerData(EMPTY, 87);
    dmaSend();
}

void sendCommand(uint16_t cmd) {
    sendRawTimerData(FULL, 8);
    sendRawTimerData(EMPTY, 4);
    sendValue(DEFAULT_ADDRESS, 16);
    sendValue(cmd, 8);
    sendValue(~cmd, 8);
    sendRawTimerData(HALF, 1);
    sendRawTimerData(EMPTY, 10);
    dmaSend();
}


#endif //ZALB_NEC_PILOT_H
