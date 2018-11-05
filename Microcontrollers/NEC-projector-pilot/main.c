#include "timers.h"
#include "dma.h"
#include "buttons.h"

int main() {
	initButtons();
	initTimers();
	initDma();
}
