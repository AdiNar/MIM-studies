Infrared pilot for NEC projector.

Implements Extended NEC Protocol: https://www.sbprojects.net/knowledge/ir/nec.php


Final project for microcontrollers course.

Written for Nucleo-F411RE (STM32F411), should work with STM32F4 family.

Project graded with 5.

# Features
Turning projector on and off.

Other features may be easily added by adding #define to nec_pilot.h.

# Cool stuff inside
Project includes all lecturers suggestions, written in a way that 

(I hope) couldn't be improved in obvious way.

 - Pilots uses DMA to reduce CPU overhead in the process, in fact after init phase CPU is used only with interrupts where some decision is needed (and in a small hack for not turning-off diode).
 - Timers work in a master-slave mode. That means, slave timer is responsible for creating carrier wave, master is responsible for logic signals layer only.
 - Timers, DMA and buttons are configured to work with interrupts, there is no loop on CPU.

# Installation and Running
Project works on my students environment.

Unfortunately it's not ported for general case, however all needed details are in `makefile`.

If `make run` succeeds, program should be successfully deployed on device.
