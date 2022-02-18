Hardware and Device Notes
=========================

Inventory
---------

cjs:
- [4000193528943][] XC9572XL CPLD learning board (on order)
- [DLC10-clone] programmer (on order)

sjn:
- [4000193528943][] XC9572XL CPLD learning board (on order)


Programmers
-----------

### DLC10

The Xilinx ["Platform Cable USB II" (model DLC10)][DLC10] part number
HW-USB-II-G ($270) seems to be the second-generation programmer (after the
DLC9) that works with all Xilinx devices, so long as you have the correct
adapter and flying cable.

[This AliExpress vesion][DLC10-clone] seems to be a cheap clone (¥2742 +
¥725 shipping) that includes an interface board and many flying leads. The
description has a table indicating the improvements over the DLC9 non-II
version (it can program the fuses as well.)


Chips and Boards
----------------

### Xilinx XC9572XL High Performance CPLD

- [Datasheet][XC9572XL]. 72 macrocells, 1600 gates.
- ISE toolchain.
- I/O: 34 (44-pin PLCC, VQFP); 52 (64-pin VQFP); 72 (100-pin TQFP).
- 5V tolerant I/O, 3.3V output capability.

[Digikey prices][dk9572] $6-10; old PLCC-48 also available at ~$3 qty.100.
Seeed had a [dev board][seeed-XC9572XL] (Digi-Key [1597-1318-ND]) but that
seems to be out of production. There seems to be a standard "learning
board" with five LEDs, 2× 20 pin headers, barrel power and JTAG turned up
by a search for "XC9572XL" on amazon.co.jp and AliExpress, giving e.g.
[B09FX9ZPLF][] (¥1,879), [4000193528943] ¥733 + +385 shipping. (Be careful,
some like [33041288904][] (¥1,124 free shipping) have either an incorrect
title or incorrect description below.)

The [DLC10-clone] programmer appears to program this.

### Seeed Papilio DUO-512KB

- [Development Board][pap-db] $70. [Classic Computing Shield][pap-sh] $15
  (VGA, PS/2, serial, Micro-SD, etc.).
- Xilinx [Spartan 6 LX9], FTDI [FT2232H][] (2× USB UART/FIFO), 512K SRAM,
  8M flash, ATmega32U4.


Other
-----

### Level Conversion

- TI [SN74LVC8T245] 8-Bit Dual-Supply Bus Transceiver With Configurable
  Voltage Translation and 3-State Outputs. All 8 bus lines switch direction
  with one `DIR` input; `O̅E̅` tri-states outputs..



<!-------------------------------------------------------------------->
[DLC10]: https://www.xilinx.com/products/boards-and-kits/hw-usb-ii-g.html
[DLC10-clone]: https://www.aliexpress.com/item/32691266814.html

[1597-1318-ND]: https://www.digikey.com/en/products/detail/seeed-technology-co-ltd/102990001/5488214?s=N4IgTCBcDaIBoGECcBWA7GOAZEBdAvkA
[33041288904]: https://www.aliexpress.com/item/33041288904.html
[4000193528943]: https://www.aliexpress.com/item/4000193528943.html
[B09FX9ZPLF]: https://www.amazon.co.jp/dp/B09FX9ZPLF
[XC9572XL]: https://www.xilinx.com/support/documentation/data_sheets/ds057.pdf
[dk9572]: https://www.digikey.com/en/products/filter/embedded-cplds-complex-programmable-logic-devices/695?s=N4IgTCBcDaIBoGECcBWA7GOAZEBdAvkA
[seeed-XC9572XL]: https://www.seeedstudio.com/XC9572XL-CPLD-development-board-v1b-p-799.html

[FT2232H]: http://www.ftdichip.com/Support/Documents/DataSheets/ICs/DS_FT2232H.pdf
[Spartan 6 LX9]: https://www.xilinx.com/support/documentation/data_sheets/ds160.pdf
[pap-db]: https://www.seeedstudio.com/Papilio-DUO-512KB-p-2328.html
[pap-sh]: https://www.seeedstudio.com/Classic-Computing-Shield-V1-01.html

[SN74LVC8T245]: https://www.ti.com/lit/ds/sces584b/sces584b.pdf
