Hardware and Device Notes
=========================

Contents:
- Inventory
- Programmers
- Chips and Boards
- Other (level conversion, etc.)
- Homebrew

Inventory
---------

cjs:
- [4000193528943][] XC9572XL CPLD learning board
- [DLC10-clone] programmer

sjn:
- [4000193528943][] XC9572XL CPLD learning board


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

- [Datasheet][XC9572XL] ([source][XC9572XL-orig]). 72 macrocells, 1600 gates.
- ISE toolchain.
- I/O: 34 (44-pin PLCC, VQFP); 52 (64-pin VQFP); 72 (100-pin TQFP).
- Vcc=3.0-3.6 V. 5V tolerant I/O. 3.3V and 2.5V output capability.

[Digikey prices][dk9572] $6-10; old PLCC-48 also available at ~$3 qty.100.
Seeed had a [dev board][seeed-XC9572XL] (Digi-Key [1597-1318-ND]) but that
seems to be out of production.

There seems to be a standard "learning board" with five LEDs, 2× 20 pin
headers, a 3.3V regulator and JTAG; find it with a search for "XC9572XL".
Sources include (+¥ is shipping):
- amazon.co.jp: [B09FX9ZPLF] ¥1,879.
- aliexpress.com: [4000193528943] ¥733 +¥385;
  [33041288904] ¥1,124 +¥0 with incorrect description.

Board configuration/wiring:
- 44-pin VQFP device (34 user I/O pins)
- Barrel connector: center-positive into power switch then single 3.3V
  regulator. 2nd regulator missing; its output pad also 3.3V.
  - __WARNING: pins labeled 1.8V have 3.3V output.__
- Pin 1 connected to 50 MHz crystal oscillator and `CLK` pin.
- 2× 20-pin headers (`•` marks gap in numbering; `P` ommited from pin names):
  - `3V3 3V3† CLK • 2 3 • 5 6 7 8 • 12 13 14 • 16 • 18 19 20 21 22 23 • GND`
  - `3V3 3V3† • 27 28 29 30 31 32 33ᵗ 34ᵗ • 36 37 38 39 40 44 42 43ᶜ 44ᶜ • GND`
  - Global control pins and pin notes:
    - † labeled 1.8V
    - ᶜ clock: `43`:GCK1 `44`:GCK2 `1`:GCK3
    - ᵗ tristate: `36`:GTS1 `34`:GTS2
    - ʳ reset: `33`:GSR
- LEDs: D1:Vcc;  D2:`31`  D3:`34`  D4:`33`  D5:`32`  (linked by the 4 jumpers)
- Non-I/O chip pins (none on I/O headers except power):
    - GND: `4` `17` `25`
    - Vccint (3.3V): `15` `35`
    - Vccio (2.5/3.3V): `26`
    - JTAG: `11`:TCK `9`:TDI `24`:TDO `10`:TMS

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
[XC9572XL-orig]: https://www.xilinx.com/support/documentation/data_sheets/ds057.pdf
[XC9572XL]: ./XC9572XL.pdf
[dk9572]: https://www.digikey.com/en/products/filter/embedded-cplds-complex-programmable-logic-devices/695?s=N4IgTCBcDaIBoGECcBWA7GOAZEBdAvkA
[seeed-XC9572XL]: https://www.seeedstudio.com/XC9572XL-CPLD-development-board-v1b-p-799.html

[FT2232H]: http://www.ftdichip.com/Support/Documents/DataSheets/ICs/DS_FT2232H.pdf
[Spartan 6 LX9]: https://www.xilinx.com/support/documentation/data_sheets/ds160.pdf
[pap-db]: https://www.seeedstudio.com/Papilio-DUO-512KB-p-2328.html
[pap-sh]: https://www.seeedstudio.com/Classic-Computing-Shield-V1-01.html

[SN74LVC8T245]: https://www.ti.com/lit/ds/sces584b/sces584b.pdf