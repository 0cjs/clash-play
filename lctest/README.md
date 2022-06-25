Testing Xylinx ISE on the LCTech CPLD Board
===========================================

This directory contains just the source files. The project needs to be
built from the command line before it can be opened in the ISE GUI, but
you can also just do everything from the command line.

    #   Xilinx environment setup - see warning in Xilinx.md
    source /opt/Xilinx/14.7/ISE_DS/settings64.sh

    #   Build the project producing lots of stuff, including
    #   the image file for the chip, `system_top.jed`.
    xtclsh lctest.tcl rebuild_project

    #   Download system_top.jed to the board via the programmer.
    #   If it states "Cable connection failed," see the cable install
    #   instructions in ../doc/Xilinx.md.
    LD_PRELOAD=.../usb-driver/libusb-driver.so impact -batch prog.scr


The IDE (`ise &`) seems to be mostly just launching external programs. So
you can use menu option to run `impact` if you set the above `LD_PRELOAD`
option in the environment when starting it.
