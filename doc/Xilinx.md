Xilinx Development Tools
========================

XXX summarise here why we need them to build stuff.

### Downloading

You'll need about 6-7 GB of free disk space for the Linux tar archive and that
much again for the extracted archive.

The product you need is __ISE Design Suite,__ available from [this download
page][ise-dl]. On that page you need to click __14.7__ to expand a list and
then scroll down to the __Full Installer for Linux__ link. If you're logged in,
clicking on it will take you to a confirmation page for your
name/company/address information (even if you've already entered it) and a red
__Download__ button that will start the download.

If you're not logged in, you'll see an AMD login page (they now own Xilinx)
where you can log in if you already have an account or create an account if
you don't have one.

### Installation

    sudo -s
    umask 022
    mkdir -p /opt
    cd /opt
    tar xf ~/Downloads/Xilinx_ISE_DS_Lin_14.7_1015_1.tar

The `xsetup` program will need an install location separate from the installer
you downloaded and extracted; this defaults to `/opt/Xilinx/`. The following
will set you up to run the install as a non-root user:

    mkdir /opt/Xilinx
    chown -R $(id -u):$(id -g) /opt/xilinx
    umask 022
    /opt/Xilinx_ISE*/xsetup

On the first run you may get some messages along the lines of
`libncurses.so.5: cannot open shared object file...`. Use `apt-file search`
on the filenames to find the packages that provide them and install those
packages. Once all the requirements are installed, a graphical installer
will start.

The installer stages are:
1. Various licensing screens: confirm that Xilinx owns all your data, your
   computer, you personally, all your relatives, etc.
2. Select the products to install: __ISE Design Suite Logic Edition.__
3. Install options:
   - YES: __Acquire or Manage a License Key__
   - NO:  WebTalk (unless you want it)
   - NO:  __Install Cable Drivers__
4. Install location: `/opt/Xilinx`
5. Summary: click "Install" button.

You then need to activate a license:

    #   See warning in "Running" section below about this
    source /opt/Xilinx/14.7/ISE_DS/settings64.sh
    xlcm

The second tab, "Manage Licenses," will have a "Local System Information"
panel showing your Network Interface Card (NIC) ID. If this is all-zeros
you will not be able to use a license. The License Manager reads the MAC
address from an `eth#`, so if you don't have one you can create one with:

    sudo ip link add eth9 type dummy
    ip link show eth9
    #   The above will select a MAC address through unknown means (cjs got
    #   7e:62:8b:e2:2b:97). To re-use an existing license address:
    #sudo ip link set eth9 address 7e:62:8b:01:23:45

Go to the [Licensing Solution Center][lsc] and from that page click on any
of the "Xilinx Product Licensing Site" links. (Those links must be clicked
from this page; they can't be saved.) This takes you to the usual address
confirmation screen; click the red __Next__ button at the bottom. This will
take you to a Product Licensing page on `xilinx.entitlenow.com`; select the
__ISE Embedded Edition License__ and click "Generate Node-Locked License."
(If you can't see the full list because the footer is covering it, delete
the `<footer>` element in your browser's developer DOM browser.) You'll
need to select a host, add its MAC address, etc. Once the process is
complete it will e-mail you the license file; you can also download it from
<http://www.xilinx.com/getlicense>; it will eventually take you to the same
page above where you'll need to click on the "Manage Licenses" tab to list
the licences you can download..

Once you have the license file (which you can rename), choose the "Load
License" button from the "Manage Licenses" tab of `xlcm`.

### "Cable" (Programming Pod) Setup

For the "cable" (programmer) installation, you can use an open source
Windriver6 replacement as mentioned in [Xilinx JTAG Linux][jtlin].

    #   Ensure your shell is one where you did _not_ source settings64.sh

    #   fxload downloads programs to FX and FX2 devices.
    sudo apt-get install build-essential libusb-dev fxload
    git clone git://git.zerfleddert.de/usb-driver
    cd usb-driver
    make            # some warnings will be emitted
    umask 022
    sudo ./setup_pcusb /opt/Xilinx/14.7/ISE_DS/ISE
    #   If programming pod was plugged in, unplug and replug.

The final command add `/etc/udev/rules.d/xusbdfwu.rules` that will detect
the various Xilinx programming pods and run `fxload` to load the firmware
into them when plugged in. (It also sets permissions on the device when it
restarts after firmware load.) Without this, connecting a pod will show (in
the kernel log) something like just the following:

    new high-speed USB device number 5 using ehci-pci
    New USB device found, idVendor=03fd, idProduct=0013, bcdDevice= 0.00
    New USB device strings: Mfr=0, Product=0, SerialNumber=0

With the udev actions in place, you will see further messages that show that,
after being programmed, the device disconnects and returns as idProduct=0008 or
similar:

    New USB device found, idVendor=03fd, idProduct=0008, bcdDevice= 0.00
    New USB device strings: Mfr=1, Product=2, SerialNumber=0
    Product: XILINX
    Manufacturer: XILINX

See below for the `LD_PRELOAD` stuff for running `impact`.

### Running

    #   Set up the Xilinx development environment. This will radically
    #   change library and other paths, so after this don't try to use the
    #   window for "regular" work.
    source /opt/Xilinx/14.7/ISE_DS/settings64.sh
    ise &

The `libusb-driver.so` file must be in the library path when running the
`impact` program to do downloads; usually this is done with:

    LD_PRELOAD=.../usb-driver/libusb-driver.so impact ...

Occasionally the driver will be unable to connect, producing errors like
the following. Re-running the program usually fixes this.

    usb_claim_interface: 0 -> -9 (could not claim interface 0: Bad file descriptor)
    usb_transfer: -9 (error sending control message: Bad file descriptor)
    write cmdbuffer failed FFFFFFFFFFFFFFF7.
    Loopback test failed. Sent character = 00, Received character = 00.
    Cable connection failed.


ISE Products
------------

This information is copied directly from the installer product selection
screen.

- __ISE WebPack.__ ISE WebPACK contains the most important tools you need for
  designing CPLDs and small to medium-sized FPGAs. Includes: ISE Design Tools
  (w/reduced device support), PlanAhead, Connectivity DSP IP. ChipScope Pro and
  The Embedded Development Kit will also be installed with WebPACK but are
  licensed separately (not included in a WebPACK license file).
- __ISE Design Suite Logic Edition.__ This installation contains everything you
  need to do a complete logic design for any size device. It includes ISE
  Design Tools (all devices supported), PlanAhead, ChipScope Pro, the Embedded
  Development Kit (EDK), ISim and Connectivity & DSP IP.
- __ISE Design Suite Embedded Edition.__
- __ISE Design Suite DSP Edition.__ This installation contains everything you
  need to do a complete embedded design. It includes ISE Design Suite Logic
  Edition, and Embedded Development Kit (EDK). EDK includes Xilinx Platform
  Studio (XPS), Software Development Kit (SDK) and Embedded IP.
- __ISE Design Suite System Edition.__ This installation contains everything
  you need to do a complete system design. It includes ISE Design Suite Logic
  Edition, the Embedded Development Kit (EDK) and System Generator for DSP.
- __Lab Tools - Standalone Installation.__ Installs only the Xilinx Lab Tools.
  This is a standalone collection of the iMPACT device configuration and
  ChipScope Pro Analyzer tools. Standalone Lab Tools are intended for use in
  lab environments where the complete Xilinx ISE Design Suite toolset is not
  required. Note: iMPACT and ChipScope are installed with all ISE Design Suite
  and ISE WebPACK products. The Lab Tools installation is not required if one
  of the ISE products has been installed.



<!-------------------------------------------------------------------->
[ise-dl]: https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/archive-ise.html
[jtlin]: https://www.george-smart.co.uk/fpga/xilinx_jtag_linux/
[lsc]: https://www.xilinx.com/support/licensing_solution_center.html
