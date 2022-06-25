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

XXX later, go to licensing site and get license.


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
