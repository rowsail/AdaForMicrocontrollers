This example is for the TMS570LS31HDK board from Texas Instruments.
It blinks the eight white LEDs located around the central MCU chip,
in counter-clockwise order.

Note that this demo is set up for "Revision D" of the board, so all
the LEDs are white.  If you have a "Revision E" board, the left and
right LEDs are different colors, and these are not driven by this demo.

Note that debugging the application requires a JTAG debug interface,
such as one of the Abatron BDIGDB devices.  Configuration of a JTAG
debugger interface is device-specific so is outside the scope of this
README, but note that once set up, debugging via GDB is like any other
remote target.  In that case full debugging is available via the GPS
debugger GUI interface.
