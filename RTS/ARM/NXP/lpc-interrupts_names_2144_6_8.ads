--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--          L P C . I n t e r r u p t _ N a m e s _ 2 1 4 4 _ 6 _ 8           --
--                                   S p e c                                  --
--                                                                            --
--    Copyright (C) 2014  Robert Kleczek                                      --
--                                                                            --
--    This program is free software: you can redistribute it and/or modify    --
--    it under the terms of the GNU General Public License as published by    --
--    the Free Software Foundation, either version 3 of the License, or       --
--    (at your option) any later version.                                     --
--                                                                            --
--    This program is distributed in the hope that it will be useful,         --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of          --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           --
--    GNU General Public License for more details.                            --
--                                                                            --
--    You should have received a copy of the GNU General Public License       --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.   --
--                                                                            --
--------------------------------------------------------------------------------

------------------------
--  Imported Packages --
------------------------

with Ada.Interrupts;
use  Ada.Interrupts;
--   Used for Interrupt_ID

package LPC.Interrupts_Names_2144_6_8 is

   ----------------------------
   --  Vector names aliases  --
   ----------------------------
   --         LPC2148        --
   ----------------------------

   INT_WDT      : Interrupt_ID renames INT_0;
   INT_RESERVED : Interrupt_ID renames INT_1;
   INT_EIRX     : Interrupt_ID renames INT_2;
   INT_EITX     : Interrupt_ID renames INT_3;
   INT_TIMER0   : Interrupt_ID renames INT_4;
   INT_TIMER1   : Interrupt_ID renames INT_5;
   INT_UART0    : Interrupt_ID renames INT_6;
   INT_UART1    : Interrupt_ID renames INT_7;
   INT_PWM0     : Interrupt_ID renames INT_8;
   INT_I2C      : Interrupt_ID renames INT_9;
   INT_SPI0     : Interrupt_ID renames INT_10;
   INT_SPI1     : Interrupt_ID renames INT_11;
   INT_PLL      : Interrupt_ID renames INT_12;
   INT_RTC      : Interrupt_ID renames INT_13;
   INT_EINT0    : Interrupt_ID renames INT_14;
   INT_EINT1    : Interrupt_ID renames INT_15;
   INT_EINT2    : Interrupt_ID renames INT_16;
   INT_EINT3    : Interrupt_ID renames INT_17;
   INT_ADC0     : Interrupt_ID renames INT_18;
   INT_I2C1     : Interrupt_ID renames INT_19;
   INT_BOD      : Interrupt_ID renames INT_20;
   INT_ADC1     : Interrupt_ID renames INT_21;
   INT_USB      : Interrupt_ID renames INT_22;

end LPC.Interrupts_Names_2144_6_8;
