--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--              A R M . S t a r t u p . C l o c k _ F 4 0 X X X               --
--                              G e n e r i c                                 --
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

with ARM.MCU;
use  ARM.MCU;

--------------------------------------------------------------------------------
--                         ARM.Startup.Clock_F40XXX                           --
--------------------------------------------------------------------------------

generic
   VDD       : Voltage;
   AHB_Clock : AHB_Frequency;
   HSE_Clock : Frequency      := No_Clock;
   Ext_Clock : Boolean        := False;
   PLL_Error : Frequency      := 0.000;        --  Maximum allowed clock difference
package ARM.Startup.Clock_F40XXX is

   pragma Preelaborate;

   procedure Init_Clock with Linker_Section => ".clock";

   procedure Reset
     with Inline_Always;

   procedure Init_High_Speed_Clock
     with Inline_Always;

   procedure Init_Low_Speed_Clock
     with Inline_Always;

end ARM.Startup.Clock_F40XXX;
