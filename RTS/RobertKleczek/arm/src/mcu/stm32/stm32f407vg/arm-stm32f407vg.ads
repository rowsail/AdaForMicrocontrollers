--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                       A R M . S T M 3 2 F 4 0 7 V G                        --
--                                  S p e c                                   --
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

with ARM.Link;
with ARM.Architecture.Startup;
with ARM.Register.FLASH_F40XXX;
with ARM.Register.RCC_F40XXX;
with ARM.Register.PWR_F40XXX;
with ARM.Driver.Clock_F40XXX;
with ARM.Driver.Power_F40XXX;
with ARM.Driver.Flash_F40XXX;

--------------------------------------------------------------------------------
--                               ARM.STM32F407VG                              --
--------------------------------------------------------------------------------

package ARM.STM32F407VG is

   pragma Preelaborate;

   -----------------------------------------------------------------------------
   --                            Global parameters                            --
   -----------------------------------------------------------------------------

   -------------------------
   --  Available vectors  --
   -------------------------

   Number_Of_Implemented_Interrupts    : constant Natural := 82;
   Number_Of_Implemented_Traps         : constant Natural := 16;

   ------------------------
   --  Is MCU has FPU ?  --
   ------------------------

   Has_FPU                             : constant Boolean := True;

   ------------------
   --  MCU limits  --
   ------------------

   --  Clock limits  --

   type HSI_Frequency  is new Frequency range 16.000 ..  16.000;
   type HSE_Frequency  is new Frequency range  4.000 ..  24.000;
   type AHB_Frequency  is new Frequency range  1.000 .. 168.000;
   type APB1_Frequency is new Frequency range  1.000 ..  42.000;
   type APB2_Frequency is new Frequency range  1.000 ..  84.000;
   type PLL_Frequency  is new Frequency range 24.000 .. 168.000;

   --  Power limits  --

   type VDD_Voltage is new Voltage range 1.8 .. 3.6;

   ---------------------
   --  Global clocks  --
   ---------------------

   --  High Speed Clock frequency typical for MCU
   HSI_Clk  : constant HSI_Frequency := 16.000;
   pragma Export (Asm, HSI_Clk, "__HSI_Clk");

   --  External (crystal or bypassed)  high speed clock
   HSE_Clk  : Frequency := No_Clock;
   pragma Export (Asm, HSE_Clk, "__HSE_Clk");

   --                           SYSTEM CLOCK                                  --
   --  Global variable contains actual AHB clock frequency
   --  at startup is always HSI
   AHB_Clk  : AHB_Frequency := AHB_Frequency (HSI_Clk);
   pragma Export (Asm, AHB_Clk, "__SYSTEM_Clk");

   --  Global variable contains actual APB1 clock frequency
   APB1_Clk : APB1_Frequency;
   pragma Export (Asm, APB1_Clk, "__APB1_Clk");

   --  Global variable contains actual APB2 clock frequency
   APB2_Clk : APB2_Frequency;
   pragma Export (Asm, APB2_Clk, "__APB2_Clk");

   --------------------
   --  Global power  --
   --------------------

   --  Global variable contains VDD voltage supply
   VDD : VDD_Voltage;
   pragma Export (Asm, VDD, "__VDD_Vlt");

   -----------------------------------------------------------------------------
   --                       Include startup code                              --
   -----------------------------------------------------------------------------

   package Startup is new ARM.Architecture.Startup
     (ARM.Link.Link_To_ROM,
      Has_FPU,
      Number_Of_Implemented_Interrupts,
      Number_Of_Implemented_Traps);

   -----------------------------------------------------------------------------
   --                          Peripherals                                    --
   -----------------------------------------------------------------------------

   package FLASH renames ARM.Register.FLASH_F40XXX;
   package RCC   renames ARM.Register.RCC_F40XXX;
   package PWR   renames ARM.Register.PWR_F40XXX;

   -----------------------------------------------------------------------------
   --                         Drivers renamings                               --
   -----------------------------------------------------------------------------

   package Driver is
      package Clock renames ARM.Driver.Clock_F40XXX;
      package Power renames ARM.Driver.Power_F40XXX;
      package Flash renames ARM.Driver.Flash_F40XXX;
   end Driver;

end ARM.STM32F407VG;
