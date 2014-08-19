--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                  S T M 3 2 F 1 0 3 X X . S t a r t u p                     --
--                                G e n e r i c                               --
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

--  Generic startup file for STM32F103 family

------------------------
--  Imported Packages --
------------------------

with MCU;

generic

   Crystal_Frequency        : Natural;  --  Onboard crystal oscillator
   Desired_Frequency        : Natural;  --  Wanted MCU clock
   Peripheral_Clock_Divider : Natural;  --  Peripheral clock is divided by ?

   Number_Of_Implemented_External_Vectors : Positive :=
     MCU.Number_Of_Implemented_External_Vectors;

   pragma Unreferenced (Crystal_Frequency);
   pragma Unreferenced (Peripheral_Clock_Divider);

package STM32F103XX.Startup is

   pragma Preelaborate;

   procedure Initialize;
   pragma Warnings (Off, Initialize);
   --  Suspend warning of same name for exported procedure Initialize
   --  instantiated only one time
   pragma Export (Ada, Initialize, "initialize_clock");
   --  Initialize clock subsystem
   pragma Warnings (On, Initialize);
   --  Turn On warnings again

private

   Last_Interrupt : constant Natural :=
     Number_Of_Implemented_External_Vectors - 1;

   ----------------------------------
   --  Some auxiliary definitions  --
   ----------------------------------

   type Int_ID is new Natural range 0 .. Last_Interrupt;
   --  Temporary Interrupt_ID definition

   type Int_Handler is access procedure (Int : Int_ID);
   --  Temporary prototype of procedures used as low level handlers

   type Interrupt_Handlers_Type is array (Int_ID) of Int_Handler;
   --  Prototype of handlers interrupt table

   type ISR is access procedure;

   -------------------------
   --  Imported handlers  --
   -------------------------

   procedure Default_Interrupt_Handler (Int : Int_ID);
   pragma Import (Ada, Default_Interrupt_Handler);

   procedure Interrupt_Handler_Ada;
   pragma Import (Ada, Interrupt_Handler_Ada);

   -------------------------------------
   --  System vectors handlers table  --
   -------------------------------------
   --         Located in ROM          --
   -------------------------------------

   External_Vectors_ROM : constant array (0 .. Last_Interrupt) of ISR
     := (others => Interrupt_Handler_Ada'Access);

   pragma Linker_Section (External_Vectors_ROM, ".rom_external_vectors");

   ----------------------------------
   --  Ada vectors handlers table  --
   ----------------------------------
   --       Located in RAM         --
   ----------------------------------

   Interrupt_Handlers_Table : Interrupt_Handlers_Type :=
     (others => Default_Interrupt_Handler'Access);
   --  Table containing handlers attached to the different external interrupts

   pragma Linker_Section (Interrupt_Handlers_Table,
                          ".ram_interrupt_handlers_table");

   pragma Warnings (Off, Interrupt_Handlers_Table);
   --  Suspend warning of same name for exported table
   --  instantiated only one time
   pragma Export (Ada,
                  Interrupt_Handlers_Table,
                  "interrupt_handlers_table");
   pragma Warnings (On, Interrupt_Handlers_Table);
   --  Turn On warnings again

end STM32F103XX.Startup;
