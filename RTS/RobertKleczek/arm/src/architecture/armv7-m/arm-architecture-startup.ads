--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--              A R M . A r c h i t e c t u r e . S t a r t u p               --
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

--  Generic startup file for ARMv7E-M architecture

--------------------------------------------------------------------------------
--                            ARM.Architecture.Startup                        --
--------------------------------------------------------------------------------

generic
   Link_To_ROM                      : Boolean;
   Has_FPU                          : Boolean;
   Number_Of_Implemented_Interrupts : Natural;
   Number_Of_Implemented_Traps      : Natural := 16;
package ARM.Architecture.Startup is

   pragma Preelaborate;
   
private

   Last_Interrupt : constant Natural :=
                      Number_Of_Implemented_Interrupts - 1;
   
   -------------------------
   --  Imported handlers  --
   -------------------------

   pragma Warnings (Off);
   procedure Start;
   pragma Machine_Attribute (Start, "naked");
   pragma Export (Asm, Start, "_start");
   pragma Warnings (On);

   procedure Interrupt_Request_Handler;
   pragma Import (Asm, Interrupt_Request_Handler, "__gnat_irq_trap");

   procedure SV_Call_Handler;
   pragma Import (Asm, SV_Call_Handler, "__gnat_sv_call_trap");

   procedure Pend_SV_Handler;
   pragma Import (Asm, Pend_SV_Handler, "__gnat_pend_sv_trap");

   procedure Sys_Tick_Handler;
   pragma Import (Asm, Sys_Tick_Handler, "__gnat_sys_tick_trap");

   -------------------------------------
   --  System vectors handlers table  --
   -------------------------------------
   --         Located in ROM          --
   -------------------------------------

   System_Traps : 
   constant array (1 .. Number_Of_Implemented_Traps) of System.Address 
     := (LD_Top_Of_Stack'Address,
         Start'Address,
         Hang'Address,
         Hang'Address,
         Hang'Address,
         Hang'Address,
         Hang'Address,
         Hang'Address,
         Hang'Address,
         Hang'Address,
         Hang'Address,
         SV_Call_Handler'Address,
         Hang'Address,
         Hang'Address,
         Pend_SV_Handler'Address,
         Sys_Tick_Handler'Address) 
     with Linker_Section => ".system_vectors";

   -------------------------------------
   --  System vectors handlers table  --
   -------------------------------------
   --         Located in ROM          --
   -------------------------------------

   User_Interrupts : constant array (0 .. Last_Interrupt) of System.Address
     := (others => Interrupt_Request_Handler'Address) 
     with Linker_Section => ".user_vectors";
   
end ARM.Architecture.Startup;
