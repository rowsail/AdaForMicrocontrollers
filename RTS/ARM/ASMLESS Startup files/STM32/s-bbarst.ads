--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--        S y s t e m . B B . A r c h i t e c t u r e . S t a r t u p         --
--                                S p e c                                     --
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

--  Package implements Startup routines

------------------------
--  Imported Packages --
------------------------

with Interfaces;
with System;
--  with System.Storage_Elements;

with System.BB.Architecture.Interrupts;
--  Used for ISR_Type & Handler procedures

with System.BB.Architecture.CPU;
--  Used for ISR_Type & Handler procedures

-----------------------------------------------------------------------------
--                   System.BB.Architecture.Startup                        --
-----------------------------------------------------------------------------

package System.BB.Architecture.Startup is
   --  This package & their children
   --  contains possibly all CPU dependent primitives
   --  An ARM version

   pragma Preelaborate;

   package SBAI renames System.BB.Architecture.Interrupts;
   package SBAC renames System.BB.Architecture.CPU;
   --     package SSE  renames System.Storage_Elements;

   --     use type SSE.Storage_Offset;

   -----------------------
   --  Typed constants  --
   -----------------------

   Stack_End                   : constant System.Address;
   pragma Import (Asm, Stack_End, "__stack_end__");

   Top_Of_System_Stack         : constant System.Address := Stack_End'Address;

   Bottom_Of_System_Stack      : constant System.Address :=
     Top_Of_System_Stack - Size_Of_System_Stack;

   Top_Of_Environment_Stack    : constant System.Address :=
     Top_Of_System_Stack - Size_Of_System_Stack + 4;

   Bottom_Of_Environment_Stack : constant System.Address :=
     Top_Of_Environment_Stack - Size_Of_Environment_Stack;

private

   -------------
   --  Types  --
   -------------

   type Vector is mod 2 ** 32;
   --  CPU ISR vector size

   ------------------------
   --  Linker Variables  --
   ------------------------

   Data_Load_Start : System.Address;
   pragma Import (Asm, Data_Load_Start, "_etext");

   Data_Load_End : System.Address;
   pragma Import (Asm, Data_Load_End, "_edata");

   Data_Ram_Start : System.Address;
   pragma Import (Asm, Data_Ram_Start, "_data");

   Ramtext_Load_Start : System.Address;
   pragma Import (Asm, Ramtext_Load_Start, "__ramtext_load_start__");

   Ramtext_Load_End : System.Address;
   pragma Import (Asm, Ramtext_Load_End, "__ramtext_load_end__");

   Ramtext_Ram_Start : System.Address;
   pragma Import (Asm, Ramtext_Ram_Start, "__ramtext_ram_start__");

   BSS_Start : constant System.Address;
   pragma Import (Asm, BSS_Start, "__bss_start__");

   BSS_End : constant System.Address;
   pragma Import (Asm, BSS_End, "__bss_end__");

   ------------------
   --  Procedures  --
   ------------------

   procedure Main (argc : Integer; argv : System.Address);
   pragma Import (C, Main, "main");
   --  Main procedure of all gcc programs

   procedure Initialize;
   pragma Import (Ada, Initialize, "initialize_clock");
   --  Initialize clock subsystem

   procedure Mem_Copy (Source, Source_End, Dest : System.Address);
   pragma Inline_Always (Mem_Copy);

   procedure Mem_Init (Dest, Dest_End : System.Address;
                       Val            : Interfaces.Unsigned_32);
   pragma Inline_Always (Mem_Init);

   --------------------
   --  ISR Handlers  --
   --------------------

   procedure Init;
   --  Reset_Handler
   --  Main entry point equals reset
   pragma Machine_Attribute (Entity => Init,
                             Attribute_Name => "naked");
   procedure Reset_Handler renames Init;

   ----------------------
   --  System Vectors  --
   ----------------------

   type System_Vectors is (InitialStack, Reset,    NMI,        HardFault,
                           MemManage,    BusFault, UsageFault, Res7,
                           Res8,         Res9,     Res10,      SVC,
                           DebugMon,     Res13,    PendSV,     SysTick);

   System_Vectors_ROM :
   constant array (InitialStack .. SysTick) of System.Address :=
     (
      Top_Of_System_Stack,
      Reset_Handler'Address,            --  Reset handler
      SBAI.NMI_Handler'Address,         --  Non maskable handler
      SBAI.HardFault_Handler'Address,   --  Hard fault handler
      SBAI.MemManage_Handler'Address,   --  Mem manage handler
      SBAI.BusFault_Handler'Address,    --  Bus fault handler
      SBAI.UsageFault_Handler'Address,  --  Usage fault handler
      0,
      0,
      0,
      0,
      SBAI.SVC_Handler'Address,         --  System service handler
      SBAI.DebugMon_Handler'Address,    --  Debug mon handler
      0,
      SBAI.PendSV_Handler'Address,      --  Pend switch handler
      SBAI.SysTick_Handler'Address      --  System ticker handler
     );

   pragma Linker_Section (System_Vectors_ROM, ".rom_system_vectors");
   --  Initialy set necessary vectors

   pragma Export_Object (System_Vectors_ROM, "entry");
   --  Export is needed for linker to start code chain

end System.BB.Architecture.Startup;
