--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                      A R M . A r c h i t e c t u r e                       --
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

------------------------
--  Imported Packages --
------------------------

with System.Storage_Elements;
use  System.Storage_Elements;

--------------------------------------------------------------------------------
--                                ARM.Architecture                            --
--------------------------------------------------------------------------------

package ARM.Architecture is

   pragma Preelaborate;

private

   LD_Data_Start   : Integer_Address;
   pragma Import (Asm, LD_Data_Start, "__data_start");

   LD_Data_End     : Integer_Address;
   pragma Import (Asm, LD_Data_End, "__data_end");

   LD_Data_Load    : Integer_Address;
   pragma Import (Asm, LD_Data_Load, "__data_load");

   LD_Init_Start    : Integer_Address;
   pragma Import (Asm, LD_Init_Start, "__init_start");

   LD_Init_End      : Integer_Address;
   pragma Import (Asm, LD_Init_End, "__init_end");

   LD_BSS_Start    : Integer_Address;
   pragma Import (Asm, LD_BSS_Start, "__bss_start");

   LD_BSS_End      : Integer_Address;
   pragma Import (Asm, LD_BSS_End, "__bss_end");

   LD_Top_Of_Stack : Integer_Address;
   pragma Import (Asm, LD_Top_Of_Stack, "__stack_end");

   pragma Warnings (Off);
   --  Copy initialized variables from flash to sram
   procedure Copy_Data
     with Inline_Always;
   pragma Machine_Attribute (Copy_Data, "naked");

   --  Copy initialized variables from flash to sram
   procedure Clear_BSS
     with Inline_Always;
   pragma Machine_Attribute (Clear_BSS, "naked");

   --  Infinite loop
   --  Other section to define some unneeded __aeabi gcc symbols (see ldscript)
   procedure Hang
     with Linker_Section => ".hang";
   pragma Machine_Attribute (Hang, "naked");

   --  WARNINGS OFF DO NOT WORK ???

   pragma Warnings (On);

end ARM.Architecture;
