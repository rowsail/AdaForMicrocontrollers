--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--        S y s t e m . B B . A r c h i t e c t u r e . S t a r t u p         --
--                                B o d y                                     --
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
with System.Machine_Code;
--  Used for asm

with System.BB.Architecture.CPU;
use System.BB.Architecture.CPU;
--  Used for Get_Cpsr, Set_Cpsr_C, Change_Cpsr_C

package body System.BB.Architecture.Startup is

   package SMC  renames System.Machine_Code;

   LFHT : constant String := ASCII.LF & ASCII.HT;

   ----------------
   --  Mem_Copy  --
   ----------------

   procedure Mem_Copy (Source, Source_End, Dest : System.Address)
   is
   begin
      SMC.Asm (
        "1:"                    & LFHT &
          "cmp   %0, %1"        & LFHT &
          "ittt  lo"            & LFHT &
          "ldrlo r3, [%0], #4"  & LFHT &
          "strlo r3, [%2], #4"  & LFHT &
          "blo   1b",

        Inputs  =>
          (System.Address'Asm_Input ("r", Source),
           System.Address'Asm_Input ("r", Source_End),
           System.Address'Asm_Input ("r", Dest)),
        Volatile => True, Clobber => ("r3"));
   end Mem_Copy;

   ----------------
   --  Mem_Init  --
   ----------------

   procedure Mem_Init (Dest, Dest_End : System.Address;
                       Val            : Interfaces.Unsigned_32)
   is
   begin
      SMC.Asm (
        "1:"                    & LFHT &
          "cmp   %0, %1"        & LFHT &
          "itt   lo"            & LFHT &
          "strlo %2, [%0], #4"  & LFHT &
          "blo   1b",

        Inputs  =>
          (System.Address'Asm_Input         ("r", Dest),
           System.Address'Asm_Input         ("r", Dest_End),
           Interfaces.Unsigned_32'Asm_Input ("r", Val)),
        Volatile => True);
   end Mem_Init;

   ----------------
   --  Init_CPU  --
   ----------------

   procedure Init
   is
      System_Clock_Temp     : Interfaces.Unsigned_32;
      for System_Clock_Temp'Address use System'To_Address (16#4000_6C04#);
      pragma Volatile (System_Clock_Temp);
      --  First backup register

   begin
      Initialize;
      --  Reset clock subsystem & turn on internal 8 MHz oscillator

      Mem_Copy (Data_Load_Start'Address,
                Data_Load_End'Address,
                Data_RAM_Start'Address);
      --  Copy initialized data to RAM

      Mem_Copy (Code_Load_Start'Address,
                Code_Load_End'Address,
                Code_RAM_Start'Address);
      --  Copy program code to RAM

      Mem_Init (BSS_Start'Address, BSS_End'Address, 0);
      --  Clear BSS region

      Mem_Init (Bottom_Of_System_Stack,
                Top_Of_System_Stack,
                16#6d737020#);
      --  Fill Main Stack region with word "msp"

      Mem_Init (Bottom_Of_Environment_Stack,
                Top_Of_Environment_Stack,
                16#70737020#);
      --  Fill Process Stack region with word "psp"

      System_Clock := System_Clock_Temp;
      --  Restore after bss initialization
      --        Init_Peripheral_Clock (Peripheral_Clock_Divider);
      --  Set peripheral clock divider
      --        RAMVectors (SysTick) := SBAI.SysTick_Handler'Address;

      Main (0, System'To_Address (0));
      --  End of startup, goto main program

      loop
         null;
      end loop;
      --  error if we are here

   end Init;

end System.BB.Architecture.Startup;
