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

with Interfaces;

with System.BB.Architecture.CPU;
use System.BB.Architecture.CPU;
--  Used for Get_Cpsr, Set_Cpsr_C, Change_Cpsr_C

--  with System.BB.Architecture.Registers;
--  use System.BB.Architecture.Registers;
--  Used for declaration of hardware registers

package body System.BB.Architecture.Startup is

   package SMC  renames System.Machine_Code;

   use type System.Storage_Elements.Storage_Offset;

   LFHT : constant String := ASCII.LF & ASCII.HT;

   ------------------
   --  Procedures  --
   ------------------

   procedure Initialize_Clock;
   pragma Import (Ada, Initialize_Clock, "initialize_clock");
   --  Initialize clock subsystem

   procedure Initialize_Stack (Stack : out Memory_Storage;
                               Value : System.Address);
   pragma Inline_Always (Initialize_Stack);
   --  Initialize stack with word

   procedure Main;
   pragma Import (C, Main, "main");
   --  Main procedure of all gcc programs

   procedure Mem_Init (Dest, Dest_End : System.Address;
                       Val            : Interfaces.Unsigned_32);
   pragma Machine_Attribute (Entity => Mem_Init,
                             Attribute_Name => "naked");

   procedure Mem_Copy (Source, Source_End, Dest : System.Address);
   pragma Machine_Attribute (Entity => Mem_Copy,
                             Attribute_Name => "naked");

   --     procedure Mem_Cpy (Dest, Source : System.Address; Count : Integer);
   --     pragma Import (Intrinsic, Mem_Cpy, "__builtin_memcpy");

   procedure Set_Stack (Stack : System.Address);
   pragma Machine_Attribute (Entity => Set_Stack,
                             Attribute_Name => "naked");
   pragma Inline_Always (Set_Stack);

   procedure Trampoline_To (Code : System.Address);
   pragma Machine_Attribute (Entity => Trampoline_To,
                             Attribute_Name => "naked");
   pragma Inline_Always (Trampoline_To);

   ------------------------
   --  Initialize_Stack  --
   ------------------------

   procedure Initialize_Stack (Stack : out Memory_Storage;
                               Value : System.Address)
   is
   begin
      for i in Stack'First .. Stack'Last
      loop
         Stack (i) := Value;
      end loop;
   end Initialize_Stack;

   ----------------
   --  Mem_Copy  --
   ----------------

   procedure Mem_Copy (Source, Source_End, Dest : System.Address)
   is
   begin
      SMC.Asm
        (
         --  Check if source=dest
         "cmp     %0, %2"             & LFHT &
           "bxeq    lr"               & LFHT &

         --  if all 16 aligned goto faster procedure
           "mov     r3, %0"           & LFHT &
           "orr     r3, r3, %1"       & LFHT &
           "orr     r3, r3, %2"       & LFHT &
           "ands    r3, r3, #15"      & LFHT &
           "beq     2f"               & LFHT &

         --   copy words one by one
           "1:"                        & LFHT &
           "cmp     %0, %1"            & LFHT &
           "ldrlo   r3, [%0], #4"      & LFHT &
           "strlo   r3, [%2], #4"      & LFHT &
           "blo     1b"                & LFHT &
           "bx      lr"                & LFHT &

         --  copy using 4x4 atn once
           "2:"                        & LFHT &
           "cmp     %0, %1"            & LFHT &
           "ldmloia %0!, {r3, r8-r10}" & LFHT &
           "stmloia %2!, {r3, r8-r10}" & LFHT &
           "blo     2b"                & LFHT &
           "bx      lr",
         Inputs  =>
           (System.Address'Asm_Input ("r", Source),
            System.Address'Asm_Input ("r", Source_End),
            System.Address'Asm_Input ("r", Dest)),
         Volatile => True);
   end Mem_Copy;

   ----------------
   --  Mem_Init  --
   ----------------

   procedure Mem_Init (Dest, Dest_End : System.Address;
                       Val            : Interfaces.Unsigned_32)
   is
   begin
      SMC.Asm (
        "orr    r3, %0, %1"                & LFHT &
          "ands   r3, %2, #15"             & LFHT &
          "beq    2f"                      & LFHT &

          "1:"                             & LFHT &
          "cmp    %0, %1"                  & LFHT &
          "strlo  %2, [%0], #4"            & LFHT &
          "blo    1b"                      & LFHT &
          "bx     lr"                      & LFHT &

          "2:"                             & LFHT &
          "mov    r3, %2"                  & LFHT &
          "mov    r8, %2"                  & LFHT &
          "mov    r9, %2"                  & LFHT &

          "3:"                             & LFHT &
          "cmp    %0,%1"                   & LFHT &
          "stmloia %0!,{%2, r3, r8, r9}"   & LFHT &
          "blo    3b"                      & LFHT &
          "bx     lr",

        Inputs  =>
          (System.Address'Asm_Input         ("r", Dest),
           System.Address'Asm_Input         ("r", Dest_End),
           Interfaces.Unsigned_32'Asm_Input ("r", Val)),
        Volatile => True);
   end Mem_Init;

   ----------------------------------
   --  Bottom_Of_Supervisor_Stack  --
   ----------------------------------

   function Bottom_Of_Supervisor_Stack return System.Address
   is
   begin
      return System_Stack.Supervisor_Stack (1)'Address;
      --  Address of first element is bottom of stack
   end Bottom_Of_Supervisor_Stack;

   -------------------------------
   --  Top_Of_Supervisor_Stack  --
   -------------------------------

   function Top_Of_Supervisor_Stack return System.Address
   is
   begin
      return
        System_Stack.Supervisor_Stack (Supervisor_Stack_Size / 4)'Address;
      --  Address of last element is top of stack

   end Top_Of_Supervisor_Stack;

   -----------------
   --  Set_Stack  --
   -----------------

   procedure Set_Stack (Stack : System.Address)
   is
   begin
      SMC.Asm (
               "mov sp, %0",
               --  Setup frame pointers

               Inputs => System.Address'Asm_Input ("r", Stack),
               Volatile => True);
   end Set_Stack;

   --------------------------
   --  Trampoline_To_Main  --
   --------------------------

   procedure Trampoline_To (Code : System.Address)
   is
   begin
      SMC.Asm (
               --  Setup frame pointers
               "mov a2, #0"     & LFHT &
                 "mov fp, a2"     & LFHT &
                 "mov r7, a2"     & LFHT &

               --  Jump to main using interworking
                 "mov r5, %0"  & LFHT &
                 "mov lr, pc"     & LFHT &
                 "bx     r5",

               Inputs => System.Address'Asm_Input ("r", Code),
               Volatile => True);
   end Trampoline_To;

   ----------------
   --  Init_CPU  --
   ----------------

   procedure Init
   is
   begin
      Set_Cpsr_C (PSR_IRQ or PSR_FIQ or PSR_Undefined_Mode);
      Set_Stack (System_Stack.Undefined_Stack
                 (Undefined_Stack_Size / 4)'Address);
      --  Enter Undefined mode & set stack at top of RAM

      Set_Cpsr_C (PSR_IRQ or PSR_FIQ or PSR_Abort_Mode);
      Set_Stack (System_Stack.Abort_Stack (Abort_Stack_Size / 4)'Address);
      --  Enter Abort mode & set stack

      Set_Cpsr_C (PSR_IRQ or PSR_FIQ or PSR_FIQ_Mode);
      Set_Stack (System_Stack.FIQ_Stack (FIQ_Stack_Size / 4)'Address);
      --  Enter FIQ mode & set stack

      Set_Cpsr_C (PSR_IRQ or PSR_FIQ or PSR_IRQ_Mode);
      Set_Stack (System_Stack.IRQ_Stack (IRQ_Stack_Size / 4)'Address);
      --  Enter IRQ mode & set stack

      Set_Cpsr_C (PSR_IRQ or PSR_FIQ or PSR_System_Mode);
      Set_Stack (Free_RAM_End'Address);
      --  Enter System/User mode & set stack (initialy set to end of RAM)

      Set_Cpsr_C (PSR_IRQ or PSR_FIQ or PSR_Supervisor_Mode);
      Set_Stack (System_Stack.Supervisor_Stack
                 (Supervisor_Stack_Size / 4)'Address);
      --  Enter Supervisor mode & set stack

      Initialize_Clock;
      --  System_Clock saved in not initialised memory region -> .system

      Mem_Copy (Data_Load_Start'Address,
                Data_Load_End'Address,
                Data_RAM_Start'Address);
      --  Copy initialized data to RAM

      Data_RAM := Data_Load;

      --        Mem_Cpy (Data_RAM_Start'Address,
      --                 Data_Load_Start'Address,
      --          Integer (Data_Load_End'Address - Data_Load_Start'Address));
      --
      --        Mem_Copy (Code_Load_Start'Address,
      --                  Code_Load_End'Address,
      --                  Code_RAM_Start'Address);
      --        --  Copy program code to RAM

      Mem_Init (BSS_Start'Address,
                BSS_End'Address,
                0);
      --  Clear BSS region

      Initialize_Stack (System_Stack.Undefined_Stack, 16#73444E55#);
      --  Fill Undefined stack region with word "UNDs" to help debugging

      Initialize_Stack (System_Stack.Abort_Stack, 16#73544241#);
      --  Fill Abort stack region with word "ABTs" to help debugging

      Initialize_Stack (System_Stack.FIQ_Stack, 16#73514946#);
      --  Fill FIQ stack region with word "FIQs" to help debugging

      Initialize_Stack (System_Stack.IRQ_Stack, 16#73515249#);
      --  Fill IRQ stack region with word "IRQs" to help debugging

      Initialize_Stack (System_Stack.Supervisor_Stack, 16#6b617453#);
      --  Fill Supervisor stack region with word "Stak" to help debugging

      Trampoline_To (Main'Address);
      --  End of startup, goto main program using interworking

      loop null; end loop;
      --  error if we are here

   end Init;

end System.BB.Architecture.Startup;
