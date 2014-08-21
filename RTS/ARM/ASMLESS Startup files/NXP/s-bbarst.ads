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

with System;

with System.BB.Architecture.Interrupts;
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

   -----------------------
   --  Typed constants  --
   -----------------------

   function Top_Of_Supervisor_Stack return System.Address;
   pragma Inline_Always (Top_Of_Supervisor_Stack);
   --  Returning top location of environment task

   function Bottom_Of_Supervisor_Stack return System.Address;
   pragma Inline_Always (Bottom_Of_Supervisor_Stack);
   --  Returning bottom location of environment task

private

   -----------------
   --  Constants  --
   -----------------

   Undefined_Stack_Size    : constant := 4;
   pragma Assert (Undefined_Stack_Size rem 4 = 0);
   --  Size of Undefined Instruction stack should be word aligned

   Abort_Stack_Size        : constant := 4;
   pragma Assert (Abort_Stack_Size rem 4 = 0);
   --  Size of Prefetch/Data Abort stack should be word aligned

   FIQ_Stack_Size          : constant := 4;
   pragma Assert (FIQ_Stack_Size rem 4 = 0);
   --  Size of FIQ stack should be word aligned

   IRQ_Stack_Size          : constant := 128;
   pragma Assert (IRQ_Stack_Size rem 4 = 0);
   --  Size of IRQ stack should be word aligned

   -------------
   --  Types  --
   -------------

   type Vector is mod 2 ** 32;
   --  CPU ISR vector size

   type Stack_Type is
      record
         Undefined_Stack  : Memory_Storage (1 .. Undefined_Stack_Size / 4);
         Abort_Stack      : Memory_Storage (1 .. Abort_Stack_Size / 4);
         FIQ_Stack        : Memory_Storage (1 .. FIQ_Stack_Size / 4);
         IRQ_Stack        : Memory_Storage (1 .. IRQ_Stack_Size / 4);
         Supervisor_Stack : Memory_Storage (1 .. Supervisor_Stack_Size / 4);
      end record;

   pragma Pack (Stack_Type);

   -----------------------
   --  Typed constants  --
   -----------------------

   LDR_PC_PC : constant Vector := 16#E59FF000#;
   --  Constant defines LDR PC, [PC + #00]

   --------------------
   --  ISR Handlers  --
   --------------------

   procedure Init;
   --  Reset_Handler
   --  Main entry point equals reset
   pragma Machine_Attribute (Entity => Init,
                             Attribute_Name => "naked");
   procedure Reset_Handler renames Init;

   --------------------
   --  System_Stack  --
   --------------------

   System_Stack : Stack_Type;
   pragma Linker_Section (System_Stack, ".ram_system_stack");
   --  Create Stack after .bss section location

   ----------------------
   --  System Vectors  --
   ----------------------

   Vectors : constant array (SBAI.ISR_Type'Range) of Vector :=
     (others => LDR_PC_PC + 16#18#);
   --  Initialy set necessary vectors

   pragma Linker_Section (Vectors, ".rom_system_vectors");

   pragma Export (C, Vectors, "entry");
   --  Export is needed for linker to start code chain

   Handlers : constant array (SBAI.ISR_Type'Range) of System.Address :=
     (SBAI.Reset =>
        System'To_Address (Reset_Handler'Address),
      SBAI.Undefined_Instruction =>
        System'To_Address (SBAI.Undefined_Instruction_Handler'Address),
      SBAI.Software_Interrupt =>
        System'To_Address (SBAI.Software_Interrupt_Handler'Address),
      SBAI.Prefetch_Abort =>
        System'To_Address (SBAI.Prefetch_Abort_Handler'Address),
      SBAI.Data_Abort =>
        System'To_Address (SBAI.Data_Abort_Handler'Address),
      SBAI.Reserved =>
        System'To_Address (SBAI.Reserved_Handler'Address),
      SBAI.IRQ =>
        System'To_Address (SBAI.IRQ_Handler'Address),
      SBAI.FIQ =>
        System'To_Address (SBAI.FIQ_Handler'Address));
   --  Handlers stores ISR routines addresses

   pragma Linker_Section (Handlers, ".rom_system_handlers");
   --  Must be same section as Vectors to keep linker dependencies

end System.BB.Architecture.Startup;
