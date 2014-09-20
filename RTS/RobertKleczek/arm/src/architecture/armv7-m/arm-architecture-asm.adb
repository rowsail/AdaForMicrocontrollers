--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                  A R M . A r c h i t e c t u r e . A S M                   --
--                                  B o d y                                   --
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
use  System.Machine_Code;

--------------------------------------------------------------------------------
--                              ARM.Architecture.ASM                          --
--------------------------------------------------------------------------------

package body ARM.Architecture.ASM is

   package SMC renames System.Machine_Code;

   -----------
   --  DSB  --
   -----------

   procedure DSB is
   begin
      SMC.Asm (
               "dsb",
               Volatile => True);
   end DSB;

   -----------
   --  ISB  --
   -----------

   procedure ISB is
   begin
      SMC.Asm (
               "isb",
               Volatile => True);
   end ISB;

   -----------------
   --  Set_Stack  --
   -----------------

   procedure Set_Stack (Stack : System.Address) is
   begin
      SMC.Asm (
               "ldr sp,=%0",
               Inputs   => (System.Address'Asm_Input ("X", Stack)),
               Volatile => True,
               Clobber  => ("sp"));
   end Set_Stack;

end ARM.Architecture.ASM;
