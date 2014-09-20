--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--               A R M . A r c h i t e c t u r e . S t a r t u p              --
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

with System;
use  System;

with ARM.Architecture.ASM;
use  ARM.Architecture.ASM;

with ARM.Registers.System_Control;
use  ARM.Registers.System_Control;

--------------------------------------------------------------------------------
--                           ARM.Architecture.Startup                         --
--------------------------------------------------------------------------------

package body ARM.Architecture.Startup is

   procedure Setup_PLL;
   pragma Import (Ada, Setup_PLL, "_ada_setup_pll");

   procedure Main;
   pragma Import (Asm, Main, "main");

   procedure ExitExit;
   pragma Import (Asm, ExitExit, "exit");

   -------------
   --  Start  --
   -------------

   procedure Start is

   begin
      --  At reset time stack is always set with value from 16#00000000#
      --  so, when we start from RAM we must manually insert
      --  code to initialize stack
      if not Link_To_ROM then
         Set_Stack (LD_Top_Of_Stack'Address);
      end if;

      --  Insert initialization code for FPU when needed
      if Has_FPU then
         --  Turn on FPU
         System_Control.CPACR := System_Control.CPACR + CP10 (Full_Access) +
           CP11 (Full_Access);
         --  Wait for bus
         DSB;
         ISB;
      end if;

      --  Insert code for copying initialized variables to RAM
      if Link_To_ROM then
         Copy_Data;
      end if;

      --  Clear BSS region
      Clear_BSS;

      --  Setup PLL's
      Setup_PLL;

      --  Call main program
      Main;

      ExitExit;  --  double Exit because of reserved word "exit"

   end Start;

end ARM.Architecture.Startup;
