--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--               A R M . R e g i s t e r . S C B _ F 1 0 3 V C                --
--                                   S p e c                                  --
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

--  Package defines STM32 F103VC family system control block related registers

-------------------------
--  Imported packages  --
-------------------------

with ARM.Register.Types;
with ARM.Register.Register;

-----------------------------------------------------------------------------
--                           ARM.Register.SCB_F103VC                       --
-----------------------------------------------------------------------------

package ARM.Register.SCB_F103VC is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                                 SCB                                 --
   -------------------------------------------------------------------------

   --     SCB Registers Collection  --

   --------------------
   --  CPU Identity  --
   --------------------

   package SCB_CPUID is

      package Tp is new Types (R32);

      type T is new Tp.Reg;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end SCB_CPUID;

   package CPUID    is new Register (SCB_CPUID.T, SCB_CPUID.Tp, 16#E000_ED00#);
   subtype CPUID_T  is CPUID.T;

   ---------------------------
   --  Option Key Register  --
   ---------------------------

   package SCB_ICSR is

      package Tp is new Types (R32);

      type T is new Tp.Reg;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end SCB_ICSR;

   package ICSR    is new Register (SCB_ICSR.T, SCB_ICSR.Tp, 16#E000_ED04#);
   subtype ICSR_T  is ICSR.T;

end ARM.Register.SCB_F103VC;
