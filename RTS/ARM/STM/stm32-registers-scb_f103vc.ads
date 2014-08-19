--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--              S T M 3 2 . R e g i s t e r s . S C B _ F 1 0 3 V C           --
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

with System;
use  System;

-----------------------------------------------------------------------------
--                           MCU.Registers.SCB                             --
-----------------------------------------------------------------------------

package STM32.Registers.SCB_F103VC is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                                 SCB                                 --
   -------------------------------------------------------------------------

   --     SCB Registers Collection  --

   subtype CPUID_T is R32;
   pragma Suppress_Initialization (CPUID_T);
   --  Type definition of hardware register: CPUID

   subtype ICSR_T is R32;
   pragma Suppress_Initialization (ICSR_T);
   --  Type definition of hardware register: ICSR

   type SCB_T is
      record
         CPUID : CPUID_T;
         ICSR  : ICSR_T;
         pragma Volatile (CPUID);
         pragma Volatile (ICSR);
      end record;

   for SCB_T use
      record
         CPUID at 16#00# range 0 .. 31;
         ICSR  at 16#04# range 0 .. 31;
      end record;

   SCB : SCB_T;

   for SCB'Address use System'To_Address (16#E000_ED00#);

   -------------------------------------------------------------------------
   --                         FIELDS Definition                           --
   -------------------------------------------------------------------------

end STM32.Registers.SCB_F103VC;
