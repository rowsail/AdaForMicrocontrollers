--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--          A  R M . R e g i s t e r s . S y s t e m _ C o n t r o l          --
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

--  Package defines ARMv7e-m family system control block related registers
--                      TO BE COMPLETED !!!

-------------------------
--  Imported packages  --
-------------------------

with System;
use  System;

with ARM.Registers;
use  ARM.Registers;

with ARM.Registers.Types;
with ARM.Registers.Functions;
with ARM.Registers.Bitfield;

--------------------------------------------------------------------------------
--                      ARM.Registers.System_Control                          --
--------------------------------------------------------------------------------

package ARM.Registers.System_Control is

   pragma Preelaborate;

   --------------------------------------------------------------------------
   --                           System_Control                             --
   --------------------------------------------------------------------------

   --     System_Control Registers Collection  --

   subtype CPUID_T is R32;
   pragma Suppress_Initialization (CPUID_T);
   --  Type definition of hardware register: CPUID

   subtype ICSR_T is R32;
   pragma Suppress_Initialization (ICSR_T);
   --  Type definition of hardware register: ICSR

   ---------------------
   --  CPACR Register --
   ---------------------

   package CPACR is

      package Tp is new Types (R32);

      package CP11 is new Bitfield (Tp, 22, 2);
      package CP10 is new Bitfield (Tp, 20, 2);


      type T is
         record
            CP11 : CPACR.CP11.T;
            CP10 : CPACR.CP10.T;

         end record;

      for T use
         record
            CP11 at 0 range CP11.R'First .. CP11.R'Last;
            CP10 at 0 range CP10.R'First .. CP10.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CPACR;

   subtype CPACR_T is CPACR.T;

   --  Field definitions

   function CP11 is new CPACR.FN.B (CPACR.CP11);
   function CP10 is new CPACR.FN.B (CPACR.CP10);

   --  Functions

   function  "+"   is new CPACR.FN.Add;
   function  "+"   is new CPACR.FN.Add_RM;
   function  "-"   is new CPACR.FN.Clear;
   function  Init  is new CPACR.FN.Init;

   --  Constant definitions

   function Access_Denied     is new CPACR.FN.C (CPACR.CP11, 2#00#);
   function Privileged_Access is new CPACR.FN.C (CPACR.CP11, 2#01#);
   function Full_Access       is new CPACR.FN.C (CPACR.CP11, 2#11#);
   function Access_Denied     is new CPACR.FN.C (CPACR.CP10, 2#00#);
   function Privileged_Access is new CPACR.FN.C (CPACR.CP10, 2#01#);
   function Full_Access       is new CPACR.FN.C (CPACR.CP10, 2#11#);

   --------------------------------------------------------------------------
   --                         Register definition                          --
   --------------------------------------------------------------------------

   type System_Control_T is
      record
         CPUID : CPUID_T;
         ICSR  : ICSR_T;
         CPACR : CPACR_T;
         pragma Volatile (CPUID);
         pragma Volatile (ICSR);
         pragma Volatile (CPACR);
      end record;

   for System_Control_T use
      record
         CPUID at 16#00# range 0 .. 31;
         ICSR  at 16#04# range 0 .. 31;
         CPACR at 16#88# range 0 .. 31;
      end record;

   System_Control : System_Control_T;

   for System_Control'Address use System'To_Address (16#E000_ED00#);

end ARM.Registers.System_Control;
