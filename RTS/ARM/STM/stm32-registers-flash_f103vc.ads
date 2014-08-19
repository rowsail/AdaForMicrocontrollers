--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--           S T M 3 2 . R e g i s t e r s . F L A S H _ F 1 0 3 V C          --
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

--  Package defines STM32 F103VC family flash related registers

-------------------------
--  Imported packages  --
-------------------------

with System;
use  System;

with Bitfields;
use  Bitfields;

with Bitfields.Types;
with Bitfields.Bitfield;
with Bitfields.Functions;

-----------------------------------------------------------------------------
--                           MCU.Registers.FLASH                           --
-----------------------------------------------------------------------------

package STM32.Registers.FLASH_F103VC is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                           Flash Control                             --
   -------------------------------------------------------------------------

   ---------------------
   --  ACR Register --
   ---------------------

   package ACR is

      package Tp is new Types (R32);

      package PRFTBS  is new Bitfield (Tp, 5);
      package PRFTBE  is new Bitfield (Tp, 4);
      package HLFCYA  is new Bitfield (Tp, 3);
      package LATENCY is new Bitfield (Tp, 0, 3);

      type T is
         record
            PRFTBS  : ACR.PRFTBS.T;
            PRFTBE  : ACR.PRFTBE.T;
            HLFCYA  : ACR.HLFCYA.T;
            LATENCY : ACR.LATENCY.T;
         end record;

      for T use
         record
            PRFTBS  at 0 range PRFTBS.R'First .. PRFTBS.R'Last;
            PRFTBE  at 0 range PRFTBE.R'First .. PRFTBE.R'Last;
            HLFCYA  at 0 range HLFCYA.R'First .. HLFCYA.R'Last;
            LATENCY at 0 range LATENCY.R'First .. LATENCY.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end ACR;

   subtype ACR_T is ACR.T;

   --  Field definitions

   function PRFTBS  is new ACR.FN.B (ACR.PRFTBS);
   function PRFTBE  is new ACR.FN.B (ACR.PRFTBE);
   function HLFCYA  is new ACR.FN.B (ACR.HLFCYA);
   function LATENCY is new ACR.FN.B (ACR.LATENCY);

   --  Functions

   function  "+"   is new ACR.FN.Add;
   function  "+"   is new ACR.FN.Add_RM;
   function  "-"   is new ACR.FN.Clear;
   function  Init  is new ACR.FN.Init;

   --  Constant definitions

   function Latency_0 is new ACR.FN.C (ACR.LATENCY, 2#000#);
   function Latency_1 is new ACR.FN.C (ACR.LATENCY, 2#001#);
   function Latency_2 is new ACR.FN.C (ACR.LATENCY, 2#010#);

   --     FLASH Registers Collection  --

   type FLASH_T is
      record
         ACR : ACR_T;
         pragma Volatile (ACR);
      end record;

   for FLASH_T use
      record
         ACR       at 16#00# range 0 .. 31;
      end record;

   FLASH : FLASH_T;

   for FLASH'Address use System'To_Address (16#4002_2000#);

end STM32.Registers.FLASH_F103VC;
