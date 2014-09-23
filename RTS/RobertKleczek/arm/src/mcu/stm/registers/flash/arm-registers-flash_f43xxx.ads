--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--             A R M . R e g i s t e r s . F L A S H _ F 4 3 X X X            --
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

--  Package defines STM32 F43XXX family flash related registers

-------------------------
--  Imported packages  --
-------------------------

with System;
use  System;

with ARM.Registers;
use  ARM.Registers;

with ARM.Registers.Types;
with ARM.Registers.Bitfield;
with ARM.Registers.Functions;

--------------------------------------------------------------------------------
--                           ARM.Registers.FLASH_F43XXX                         --
--------------------------------------------------------------------------------

package ARM.Registers.FLASH_F43XXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                Flash Access Control Register                        --
   -------------------------------------------------------------------------

   --------------------
   --  ACR Register  --
   --------------------

   package ACR is

      package Tp is new Types (R32);

      package DCRST   is new Bitfield (Tp, 12);
      package ICRST   is new Bitfield (Tp, 11);
      package DCEN    is new Bitfield (Tp, 10);
      package ICEN    is new Bitfield (Tp, 9);
      package PRFTEN  is new Bitfield (Tp, 8);
      package LATENCY is new Bitfield (Tp, 0, 4);

      type T is
         record
            DCRST   : ACR.DCRST.T;
            ICRST   : ACR.ICRST.T;
            DCEN    : ACR.DCEN.T;
            ICEN    : ACR.ICEN.T;
            PRFTEN  : ACR.PRFTEN.T;
            LATENCY : ACR.LATENCY.T;
         end record;

      for T use
         record
            DCRST   at 0 range DCRST.R'First .. DCRST.R'Last;
            ICRST   at 0 range ICRST.R'First .. ICRST.R'Last;
            DCEN    at 0 range DCEN.R'First .. DCEN.R'Last;
            ICEN    at 0 range ICEN.R'First .. ICEN.R'Last;
            PRFTEN  at 0 range PRFTEN.R'First .. PRFTEN.R'Last;
            LATENCY at 0 range LATENCY.R'First .. LATENCY.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end ACR;

   subtype ACR_T is ACR.T;

   --  Field definitions

   function DCRST   is new ACR.FN.B (ACR.DCRST);
   function ICRST   is new ACR.FN.B (ACR.ICRST);
   function DCEN    is new ACR.FN.B (ACR.DCEN);
   function ICEN    is new ACR.FN.B (ACR.ICEN);
   function PRFTEN  is new ACR.FN.B (ACR.PRFTEN);
   function LATENCY is new ACR.FN.B (ACR.LATENCY);

   --  Functions

   function  "+"   is new ACR.FN.Add;
   function  "+"   is new ACR.FN.Add_RM;
   function  "-"   is new ACR.FN.Clear;
   function  Init  is new ACR.FN.Init;

   --  Constant definitions

   function Data_Cache_Is_Not_Reset is new ACR.FN.C (ACR.DCRST, 2#0#);
   function Data_Cache_Is_Reset     is new ACR.FN.C (ACR.DCRST, 2#1#);

   function Instruction_Cache_Is_Not_Reset is new ACR.FN.C (ACR.ICRST, 2#0#);
   function Instruction_Cache_Is_Reset     is new ACR.FN.C (ACR.ICRST, 2#1#);

   function Data_Cache_Is_Disabled is new ACR.FN.C (ACR.DCEN, 2#0#);
   function Data_Cache_Is_Enabled  is new ACR.FN.C (ACR.DCEN, 2#1#);

   function Instruction_Cache_Is_Disabled is new ACR.FN.C (ACR.ICEN, 2#0#);
   function Instruction_Cache_Is_Enabled  is new ACR.FN.C (ACR.ICEN, 2#1#);

   function Prefetch_Is_Disabled is new ACR.FN.C (ACR.PRFTEN, 2#0#);
   function Prefetch_Is_Enabled  is new ACR.FN.C (ACR.PRFTEN, 2#1#);

   function Latency_0WS  is new ACR.FN.C (ACR.LATENCY, 2#0000#);
   function Latency_1WS  is new ACR.FN.C (ACR.LATENCY, 2#0001#);
   function Latency_2WS  is new ACR.FN.C (ACR.LATENCY, 2#0010#);
   function Latency_3WS  is new ACR.FN.C (ACR.LATENCY, 2#0011#);
   function Latency_4WS  is new ACR.FN.C (ACR.LATENCY, 2#0100#);
   function Latency_5WS  is new ACR.FN.C (ACR.LATENCY, 2#0101#);
   function Latency_6WS  is new ACR.FN.C (ACR.LATENCY, 2#0110#);
   function Latency_7WS  is new ACR.FN.C (ACR.LATENCY, 2#0111#);
   function Latency_8WS  is new ACR.FN.C (ACR.LATENCY, 2#1000#);
   function Latency_9WS  is new ACR.FN.C (ACR.LATENCY, 2#1001#);
   function Latency_10WS is new ACR.FN.C (ACR.LATENCY, 2#1010#);
   function Latency_11WS is new ACR.FN.C (ACR.LATENCY, 2#1011#);
   function Latency_12WS is new ACR.FN.C (ACR.LATENCY, 2#1100#);
   function Latency_13WS is new ACR.FN.C (ACR.LATENCY, 2#1101#);
   function Latency_14WS is new ACR.FN.C (ACR.LATENCY, 2#1110#);
   function Latency_15WS is new ACR.FN.C (ACR.LATENCY, 2#1111#);


   --------------------
   --  Key Register  --
   --------------------

   subtype KEYR_T is R32;
   pragma Suppress_Initialization (KEYR_T);

   ---------------------------
   --  Option Key Register  --
   ---------------------------

   subtype OPTKEYR_T is R32;
   pragma Suppress_Initialization (OPTKEYR_T);

   ---------------------
   --  SR Register --
   ---------------------

   package SR is

      package Tp is new Types (R32);

      package BSY    is new Bitfield (Tp, 16);
      package RDERR  is new Bitfield (Tp, 8);
      package PGSERR is new Bitfield (Tp, 7);
      package PGPERR is new Bitfield (Tp, 6);
      package PGAERR is new Bitfield (Tp, 5);
      package WRPERR is new Bitfield (Tp, 4);
      package OPERR  is new Bitfield (Tp, 1);
      package EOP    is new Bitfield (Tp, 0);

      type T is
         record
            BSY    : SR.BSY.T;
            RDERR  : SR.RDERR.T;
            PGSERR : SR.PGSERR.T;
            PGPERR : SR.PGPERR.T;
            PGAERR : SR.PGAERR.T;
            WRPERR : SR.WRPERR.T;
            OPERR  : SR.OPERR.T;
            EOP    : SR.EOP.T;
         end record;

      for T use
         record
            BSY    at 0 range BSY.R'First .. BSY.R'Last;
            RDERR  at 0 range RDERR.R'First .. RDERR.R'Last;
            PGSERR at 0 range PGSERR.R'First .. PGSERR.R'Last;
            PGPERR at 0 range PGPERR.R'First .. PGPERR.R'Last;
            PGAERR at 0 range PGAERR.R'First .. PGAERR.R'Last;
            WRPERR at 0 range WRPERR.R'First .. WRPERR.R'Last;
            OPERR  at 0 range OPERR.R'First .. OPERR.R'Last;
            EOP    at 0 range EOP.R'First .. EOP.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end SR;

   subtype SR_T is SR.T;

   --  Field definitions

   function BSY    is new SR.FN.B (SR.BSY);
   function RDERR  is new SR.FN.B (SR.RDERR);
   function PGSERR is new SR.FN.B (SR.PGSERR);
   function PGPERR is new SR.FN.B (SR.PGPERR);
   function PGAERR is new SR.FN.B (SR.PGAERR);
   function WRPERR is new SR.FN.B (SR.WRPERR);
   function OPERR  is new SR.FN.B (SR.OPERR);
   function EOP    is new SR.FN.B (SR.EOP);

   --  Functions

   function  "+"   is new SR.FN.Add;
   function  "+"   is new SR.FN.Add_RM;
   function  "-"   is new SR.FN.Clear;
   function  Init  is new SR.FN.Init;

   --  Constant definitions

   function No_Flash_Memory_Operation_Ongoing is new SR.FN.C (SR.BSY, 2#0#);
   function Flash_Memory_Operation_Ongoing    is new SR.FN.C (SR.BSY, 2#1#);

   function Reset is new SR.FN.C (SR.RDERR, 2#1#);

   function Reset is new SR.FN.C (SR.PGSERR, 2#1#);

   function Reset is new SR.FN.C (SR.PGPERR, 2#1#);

   function Reset is new SR.FN.C (SR.PGAERR, 2#1#);

   function Reset is new SR.FN.C (SR.WRPERR, 2#1#);

   function Is_Operation_Error is new SR.FN.C (SR.OPERR, 2#1#);

   function Is_End_Of_Operation is new SR.FN.C (SR.EOP, 2#1#);
   function Reset               is new SR.FN.C (SR.EOP, 2#1#);

   -----------------------
   --  Control Register --
   -----------------------

   package CR is

      package Tp is new Types (R32);

      package LOCK  is new Bitfield (Tp, 31);
      package ERRIE is new Bitfield (Tp, 25);
      package EOPIE is new Bitfield (Tp, 24);
      package STRT  is new Bitfield (Tp, 16);
      package MER1  is new Bitfield (Tp, 15);
      package PSIZE is new Bitfield (Tp, 8, 2);
      package SNB   is new Bitfield (Tp, 3, 5);
      package MER   is new Bitfield (Tp, 2);
      package SER   is new Bitfield (Tp, 1);
      package PG    is new Bitfield (Tp, 0);

      type T is
         record
            LOCK  : CR.LOCK.T;
            ERRIE : CR.ERRIE.T;
            EOPIE : CR.EOPIE.T;
            STRT  : CR.STRT.T;
            MER1  : CR.MER1.T;
            PSIZE : CR.PSIZE.T;
            SNB   : CR.SNB.T;
            MER   : CR.MER.T;
            SER   : CR.SER.T;
            PG    : CR.PG.T;
         end record;

      for T use
         record
            LOCK  at 0 range LOCK.R'First .. LOCK.R'Last;
            ERRIE at 0 range ERRIE.R'First .. ERRIE.R'Last;
            EOPIE at 0 range EOPIE.R'First .. EOPIE.R'Last;
            STRT  at 0 range STRT.R'First .. STRT.R'Last;
            MER1  at 0 range MER1.R'First .. MER1.R'Last;
            PSIZE at 0 range PSIZE.R'First .. PSIZE.R'Last;
            SNB   at 0 range SNB.R'First .. SNB.R'Last;
            MER   at 0 range MER.R'First .. MER.R'Last;
            SER   at 0 range SER.R'First .. SER.R'Last;
            PG    at 0 range PG.R'First .. PG.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CR;

   subtype CR_T is CR.T;

   --  Field definitions

   function LOCK  is new CR.FN.B (CR.LOCK);
   function ERRIE is new CR.FN.B (CR.ERRIE);
   function EOPIE is new CR.FN.B (CR.EOPIE);
   function STRT  is new CR.FN.B (CR.STRT);
   function MER1  is new CR.FN.B (CR.MER1);
   function PSIZE is new CR.FN.B (CR.PSIZE);
   function SNB   is new CR.FN.B (CR.SNB);
   function MER   is new CR.FN.B (CR.MER);
   function SER   is new CR.FN.B (CR.SER);
   function PG    is new CR.FN.B (CR.PG);

   --  Functions

   function  "+"   is new CR.FN.Add;
   function  "+"   is new CR.FN.Add_RM;
   function  "-"   is new CR.FN.Clear;
   function  Init  is new CR.FN.Init;

   --  Constant definitions

   function Is_Locked is new CR.FN.C (CR.LOCK, 2#1#);

   function Interrupt_Generation_Disabled is new CR.FN.C (CR.ERRIE, 2#0#);
   function Interrupt_Generation_Enabled  is new CR.FN.C (CR.ERRIE, 2#1#);

   function Interrupt_Generation_Disabled is new CR.FN.C (CR.EOPIE, 2#0#);
   function Interrupt_Generation_Enabled  is new CR.FN.C (CR.EOPIE, 2#1#);

   function Start is new CR.FN.C (CR.STRT, 2#1#);

   function Program_X8  is new CR.FN.C (CR.PSIZE, 2#00#);
   function Program_X16 is new CR.FN.C (CR.PSIZE, 2#01#);
   function Program_X32 is new CR.FN.C (CR.PSIZE, 2#10#);
   function Program_X64 is new CR.FN.C (CR.PSIZE, 2#11#);

   ------------------------------
   --  Option Control Register --
   ------------------------------

   package OPTCR is

      package Tp is new Types (R32);

      package SPRMOD     is new Bitfield (Tp, 31);
      package DB1M       is new Bitfield (Tp, 30);
      package NWRP       is new Bitfield (Tp, 16, 12);
      package RDP        is new Bitfield (Tp, 8, 8);
      package NRST_STDBY is new Bitfield (Tp, 7);
      package NRST_STOP  is new Bitfield (Tp, 6);
      package WDG_SW     is new Bitfield (Tp, 5);
      package BFB2       is new Bitfield (Tp, 4);
      package BOR_LEV    is new Bitfield (Tp, 2, 2);
      package OPTSTRT    is new Bitfield (Tp, 1);
      package OPTLOCK    is new Bitfield (Tp, 0);

      type T is
         record
            SPRMOD     : OPTCR.SPRMOD.T;
            DB1M       : OPTCR.DB1M.T;
            NWRP       : OPTCR.NWRP.T;
            RDP        : OPTCR.RDP.T;
            NRST_STDBY : OPTCR.NRST_STDBY.T;
            NRST_STOP  : OPTCR.NRST_STOP.T;
            WDG_SW     : OPTCR.WDG_SW.T;
            BFB2       : OPTCR.BFB2.T;
            BOR_LEV    : OPTCR.BOR_LEV.T;
            OPTSTRT    : OPTCR.OPTSTRT.T;
            OPTLOCK    : OPTCR.OPTLOCK.T;
         end record;

      for T use
         record
            SPRMOD     at 0 range SPRMOD.R'First .. SPRMOD.R'Last;
            DB1M       at 0 range DB1M.R'First .. DB1M.R'Last;
            NWRP       at 0 range NWRP.R'First .. NWRP.R'Last;
            RDP        at 0 range RDP.R'First .. RDP.R'Last;
            NRST_STDBY at 0 range NRST_STDBY.R'First .. NRST_STDBY.R'Last;
            NRST_STOP  at 0 range NRST_STOP.R'First .. NRST_STOP.R'Last;
            WDG_SW     at 0 range WDG_SW.R'First .. WDG_SW.R'Last;
            BFB2       at 0 range BFB2.R'First .. BFB2.R'Last;
            BOR_LEV    at 0 range BOR_LEV.R'First .. BOR_LEV.R'Last;
            OPTSTRT    at 0 range OPTSTRT.R'First .. OPTSTRT.R'Last;
            OPTLOCK    at 0 range OPTLOCK.R'First .. OPTLOCK.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end OPTCR;

   subtype OPTCR_T is OPTCR.T;

   --  Field definitions

   function SPRMOD     is new OPTCR.FN.B (OPTCR.SPRMOD);
   function DB1M       is new OPTCR.FN.B (OPTCR.DB1M);
   function NWRP       is new OPTCR.FN.B (OPTCR.NWRP);
   function RDP        is new OPTCR.FN.B (OPTCR.RDP);
   function NRST_STDBY is new OPTCR.FN.B (OPTCR.NRST_STDBY);
   function NRST_STOP  is new OPTCR.FN.B (OPTCR.NRST_STOP);
   function WDG_SW     is new OPTCR.FN.B (OPTCR.WDG_SW);
   function BFB2       is new OPTCR.FN.B (OPTCR.BFB2);
   function BOR_LEV    is new OPTCR.FN.B (OPTCR.BOR_LEV);
   function OPTSTRT    is new OPTCR.FN.B (OPTCR.OPTSTRT);
   function OPTLOCK    is new OPTCR.FN.B (OPTCR.OPTLOCK);

   --  Functions

   function  "+"   is new OPTCR.FN.Add;
   function  "+"   is new OPTCR.FN.Add_RM;
   function  "-"   is new OPTCR.FN.Clear;
   function  Init  is new OPTCR.FN.Init;

   --  Constant definitions

   function PCROP_Disabled is new OPTCR.FN.C (OPTCR.SPRMOD, 2#0#);
   function PCROP_Enabled  is new OPTCR.FN.C (OPTCR.SPRMOD, 2#1#);

   function Single_Bank_1M is new OPTCR.FN.C (OPTCR.DB1M, 2#0#);
   function Dual_Bank_1M   is new OPTCR.FN.C (OPTCR.DB1M, 2#1#);

   function Dual_Bank_Boot_Disabled is new OPTCR.FN.C (OPTCR.BFB2, 2#0#);
   function Dual_Bank_Boot_Enabled  is new OPTCR.FN.C (OPTCR.BFB2, 2#1#);

   function BOR_Level_3   is new OPTCR.FN.C (OPTCR.BOR_LEV, 2#00#);
   function BOR_Level_2   is new OPTCR.FN.C (OPTCR.BOR_LEV, 2#01#);
   function BOR_Level_1   is new OPTCR.FN.C (OPTCR.BOR_LEV, 2#10#);
   function BOR_Level_Off is new OPTCR.FN.C (OPTCR.BOR_LEV, 2#11#);

   function Start is new OPTCR.FN.C (OPTCR.OPTSTRT, 2#1#);

   function Is_Locked is new OPTCR.FN.C (OPTCR.OPTLOCK, 2#1#);

   --------------------------------
   --  Option Control Register 1 --
   --------------------------------

   package OPTCR1 is

      package Tp is new Types (R32);

      package NWRP is new Bitfield (Tp, 16, 12);

      type T is
         record
            NWRP : OPTCR1.NWRP.T;
         end record;

      for T use
         record
            NWRP at 0 range NWRP.R'First .. NWRP.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end OPTCR1;

   subtype OPTCR1_T is OPTCR1.T;

   --  Field definitions

   function NWRP is new OPTCR1.FN.B (OPTCR1.NWRP);

   --  Functions

   function  "+"   is new OPTCR1.FN.Add;
   function  "+"   is new OPTCR1.FN.Add_RM;
   function  "-"   is new OPTCR1.FN.Clear;
   function  Init  is new OPTCR1.FN.Init;

   --------------------------------------------------------------------------
   --                         Register definition                          --
   --------------------------------------------------------------------------

   type FLASH_T is
      record
         ACR      : ACR_T;
         KEYR     : KEYR_T;
         OPTKEYR  : OPTKEYR_T;
         SR       : SR_T;
         CR       : CR_T;
         OPTCR    : OPTCR_T;
         OPTCR1   : OPTCR1_T;
         pragma Volatile (ACR);
         pragma Volatile (KEYR);
         pragma Volatile (OPTKEYR);
         pragma Volatile (SR);
         pragma Volatile (CR);
         pragma Volatile (OPTCR);
         pragma Volatile (OPTCR1);
      end record;

   for FLASH_T use
      record
         ACR     at 16#00# range 0 .. 31;
         KEYR    at 16#04# range 0 .. 31;
         OPTKEYR at 16#08# range 0 .. 31;
         SR      at 16#0C# range 0 .. 31;
         CR      at 16#10# range 0 .. 31;
         OPTCR   at 16#14# range 0 .. 31;
         OPTCR1  at 16#18# range 0 .. 31;
      end record;

   FLASH : FLASH_T;

   for FLASH'Address use System'To_Address (16#4002_2000#);

end ARM.Registers.FLASH_F43XXX;
