--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--             A R M . R e g i s t e r . F L A S H _ F 4 1 X X X            --
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

--  Package defines STM32 F41XXX family flash related registers

-------------------------
--  Imported packages  --
-------------------------

with ARM.Register;
use  ARM.Register;

with ARM.Register.Types;
with ARM.Register.Register;
with ARM.Register.Bitfield;

--------------------------------------------------------------------------------
--                            ARM.Register.FLASH_F41XXX                       --
--------------------------------------------------------------------------------

package ARM.Register.FLASH_F41XXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                Flash Access Control Register                        --
   -------------------------------------------------------------------------

   --------------------
   --  ACR Register  --
   --------------------

   package FLASH_ACR is

      package Tp is new Types (R32);

      package DCRST   is new Bitfield (Tp, 12);
      package ICRST   is new Bitfield (Tp, 11);
      package DCEN    is new Bitfield (Tp, 10);
      package ICEN    is new Bitfield (Tp, 9);
      package PRFTEN  is new Bitfield (Tp, 8);
      package LATENCY is new Bitfield (Tp, 0, 3);

      type T is
         record
            DCRST   : FLASH_ACR.DCRST  .T;
            ICRST   : FLASH_ACR.ICRST  .T;
            DCEN    : FLASH_ACR.DCEN   .T;
            ICEN    : FLASH_ACR.ICEN   .T;
            PRFTEN  : FLASH_ACR.PRFTEN .T;
            LATENCY : FLASH_ACR.LATENCY.T;
         end record;

      for T use
         record
            DCRST   at 0 range DCRST  .F .. DCRST  .L;
            ICRST   at 0 range ICRST  .F .. ICRST  .L;
            DCEN    at 0 range DCEN   .F .. DCEN   .L;
            ICEN    at 0 range ICEN   .F .. ICEN   .L;
            PRFTEN  at 0 range PRFTEN .F .. PRFTEN .L;
            LATENCY at 0 range LATENCY.F .. LATENCY.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_ACR;

   package ACR    is new Register (FLASH_ACR.T, FLASH_ACR.Tp, 16#4002_3C00#);
   subtype ACR_T  is ACR.T;
   subtype ACR_F  is ACR.F;
   

   --  Field definitions

   function DCRST   is new ACR.B (FLASH_ACR.DCRST  ) with Inline_Always;
   function ICRST   is new ACR.B (FLASH_ACR.ICRST  ) with Inline_Always;
   function DCEN    is new ACR.B (FLASH_ACR.DCEN   ) with Inline_Always;
   function ICEN    is new ACR.B (FLASH_ACR.ICEN   ) with Inline_Always;
   function PRFTEN  is new ACR.B (FLASH_ACR.PRFTEN ) with Inline_Always;
   function LATENCY is new ACR.B (FLASH_ACR.LATENCY) with Inline_Always;

   --  Functions

   function "+"  is new ACR.Add      with Inline_Always;
   function "+"  is new ACR.Add_F    with Inline_Always;
   function "+"  is new ACR.Add_FF   with Inline_Always;
   function "-"  is new ACR.Clear    with Inline_Always;
   function "-"  is new ACR.Clear_FF with Inline_Always;
   function "="  is new ACR.Equal    with Inline_Always;
   function Init is new ACR.Init     with Inline_Always;

   --  Constant definitions

   function Data_Cache_Is_Not_Reset is new ACR.C (FLASH_ACR.DCRST, 2#0#) with Inline_Always;
   function Data_Cache_Is_Reset     is new ACR.C (FLASH_ACR.DCRST, 2#1#) with Inline_Always;

   function Instruction_Cache_Is_Not_Reset is new ACR.C (FLASH_ACR.ICRST, 2#0#) with Inline_Always;
   function Instruction_Cache_Is_Reset     is new ACR.C (FLASH_ACR.ICRST, 2#1#) with Inline_Always;

   function Data_Cache_Is_Disabled is new ACR.C (FLASH_ACR.DCEN, 2#0#) with Inline_Always;
   function Data_Cache_Is_Enabled  is new ACR.C (FLASH_ACR.DCEN, 2#1#) with Inline_Always;

   function Instruction_Cache_Is_Disabled is new ACR.C (FLASH_ACR.ICEN, 2#0#) with Inline_Always;
   function Instruction_Cache_Is_Enabled  is new ACR.C (FLASH_ACR.ICEN, 2#1#) with Inline_Always;

   function Prefetch_Is_Disabled is new ACR.C (FLASH_ACR.PRFTEN, 2#0#) with Inline_Always;
   function Prefetch_Is_Enabled  is new ACR.C (FLASH_ACR.PRFTEN, 2#1#) with Inline_Always;

   function Latency_0WS is new ACR.C (FLASH_ACR.LATENCY, 2#000#) with Inline_Always;
   function Latency_1WS is new ACR.C (FLASH_ACR.LATENCY, 2#001#) with Inline_Always;
   function Latency_2WS is new ACR.C (FLASH_ACR.LATENCY, 2#010#) with Inline_Always;
   function Latency_3WS is new ACR.C (FLASH_ACR.LATENCY, 2#011#) with Inline_Always;
   function Latency_4WS is new ACR.C (FLASH_ACR.LATENCY, 2#100#) with Inline_Always;
   function Latency_5WS is new ACR.C (FLASH_ACR.LATENCY, 2#101#) with Inline_Always;
   function Latency_6WS is new ACR.C (FLASH_ACR.LATENCY, 2#110#) with Inline_Always;
   function Latency_7WS is new ACR.C (FLASH_ACR.LATENCY, 2#111#) with Inline_Always;

   --------------------
   --  Key Register  --
   --------------------

   package FLASH_KEYR is

      package Tp is new Types (R32);

      type T is new Tp.Reg;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_KEYR;

   package KEYR    is new Register (FLASH_KEYR.T, FLASH_KEYR.Tp, 16#4002_2004#);
   subtype KEYR_T  is KEYR.T;

   ---------------------------
   --  Option Key Register  --
   ---------------------------

   package FLASH_OPTKEYR is

      package Tp is new Types (R32);

      type T is new Tp.Reg;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_OPTKEYR;

   package OPTKEYR    is new Register (FLASH_OPTKEYR.T, FLASH_OPTKEYR.Tp, 16#4002_2008#);
   subtype OPTKEYR_T  is OPTKEYR.T;

   ---------------------
   --  SR Register --
   ---------------------

   package FLASH_SR is

      package Tp is new Types (R32);

      package BSY    is new Bitfield (Tp, 16);
      package PGSERR is new Bitfield (Tp, 7);
      package PGPERR is new Bitfield (Tp, 6);
      package PGAERR is new Bitfield (Tp, 5);
      package WRPERR is new Bitfield (Tp, 4);
      package OPERR  is new Bitfield (Tp, 1);
      package EOP    is new Bitfield (Tp, 0);

      type T is
         record
            BSY    : FLASH_SR.BSY   .T;
            PGSERR : FLASH_SR.PGSERR.T;
            PGPERR : FLASH_SR.PGPERR.T;
            PGAERR : FLASH_SR.PGAERR.T;
            WRPERR : FLASH_SR.WRPERR.T;
            OPERR  : FLASH_SR.OPERR .T;
            EOP    : FLASH_SR.EOP   .T;
         end record;

      for T use
         record
            BSY    at 0 range BSY   .F .. BSY   .L;
            PGSERR at 0 range PGSERR.F .. PGSERR.L;
            PGPERR at 0 range PGPERR.F .. PGPERR.L;
            PGAERR at 0 range PGAERR.F .. PGAERR.L;
            WRPERR at 0 range WRPERR.F .. WRPERR.L;
            OPERR  at 0 range OPERR .F .. OPERR .L;
            EOP    at 0 range EOP   .F .. EOP   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_SR;

   package SR    is new Register (FLASH_SR.T, FLASH_SR.Tp, 16#4002_200C#);
   subtype SR_T  is SR.T;
   subtype SR_F  is SR.F;
   

   --  Field definitions

   function BSY    is new SR.B (FLASH_SR.BSY   ) with Inline_Always;
   function PGSERR is new SR.B (FLASH_SR.PGSERR) with Inline_Always;
   function PGPERR is new SR.B (FLASH_SR.PGPERR) with Inline_Always;
   function PGAERR is new SR.B (FLASH_SR.PGAERR) with Inline_Always;
   function WRPERR is new SR.B (FLASH_SR.WRPERR) with Inline_Always;
   function OPERR  is new SR.B (FLASH_SR.OPERR ) with Inline_Always;
   function EOP    is new SR.B (FLASH_SR.EOP   ) with Inline_Always;

   --  Functions

   function "+"  is new SR.Add      with Inline_Always;
   function "+"  is new SR.Add_F    with Inline_Always;
   function "+"  is new SR.Add_FF   with Inline_Always;
   function "-"  is new SR.Clear    with Inline_Always;
   function "-"  is new SR.Clear_FF with Inline_Always;
   function "="  is new SR.Equal    with Inline_Always;
   function Init is new SR.Init     with Inline_Always;

   --  Constant definitions

   function No_Flash_Memory_Operation_Ongoing is new SR.C (FLASH_SR.BSY, 2#0#) with Inline_Always;
   function Flash_Memory_Operation_Ongoing    is new SR.C (FLASH_SR.BSY, 2#1#) with Inline_Always;

   function Reset is new SR.C (FLASH_SR.PGSERR, 2#1#) with Inline_Always;

   function Reset is new SR.C (FLASH_SR.PGPERR, 2#1#) with Inline_Always;

   function Reset is new SR.C (FLASH_SR.PGAERR, 2#1#) with Inline_Always;

   function Reset is new SR.C (FLASH_SR.WRPERR, 2#1#) with Inline_Always;

   function Is_Operation_Error is new SR.C (FLASH_SR.OPERR, 2#1#) with Inline_Always;

   function Is_End_Of_Operation is new SR.C (FLASH_SR.EOP, 2#1#) with Inline_Always;
   function Reset               is new SR.C (FLASH_SR.EOP, 2#1#) with Inline_Always;

   -----------------------
   --  Control Register --
   -----------------------

   package FLASH_CR is

      package Tp is new Types (R32);

      package LOCK  is new Bitfield (Tp, 31);
      package ERRIE is new Bitfield (Tp, 25);
      package EOPIE is new Bitfield (Tp, 24);
      package STRT  is new Bitfield (Tp, 16);
      package PSIZE is new Bitfield (Tp, 8, 2);
      package SNB   is new Bitfield (Tp, 3, 4);
      package MER   is new Bitfield (Tp, 2);
      package SER   is new Bitfield (Tp, 1);
      package PG    is new Bitfield (Tp, 0);

      type T is
         record
            LOCK  : FLASH_CR.LOCK. T;
            ERRIE : FLASH_CR.ERRIE.T;
            EOPIE : FLASH_CR.EOPIE.T;
            STRT  : FLASH_CR.STRT .T;
            PSIZE : FLASH_CR.PSIZE.T;
            SNB   : FLASH_CR.SNB  .T;
            MER   : FLASH_CR.MER  .T;
            SER   : FLASH_CR.SER  .T;
            PG    : FLASH_CR.PG   .T;
         end record;

      for T use
         record
            LOCK  at 0 range LOCK .F .. LOCK .L;
            ERRIE at 0 range ERRIE.F .. ERRIE.L;
            EOPIE at 0 range EOPIE.F .. EOPIE.L;
            STRT  at 0 range STRT .F .. STRT .L;
            PSIZE at 0 range PSIZE.F .. PSIZE.L;
            SNB   at 0 range SNB  .F .. SNB  .L;
            MER   at 0 range MER  .F .. MER  .L;
            SER   at 0 range SER  .F .. SER  .L;
            PG    at 0 range PG   .F .. PG   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_CR;

   package CR    is new Register (FLASH_CR.T, FLASH_CR.Tp, 16#4002_2010#);
   subtype CR_T  is CR.T;
   subtype CR_F  is CR.F;
   

   --  Field definitions

   function LOCK  is new CR.B (FLASH_CR.LOCK ) with Inline_Always;
   function ERRIE is new CR.B (FLASH_CR.ERRIE) with Inline_Always;
   function EOPIE is new CR.B (FLASH_CR.EOPIE) with Inline_Always;
   function STRT  is new CR.B (FLASH_CR.STRT ) with Inline_Always;
   function PSIZE is new CR.B (FLASH_CR.PSIZE) with Inline_Always;
   function SNB   is new CR.B (FLASH_CR.SNB  ) with Inline_Always;
   function MER   is new CR.B (FLASH_CR.MER  ) with Inline_Always;
   function SER   is new CR.B (FLASH_CR.SER  ) with Inline_Always;
   function PG    is new CR.B (FLASH_CR.PG   ) with Inline_Always;

   --  Functions

   function "+"  is new CR.Add      with Inline_Always;
   function "+"  is new CR.Add_F    with Inline_Always;
   function "+"  is new CR.Add_FF   with Inline_Always;
   function "-"  is new CR.Clear    with Inline_Always;
   function "-"  is new CR.Clear_FF with Inline_Always;
   function "="  is new CR.Equal    with Inline_Always;
   function Init is new CR.Init     with Inline_Always;

   --  Constant definitions

   function Is_Locked is new CR.C (FLASH_CR.LOCK, 2#1#) with Inline_Always;

   function Interrupt_Generation_Disabled is new CR.C (FLASH_CR.ERRIE, 2#0#) with Inline_Always;
   function Interrupt_Generation_Enabled  is new CR.C (FLASH_CR.ERRIE, 2#1#) with Inline_Always;

   function Interrupt_Generation_Disabled is new CR.C (FLASH_CR.EOPIE, 2#0#) with Inline_Always;
   function Interrupt_Generation_Enabled  is new CR.C (FLASH_CR.EOPIE, 2#1#) with Inline_Always;

   function Start is new CR.C (FLASH_CR.STRT, 2#1#) with Inline_Always;

   function Program_X8  is new CR.C (FLASH_CR.PSIZE, 2#00#) with Inline_Always;
   function Program_X16 is new CR.C (FLASH_CR.PSIZE, 2#01#) with Inline_Always;
   function Program_X32 is new CR.C (FLASH_CR.PSIZE, 2#10#) with Inline_Always;
   function Program_X64 is new CR.C (FLASH_CR.PSIZE, 2#11#) with Inline_Always;

   ------------------------------
   --  Option Control Register --
   ------------------------------

   package FLASH_OPTCR is

      package Tp is new Types (R32);

      package NWRP       is new Bitfield (Tp, 16, 12);
      package RDP        is new Bitfield (Tp, 8, 8);
      package NRST_STDBY is new Bitfield (Tp, 7);
      package NRST_STOP  is new Bitfield (Tp, 6);
      package WDG_SW     is new Bitfield (Tp, 5);
      package BOR_LEV    is new Bitfield (Tp, 2, 2);
      package OPTSTRT    is new Bitfield (Tp, 1);
      package OPTLOCK    is new Bitfield (Tp, 0);

      type T is
         record
            NWRP       : FLASH_OPTCR.NWRP      .T;
            RDP        : FLASH_OPTCR.RDP       .T;
            NRST_STDBY : FLASH_OPTCR.NRST_STDBY.T;
            NRST_STOP  : FLASH_OPTCR.NRST_STOP .T;
            WDG_SW     : FLASH_OPTCR.WDG_SW    .T;
            BOR_LEV    : FLASH_OPTCR.BOR_LEV   .T;
            OPTSTRT    : FLASH_OPTCR.OPTSTRT   .T;
            OPTLOCK    : FLASH_OPTCR.OPTLOCK   .T;
         end record;

      for T use
         record
            NWRP       at 0 range NWRP      .F .. NWRP      .L;
            RDP        at 0 range RDP       .F .. RDP       .L;
            NRST_STDBY at 0 range NRST_STDBY.F .. NRST_STDBY.L;
            NRST_STOP  at 0 range NRST_STOP .F .. NRST_STOP .L;
            WDG_SW     at 0 range WDG_SW    .F .. WDG_SW    .L;
            BOR_LEV    at 0 range BOR_LEV   .F .. BOR_LEV   .L;
            OPTSTRT    at 0 range OPTSTRT   .F .. OPTSTRT   .L;
            OPTLOCK    at 0 range OPTLOCK   .F .. OPTLOCK   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_OPTCR;

   package OPTCR    is new Register (FLASH_OPTCR.T, FLASH_OPTCR.Tp, 16#4002_2014#);
   subtype OPTCR_T  is OPTCR.T;
   subtype OPTCR_F  is OPTCR.F;
   


   --  Field definitions

   function NWRP       is new OPTCR.B (FLASH_OPTCR.NWRP      ) with Inline_Always;
   function RDP        is new OPTCR.B (FLASH_OPTCR.RDP       ) with Inline_Always;
   function NRST_STDBY is new OPTCR.B (FLASH_OPTCR.NRST_STDBY) with Inline_Always;
   function NRST_STOP  is new OPTCR.B (FLASH_OPTCR.NRST_STOP ) with Inline_Always;
   function WDG_SW     is new OPTCR.B (FLASH_OPTCR.WDG_SW    ) with Inline_Always;
   function BOR_LEV    is new OPTCR.B (FLASH_OPTCR.BOR_LEV   ) with Inline_Always;
   function OPTSTRT    is new OPTCR.B (FLASH_OPTCR.OPTSTRT   ) with Inline_Always;
   function OPTLOCK    is new OPTCR.B (FLASH_OPTCR.OPTLOCK   ) with Inline_Always;

   --  Functions

   function "+"  is new OPTCR.Add      with Inline_Always;
   function "+"  is new OPTCR.Add_F    with Inline_Always;
   function "+"  is new OPTCR.Add_FF   with Inline_Always;
   function "-"  is new OPTCR.Clear    with Inline_Always;
   function "-"  is new OPTCR.Clear_FF with Inline_Always;
   function "="  is new OPTCR.Equal    with Inline_Always;
   function Init is new OPTCR.Init     with Inline_Always;

   --  Constant definitions

   function BOR_Level_3   is new OPTCR.C (FLASH_OPTCR.BOR_LEV, 2#00#) with Inline_Always;
   function BOR_Level_2   is new OPTCR.C (FLASH_OPTCR.BOR_LEV, 2#01#) with Inline_Always;
   function BOR_Level_1   is new OPTCR.C (FLASH_OPTCR.BOR_LEV, 2#10#) with Inline_Always;
   function BOR_Level_Off is new OPTCR.C (FLASH_OPTCR.BOR_LEV, 2#11#) with Inline_Always;

   function Start is new OPTCR.C (FLASH_OPTCR.OPTSTRT, 2#1#) with Inline_Always;

   function Is_Locked is new OPTCR.C (FLASH_OPTCR.OPTLOCK, 2#1#) with Inline_Always;

end ARM.Register.FLASH_F41XXX;
