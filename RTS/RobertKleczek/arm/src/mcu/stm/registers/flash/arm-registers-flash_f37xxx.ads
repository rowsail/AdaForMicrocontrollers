--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--             A R M . R e g i s t e r s . F L A S H _ F 3 7 X X X            --
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

--  Package defines STM32 F37XXX family flash related registers

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
--                           ARM.Registers.FLASH_F37XXX                         --
--------------------------------------------------------------------------------

package ARM.Registers.FLASH_F37XXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                                Flash                                --
   -------------------------------------------------------------------------

   -------------------------------
   --  Access Control Register  --
   -------------------------------

   package ACR is

      package Tp is new Types (R32);

      package PRFTBS  is new Bitfield (Tp, 5);
      package PRFTBE  is new Bitfield (Tp, 4);
      package LATENCY is new Bitfield (Tp, 0, 3);

      type T is
         record
            PRFTBS  : ACR.PRFTBS.T;
            PRFTBE  : ACR.PRFTBE.T;
            LATENCY : ACR.LATENCY.T;
         end record;

      for T use
         record
            PRFTBS  at 0 range PRFTBS.R'First .. PRFTBS.R'Last;
            PRFTBE  at 0 range PRFTBE.R'First .. PRFTBE.R'Last;
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
   function LATENCY is new ACR.FN.B (ACR.LATENCY);

   --  Functions

   function  "+"   is new ACR.FN.Add;
   function  "+"   is new ACR.FN.Add_RM;
   function  "-"   is new ACR.FN.Clear;
   function  Init  is new ACR.FN.Init;

   --  Constant definitions

   function Prefetch_Buffer_Is_Disabled is new ACR.FN.C (ACR.PRFTBS, 2#0#);
   function Prefetch_Buffer_Is_Enabled  is new ACR.FN.C (ACR.PRFTBS, 2#1#);

   function Prefetch_Is_Disabled is new ACR.FN.C (ACR.PRFTBE, 2#0#);
   function Prefetch_Is_Enabled  is new ACR.FN.C (ACR.PRFTBE, 2#1#);

   function Latency_0WS is new ACR.FN.C (ACR.LATENCY, 2#000#);
   function Latency_1WS is new ACR.FN.C (ACR.LATENCY, 2#001#);
   function Latency_2WS is new ACR.FN.C (ACR.LATENCY, 2#010#);

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

   ----------------------
   --  Status Register --
   ----------------------

   package SR is

      package Tp is new Types (R32);

      package EOP      is new Bitfield (Tp, 5);
      package WRPRTERR is new Bitfield (Tp, 4);
      package PGERR    is new Bitfield (Tp, 2);
      package BSY      is new Bitfield (Tp, 0);

      type T is
         record
            EOP      : SR.EOP.T;
            WRPRTERR : SR.WRPRTERR.T;
            PGERR    : SR.PGERR.T;
            BSY      : SR.BSY.T;
         end record;

      for T use
         record
            EOP      at 0 range EOP.R'First .. EOP.R'Last;
            WRPRTERR at 0 range WRPRTERR.R'First .. WRPRTERR.R'Last;
            PGERR    at 0 range PGERR.R'First .. PGERR.R'Last;
            BSY      at 0 range BSY.R'First .. BSY.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end SR;

   subtype SR_T is SR.T;

   --  Field definitions

   function EOP      is new SR.FN.B (SR.EOP);
   function WRPRTERR is new SR.FN.B (SR.WRPRTERR);
   function PGERR    is new SR.FN.B (SR.PGERR);
   function BSY      is new SR.FN.B (SR.BSY);

   --  Functions

   function  "+"   is new SR.FN.Add;
   function  "+"   is new SR.FN.Add_RM;
   function  "-"   is new SR.FN.Clear;
   function  Init  is new SR.FN.Init;

   --  Constant definitions

   function Reset is new SR.FN.C (SR.EOP, 2#1#);

   function Reset is new SR.FN.C (SR.WRPRTERR, 2#1#);

   function Reset is new SR.FN.C (SR.PGERR, 2#1#);

   function Busy  is new SR.FN.C (SR.BSY, 2#1#);

   -----------------------
   --  Control Register --
   -----------------------

   package CR is

      package Tp is new Types (R32);

      package OBL_LAUNCH is new Bitfield (Tp, 13);
      package EOPIE      is new Bitfield (Tp, 12);
      package ERRIE      is new Bitfield (Tp, 10);
      package OPTWRE     is new Bitfield (Tp, 9);
      package LOCK       is new Bitfield (Tp, 7);
      package STRT       is new Bitfield (Tp, 6);
      package OPTER      is new Bitfield (Tp, 5);
      package OPTPG      is new Bitfield (Tp, 4);
      package MER        is new Bitfield (Tp, 2);
      package PER        is new Bitfield (Tp, 1);
      package PG         is new Bitfield (Tp, 0);

      type T is
         record
            OBL_LAUNCH : CR.OBL_LAUNCH.T;
            EOPIE      : CR.EOPIE.T;
            ERRIE      : CR.ERRIE.T;
            OPTWRE     : CR.OPTWRE.T;
            LOCK       : CR.LOCK.T;
            STRT       : CR.STRT.T;
            OPTER      : CR.OPTER.T;
            OPTPG      : CR.OPTPG.T;
            MER        : CR.MER.T;
            PER        : CR.PER.T;
            PG         : CR.PG.T;
         end record;

      for T use
         record
            OBL_LAUNCH at 0 range OBL_LAUNCH.R'First .. OBL_LAUNCH.R'Last;
            EOPIE      at 0 range EOPIE.R'First .. EOPIE.R'Last;
            ERRIE      at 0 range ERRIE.R'First .. ERRIE.R'Last;
            OPTWRE     at 0 range OPTWRE.R'First .. OPTWRE.R'Last;
            LOCK       at 0 range LOCK.R'First .. LOCK.R'Last;
            STRT       at 0 range STRT.R'First .. STRT.R'Last;
            OPTER      at 0 range OPTER.R'First .. OPTER.R'Last;
            OPTPG      at 0 range OPTPG.R'First .. OPTPG.R'Last;
            MER        at 0 range MER.R'First .. MER.R'Last;
            PER        at 0 range PER.R'First .. PER.R'Last;
            PG         at 0 range PG.R'First .. PG.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CR;

   subtype CR_T is CR.T;

   --  Field definitions

   function OBL_LAUNCH is new CR.FN.B (CR.OBL_LAUNCH);
   function EOPIE      is new CR.FN.B (CR.EOPIE);
   function ERRIE      is new CR.FN.B (CR.ERRIE);
   function OPTWRE     is new CR.FN.B (CR.OPTWRE);
   function LOCK       is new CR.FN.B (CR.LOCK);
   function STRT       is new CR.FN.B (CR.STRT);
   function OPTER      is new CR.FN.B (CR.OPTER);
   function OPTPG      is new CR.FN.B (CR.OPTPG);
   function MER        is new CR.FN.B (CR.MER);
   function PER        is new CR.FN.B (CR.PER);
   function PG         is new CR.FN.B (CR.PG);

   --  Functions

   function  "+"   is new CR.FN.Add;
   function  "+"   is new CR.FN.Add_RM;
   function  "-"   is new CR.FN.Clear;
   function  Init  is new CR.FN.Init;

   --  Constant definitions

   function Inactive is new CR.FN.C (CR.OBL_LAUNCH, 2#0#);
   function Active   is new CR.FN.C (CR.OBL_LAUNCH, 2#1#);

   function Interrupt_Generation_Disabled is new CR.FN.C (CR.EOPIE, 2#0#);
   function Interrupt_Generation_Enabled  is new CR.FN.C (CR.EOPIE, 2#1#);

   function Interrupt_Generation_Disabled is new CR.FN.C (CR.ERRIE, 2#0#);
   function Interrupt_Generation_Enabled  is new CR.FN.C (CR.ERRIE, 2#1#);

   function Is_Set is new CR.FN.C (CR.OPTWRE, 2#1#);

   function Is_Locked is new CR.FN.C (CR.LOCK, 2#1#);

   function Start is new CR.FN.C (CR.STRT, 2#1#);

   ------------------------
   --  Address Register  --
   ------------------------

   subtype AR_T is R32;
   pragma Suppress_Initialization (AR_T);

   ---------------------------
   --  Option Byte Register --
   ---------------------------

   package OBR is

      package Tp is new Types (R32);

      package Data1               is new Bitfield (Tp, 24, 8);
      package Data0               is new Bitfield (Tp, 16, 8);
      package SDADC12_VDD_MONITOR is new Bitfield (Tp, 15);
      package SRAM_PE             is new Bitfield (Tp, 14);
      package VDDA_MONITOR        is new Bitfield (Tp, 13);
      package NBOOT1              is new Bitfield (Tp, 12);
      package NRST_STDBY          is new Bitfield (Tp, 10);
      package NRST_STOP           is new Bitfield (Tp, 9);
      package WDG_SW              is new Bitfield (Tp, 8);
      package LEVEL2_PROT         is new Bitfield (Tp, 2);
      package LEVEL1_PROT         is new Bitfield (Tp, 1);
      package OPTERR              is new Bitfield (Tp, 0);


      type T is
         record
            Data1               : OBR.Data1.T;
            Data0               : OBR.Data0.T;
            SDADC12_VDD_MONITOR : OBR.SDADC12_VDD_MONITOR.T;
            SRAM_PE             : OBR.SRAM_PE.T;
            VDDA_MONITOR        : OBR.VDDA_MONITOR.T;
            NBOOT1              : OBR.NBOOT1.T;
            NRST_STDBY          : OBR.NRST_STDBY.T;
            NRST_STOP           : OBR.NRST_STOP.T;
            WDG_SW              : OBR.WDG_SW.T;
            LEVEL2_PROT         : OBR.LEVEL2_PROT.T;
            LEVEL1_PROT         : OBR.LEVEL1_PROT.T;
            OPTERR              : OBR.OPTERR.T;
         end record;

      for T use
         record
            Data1               at 0 range Data1.R'First .. Data1.R'Last;
            Data0               at 0 range Data0.R'First .. Data0.R'Last;
            SDADC12_VDD_MONITOR at 0 range SDADC12_VDD_MONITOR.R'First .. SDADC12_VDD_MONITOR.R'Last;
            SRAM_PE             at 0 range SRAM_PE.R'First .. SRAM_PE.R'Last;
            VDDA_MONITOR        at 0 range VDDA_MONITOR.R'First .. VDDA_MONITOR.R'Last;
            NBOOT1              at 0 range NBOOT1.R'First .. NBOOT1.R'Last;
            NRST_STDBY          at 0 range NRST_STDBY.R'First .. NRST_STDBY.R'Last;
            NRST_STOP           at 0 range NRST_STOP.R'First .. NRST_STOP.R'Last;
            WDG_SW              at 0 range WDG_SW.R'First .. WDG_SW.R'Last;
            LEVEL2_PROT         at 0 range LEVEL2_PROT.R'First .. LEVEL2_PROT.R'Last;
            LEVEL1_PROT         at 0 range LEVEL1_PROT.R'First .. LEVEL1_PROT.R'Last;
            OPTERR              at 0 range OPTERR.R'First .. OPTERR.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end OBR;

   subtype OBR_T is OBR.T;

   --  Field definitions

   function Data1               is new OBR.FN.B (OBR.Data1);
   function Data0               is new OBR.FN.B (OBR.Data0);
   function SDADC12_VDD_MONITOR is new OBR.FN.B (OBR.SDADC12_VDD_MONITOR);
   function SRAM_PE             is new OBR.FN.B (OBR.SRAM_PE);
   function VDDA_MONITOR        is new OBR.FN.B (OBR.VDDA_MONITOR);
   function NBOOT1              is new OBR.FN.B (OBR.NBOOT1);
   function NRST_STDBY          is new OBR.FN.B (OBR.NRST_STDBY);
   function NRST_STOP           is new OBR.FN.B (OBR.NRST_STOP);
   function WDG_SW              is new OBR.FN.B (OBR.WDG_SW);
   function LEVEL2_PROT         is new OBR.FN.B (OBR.LEVEL2_PROT);
   function LEVEL1_PROT         is new OBR.FN.B (OBR.LEVEL1_PROT);
   function OPTERR              is new OBR.FN.B (OBR.OPTERR);

   --  Functions

   function  "+"   is new OBR.FN.Add;
   function  "+"   is new OBR.FN.Add_RM;
   function  "-"   is new OBR.FN.Clear;
   function  Init  is new OBR.FN.Init;

   --  Constant definitions

   function Is_Level_2_Read_Protected is new OBR.FN.C (OBR.LEVEL2_PROT, 2#1#);
   function Is_Level_1_Read_Protected is new OBR.FN.C (OBR.LEVEL1_PROT, 2#1#);
   function Is_Option_Byte_Load_Error is new OBR.FN.C (OBR.OPTERR, 2#1#);

   --------------------------------
   --  Write Protecion Register  --
   --------------------------------

   subtype WRPR_T is R32;
   pragma Suppress_Initialization (WRPR_T);

   --------------------------------------------------------------------------
   --                         Register definition                          --
   --------------------------------------------------------------------------

   type FLASH_T is
      record
         ACR     : ACR_T;
         KEYR    : KEYR_T;
         OPTKEYR : OPTKEYR_T;
         SR      : SR_T;
         CR      : CR_T;
         AR      : AR_T;
         OBR     : OBR_T;
         WRPR    : WRPR_T;
         pragma Volatile (ACR);
         pragma Volatile (KEYR);
         pragma Volatile (OPTKEYR);
         pragma Volatile (SR);
         pragma Volatile (CR);
         pragma Volatile (AR);
         pragma Volatile (OBR);
         pragma Volatile (WRPR);
      end record;

   for FLASH_T use
      record
         ACR       at 16#00# range 0 .. 31;
         KEYR      at 16#04# range 0 .. 31;
         OPTKEYR   at 16#08# range 0 .. 31;
         SR        at 16#0C# range 0 .. 31;
         CR        at 16#10# range 0 .. 31;
         AR        at 16#14# range 0 .. 31;
         OBR       at 16#1C# range 0 .. 31;
         WRPR      at 16#20# range 0 .. 31;
      end record;

   FLASH : FLASH_T;

   for FLASH'Address use System'To_Address (16#4002_2000#);

end ARM.Registers.FLASH_F37XXX;
