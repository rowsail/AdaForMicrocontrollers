--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--               A R M . R e g i s t e r . F L A S H _ F 3 7 X X X            --
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

with ARM.Register;
use  ARM.Register;

with ARM.Register.Types;
with ARM.Register.Bitfield;
with ARM.Register.Register;

--------------------------------------------------------------------------------
--                          ARM.Register.FLASH_F37XXX                         --
--------------------------------------------------------------------------------

package ARM.Register.FLASH_F37XXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                                Flash                                --
   -------------------------------------------------------------------------

   -------------------------------
   --  Access Control Register  --
   -------------------------------

   package FLASH_ACR is

      package Tp is new Types (R32);

      package PRFTBS  is new Bitfield (Tp, 5);
      package PRFTBE  is new Bitfield (Tp, 4);
      package LATENCY is new Bitfield (Tp, 0, 3);

      type T is
         record
            PRFTBS  : FLASH_ACR.PRFTBS.T;
            PRFTBE  : FLASH_ACR.PRFTBE.T;
            LATENCY : FLASH_ACR.LATENCY.T;
         end record;

      for T use
         record
            PRFTBS  at 0 range PRFTBS .F .. PRFTBS .L;
            PRFTBE  at 0 range PRFTBE .F .. PRFTBE .L;
            LATENCY at 0 range LATENCY.F .. LATENCY.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_ACR;

   package ACR    is new Register (FLASH_ACR.T, FLASH_ACR.Tp, 16#4002_2000#);
   subtype ACR_T  is ACR.T;
   subtype ACR_F  is ACR.F;
   

   --  Field definitions

   function PRFTBS  is new ACR.B (FLASH_ACR.PRFTBS ) with Inline_Always;
   function PRFTBE  is new ACR.B (FLASH_ACR.PRFTBE ) with Inline_Always;
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

   function Prefetch_Buffer_Is_Disabled is new ACR.C (FLASH_ACR.PRFTBS, 2#0#) with Inline_Always;
   function Prefetch_Buffer_Is_Enabled  is new ACR.C (FLASH_ACR.PRFTBS, 2#1#) with Inline_Always;

   function Prefetch_Is_Disabled is new ACR.C (FLASH_ACR.PRFTBE, 2#0#) with Inline_Always;
   function Prefetch_Is_Enabled  is new ACR.C (FLASH_ACR.PRFTBE, 2#1#) with Inline_Always;

   function Latency_0WS is new ACR.C (FLASH_ACR.LATENCY, 2#000#) with Inline_Always;
   function Latency_1WS is new ACR.C (FLASH_ACR.LATENCY, 2#001#) with Inline_Always;
   function Latency_2WS is new ACR.C (FLASH_ACR.LATENCY, 2#010#) with Inline_Always;

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

   ----------------------
   --  Status Register --
   ----------------------

   package FLASH_SR is

      package Tp is new Types (R32);

      package EOP      is new Bitfield (Tp, 5);
      package WRPRTERR is new Bitfield (Tp, 4);
      package PGERR    is new Bitfield (Tp, 2);
      package BSY      is new Bitfield (Tp, 0);

      type T is
         record
            EOP      : FLASH_SR.EOP     .T;
            WRPRTERR : FLASH_SR.WRPRTERR.T;
            PGERR    : FLASH_SR.PGERR   .T;
            BSY      : FLASH_SR.BSY     .T;
         end record;

      for T use
         record
            EOP      at 0 range EOP     .F .. EOP     .L;
            WRPRTERR at 0 range WRPRTERR.F .. WRPRTERR.L;
            PGERR    at 0 range PGERR   .F .. PGERR   .L;
            BSY      at 0 range BSY     .F .. BSY     .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_SR;

   package SR    is new Register (FLASH_SR.T, FLASH_SR.Tp, 16#4002_200C#);
   subtype SR_T  is SR.T;
   subtype SR_F  is SR.F;
   

   --  Field definitions

   function EOP      is new SR.B (FLASH_SR.EOP     ) with Inline_Always;
   function WRPRTERR is new SR.B (FLASH_SR.WRPRTERR) with Inline_Always;
   function PGERR    is new SR.B (FLASH_SR.PGERR   ) with Inline_Always;
   function BSY      is new SR.B (FLASH_SR.BSY     ) with Inline_Always;

   --  Functions

   function "+"  is new SR.Add      with Inline_Always;
   function "+"  is new SR.Add_F    with Inline_Always;
   function "+"  is new SR.Add_FF   with Inline_Always;
   function "-"  is new SR.Clear    with Inline_Always;
   function "-"  is new SR.Clear_FF with Inline_Always;
   function "="  is new SR.Equal    with Inline_Always;
   function Init is new SR.Init     with Inline_Always;

   --  Constant definitions

   function Reset is new SR.C (FLASH_SR.EOP, 2#1#) with Inline_Always;

   function Reset is new SR.C (FLASH_SR.WRPRTERR, 2#1#) with Inline_Always;

   function Reset is new SR.C (FLASH_SR.PGERR, 2#1#) with Inline_Always;

   function Busy  is new SR.C (FLASH_SR.BSY, 2#1#) with Inline_Always;

   -----------------------
   --  Control Register --
   -----------------------

   package FLASH_CR is

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
            OBL_LAUNCH : FLASH_CR.OBL_LAUNCH.T;
            EOPIE      : FLASH_CR.EOPIE     .T;
            ERRIE      : FLASH_CR.ERRIE     .T;
            OPTWRE     : FLASH_CR.OPTWRE    .T;
            LOCK       : FLASH_CR.LOCK      .T;
            STRT       : FLASH_CR.STRT      .T;
            OPTER      : FLASH_CR.OPTER     .T;
            OPTPG      : FLASH_CR.OPTPG     .T;
            MER        : FLASH_CR.MER       .T;
            PER        : FLASH_CR.PER       .T;
            PG         : FLASH_CR.PG        .T;
         end record;

      for T use
         record
            OBL_LAUNCH at 0 range OBL_LAUNCH.F .. OBL_LAUNCH.L;
            EOPIE      at 0 range EOPIE     .F .. EOPIE     .L;
            ERRIE      at 0 range ERRIE     .F .. ERRIE     .L;
            OPTWRE     at 0 range OPTWRE    .F .. OPTWRE    .L;
            LOCK       at 0 range LOCK      .F .. LOCK      .L;
            STRT       at 0 range STRT      .F .. STRT      .L;
            OPTER      at 0 range OPTER     .F .. OPTER     .L;
            OPTPG      at 0 range OPTPG     .F .. OPTPG     .L;
            MER        at 0 range MER       .F .. MER       .L;
            PER        at 0 range PER       .F .. PER       .L;
            PG         at 0 range PG        .F .. PG        .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_CR;

   package CR    is new Register (FLASH_CR.T, FLASH_CR.Tp, 16#4002_2010#);
   subtype CR_T  is CR.T;
   subtype CR_F  is CR.F;
   

   --  Field definitions

   function OBL_LAUNCH is new CR.B (FLASH_CR.OBL_LAUNCH) with Inline_Always;
   function EOPIE      is new CR.B (FLASH_CR.EOPIE     ) with Inline_Always;
   function ERRIE      is new CR.B (FLASH_CR.ERRIE     ) with Inline_Always;
   function OPTWRE     is new CR.B (FLASH_CR.OPTWRE    ) with Inline_Always;
   function LOCK       is new CR.B (FLASH_CR.LOCK      ) with Inline_Always;
   function STRT       is new CR.B (FLASH_CR.STRT      ) with Inline_Always;
   function OPTER      is new CR.B (FLASH_CR.OPTER     ) with Inline_Always;
   function OPTPG      is new CR.B (FLASH_CR.OPTPG     ) with Inline_Always;
   function MER        is new CR.B (FLASH_CR.MER       ) with Inline_Always;
   function PER        is new CR.B (FLASH_CR.PER       ) with Inline_Always;
   function PG         is new CR.B (FLASH_CR.PG        ) with Inline_Always;

   --  Functions

   function "+"  is new CR.Add      with Inline_Always;
   function "+"  is new CR.Add_F    with Inline_Always;
   function "+"  is new CR.Add_FF   with Inline_Always;
   function "-"  is new CR.Clear    with Inline_Always;
   function "-"  is new CR.Clear_FF with Inline_Always;
   function "="  is new CR.Equal    with Inline_Always;
   function Init is new CR.Init     with Inline_Always;

   --  Constant definitions

   function Inactive is new CR.C (FLASH_CR.OBL_LAUNCH, 2#0#) with Inline_Always;
   function Active   is new CR.C (FLASH_CR.OBL_LAUNCH, 2#1#) with Inline_Always;

   function Interrupt_Generation_Disabled is new CR.C (FLASH_CR.EOPIE, 2#0#) with Inline_Always;
   function Interrupt_Generation_Enabled  is new CR.C (FLASH_CR.EOPIE, 2#1#) with Inline_Always;

   function Interrupt_Generation_Disabled is new CR.C (FLASH_CR.ERRIE, 2#0#) with Inline_Always;
   function Interrupt_Generation_Enabled  is new CR.C (FLASH_CR.ERRIE, 2#1#) with Inline_Always;

   function Is_Set is new CR.C (FLASH_CR.OPTWRE, 2#1#) with Inline_Always;

   function Is_Locked is new CR.C (FLASH_CR.LOCK, 2#1#) with Inline_Always;

   function Start is new CR.C (FLASH_CR.STRT, 2#1#) with Inline_Always;

   ------------------------
   --  Address Register  --
   ------------------------

   package FLASH_AR is

      package Tp is new Types (R32);

      type T is new Tp.Reg;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_AR;

   package AR    is new Register (FLASH_AR.T, FLASH_AR.Tp, 16#4002_2014#);
   subtype AR_T  is AR.T;

   ---------------------------
   --  Option Byte Register --
   ---------------------------

   package FLASH_OBR is

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
            Data1               : FLASH_OBR.Data1              .T;
            Data0               : FLASH_OBR.Data0              .T;
            SDADC12_VDD_MONITOR : FLASH_OBR.SDADC12_VDD_MONITOR.T;
            SRAM_PE             : FLASH_OBR.SRAM_PE            .T;
            VDDA_MONITOR        : FLASH_OBR.VDDA_MONITOR       .T;
            NBOOT1              : FLASH_OBR.NBOOT1             .T;
            NRST_STDBY          : FLASH_OBR.NRST_STDBY         .T;
            NRST_STOP           : FLASH_OBR.NRST_STOP          .T;
            WDG_SW              : FLASH_OBR.WDG_SW             .T;
            LEVEL2_PROT         : FLASH_OBR.LEVEL2_PROT        .T;
            LEVEL1_PROT         : FLASH_OBR.LEVEL1_PROT        .T;
            OPTERR              : FLASH_OBR.OPTERR             .T;
         end record;

      for T use
         record
            Data1               at 0 range Data1              .F .. Data1              .L;
            Data0               at 0 range Data0              .F .. Data0              .L;
            SDADC12_VDD_MONITOR at 0 range SDADC12_VDD_MONITOR.F .. SDADC12_VDD_MONITOR.L;
            SRAM_PE             at 0 range SRAM_PE            .F .. SRAM_PE            .L;
            VDDA_MONITOR        at 0 range VDDA_MONITOR       .F .. VDDA_MONITOR       .L;
            NBOOT1              at 0 range NBOOT1             .F .. NBOOT1             .L;
            NRST_STDBY          at 0 range NRST_STDBY         .F .. NRST_STDBY         .L;
            NRST_STOP           at 0 range NRST_STOP          .F .. NRST_STOP          .L;
            WDG_SW              at 0 range WDG_SW             .F .. WDG_SW             .L;
            LEVEL2_PROT         at 0 range LEVEL2_PROT        .F .. LEVEL2_PROT        .L;
            LEVEL1_PROT         at 0 range LEVEL1_PROT        .F .. LEVEL1_PROT        .L;
            OPTERR              at 0 range OPTERR             .F .. OPTERR             .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_OBR;

   package OBR    is new Register (FLASH_OBR.T, FLASH_OBR.Tp, 16#4002_201C#);
   subtype OBR_T  is OBR.T;
   subtype OBR_F  is OBR.F;
   

   --  Field definitions

   function Data1               is new OBR.B (FLASH_OBR.Data1              ) with Inline_Always;
   function Data0               is new OBR.B (FLASH_OBR.Data0              ) with Inline_Always;
   function SDADC12_VDD_MONITOR is new OBR.B (FLASH_OBR.SDADC12_VDD_MONITOR) with Inline_Always;
   function SRAM_PE             is new OBR.B (FLASH_OBR.SRAM_PE            ) with Inline_Always;
   function VDDA_MONITOR        is new OBR.B (FLASH_OBR.VDDA_MONITOR       ) with Inline_Always;
   function NBOOT1              is new OBR.B (FLASH_OBR.NBOOT1             ) with Inline_Always;
   function NRST_STDBY          is new OBR.B (FLASH_OBR.NRST_STDBY         ) with Inline_Always;
   function NRST_STOP           is new OBR.B (FLASH_OBR.NRST_STOP          ) with Inline_Always;
   function WDG_SW              is new OBR.B (FLASH_OBR.WDG_SW             ) with Inline_Always;
   function LEVEL2_PROT         is new OBR.B (FLASH_OBR.LEVEL2_PROT        ) with Inline_Always;
   function LEVEL1_PROT         is new OBR.B (FLASH_OBR.LEVEL1_PROT        ) with Inline_Always;
   function OPTERR              is new OBR.B (FLASH_OBR.OPTERR             ) with Inline_Always;

   --  Functions

   function "+"  is new OBR.Add      with Inline_Always;
   function "+"  is new OBR.Add_F    with Inline_Always;
   function "+"  is new OBR.Add_FF   with Inline_Always;
   function "-"  is new OBR.Clear    with Inline_Always;
   function "-"  is new OBR.Clear_FF with Inline_Always;
   function "="  is new OBR.Equal    with Inline_Always;
   function Init is new OBR.Init     with Inline_Always;

   --  Constant definitions

   function Is_Level_2_Read_Protected is new OBR.C (FLASH_OBR.LEVEL2_PROT, 2#1#) with Inline_Always;
   function Is_Level_1_Read_Protected is new OBR.C (FLASH_OBR.LEVEL1_PROT, 2#1#) with Inline_Always;
   function Is_Option_Byte_Load_Error is new OBR.C (FLASH_OBR.OPTERR, 2#1#) with Inline_Always;

   --------------------------------
   --  Write Protecion Register  --
   --------------------------------

   package FLASH_WRPR is

      package Tp is new Types (R32);

      type T is new Tp.Reg;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end FLASH_WRPR;

   package WRPR    is new Register (FLASH_WRPR.T, FLASH_WRPR.Tp, 16#4002_2020#);
   subtype WRPR_T  is WRPR.T;

end ARM.Register.FLASH_F37XXX;
