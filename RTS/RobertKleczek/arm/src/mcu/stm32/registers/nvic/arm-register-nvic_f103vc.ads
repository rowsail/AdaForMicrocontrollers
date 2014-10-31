--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--              S T M 3 2 . R e g i s t e r . N V I C _ F 1 0 3 V C           --
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

--  Package defines STM32 F103VC family nested vectored interrupt related registers

-------------------------
--  Imported packages  --
-------------------------

with System;

with ARM.Register;
use  ARM.Register;

with ARM.Register.Types;
with ARM.Register.Bitfield;
with ARM.Register.Register;

-----------------------------------------------------------------------------
--                        ARM.Register.NVIC_F103VC                         --
-----------------------------------------------------------------------------

package ARM.Register.NVIC_F103VC is

   pragma Preelaborate;

   --------------------------------------------------------------------------
   --                                 NVIC                                 --
   --------------------------------------------------------------------------

   -------------------------
   --  NVIC_ICTR Register --
   -------------------------

   package NVIC_ICTR is

      package Tp is new Types (R32);

      package INTLINESNUM is new Bitfield (Tp, 0, 5);

      type T is
         record
            INTLINESNUM : NVIC_ICTR.INTLINESNUM.T;
         end record;

      for T use
         record
            INTLINESNUM at 0 range INTLINESNUM.F .. INTLINESNUM.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_ICTR;

   package ICTR    is new Register (NVIC_ICTR.T, NVIC_ICTR.Tp, 16#E000_E004#);
   subtype ICTR_T  is ICTR.T;
   subtype ICTR_F  is ICTR.F;
   

   --  Field definitions

   function INTLINESNUM is new ICTR.B (NVIC_ICTR.INTLINESNUM) with Inline_Always;

   --  Functions

   function "+"  is new ICTR.Add      with Inline_Always;
   function "+"  is new ICTR.Add_F    with Inline_Always;
   function "+"  is new ICTR.Add_FF   with Inline_Always;
   function "-"  is new ICTR.Clear    with Inline_Always;
   function "-"  is new ICTR.Clear_FF with Inline_Always;
   function "="  is new ICTR.Equal    with Inline_Always;
   function Init is new ICTR.Init     with Inline_Always;

   ----------------------------
   --  SYSTICK_CTRL Register --
   ----------------------------

   package NVIC_SYSTICK_CTRL is

      package Tp is new Types (R32);

      package COUNTFLAG is new Bitfield (Tp, 16);
      package CLKSOURCE is new Bitfield (Tp, 2);
      package TICKINT   is new Bitfield (Tp, 1);
      package ENABLE    is new Bitfield (Tp, 0);

      type T is
         record
            COUNTFLAG : NVIC_SYSTICK_CTRL.COUNTFLAG.T;
            CLKSOURCE : NVIC_SYSTICK_CTRL.CLKSOURCE.T;
            TICKINT   : NVIC_SYSTICK_CTRL.TICKINT  .T;
            ENABLE    : NVIC_SYSTICK_CTRL.ENABLE   .T;
         end record;

      for T use
         record
            COUNTFLAG at 0 range COUNTFLAG.F .. COUNTFLAG.L;
            CLKSOURCE at 0 range CLKSOURCE.F .. CLKSOURCE.L;
            TICKINT   at 0 range TICKINT  .F .. TICKINT  .L;
            ENABLE    at 0 range ENABLE.   F .. ENABLE   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_SYSTICK_CTRL;

   package SYSTICK_CTRL    is new Register (NVIC_SYSTICK_CTRL.T, NVIC_SYSTICK_CTRL.Tp, 16#E000_E010#);
   subtype SYSTICK_CTRL_T  is SYSTICK_CTRL.T;
   subtype SYSTICK_CTRL_F  is SYSTICK_CTRL.F;
   

   --  Field definitions

   function COUNTFLAG is new SYSTICK_CTRL.B (NVIC_SYSTICK_CTRL.COUNTFLAG) with Inline_Always;
   function CLKSOURCE is new SYSTICK_CTRL.B (NVIC_SYSTICK_CTRL.CLKSOURCE) with Inline_Always;
   function TICKINT   is new SYSTICK_CTRL.B (NVIC_SYSTICK_CTRL.TICKINT  ) with Inline_Always;
   function ENABLE    is new SYSTICK_CTRL.B (NVIC_SYSTICK_CTRL.ENABLE   ) with Inline_Always;

   --  Functions

   function "+"  is new SYSTICK_CTRL.Add      with Inline_Always;
   function "+"  is new SYSTICK_CTRL.Add_F    with Inline_Always;
   function "+"  is new SYSTICK_CTRL.Add_FF   with Inline_Always;
   function "-"  is new SYSTICK_CTRL.Clear    with Inline_Always;
   function "-"  is new SYSTICK_CTRL.Clear_FF with Inline_Always;
   function "="  is new SYSTICK_CTRL.Equal    with Inline_Always;
   function Init is new SYSTICK_CTRL.Init     with Inline_Always;

   --  Constant definitions

   function Reached_Zero is new
     SYSTICK_CTRL.C (NVIC_SYSTICK_CTRL.COUNTFLAG, 2#1#) with Inline_Always;
   function External_Reference_Clock is new
     SYSTICK_CTRL.C (NVIC_SYSTICK_CTRL.CLKSOURCE, 2#0#) with Inline_Always;
   function Core_Clock is new
     SYSTICK_CTRL.C (NVIC_SYSTICK_CTRL.CLKSOURCE, 2#1#) with Inline_Always;
   function Not_Pend_SYSTICK is new
     SYSTICK_CTRL.C (NVIC_SYSTICK_CTRL.TICKINT, 2#0#) with Inline_Always;
   function Pend_SYSTICK is new
     SYSTICK_CTRL.C (NVIC_SYSTICK_CTRL.TICKINT, 2#1#) with Inline_Always;
   function Disabled is new SYSTICK_CTRL.C (NVIC_SYSTICK_CTRL.ENABLE, 2#0#) with Inline_Always;
   function Enabled  is new SYSTICK_CTRL.C (NVIC_SYSTICK_CTRL.ENABLE, 2#1#) with Inline_Always;

   ----------------------------
   --  SYSTICK_LOAD Register --
   ----------------------------

   package NVIC_SYSTICK_LOAD is

      package Tp is new Types (R32);

      package RELOAD is new Bitfield (Tp, 0, 24);

      type T is
         record
            RELOAD : NVIC_SYSTICK_LOAD.RELOAD.T;
         end record;

      for T use
         record
            RELOAD at 0 range RELOAD.F .. RELOAD.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_SYSTICK_LOAD;

   package SYSTICK_LOAD    is new Register (NVIC_SYSTICK_LOAD.T, NVIC_SYSTICK_LOAD.Tp, 16#E000_E014#);
   subtype SYSTICK_LOAD_T  is SYSTICK_LOAD.T;
   subtype SYSTICK_LOAD_F  is SYSTICK_LOAD.F;
   

   --  Field definitions

   function RELOAD is new SYSTICK_LOAD.B (NVIC_SYSTICK_LOAD.RELOAD) with  Inline_Always;

   --  Functions

   function "+"  is new SYSTICK_LOAD.Add      with Inline_Always;
   function "+"  is new SYSTICK_LOAD.Add_F    with Inline_Always;
   function "+"  is new SYSTICK_LOAD.Add_FF   with Inline_Always;
   function "-"  is new SYSTICK_LOAD.Clear    with Inline_Always;
   function "-"  is new SYSTICK_LOAD.Clear_FF with Inline_Always;
   function "="  is new SYSTICK_LOAD.Equal    with Inline_Always;
   function Init is new SYSTICK_LOAD.Init     with Inline_Always;

   ---------------------------
   --  SYSTICK_VAL Register --
   ---------------------------

   package NVIC_SYSTICK_VAL is

      package Tp is new Types (R32);

      package CURRENT is new Bitfield (Tp, 0, 24);

      type T is
         record
            CURRENT : NVIC_SYSTICK_VAL.CURRENT.T;
         end record;

      for T use
         record
            CURRENT at 0 range CURRENT.F .. CURRENT.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_SYSTICK_VAL;

   package SYSTICK_VAL     is new Register (NVIC_SYSTICK_VAL.T, NVIC_SYSTICK_VAL.Tp, 16#E000_E018#);
   subtype SYSTICK_VAL_T  is SYSTICK_VAL.T;
   subtype SYSTICK_VAL_F  is SYSTICK_VAL.F;
   

   --  Field definitions

   function CURRENT is new SYSTICK_VAL.B (NVIC_SYSTICK_VAL.CURRENT) with Inline_Always;

   --  Functions

   function "+"  is new SYSTICK_VAL.Add      with Inline_Always;
   function "+"  is new SYSTICK_VAL.Add_F    with Inline_Always;
   function "+"  is new SYSTICK_VAL.Add_FF   with Inline_Always;
   function "-"  is new SYSTICK_VAL.Clear    with Inline_Always;
   function "-"  is new SYSTICK_VAL.Clear_FF with Inline_Always;
   function "="  is new SYSTICK_VAL.Equal    with Inline_Always;
   function Init is new SYSTICK_VAL.Init     with Inline_Always;

   -----------------------------
   --  SYSTICK_CALIB Register --
   -----------------------------

   package NVIC_SYSTICK_CALIB is

      package Tp is new Types (R32);

      package NOREF is new Bitfield (Tp, 31);
      package SKEW  is new Bitfield (Tp, 30);
      package TENMS is new Bitfield (Tp, 0, 24);

      type T is
         record
            NOREF : NVIC_SYSTICK_CALIB.NOREF.T;
            SKEW  : NVIC_SYSTICK_CALIB.SKEW .T;
            TENMS : NVIC_SYSTICK_CALIB.TENMS.T;
         end record;

      for T use
         record
            NOREF at 0 range NOREF.F .. NOREF.L;
            SKEW  at 0 range SKEW .F .. SKEW .L;
            TENMS at 0 range TENMS.F .. TENMS.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_SYSTICK_CALIB;

   package SYSTICK_CALIB    is new Register (NVIC_SYSTICK_CALIB.T, NVIC_SYSTICK_CALIB.Tp, 16#E000_E01C#);
   subtype SYSTICK_CALIB_T  is SYSTICK_CALIB.T;
   subtype SYSTICK_CALIB_F  is SYSTICK_CALIB.F;
   

   --  Field definitions

   function NOREF is new SYSTICK_CALIB.B (NVIC_SYSTICK_CALIB.NOREF) with Inline_Always;
   function SKEW  is new SYSTICK_CALIB.B (NVIC_SYSTICK_CALIB.SKEW ) with Inline_Always;
   function TENMS is new SYSTICK_CALIB.B (NVIC_SYSTICK_CALIB.TENMS) with Inline_Always;

   --  Functions

   function "+"  is new SYSTICK_CALIB.Add      with Inline_Always;
   function "+"  is new SYSTICK_CALIB.Add_F    with Inline_Always;
   function "+"  is new SYSTICK_CALIB.Add_FF   with Inline_Always;
   function "-"  is new SYSTICK_CALIB.Clear    with Inline_Always;
   function "-"  is new SYSTICK_CALIB.Clear_FF with Inline_Always;
   function "="  is new SYSTICK_CALIB.Equal    with Inline_Always;
   function Init is new SYSTICK_CALIB.Init     with Inline_Always;

   --  Constant definitions

   function Not_Provided is new
     SYSTICK_CALIB.C (NVIC_SYSTICK_CALIB.NOREF, 2#1#) with Inline_Always;
   function Not_Exactly_10mS is new
     SYSTICK_CALIB.C (NVIC_SYSTICK_CALIB.SKEW, 2#1#) with Inline_Always;

   --------------------
   --  ISER Register --
   --------------------

   type ISER_T is array (0 .. 239) of Boolean with Pack;
   for ISER_T'Size use 240;

   ISER : ISER_T with Volatile;
   for ISER'Address use System'To_Address (16#E000_E100#);
   --  Type definition of hardware register: ISER

   --------------------
   --  ICER Register --
   --------------------

   type ICER_T is array (0 .. 239) of Boolean with Pack;
   for ICER_T'Size use 240;

   ICER : ICER_T with Volatile;
   for ICER'Address use System'To_Address (16#E000_E180#);
   --  Type definition of hardware register: ICER

   --------------------
   --  ISPR Register --
   --------------------

   type ISPR_T is array (0 .. 239) of Boolean with Pack;
   for ISPR_T'Size use 240;

   ISPR : ISPR_T with Volatile;
   for ISPR'Address use System'To_Address (16#E000_E200#);
   --  Type definition of hardware register: ISPR

   --------------------
   --  ICPR Register --
   --------------------

   type ICPR_T is array (0 .. 239) of Boolean with Pack;
   for ICPR_T'Size use 240;

   ICPR : ICPR_T with Volatile;
   for ICPR'Address use System'To_Address (16#E000_E280#);
   --  Type definition of hardware register: ICPR

   --------------------
   --  IABR Register --
   --------------------

   type IABR_T is array (0 .. 239) of Boolean with Pack;
   for IABR_T'Size use 240;

   IABR : IABR_T with Volatile;
   for IABR'Address use System'To_Address (16#E000_E300#);
   --  Type definition of hardware register: IABR

   ------------------
   --  IP Register --
   ------------------

   subtype PRIORITY_T is R8;

   type IP_T is array (0 .. 239) of PRIORITY_T with Pack;
   for IP_T'Size use 240 * 8;

   IP : IP_T with Volatile;
   for IP'Address use System'To_Address (16#E000_E400#);
   --  Type definition of hardware register: IP

   ---------------------
   --  CPUID Register --
   ---------------------

   package NVIC_CPUID is

      package Tp is new Types (R32);

      package IMPLEMENTER is new Bitfield (Tp, 24, 8);
      package VARIANT     is new Bitfield (Tp, 20, 4);
      package PARTNO      is new Bitfield (Tp, 4, 12);
      package REVISION    is new Bitfield (Tp, 0, 4);

      type T is
         record
            IMPLEMENTER : NVIC_CPUID.IMPLEMENTER.T;
            VARIANT     : NVIC_CPUID.VARIANT    .T;
            PARTNO      : NVIC_CPUID.PARTNO     .T;
            REVISION    : NVIC_CPUID.REVISION   .T;
         end record;

      for T use
         record
            IMPLEMENTER at 0 range IMPLEMENTER.F .. IMPLEMENTER.L;
            VARIANT     at 0 range VARIANT    .F .. VARIANT    .L;
            PARTNO      at 0 range PARTNO     .F .. PARTNO     .L;
            REVISION    at 0 range REVISION   .F .. REVISION   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_CPUID;

   package CPUID    is new Register (NVIC_CPUID.T, NVIC_CPUID.Tp, 16#E000_ED00#);
   subtype CPUID_T  is CPUID.T;
   subtype CPUID_F  is CPUID.F;
   

   --  Field definitions

   function IMPLEMENTER is new CPUID.B (NVIC_CPUID.IMPLEMENTER) with Inline_Always;
   function VARIANT     is new CPUID.B (NVIC_CPUID.VARIANT    ) with Inline_Always;
   function PARTNO      is new CPUID.B (NVIC_CPUID.PARTNO     ) with Inline_Always;
   function REVISION    is new CPUID.B (NVIC_CPUID.REVISION   ) with Inline_Always;

   --  Functions

   function "+"  is new CPUID.Add      with Inline_Always;
   function "+"  is new CPUID.Add_F    with Inline_Always;
   function "+"  is new CPUID.Add_FF   with Inline_Always;
   function "-"  is new CPUID.Clear    with Inline_Always;
   function "-"  is new CPUID.Clear_FF with Inline_Always;
   function "="  is new CPUID.Equal    with Inline_Always;
   function Init is new CPUID.Init     with Inline_Always;

   --------------------
   --  ICSR Register --
   --------------------

   package NVIC_ICSR is

      package Tp is new Types (R32);

      package NMIPENDSET  is new Bitfield (Tp, 31);
      package PENDSVSET   is new Bitfield (Tp, 28);
      package PENDSVCLR   is new Bitfield (Tp, 27);
      package PENDSTSET   is new Bitfield (Tp, 26);
      package PENDSTCLR   is new Bitfield (Tp, 25);
      package ISRPREEMPT  is new Bitfield (Tp, 23);
      package ISRPENDING  is new Bitfield (Tp, 22);
      package VECTPENDING is new Bitfield (Tp, 12, 9);
      package RETTOBASE   is new Bitfield (Tp, 11);
      package VECTACTIVE  is new Bitfield (Tp, 0, 9);

      type T is
         record
            NMIPENDSET  : NVIC_ICSR.NMIPENDSET .T;
            PENDSVSET   : NVIC_ICSR.PENDSVSET  .T;
            PENDSVCLR   : NVIC_ICSR.PENDSVCLR  .T;
            PENDSTSET   : NVIC_ICSR.PENDSTSET  .T;
            PENDSTCLR   : NVIC_ICSR.PENDSTCLR  .T;
            ISRPREEMPT  : NVIC_ICSR.ISRPREEMPT .T;
            ISRPENDING  : NVIC_ICSR.ISRPENDING .T;
            VECTPENDING : NVIC_ICSR.VECTPENDING.T;
            RETTOBASE   : NVIC_ICSR.RETTOBASE  .T;
            VECTACTIVE  : NVIC_ICSR.VECTACTIVE .T;
         end record;

      for T use
         record
            NMIPENDSET  at 0 range NMIPENDSET .F .. NMIPENDSET .L;
            PENDSVSET   at 0 range PENDSVSET  .F .. PENDSVSET  .L;
            PENDSVCLR   at 0 range PENDSVCLR  .F .. PENDSVCLR  .L;
            PENDSTSET   at 0 range PENDSTSET  .F .. PENDSTSET  .L;
            PENDSTCLR   at 0 range PENDSTCLR  .F .. PENDSTCLR  .L;
            ISRPREEMPT  at 0 range ISRPREEMPT .F .. ISRPREEMPT .L;
            ISRPENDING  at 0 range ISRPENDING .F .. ISRPENDING .L;
            VECTPENDING at 0 range VECTPENDING.F .. VECTPENDING.L;
            RETTOBASE   at 0 range RETTOBASE  .F .. RETTOBASE  .L;
            VECTACTIVE  at 0 range VECTACTIVE .F .. VECTACTIVE .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_ICSR;

   package ICSR    is new Register (NVIC_ICSR.T, NVIC_ICSR.Tp, 16#E000_ED04#);
   subtype ICSR_T  is ICSR.T;
   subtype ICSR_F  is ICSR.F;
   

   --  Field definitions

   function NMIPENDSET  is new ICSR.B (NVIC_ICSR.NMIPENDSET ) with Inline_Always;
   function PENDSVSET   is new ICSR.B (NVIC_ICSR.PENDSVSET  ) with Inline_Always;
   function PENDSVCLR   is new ICSR.B (NVIC_ICSR.PENDSVCLR  ) with Inline_Always;
   function PENDSTSET   is new ICSR.B (NVIC_ICSR.PENDSTSET  ) with Inline_Always;
   function PENDSTCLR   is new ICSR.B (NVIC_ICSR.PENDSTCLR  ) with Inline_Always;
   function ISRPREEMPT  is new ICSR.B (NVIC_ICSR.ISRPREEMPT ) with Inline_Always;
   function ISRPENDING  is new ICSR.B (NVIC_ICSR.ISRPENDING ) with Inline_Always;
   function VECTPENDING is new ICSR.B (NVIC_ICSR.VECTPENDING) with Inline_Always;
   function RETTOBASE   is new ICSR.B (NVIC_ICSR.RETTOBASE  ) with Inline_Always;
   function VECTACTIVE  is new ICSR.B (NVIC_ICSR.VECTACTIVE ) with Inline_Always;

   --  Functions

   function "+"  is new ICSR.Add      with Inline_Always;
   function "+"  is new ICSR.Add_F    with Inline_Always;
   function "+"  is new ICSR.Add_FF   with Inline_Always;
   function "-"  is new ICSR.Clear    with Inline_Always;
   function "-"  is new ICSR.Clear_FF with Inline_Always;
   function "="  is new ICSR.Equal    with Inline_Always;
   function Init is new ICSR.Init     with Inline_Always;

   --------------------
   --  VTOR Register --
   --------------------

   package NVIC_VTOR is

      package Tp is new Types (R32);

      package TBLBASE is new Bitfield (Tp, 29);
      package TBLOFF  is new Bitfield (Tp, 7, 22);

      type T is
         record
            TBLBASE : NVIC_VTOR.TBLBASE.T;
            TBLOFF  : NVIC_VTOR.TBLOFF .T;
         end record;

      for T use
         record
            TBLBASE at 0 range TBLBASE.F .. TBLBASE.L;
            TBLOFF  at 0 range TBLOFF .F .. TBLOFF .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_VTOR;

   package VTOR    is new Register (NVIC_VTOR.T, NVIC_VTOR.Tp, 16#E000_ED08#);
   subtype VTOR_T  is VTOR.T;
   subtype VTOR_F  is VTOR.F;
   

   --  Field definitions

   function TBLBASE is new VTOR.B (NVIC_VTOR.TBLBASE) with Inline_Always;
   function TBLOFF  is new VTOR.B (NVIC_VTOR.TBLOFF ) with Inline_Always;

   --  Functions

   function "+"  is new VTOR.Add      with Inline_Always;
   function "+"  is new VTOR.Add_F    with Inline_Always;
   function "+"  is new VTOR.Add_FF   with Inline_Always;
   function "-"  is new VTOR.Clear    with Inline_Always;
   function "-"  is new VTOR.Clear_FF with Inline_Always;
   function "="  is new VTOR.Equal    with Inline_Always;
   function Init is new VTOR.Init     with Inline_Always;

   --  Constant definitions

   function Table_Base_Is_In_Code is new VTOR.C (NVIC_VTOR.TBLBASE, 2#0#) with Inline_Always;
   function Table_Base_Is_In_RAM  is new VTOR.C (NVIC_VTOR.TBLBASE, 2#1#) with Inline_Always;

   ---------------------
   --  AIRCR Register --
   ---------------------

   package NVIC_AIRCR is

      package Tp is new Types (R32);

      package VECTKEY       is new Bitfield (Tp, 16, 16);
      package ENDIANESS     is new Bitfield (Tp, 15);
      package PRIGROUP      is new Bitfield (Tp, 8, 3);
      package SYSRESETREQ   is new Bitfield (Tp, 2);
      package VECTCLRACTIVE is new Bitfield (Tp, 1);
      package VECTRESET     is new Bitfield (Tp, 0);

      type T is
         record
            VECTKEY       : NVIC_AIRCR.VECTKEY      .T;
            ENDIANESS     : NVIC_AIRCR.ENDIANESS    .T;
            PRIGROUP      : NVIC_AIRCR.PRIGROUP     .T;
            SYSRESETREQ   : NVIC_AIRCR.SYSRESETREQ  .T;
            VECTCLRACTIVE : NVIC_AIRCR.VECTCLRACTIVE.T;
            VECTRESET     : NVIC_AIRCR.VECTRESET    .T;
         end record;

      for T use
         record
            VECTKEY       at 0 range VECTKEY      .F .. VECTKEY      .L;
            ENDIANESS     at 0 range ENDIANESS    .F .. ENDIANESS    .L;
            PRIGROUP      at 0 range PRIGROUP     .F .. PRIGROUP     .L;
            SYSRESETREQ   at 0 range SYSRESETREQ  .F .. SYSRESETREQ  .L;
            VECTCLRACTIVE at 0 range VECTCLRACTIVE.F .. VECTCLRACTIVE.L;
            VECTRESET     at 0 range VECTRESET    .F .. VECTRESET    .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_AIRCR;

   package AIRCR    is new Register (NVIC_AIRCR.T, NVIC_AIRCR.Tp, 16#E000_ED0C#);
   subtype AIRCR_T  is AIRCR.T;
   subtype AIRCR_F  is AIRCR.F;
   

   --  Field definitions

   function VECTKEY       is new AIRCR.B (NVIC_AIRCR.VECTKEY      ) with Inline_Always;
   function ENDIANESS     is new AIRCR.B (NVIC_AIRCR.ENDIANESS    ) with Inline_Always;
   function PRIGROUP      is new AIRCR.B (NVIC_AIRCR.PRIGROUP     ) with Inline_Always;
   function SYSRESETREQ   is new AIRCR.B (NVIC_AIRCR.SYSRESETREQ  ) with Inline_Always;
   function VECTCLRACTIVE is new AIRCR.B (NVIC_AIRCR.VECTCLRACTIVE) with Inline_Always;
   function VECTRESET     is new AIRCR.B (NVIC_AIRCR.VECTRESET    ) with Inline_Always;

   --  Functions

   function "+"  is new AIRCR.Add      with Inline_Always;
   function "+"  is new AIRCR.Add_F    with Inline_Always;
   function "+"  is new AIRCR.Add_FF   with Inline_Always;
   function "-"  is new AIRCR.Clear    with Inline_Always;
   function "-"  is new AIRCR.Clear_FF with Inline_Always;
   function "="  is new AIRCR.Equal    with Inline_Always;
   function Init is new AIRCR.Init     with Inline_Always;

   --  Constant definitions

   function Register_Key  is new AIRCR.C (NVIC_AIRCR.VECTKEY, 16#05FA#) with Inline_Always;
   function Little_Endian is new AIRCR.C (NVIC_AIRCR.ENDIANESS, 2#0#) with Inline_Always;
   function Big_Endian    is new AIRCR.C (NVIC_AIRCR.ENDIANESS, 2#1#) with Inline_Always;
   function Prigroup_7_1  is new AIRCR.C (NVIC_AIRCR.PRIGROUP, 2#000#) with Inline_Always;
   function Prigroup_6_2  is new AIRCR.C (NVIC_AIRCR.PRIGROUP, 2#001#) with Inline_Always;
   function Prigroup_5_3  is new AIRCR.C (NVIC_AIRCR.PRIGROUP, 2#010#) with Inline_Always;
   function Prigroup_4_4  is new AIRCR.C (NVIC_AIRCR.PRIGROUP, 2#011#) with Inline_Always;
   function Prigroup_3_5  is new AIRCR.C (NVIC_AIRCR.PRIGROUP, 2#100#) with Inline_Always;
   function Prigroup_2_6  is new AIRCR.C (NVIC_AIRCR.PRIGROUP, 2#101#) with Inline_Always;
   function Prigroup_1_7  is new AIRCR.C (NVIC_AIRCR.PRIGROUP, 2#110#) with Inline_Always;
   function Prigroup_0_8  is new AIRCR.C (NVIC_AIRCR.PRIGROUP, 2#111#) with Inline_Always;

   ---------------------
   --  SCR Register --
   ---------------------

   package NVIC_SCR is

      package Tp is new Types (R32);

      package SEVONPEND   is new Bitfield (Tp, 4);
      package SLEEPDEEP   is new Bitfield (Tp, 2);
      package SLEEPONEXIT is new Bitfield (Tp, 1);

      type T is
         record
            SEVONPEND   : NVIC_SCR.SEVONPEND  .T;
            SLEEPDEEP   : NVIC_SCR.SLEEPDEEP  .T;
            SLEEPONEXIT : NVIC_SCR.SLEEPONEXIT.T;
         end record;

      for T use
         record
            SEVONPEND   at 0 range SEVONPEND  .F .. SEVONPEND  .L;
            SLEEPDEEP   at 0 range SLEEPDEEP  .F .. SLEEPDEEP  .L;
            SLEEPONEXIT at 0 range SLEEPONEXIT.F .. SLEEPONEXIT.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_SCR;

   package SCR    is new Register (NVIC_SCR.T, NVIC_SCR.Tp, 16#E000_ED10#);
   subtype SCR_T  is SCR.T;
   subtype SCR_F  is SCR.F;
   

   --  Field definitions

   function SEVONPEND   is new SCR.B (NVIC_SCR.SEVONPEND  ) with Inline_Always;
   function SLEEPDEEP   is new SCR.B (NVIC_SCR.SLEEPDEEP  ) with Inline_Always;
   function SLEEPONEXIT is new SCR.B (NVIC_SCR.SLEEPONEXIT) with Inline_Always;

   --  Functions

   function "+"  is new SCR.Add      with Inline_Always;
   function "+"  is new SCR.Add_F    with Inline_Always;
   function "+"  is new SCR.Add_FF   with Inline_Always;
   function "-"  is new SCR.Clear    with Inline_Always;
   function "-"  is new SCR.Clear_FF with Inline_Always;
   function "="  is new SCR.Equal    with Inline_Always;
   function Init is new SCR.Init     with Inline_Always;

   -------------------
   --  CCR Register --
   -------------------

   package NVIC_CCR is

      package Tp is new Types (R32);

      package STKALIGN        is new Bitfield (Tp, 9);
      package BFHFNMIGN       is new Bitfield (Tp, 8);
      package DIV_0_TRP       is new Bitfield (Tp, 4);
      package UNALIGN_TRP     is new Bitfield (Tp, 3);
      package USERSETMPEND    is new Bitfield (Tp, 1);
      package NONEBASETHRDENA is new Bitfield (Tp, 0);

      type T is
         record
            STKALIGN        : NVIC_CCR.STKALIGN       .T;
            BFHFNMIGN       : NVIC_CCR.BFHFNMIGN      .T;
            DIV_0_TRP       : NVIC_CCR.DIV_0_TRP      .T;
            UNALIGN_TRP     : NVIC_CCR.UNALIGN_TRP    .T;
            USERSETMPEND    : NVIC_CCR.USERSETMPEND   .T;
            NONEBASETHRDENA : NVIC_CCR.NONEBASETHRDENA.T;
         end record;

      for T use
         record
            STKALIGN        at 0 range STKALIGN       .F .. STKALIGN       .L;
            BFHFNMIGN       at 0 range BFHFNMIGN      .F .. BFHFNMIGN      .L;
            DIV_0_TRP       at 0 range DIV_0_TRP      .F .. DIV_0_TRP      .L;
            UNALIGN_TRP     at 0 range UNALIGN_TRP    .F .. UNALIGN_TRP    .L;
            USERSETMPEND    at 0 range USERSETMPEND   .F .. USERSETMPEND   .L;
            NONEBASETHRDENA at 0 range NONEBASETHRDENA.F .. NONEBASETHRDENA.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_CCR;

   package CCR    is new Register (NVIC_CCR.T, NVIC_CCR.Tp, 16#E000_ED14#);
   subtype CCR_T  is CCR.T;
   subtype CCR_F  is CCR.F;
   

   --  Field definitions

   function STKALIGN        is new CCR.B (NVIC_CCR.STKALIGN       ) with Inline_Always;
   function BFHFNMIGN       is new CCR.B (NVIC_CCR.BFHFNMIGN      ) with Inline_Always;
   function DIV_0_TRP       is new CCR.B (NVIC_CCR.DIV_0_TRP      ) with Inline_Always;
   function UNALIGN_TRP     is new CCR.B (NVIC_CCR.UNALIGN_TRP    ) with Inline_Always;
   function USERSETMPEND    is new CCR.B (NVIC_CCR.USERSETMPEND   ) with Inline_Always;
   function NONEBASETHRDENA is new CCR.B (NVIC_CCR.NONEBASETHRDENA) with Inline_Always;

   --  Functions

   function "+"  is new CCR.Add      with Inline_Always;
   function "+"  is new CCR.Add_F    with Inline_Always;
   function "+"  is new CCR.Add_FF   with Inline_Always;
   function "-"  is new CCR.Clear    with Inline_Always;
   function "-"  is new CCR.Clear_FF with Inline_Always;
   function "="  is new CCR.Equal    with Inline_Always;
   function Init is new CCR.Init     with Inline_Always;

   -------------------
   --  SHP Register --
   -------------------

   type System_Handler is (MemManage,    BusFault,  UsageFault, Reserved1,
                           Reserved2,    Reserved3, Reserved4,  SVC,
                           DebugMonitor, Reserved5, PendSV,     SysTick);

   type SHP_T is array (System_Handler) of PRIORITY_T with Pack;
   for SHP_T'Size use 12 * 8;

   SHP : SHP_T with Volatile;
   for SHP'Address use System'To_Address (16#E000_ED18#);
   --  Type definition of hardware register: SHP

   ---------------------
   --  SHCSR Register --
   ---------------------

   package NVIC_SHCSR is

      package Tp is new Types (R32);

      package USGFAULTENA    is new Bitfield (Tp, 18);
      package BUSFAULTENA    is new Bitfield (Tp, 17);
      package MEMFAULTENA    is new Bitfield (Tp, 16);
      package SVCALLPENDED   is new Bitfield (Tp, 15);
      package BUSFAULTPENDED is new Bitfield (Tp, 14);
      package MEMFAULTPENDED is new Bitfield (Tp, 13);
      package USGFAULTPENDED is new Bitfield (Tp, 12);
      package SYSTICKACT     is new Bitfield (Tp, 11);
      package PENDSVACT      is new Bitfield (Tp, 10);
      package MONITORACT     is new Bitfield (Tp, 8);
      package SVCALLACT      is new Bitfield (Tp, 7);
      package USGFAULTACT    is new Bitfield (Tp, 3);
      package BUSFAULTACT    is new Bitfield (Tp, 1);
      package MEMFAULTACT    is new Bitfield (Tp, 0);

      type T is
         record
            USGFAULTENA    : NVIC_SHCSR.USGFAULTENA   .T;
            BUSFAULTENA    : NVIC_SHCSR.BUSFAULTENA   .T;
            MEMFAULTENA    : NVIC_SHCSR.MEMFAULTENA   .T;
            SVCALLPENDED   : NVIC_SHCSR.SVCALLPENDED  .T;
            BUSFAULTPENDED : NVIC_SHCSR.BUSFAULTPENDED.T;
            MEMFAULTPENDED : NVIC_SHCSR.MEMFAULTPENDED.T;
            USGFAULTPENDED : NVIC_SHCSR.USGFAULTPENDED.T;
            SYSTICKACT     : NVIC_SHCSR.SYSTICKACT    .T;
            PENDSVACT      : NVIC_SHCSR.PENDSVACT     .T;
            MONITORACT     : NVIC_SHCSR.MONITORACT    .T;
            SVCALLACT      : NVIC_SHCSR.SVCALLACT     .T;
            USGFAULTACT    : NVIC_SHCSR.USGFAULTACT   .T;
            BUSFAULTACT    : NVIC_SHCSR.BUSFAULTACT   .T;
            MEMFAULTACT    : NVIC_SHCSR.MEMFAULTACT   .T;
         end record;

      for T use
         record
            USGFAULTENA    at 0 range USGFAULTENA   .F .. USGFAULTENA   .L;
            BUSFAULTENA    at 0 range BUSFAULTENA   .F .. BUSFAULTENA   .L;
            MEMFAULTENA    at 0 range MEMFAULTENA   .F .. MEMFAULTENA   .L;
            SVCALLPENDED   at 0 range SVCALLPENDED  .F .. SVCALLPENDED  .L;
            BUSFAULTPENDED at 0 range BUSFAULTPENDED.F .. BUSFAULTPENDED.L;
            MEMFAULTPENDED at 0 range MEMFAULTPENDED.F .. MEMFAULTPENDED.L;
            USGFAULTPENDED at 0 range USGFAULTPENDED.F .. USGFAULTPENDED.L;
            SYSTICKACT     at 0 range SYSTICKACT    .F .. SYSTICKACT    .L;
            PENDSVACT      at 0 range PENDSVACT     .F .. PENDSVACT     .L;
            MONITORACT     at 0 range MONITORACT    .F .. MONITORACT    .L;
            SVCALLACT      at 0 range SVCALLACT     .F .. SVCALLACT     .L;
            USGFAULTACT    at 0 range USGFAULTACT   .F .. USGFAULTACT   .L;
            BUSFAULTACT    at 0 range BUSFAULTACT   .F .. BUSFAULTACT   .L;
            MEMFAULTACT    at 0 range MEMFAULTACT   .F .. MEMFAULTACT   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_SHCSR;

   package SHCSR    is new Register (NVIC_SHCSR.T, NVIC_SHCSR.Tp, 16#E000_ED24#);
   subtype SHCSR_T  is SHCSR.T;
   subtype SHCSR_F  is SHCSR.F;
   

   --  Field definitions

   function USGFAULTENA    is new SHCSR.B (NVIC_SHCSR.USGFAULTENA   );
   function BUSFAULTENA    is new SHCSR.B (NVIC_SHCSR.BUSFAULTENA   );
   function MEMFAULTENA    is new SHCSR.B (NVIC_SHCSR.MEMFAULTENA   );
   function SVCALLPENDED   is new SHCSR.B (NVIC_SHCSR.SVCALLPENDED  );
   function BUSFAULTPENDED is new SHCSR.B (NVIC_SHCSR.BUSFAULTPENDED);
   function MEMFAULTPENDED is new SHCSR.B (NVIC_SHCSR.MEMFAULTPENDED);
   function USGFAULTPENDED is new SHCSR.B (NVIC_SHCSR.USGFAULTPENDED);
   function SYSTICKACT     is new SHCSR.B (NVIC_SHCSR.SYSTICKACT    );
   function PENDSVACT      is new SHCSR.B (NVIC_SHCSR.PENDSVACT     );
   function MONITORACT     is new SHCSR.B (NVIC_SHCSR.MONITORACT    );
   function SVCALLACT      is new SHCSR.B (NVIC_SHCSR.SVCALLACT     );
   function USGFAULTACT    is new SHCSR.B (NVIC_SHCSR.USGFAULTACT   );
   function BUSFAULTACT    is new SHCSR.B (NVIC_SHCSR.BUSFAULTACT   );
   function MEMFAULTACT    is new SHCSR.B (NVIC_SHCSR.MEMFAULTACT   );

   --  Functions

   function "+"  is new SHCSR.Add      with Inline_Always;
   function "+"  is new SHCSR.Add_F    with Inline_Always;
   function "+"  is new SHCSR.Add_FF   with Inline_Always;
   function "-"  is new SHCSR.Clear    with Inline_Always;
   function "-"  is new SHCSR.Clear_FF with Inline_Always;
   function "="  is new SHCSR.Equal    with Inline_Always;
   function Init is new SHCSR.Init     with Inline_Always;

   ---------------------
   --  CFSR Register --
   ---------------------

   package NVIC_CFSR is

      package Tp is new Types (R32);

      package DIVBYZERO   is new Bitfield (Tp, 25);
      package UNALIGNED   is new Bitfield (Tp, 24);
      package NOCP        is new Bitfield (Tp, 19);
      package INVPC       is new Bitfield (Tp, 18);
      package INVSTATE    is new Bitfield (Tp, 17);
      package UNDEFINSTR  is new Bitfield (Tp, 16);
      package BFARVALID   is new Bitfield (Tp, 15);
      package STKERR      is new Bitfield (Tp, 12);
      package UNSTKERR    is new Bitfield (Tp, 11);
      package IMPRECISERR is new Bitfield (Tp, 10);
      package PRECISERR   is new Bitfield (Tp, 9);
      package IBUSERR     is new Bitfield (Tp, 8);
      package MMARVALID   is new Bitfield (Tp, 7);
      package MSTKERR     is new Bitfield (Tp, 4);
      package MUNSTKERR   is new Bitfield (Tp, 3);
      package DACCVIOL    is new Bitfield (Tp, 1);
      package IACCVIOL    is new Bitfield (Tp, 0);

      type T is
         record
            DIVBYZERO   : NVIC_CFSR.DIVBYZERO  .T;
            UNALIGNED   : NVIC_CFSR.UNALIGNED  .T;
            NOCP        : NVIC_CFSR.NOCP       .T;
            INVPC       : NVIC_CFSR.INVPC      .T;
            INVSTATE    : NVIC_CFSR.INVSTATE   .T;
            UNDEFINSTR  : NVIC_CFSR.UNDEFINSTR .T;
            BFARVALID   : NVIC_CFSR.BFARVALID  .T;
            STKERR      : NVIC_CFSR.STKERR     .T;
            UNSTKERR    : NVIC_CFSR.UNSTKERR   .T;
            IMPRECISERR : NVIC_CFSR.IMPRECISERR.T;
            PRECISERR   : NVIC_CFSR.PRECISERR  .T;
            IBUSERR     : NVIC_CFSR.IBUSERR    .T;
            MMARVALID   : NVIC_CFSR.MMARVALID  .T;
            MSTKERR     : NVIC_CFSR.MSTKERR    .T;
            MUNSTKERR   : NVIC_CFSR.MUNSTKERR  .T;
            DACCVIOL    : NVIC_CFSR.DACCVIOL   .T;
            IACCVIOL    : NVIC_CFSR.IACCVIOL   .T;
         end record;

      for T use
         record
            DIVBYZERO   at 0 range DIVBYZERO  .F .. DIVBYZERO  .L;
            UNALIGNED   at 0 range UNALIGNED  .F .. UNALIGNED  .L;
            NOCP        at 0 range NOCP       .F .. NOCP       .L;
            INVPC       at 0 range INVPC      .F .. INVPC      .L;
            INVSTATE    at 0 range INVSTATE   .F .. INVSTATE   .L;
            UNDEFINSTR  at 0 range UNDEFINSTR .F .. UNDEFINSTR .L;
            BFARVALID   at 0 range BFARVALID  .F .. BFARVALID  .L;
            STKERR      at 0 range STKERR     .F .. STKERR     .L;
            UNSTKERR    at 0 range UNSTKERR   .F .. UNSTKERR   .L;
            IMPRECISERR at 0 range IMPRECISERR.F .. IMPRECISERR.L;
            PRECISERR   at 0 range PRECISERR  .F .. PRECISERR  .L;
            IBUSERR     at 0 range IBUSERR    .F .. IBUSERR    .L;
            MMARVALID   at 0 range MMARVALID  .F .. MMARVALID  .L;
            MSTKERR     at 0 range MSTKERR    .F .. MSTKERR    .L;
            MUNSTKERR   at 0 range MUNSTKERR  .F .. MUNSTKERR  .L;
            DACCVIOL    at 0 range DACCVIOL   .F .. DACCVIOL   .L;
            IACCVIOL    at 0 range IACCVIOL   .F .. IACCVIOL   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_CFSR;

   package CFSR    is new Register (NVIC_CFSR.T, NVIC_CFSR.Tp, 16#E000_ED28#);
   subtype CFSR_T  is CFSR.T;
   subtype CFSR_F  is CFSR.F;
   

   --  Field definitions

   function DIVBYZERO   is new CFSR.B (NVIC_CFSR.DIVBYZERO  ) with Inline_Always;
   function UNALIGNED   is new CFSR.B (NVIC_CFSR.UNALIGNED  ) with Inline_Always;
   function NOCP        is new CFSR.B (NVIC_CFSR.NOCP       ) with Inline_Always;
   function INVPC       is new CFSR.B (NVIC_CFSR.INVPC      ) with Inline_Always;
   function INVSTATE    is new CFSR.B (NVIC_CFSR.INVSTATE   ) with Inline_Always;
   function UNDEFINSTR  is new CFSR.B (NVIC_CFSR.UNDEFINSTR ) with Inline_Always;
   function BFARVALID   is new CFSR.B (NVIC_CFSR.BFARVALID  ) with Inline_Always;
   function STKERR      is new CFSR.B (NVIC_CFSR.STKERR     ) with Inline_Always;
   function UNSTKERR    is new CFSR.B (NVIC_CFSR.UNSTKERR   ) with Inline_Always;
   function IMPRECISERR is new CFSR.B (NVIC_CFSR.IMPRECISERR) with Inline_Always;
   function PRECISERR   is new CFSR.B (NVIC_CFSR.PRECISERR  ) with Inline_Always;
   function IBUSERR     is new CFSR.B (NVIC_CFSR.IBUSERR    ) with Inline_Always;
   function MMARVALID   is new CFSR.B (NVIC_CFSR.MMARVALID  ) with Inline_Always;
   function MSTKERR     is new CFSR.B (NVIC_CFSR.MSTKERR    ) with Inline_Always;
   function MUNSTKERR   is new CFSR.B (NVIC_CFSR.MUNSTKERR  ) with Inline_Always;
   function DACCVIOL    is new CFSR.B (NVIC_CFSR.DACCVIOL   ) with Inline_Always;
   function IACCVIOL    is new CFSR.B (NVIC_CFSR.IACCVIOL   ) with Inline_Always;

   --  Functions

   function "+"  is new CFSR.Add      with Inline_Always;
   function "+"  is new CFSR.Add_F    with Inline_Always;
   function "+"  is new CFSR.Add_FF   with Inline_Always;
   function "-"  is new CFSR.Clear    with Inline_Always;
   function "-"  is new CFSR.Clear_FF with Inline_Always;
   function "="  is new CFSR.Equal    with Inline_Always;
   function Init is new CFSR.Init     with Inline_Always;

   ---------------------
   --  HFSR Register --
   ---------------------

   package NVIC_HFSR is

      package Tp is new Types (R32);

      package DEBUGEVT is new Bitfield (Tp, 31);
      package FORCED   is new Bitfield (Tp, 30);
      package VECTTBL  is new Bitfield (Tp, 1);

      type T is
         record
            DEBUGEVT : NVIC_HFSR.DEBUGEVT.T;
            FORCED   : NVIC_HFSR.FORCED  .T;
            VECTTBL  : NVIC_HFSR.VECTTBL .T;
         end record;

      for T use
         record
            DEBUGEVT at 0 range DEBUGEVT.F .. DEBUGEVT.L;
            FORCED   at 0 range FORCED  .F .. FORCED  .L;
            VECTTBL  at 0 range VECTTBL .F .. VECTTBL .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_HFSR;

   package HFSR    is new Register (NVIC_HFSR.T, NVIC_HFSR.Tp, 16#E000_ED2C#);
   subtype HFSR_T  is HFSR.T;
   subtype HFSR_F  is HFSR.F;
   

   --  Field definitions

   function DEBUGEVT is new HFSR.B (NVIC_HFSR.DEBUGEVT) with Inline_Always;
   function FORCED   is new HFSR.B (NVIC_HFSR.FORCED  ) with Inline_Always;
   function VECTTBL  is new HFSR.B (NVIC_HFSR.VECTTBL ) with Inline_Always;

   --  Functions

   function "+"  is new HFSR.Add      with Inline_Always;
   function "+"  is new HFSR.Add_F    with Inline_Always;
   function "+"  is new HFSR.Add_FF   with Inline_Always;
   function "-"  is new HFSR.Clear    with Inline_Always;
   function "-"  is new HFSR.Clear_FF with Inline_Always;
   function "="  is new HFSR.Equal    with Inline_Always;
   function Init is new HFSR.Init     with Inline_Always;

   ---------------------
   --  DFSR Register --
   ---------------------

   package NVIC_DFSR is

      package Tp is new Types (R32);

      package EXTERNAL is new Bitfield (Tp, 4);
      package VCATCH   is new Bitfield (Tp, 3);
      package DWTTRAP  is new Bitfield (Tp, 2);
      package BKPT     is new Bitfield (Tp, 1);
      package HALTED   is new Bitfield (Tp, 0);

      type T is
         record
            EXTERNAL : NVIC_DFSR.EXTERNAL.T;
            VCATCH   : NVIC_DFSR.VCATCH  .T;
            DWTTRAP  : NVIC_DFSR.DWTTRAP .T;
            BKPT     : NVIC_DFSR.BKPT    .T;
            HALTED   : NVIC_DFSR.HALTED  .T;
         end record;

      for T use
         record
            EXTERNAL at 0 range EXTERNAL.F .. EXTERNAL.L;
            VCATCH   at 0 range VCATCH  .F .. VCATCH  .L;
            DWTTRAP  at 0 range DWTTRAP .F .. DWTTRAP .L;
            BKPT     at 0 range BKPT    .F .. BKPT    .L;
            HALTED   at 0 range HALTED  .F .. HALTED  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_DFSR;

   package DFSR    is new Register (NVIC_DFSR.T, NVIC_DFSR.Tp, 16#E000_ED30#);
   subtype DFSR_T  is DFSR.T;
   subtype DFSR_F  is DFSR.F;
   

   --  Field definitions

   function EXTERNAL is new DFSR.B (NVIC_DFSR.EXTERNAL) with Inline_Always;
   function VCATCH   is new DFSR.B (NVIC_DFSR.VCATCH  ) with Inline_Always;
   function DWTTRAP  is new DFSR.B (NVIC_DFSR.DWTTRAP ) with Inline_Always;
   function BKPT     is new DFSR.B (NVIC_DFSR.BKPT    ) with Inline_Always;
   function HALTED   is new DFSR.B (NVIC_DFSR.HALTED  ) with Inline_Always;

   --  Functions

   function "+"  is new DFSR.Add      with Inline_Always;
   function "+"  is new DFSR.Add_F    with Inline_Always;
   function "+"  is new DFSR.Add_FF   with Inline_Always;
   function "-"  is new DFSR.Clear    with Inline_Always;
   function "-"  is new DFSR.Clear_FF with Inline_Always;
   function "="  is new DFSR.Equal    with Inline_Always;
   function Init is new DFSR.Init     with Inline_Always;

   ---------------------
   --  MMFAR Register --
   ---------------------

   subtype MMFAR_T is R32;
   pragma Suppress_Initialization (MMFAR_T);

   MMFAR : MMFAR_T with Volatile;
   for MMFAR'Address use System'To_Address (16#E000_ED34#);
   --  Type definition of hardware register: MMFAR

   --------------------
   --  BFAR Register --
   --------------------

   subtype BFAR_T is R32;
   pragma Suppress_Initialization (BFAR_T);

   BFAR : BFAR_T with Volatile;
   for BFAR'Address use System'To_Address (16#E000_ED38#);
   --  Type definition of hardware register: BFAR

   ---------------------
   --  AFSR Register --
   ---------------------

   package NVIC_AFSR is

      package Tp is new Types (R32);

      package IMPDEF is new Bitfield (Tp, 0, 32);

      type T is
         record
            IMPDEF : NVIC_AFSR.IMPDEF.T;
         end record;

      for T use
         record
            IMPDEF at 0 range IMPDEF.F .. IMPDEF.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_AFSR;

   package AFSR    is new Register (NVIC_AFSR.T, NVIC_AFSR.Tp, 16#E000_ED3C#);
   subtype AFSR_T  is AFSR.T;
   subtype AFSR_F  is AFSR.F;
   

   --  Field definitions

   function IMPDEF is new AFSR.B (NVIC_AFSR.IMPDEF) with Inline_Always;

   --  Functions

   function "+"  is new AFSR.Add      with Inline_Always;
   function "+"  is new AFSR.Add_F    with Inline_Always;
   function "+"  is new AFSR.Add_FF   with Inline_Always;
   function "-"  is new AFSR.Clear    with Inline_Always;
   function "-"  is new AFSR.Clear_FF with Inline_Always;
   function "="  is new AFSR.Equal    with Inline_Always;
   function Init is new AFSR.Init     with Inline_Always;

   --------------------
   --  PFR0 Register --
   --------------------

   subtype PFR0_T is R32;
   pragma Suppress_Initialization (PFR0_T);

   PFR0 : PFR0_T with Volatile;
   for PFR0'Address use System'To_Address (16#E000_ED40#);
   --  Type definition of hardware register: PFR0

   --------------------
   --  PFR1 Register --
   --------------------

   subtype PFR1_T is R32;
   pragma Suppress_Initialization (PFR1_T);

   PFR1 : PFR1_T with Volatile;
   for PFR1'Address use System'To_Address (16#E000_ED44#);
   --  Type definition of hardware register: PFR1

   --------------------
   --  DFR0 Register --
   --------------------

   subtype DFR0_T is R32;
   pragma Suppress_Initialization (DFR0_T);

   DFR0 : DFR0_T with Volatile;
   for DFR0'Address use System'To_Address (16#E000_ED48#);
   --  Type definition of hardware register: DFR0

   --------------------
   --  AFR0 Register --
   --------------------

   subtype AFR0_T is R32;
   pragma Suppress_Initialization (AFR0_T);

   AFR0 : AFR0_T with Volatile;
   for AFR0'Address use System'To_Address (16#E000_ED4C#);
   --  Type definition of hardware register: AFR0

   ---------------------
   --  MMFR0 Register --
   ---------------------

   subtype MMFR0_T is R32;
   pragma Suppress_Initialization (MMFR0_T);

   MMFR0 : MMFR0_T with Volatile;
   for MMFR0'Address use System'To_Address (16#E000_ED50#);
   --  Type definition of hardware register: MMFR0

   ---------------------
   --  MMFR1 Register --
   ---------------------

   subtype MMFR1_T is R32;
   pragma Suppress_Initialization (MMFR1_T);

   MMFR1 : MMFR1_T with Volatile;
   for MMFR1'Address use System'To_Address (16#E000_ED54#);
   --  Type definition of hardware register: MMFR1

   ---------------------
   --  MMFR2 Register --
   ---------------------

   subtype MMFR2_T is R32;
   pragma Suppress_Initialization (MMFR2_T);

   MMFR2 : MMFR2_T with Volatile;
   for MMFR2'Address use System'To_Address (16#E000_ED58#);
   --  Type definition of hardware register: MMFR2

   ---------------------
   --  MMFR3 Register --
   ---------------------

   subtype MMFR3_T is R32;
   pragma Suppress_Initialization (MMFR3_T);

   MMFR3 : MMFR3_T with Volatile;
   for MMFR3'Address use System'To_Address (16#E000_ED5C#);
   --  Type definition of hardware register: MMFR3

   ---------------------
   --  ISAR0 Register --
   ---------------------

   subtype ISAR0_T is R32;
   pragma Suppress_Initialization (ISAR0_T);

   ISAR0 : ISAR0_T with Volatile;
   for ISAR0'Address use System'To_Address (16#E000_ED60#);
   --  Type definition of hardware register: ISAR0

   ---------------------
   --  ISAR1 Register --
   ---------------------

   subtype ISAR1_T is R32;
   pragma Suppress_Initialization (ISAR1_T);

   ISAR1 : ISAR1_T with Volatile;
   for ISAR1'Address use System'To_Address (16#E000_ED64#);
   --  Type definition of hardware register: ISAR1

   ---------------------
   --  ISAR2 Register --
   ---------------------

   subtype ISAR2_T is R32;
   pragma Suppress_Initialization (ISAR2_T);

   ISAR2 : ISAR2_T with Volatile;
   for ISAR2'Address use System'To_Address (16#E000_ED68#);
   --  Type definition of hardware register: ISAR2

   ---------------------
   --  ISAR3 Register --
   ---------------------

   subtype ISAR3_T is R32;
   pragma Suppress_Initialization (ISAR3_T);

   ISAR3 : ISAR3_T with Volatile;
   for ISAR3'Address use System'To_Address (16#E000_ED6C#);
   --  Type definition of hardware register: ISAR3

   ---------------------
   --  ISAR4 Register --
   ---------------------

   subtype ISAR4_T is R32;
   pragma Suppress_Initialization (ISAR4_T);

   ISAR4 : ISAR4_T with Volatile;
   for ISAR4'Address use System'To_Address (16#E000_ED70#);
   --  Type definition of hardware register: ISAR4

   ---------------------
   --  STIR Register --
   ---------------------

   package NVIC_STIR is

      package Tp is new Types (R32);

      package INTID is new Bitfield (Tp, 0, 9);

      type T is
         record
            INTID : NVIC_STIR.INTID.T;
         end record;

      for T use
         record
            INTID at 0 range INTID.F .. INTID.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end NVIC_STIR;

   package STIR    is new Register (NVIC_STIR.T, NVIC_STIR.Tp, 16#E000_EF00#);
   subtype STIR_T  is STIR.T;
   subtype STIR_F  is STIR.F;
   

   --  Field definitions

   function INTID is new STIR.B (NVIC_STIR.INTID) with Inline_Always;

   --  Functions

   function "+"  is new STIR.Add      with Inline_Always;
   function "+"  is new STIR.Add_F    with Inline_Always;
   function "+"  is new STIR.Add_FF   with Inline_Always;
   function "-"  is new STIR.Clear    with Inline_Always;
   function "-"  is new STIR.Clear_FF with Inline_Always;
   function "="  is new STIR.Equal    with Inline_Always;
   function Init is new STIR.Init     with Inline_Always;

end ARM.Register.NVIC_F103VC;
