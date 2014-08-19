--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--            S T M 3 2 . R e g i s t e r s . N V I C _ F 1 0 3 V C           --
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
use  System;

with Bitfields;
use  Bitfields;

with Bitfields.Types;
with Bitfields.Bitfield;
with Bitfields.Functions;

-----------------------------------------------------------------------------
--                 System.BB.Architecture.Registers.NVIC                   --
-----------------------------------------------------------------------------

package STM32.Registers.NVIC_F103VC is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                                 NVIC                                --
   -------------------------------------------------------------------------

   --------------------
   --  ICTR Register --
   --------------------

   package ICTR is

      package Tp is new Types (R32);

      package INTLINESNUM is new Bitfield (Tp, 0, 5);

      type T is
         record
            INTLINESNUM : ICTR.INTLINESNUM.T;
         end record;

      for T use
         record
            INTLINESNUM at 0 range INTLINESNUM.R'First .. INTLINESNUM.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end ICTR;

   subtype ICTR_T is ICTR.T;

   --  Field definitions

   function INTLINESNUM is new ICTR.FN.B (ICTR.INTLINESNUM);

   --  Functions

   function  "+"   is new ICTR.FN.Add;
   function  "+"   is new ICTR.FN.Add_RM;
   function  "-"   is new ICTR.FN.Clear;
   function  Init  is new ICTR.FN.Init;

   ----------------------------
   --  SYSTICK_CTRL Register --
   ----------------------------

   package SYSTICK_CTRL is

      package Tp is new Types (R32);

      package COUNTFLAG is new Bitfield (Tp, 16);
      package CLKSOURCE is new Bitfield (Tp, 2);
      package TICKINT   is new Bitfield (Tp, 1);
      package ENABLE    is new Bitfield (Tp, 0);

      type T is
         record
            COUNTFLAG : SYSTICK_CTRL.COUNTFLAG.T;
            CLKSOURCE : SYSTICK_CTRL.CLKSOURCE.T;
            TICKINT   : SYSTICK_CTRL.TICKINT.T;
            ENABLE    : SYSTICK_CTRL.ENABLE.T;
         end record;

      for T use
         record
            COUNTFLAG at 0 range COUNTFLAG.R'First .. COUNTFLAG.R'Last;
            CLKSOURCE at 0 range CLKSOURCE.R'First .. CLKSOURCE.R'Last;
            TICKINT   at 0 range TICKINT.R'First .. TICKINT.R'Last;
            ENABLE    at 0 range ENABLE.R'First .. ENABLE.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end SYSTICK_CTRL;

   subtype SYSTICK_CTRL_T is SYSTICK_CTRL.T;

   --  Field definitions

   function COUNTFLAG is new SYSTICK_CTRL.FN.B (SYSTICK_CTRL.COUNTFLAG);
   function CLKSOURCE is new SYSTICK_CTRL.FN.B (SYSTICK_CTRL.CLKSOURCE);
   function TICKINT   is new SYSTICK_CTRL.FN.B (SYSTICK_CTRL.TICKINT);
   function ENABLE    is new SYSTICK_CTRL.FN.B (SYSTICK_CTRL.ENABLE);

   --  Functions

   function  "+"   is new SYSTICK_CTRL.FN.Add;
   function  "+"   is new SYSTICK_CTRL.FN.Add_RM;
   function  "-"   is new SYSTICK_CTRL.FN.Clear;
   function  Init  is new SYSTICK_CTRL.FN.Init;

   --  Constant definitions

   function Reached_Zero is new
     SYSTICK_CTRL.FN.C (SYSTICK_CTRL.COUNTFLAG, 2#1#);
   function External_Reference_Clock is new
     SYSTICK_CTRL.FN.C (SYSTICK_CTRL.CLKSOURCE, 2#0#);
   function Core_Clock is new
     SYSTICK_CTRL.FN.C (SYSTICK_CTRL.CLKSOURCE, 2#1#);
   function Not_Pend_SYSTICK is new
     SYSTICK_CTRL.FN.C (SYSTICK_CTRL.TICKINT, 2#0#);
   function Pend_SYSTICK is new
     SYSTICK_CTRL.FN.C (SYSTICK_CTRL.TICKINT, 2#1#);
   function Disabled is new SYSTICK_CTRL.FN.C (SYSTICK_CTRL.ENABLE, 2#0#);
   function Enabled  is new SYSTICK_CTRL.FN.C (SYSTICK_CTRL.ENABLE, 2#1#);

   ----------------------------
   --  SYSTICK_LOAD Register --
   ----------------------------

   package SYSTICK_LOAD is

      package Tp is new Types (R32);

      package RELOAD is new Bitfield (Tp, 0, 24);

      type T is
         record
            RELOAD : SYSTICK_LOAD.RELOAD.T;
         end record;

      for T use
         record
            RELOAD at 0 range RELOAD.R'First .. RELOAD.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end SYSTICK_LOAD;

   subtype SYSTICK_LOAD_T is SYSTICK_LOAD.T;

   --  Field definitions

   function RELOAD is new SYSTICK_LOAD.FN.B (SYSTICK_LOAD.RELOAD);

   --  Functions

   function  "+"   is new SYSTICK_LOAD.FN.Add;
   function  "+"   is new SYSTICK_LOAD.FN.Add_RM;
   function  "-"   is new SYSTICK_LOAD.FN.Clear;
   function  Init  is new SYSTICK_LOAD.FN.Init;

   ---------------------------
   --  SYSTICK_VAL Register --
   ---------------------------

   package SYSTICK_VAL is

      package Tp is new Types (R32);

      package CURRENT is new Bitfield (Tp, 0, 24);

      type T is
         record
            CURRENT : SYSTICK_VAL.CURRENT.T;
         end record;

      for T use
         record
            CURRENT at 0 range CURRENT.R'First .. CURRENT.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end SYSTICK_VAL;

   subtype SYSTICK_VAL_T is SYSTICK_VAL.T;

   --  Field definitions

   function CURRENT is new SYSTICK_VAL.FN.B (SYSTICK_VAL.CURRENT);

   --  Functions

   function  "+"   is new SYSTICK_VAL.FN.Add;
   function  "+"   is new SYSTICK_VAL.FN.Add_RM;
   function  "-"   is new SYSTICK_VAL.FN.Clear;
   function  Init  is new SYSTICK_VAL.FN.Init;

   -----------------------------
   --  SYSTICK_CALIB Register --
   -----------------------------

   package SYSTICK_CALIB is

      package Tp is new Types (R32);

      package NOREF is new Bitfield (Tp, 31);
      package SKEW  is new Bitfield (Tp, 30);
      package TENMS is new Bitfield (Tp, 0, 24);

      type T is
         record
            NOREF : SYSTICK_CALIB.NOREF.T;
            SKEW  : SYSTICK_CALIB.SKEW.T;
            TENMS : SYSTICK_CALIB.TENMS.T;
         end record;

      for T use
         record
            NOREF at 0 range NOREF.R'First .. NOREF.R'Last;
            SKEW  at 0 range SKEW.R'First .. SKEW.R'Last;
            TENMS at 0 range TENMS.R'First .. TENMS.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end SYSTICK_CALIB;

   subtype SYSTICK_CALIB_T is SYSTICK_CALIB.T;

   --  Field definitions

   function NOREF is new SYSTICK_CALIB.FN.B (SYSTICK_CALIB.NOREF);
   function SKEW  is new SYSTICK_CALIB.FN.B (SYSTICK_CALIB.SKEW);
   function TENMS is new SYSTICK_CALIB.FN.B (SYSTICK_CALIB.TENMS);

   --  Functions

   function  "+"   is new SYSTICK_CALIB.FN.Add;
   function  "+"   is new SYSTICK_CALIB.FN.Add_RM;
   function  "-"   is new SYSTICK_CALIB.FN.Clear;
   function  Init  is new SYSTICK_CALIB.FN.Init;

   --  Constant definitions

   function Not_Provided is new
     SYSTICK_CALIB.FN.C (SYSTICK_CALIB.NOREF, 2#1#);
   function Not_Exactly_10mS is new
     SYSTICK_CALIB.FN.C (SYSTICK_CALIB.SKEW, 2#1#);

   --------------------
   --  ISER Register --
   --------------------

   type ISER_T is array (0 .. 239) of Boolean;
   pragma Pack (ISER_T);
   for ISER_T'Size use 240;
   --  Type definition of hardware register: ISER

   --------------------
   --  ICER Register --
   --------------------

   type ICER_T is array (0 .. 239) of Boolean;
   pragma Pack (ICER_T);
   for ICER_T'Size use 240;
   --  Type definition of hardware register: ICER

   --------------------
   --  ISPR Register --
   --------------------

   type ISPR_T is array (0 .. 239) of Boolean;
   pragma Pack (ISPR_T);
   for ISPR_T'Size use 240;
   --  Type definition of hardware register: ISPR

   --------------------
   --  ICPR Register --
   --------------------

   type ICPR_T is array (0 .. 239) of Boolean;
   pragma Pack (ICPR_T);
   for ICPR_T'Size use 240;
   --  Type definition of hardware register: ICPR

   --------------------
   --  IABR Register --
   --------------------

   type IABR_T is array (0 .. 239) of Boolean;
   pragma Pack (IABR_T);
   for IABR_T'Size use 240;
   --  Type definition of hardware register: IABR

   ------------------
   --  IP Register --
   ------------------

   subtype PRIORITY_T is R8;

   type IP_T is array (0 .. 239) of PRIORITY_T;
   pragma Pack (IP_T);
   for IP_T'Size use 240 * 8;
   --  Type definition of hardware register: IP

   ---------------------
   --  CPUID Register --
   ---------------------

   package CPUID is

      package Tp is new Types (R32);

      package IMPLEMENTER is new Bitfield (Tp, 24, 8);
      package VARIANT     is new Bitfield (Tp, 20, 4);
      package PARTNO      is new Bitfield (Tp, 4, 12);
      package REVISION    is new Bitfield (Tp, 0, 4);

      type T is
         record
            IMPLEMENTER : CPUID.IMPLEMENTER.T;
            VARIANT     : CPUID.VARIANT.T;
            PARTNO      : CPUID.PARTNO.T;
            REVISION    : CPUID.REVISION.T;
         end record;

      for T use
         record
            IMPLEMENTER at 0 range IMPLEMENTER.R'First .. IMPLEMENTER.R'Last;
            VARIANT     at 0 range VARIANT.R'First .. VARIANT.R'Last;
            PARTNO      at 0 range PARTNO.R'First .. PARTNO.R'Last;
            REVISION    at 0 range REVISION.R'First .. REVISION.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CPUID;

   subtype CPUID_T is CPUID.T;

   --  Field definitions

   function IMPLEMENTER is new CPUID.FN.B (CPUID.IMPLEMENTER);
   function VARIANT     is new CPUID.FN.B (CPUID.VARIANT);
   function PARTNO      is new CPUID.FN.B (CPUID.PARTNO);
   function REVISION    is new CPUID.FN.B (CPUID.REVISION);

   --  Functions

   function  "+"   is new CPUID.FN.Add;
   function  "+"   is new CPUID.FN.Add_RM;
   function  "-"   is new CPUID.FN.Clear;
   function  Init  is new CPUID.FN.Init;

   --------------------
   --  ICSR Register --
   --------------------

   package ICSR is

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
            NMIPENDSET  : ICSR.NMIPENDSET.T;
            PENDSVSET   : ICSR.PENDSVSET.T;
            PENDSVCLR   : ICSR.PENDSVCLR.T;
            PENDSTSET   : ICSR.PENDSTSET.T;
            PENDSTCLR   : ICSR.PENDSTCLR.T;
            ISRPREEMPT  : ICSR.ISRPREEMPT.T;
            ISRPENDING  : ICSR.ISRPENDING.T;
            VECTPENDING : ICSR.VECTPENDING.T;
            RETTOBASE   : ICSR.RETTOBASE.T;
            VECTACTIVE  : ICSR.VECTACTIVE.T;
         end record;

      for T use
         record
            NMIPENDSET  at 0 range NMIPENDSET.R'First .. NMIPENDSET.R'Last;
            PENDSVSET   at 0 range PENDSVSET.R'First .. PENDSVSET.R'Last;
            PENDSVCLR   at 0 range PENDSVCLR.R'First .. PENDSVCLR.R'Last;
            PENDSTSET   at 0 range PENDSTSET.R'First .. PENDSTSET.R'Last;
            PENDSTCLR   at 0 range PENDSTCLR.R'First .. PENDSTCLR.R'Last;
            ISRPREEMPT  at 0 range ISRPREEMPT.R'First .. ISRPREEMPT.R'Last;
            ISRPENDING  at 0 range ISRPENDING.R'First .. ISRPENDING.R'Last;
            VECTPENDING at 0 range VECTPENDING.R'First .. VECTPENDING.R'Last;
            RETTOBASE   at 0 range RETTOBASE.R'First .. RETTOBASE.R'Last;
            VECTACTIVE  at 0 range VECTACTIVE.R'First .. VECTACTIVE.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end ICSR;

   subtype ICSR_T is ICSR.T;

   --  Field definitions

   function NMIPENDSET  is new ICSR.FN.B (ICSR.NMIPENDSET);
   function PENDSVSET   is new ICSR.FN.B (ICSR.PENDSVSET);
   function PENDSVCLR   is new ICSR.FN.B (ICSR.PENDSVCLR);
   function PENDSTSET   is new ICSR.FN.B (ICSR.PENDSTSET);
   function PENDSTCLR   is new ICSR.FN.B (ICSR.PENDSTCLR);
   function ISRPREEMPT  is new ICSR.FN.B (ICSR.ISRPREEMPT);
   function ISRPENDING  is new ICSR.FN.B (ICSR.ISRPENDING);
   function VECTPENDING is new ICSR.FN.B (ICSR.VECTPENDING);
   function RETTOBASE   is new ICSR.FN.B (ICSR.RETTOBASE);
   function VECTACTIVE  is new ICSR.FN.B (ICSR.VECTACTIVE);

   --  Functions

   function  "+"   is new ICSR.FN.Add;
   function  "+"   is new ICSR.FN.Add_RM;
   function  "-"   is new ICSR.FN.Clear;
   function  Init  is new ICSR.FN.Init;

   --------------------
   --  VTOR Register --
   --------------------

   package VTOR is

      package Tp is new Types (R32);

      package TBLBASE is new Bitfield (Tp, 29);
      package TBLOFF  is new Bitfield (Tp, 7, 22);

      type T is
         record
            TBLBASE : VTOR.TBLBASE.T;
            TBLOFF  : VTOR.TBLOFF.T;
         end record;

      for T use
         record
            TBLBASE at 0 range TBLBASE.R'First .. TBLBASE.R'Last;
            TBLOFF  at 0 range TBLOFF.R'First .. TBLOFF.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end VTOR;

   subtype VTOR_T is VTOR.T;

   --  Field definitions

   function TBLBASE is new VTOR.FN.B (VTOR.TBLBASE);
   function TBLOFF  is new VTOR.FN.B (VTOR.TBLOFF);

   --  Functions

   function  "+"   is new VTOR.FN.Add;
   function  "+"   is new VTOR.FN.Add_RM;
   function  "-"   is new VTOR.FN.Clear;
   function  Init  is new VTOR.FN.Init;

   --  Constant definitions

   function Table_Base_Is_In_Code is new VTOR.FN.C (VTOR.TBLBASE, 2#0#);
   function Table_Base_Is_In_RAM  is new VTOR.FN.C (VTOR.TBLBASE, 2#1#);

   ---------------------
   --  AIRCR Register --
   ---------------------

   package AIRCR is

      package Tp is new Types (R32);

      package VECTKEY       is new Bitfield (Tp, 16, 16);
      package ENDIANESS     is new Bitfield (Tp, 15);
      package PRIGROUP      is new Bitfield (Tp, 8, 3);
      package SYSRESETREQ   is new Bitfield (Tp, 2);
      package VECTCLRACTIVE is new Bitfield (Tp, 1);
      package VECTRESET     is new Bitfield (Tp, 0);

      type T is
         record
            VECTKEY       : AIRCR.VECTKEY.T;
            ENDIANESS     : AIRCR.ENDIANESS.T;
            PRIGROUP      : AIRCR.PRIGROUP.T;
            SYSRESETREQ   : AIRCR.SYSRESETREQ.T;
            VECTCLRACTIVE : AIRCR.VECTCLRACTIVE.T;
            VECTRESET     : AIRCR.VECTRESET.T;
         end record;

      for T use
         record
            VECTKEY       at 0 range VECTKEY.R'First .. VECTKEY.R'Last;
            ENDIANESS     at 0 range ENDIANESS.R'First .. ENDIANESS.R'Last;
            PRIGROUP      at 0 range PRIGROUP.R'First .. PRIGROUP.R'Last;
            SYSRESETREQ
            at 0 range SYSRESETREQ.R'First .. SYSRESETREQ.R'Last;
            VECTCLRACTIVE
            at 0 range VECTCLRACTIVE.R'First .. VECTCLRACTIVE.R'Last;
            VECTRESET     at 0 range VECTRESET.R'First .. VECTRESET.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AIRCR;

   subtype AIRCR_T is AIRCR.T;

   --  Field definitions

   function VECTKEY       is new AIRCR.FN.B (AIRCR.VECTKEY);
   function ENDIANESS     is new AIRCR.FN.B (AIRCR.ENDIANESS);
   function PRIGROUP      is new AIRCR.FN.B (AIRCR.PRIGROUP);
   function SYSRESETREQ   is new AIRCR.FN.B (AIRCR.SYSRESETREQ);
   function VECTCLRACTIVE is new AIRCR.FN.B (AIRCR.VECTCLRACTIVE);
   function VECTRESET     is new AIRCR.FN.B (AIRCR.VECTRESET);

   --  Functions

   function  "+"   is new AIRCR.FN.Add;
   function  "+"   is new AIRCR.FN.Add_RM;
   function  "-"   is new AIRCR.FN.Clear;
   function  Init  is new AIRCR.FN.Init;

   --  Constant definitions

   function Register_Key  is new AIRCR.FN.C (AIRCR.VECTKEY, 16#05FA#);
   function Little_Endian is new AIRCR.FN.C (AIRCR.ENDIANESS, 2#0#);
   function Big_Endian    is new AIRCR.FN.C (AIRCR.ENDIANESS, 2#1#);
   function Prigroup_7_1  is new AIRCR.FN.C (AIRCR.PRIGROUP, 2#000#);
   function Prigroup_6_2  is new AIRCR.FN.C (AIRCR.PRIGROUP, 2#001#);
   function Prigroup_5_3  is new AIRCR.FN.C (AIRCR.PRIGROUP, 2#010#);
   function Prigroup_4_4  is new AIRCR.FN.C (AIRCR.PRIGROUP, 2#011#);
   function Prigroup_3_5  is new AIRCR.FN.C (AIRCR.PRIGROUP, 2#100#);
   function Prigroup_2_6  is new AIRCR.FN.C (AIRCR.PRIGROUP, 2#101#);
   function Prigroup_1_7  is new AIRCR.FN.C (AIRCR.PRIGROUP, 2#110#);
   function Prigroup_0_8  is new AIRCR.FN.C (AIRCR.PRIGROUP, 2#111#);

   ---------------------
   --  SCR Register --
   ---------------------

   package SCR is

      package Tp is new Types (R32);

      package SEVONPEND   is new Bitfield (Tp, 4);
      package SLEEPDEEP   is new Bitfield (Tp, 2);
      package SLEEPONEXIT is new Bitfield (Tp, 1);

      type T is
         record
            SEVONPEND   : SCR.SEVONPEND.T;
            SLEEPDEEP   : SCR.SLEEPDEEP.T;
            SLEEPONEXIT : SCR.SLEEPONEXIT.T;
         end record;

      for T use
         record
            SEVONPEND   at 0 range SEVONPEND.R'First .. SEVONPEND.R'Last;
            SLEEPDEEP   at 0 range SLEEPDEEP.R'First .. SLEEPDEEP.R'Last;
            SLEEPONEXIT at 0 range SLEEPONEXIT.R'First .. SLEEPONEXIT.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end SCR;

   subtype SCR_T is SCR.T;

   --  Field definitions

   function SEVONPEND   is new SCR.FN.B (SCR.SEVONPEND);
   function SLEEPDEEP   is new SCR.FN.B (SCR.SLEEPDEEP);
   function SLEEPONEXIT is new SCR.FN.B (SCR.SLEEPONEXIT);

   --  Functions

   function  "+"   is new SCR.FN.Add;
   function  "+"   is new SCR.FN.Add_RM;
   function  "-"   is new SCR.FN.Clear;
   function  Init  is new SCR.FN.Init;

   -------------------
   --  CCR Register --
   -------------------

   package CCR is

      package Tp is new Types (R32);

      package STKALIGN        is new Bitfield (Tp, 9);
      package BFHFNMIGN       is new Bitfield (Tp, 8);
      package DIV_0_TRP       is new Bitfield (Tp, 4);
      package UNALIGN_TRP     is new Bitfield (Tp, 3);
      package USERSETMPEND    is new Bitfield (Tp, 1);
      package NONEBASETHRDENA is new Bitfield (Tp, 0);

      type T is
         record
            STKALIGN        : CCR.STKALIGN.T;
            BFHFNMIGN       : CCR.BFHFNMIGN.T;
            DIV_0_TRP       : CCR.DIV_0_TRP.T;
            UNALIGN_TRP     : CCR.UNALIGN_TRP.T;
            USERSETMPEND    : CCR.USERSETMPEND.T;
            NONEBASETHRDENA : CCR.NONEBASETHRDENA.T;
         end record;

      for T use
         record
            STKALIGN
            at 0 range STKALIGN.R'First .. STKALIGN.R'Last;
            BFHFNMIGN
            at 0 range BFHFNMIGN.R'First .. BFHFNMIGN.R'Last;
            DIV_0_TRP
            at 0 range DIV_0_TRP.R'First .. DIV_0_TRP.R'Last;
            UNALIGN_TRP
            at 0 range UNALIGN_TRP.R'First .. UNALIGN_TRP.R'Last;
            USERSETMPEND
            at 0 range USERSETMPEND.R'First .. USERSETMPEND.R'Last;
            NONEBASETHRDENA
            at 0 range NONEBASETHRDENA.R'First .. NONEBASETHRDENA.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CCR;

   subtype CCR_T is CCR.T;

   --  Field definitions

   function STKALIGN        is new CCR.FN.B (CCR.STKALIGN);
   function BFHFNMIGN       is new CCR.FN.B (CCR.BFHFNMIGN);
   function DIV_0_TRP       is new CCR.FN.B (CCR.DIV_0_TRP);
   function UNALIGN_TRP     is new CCR.FN.B (CCR.UNALIGN_TRP);
   function USERSETMPEND    is new CCR.FN.B (CCR.USERSETMPEND);
   function NONEBASETHRDENA is new CCR.FN.B (CCR.NONEBASETHRDENA);

   --  Functions

   function  "+"   is new CCR.FN.Add;
   function  "+"   is new CCR.FN.Add_RM;
   function  "-"   is new CCR.FN.Clear;
   function  Init  is new CCR.FN.Init;

   -------------------
   --  SHP Register --
   -------------------

   type System_Handler is (MemManage,    BusFault,  UsageFault, Reserved1,
                           Reserved2,    Reserved3, Reserved4,  SVC,
                           DebugMonitor, Reserved5, PendSV,     SysTick);

   type SHP_T is array (System_Handler) of PRIORITY_T;
   pragma Pack (SHP_T);
   for SHP_T'Size use 12 * 8;
   --  Type definition of hardware register: SHP

   ---------------------
   --  SHCSR Register --
   ---------------------

   package SHCSR is

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
            USGFAULTENA    : SHCSR.USGFAULTENA.T;
            BUSFAULTENA    : SHCSR.BUSFAULTENA.T;
            MEMFAULTENA    : SHCSR.MEMFAULTENA.T;
            SVCALLPENDED   : SHCSR.SVCALLPENDED.T;
            BUSFAULTPENDED : SHCSR.BUSFAULTPENDED.T;
            MEMFAULTPENDED : SHCSR.MEMFAULTPENDED.T;
            USGFAULTPENDED : SHCSR.USGFAULTPENDED.T;
            SYSTICKACT     : SHCSR.SYSTICKACT.T;
            PENDSVACT      : SHCSR.PENDSVACT.T;
            MONITORACT     : SHCSR.MONITORACT.T;
            SVCALLACT      : SHCSR.SVCALLACT.T;
            USGFAULTACT    : SHCSR.USGFAULTACT.T;
            BUSFAULTACT    : SHCSR.BUSFAULTACT.T;
            MEMFAULTACT    : SHCSR.MEMFAULTACT.T;
         end record;

      for T use
         record
            USGFAULTENA
            at 0 range USGFAULTENA.R'First .. USGFAULTENA.R'Last;
            BUSFAULTENA
            at 0 range BUSFAULTENA.R'First .. BUSFAULTENA.R'Last;
            MEMFAULTENA
            at 0 range MEMFAULTENA.R'First .. MEMFAULTENA.R'Last;
            SVCALLPENDED
            at 0 range SVCALLPENDED.R'First .. SVCALLPENDED.R'Last;
            BUSFAULTPENDED
            at 0 range BUSFAULTPENDED.R'First .. BUSFAULTPENDED.R'Last;
            MEMFAULTPENDED
            at 0 range MEMFAULTPENDED.R'First .. MEMFAULTPENDED.R'Last;
            USGFAULTPENDED
            at 0 range USGFAULTPENDED.R'First .. USGFAULTPENDED.R'Last;
            SYSTICKACT
            at 0 range SYSTICKACT.R'First .. SYSTICKACT.R'Last;
            PENDSVACT
            at 0 range PENDSVACT.R'First .. PENDSVACT.R'Last;
            MONITORACT
            at 0 range MONITORACT.R'First .. MONITORACT.R'Last;
            SVCALLACT
            at 0 range SVCALLACT.R'First .. SVCALLACT.R'Last;
            USGFAULTACT
            at 0 range USGFAULTACT.R'First .. USGFAULTACT.R'Last;
            BUSFAULTACT
            at 0 range BUSFAULTACT.R'First .. BUSFAULTACT.R'Last;
            MEMFAULTACT
            at 0 range MEMFAULTACT.R'First .. MEMFAULTACT.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end SHCSR;

   subtype SHCSR_T is SHCSR.T;

   --  Field definitions

   function USGFAULTENA    is new SHCSR.FN.B (SHCSR.USGFAULTENA);
   function BUSFAULTENA    is new SHCSR.FN.B (SHCSR.BUSFAULTENA);
   function MEMFAULTENA    is new SHCSR.FN.B (SHCSR.MEMFAULTENA);
   function SVCALLPENDED   is new SHCSR.FN.B (SHCSR.SVCALLPENDED);
   function BUSFAULTPENDED is new SHCSR.FN.B (SHCSR.BUSFAULTPENDED);
   function MEMFAULTPENDED is new SHCSR.FN.B (SHCSR.MEMFAULTPENDED);
   function USGFAULTPENDED is new SHCSR.FN.B (SHCSR.USGFAULTPENDED);
   function SYSTICKACT     is new SHCSR.FN.B (SHCSR.SYSTICKACT);
   function PENDSVACT      is new SHCSR.FN.B (SHCSR.PENDSVACT);
   function MONITORACT     is new SHCSR.FN.B (SHCSR.MONITORACT);
   function SVCALLACT      is new SHCSR.FN.B (SHCSR.SVCALLACT);
   function USGFAULTACT    is new SHCSR.FN.B (SHCSR.USGFAULTACT);
   function BUSFAULTACT    is new SHCSR.FN.B (SHCSR.BUSFAULTACT);
   function MEMFAULTACT    is new SHCSR.FN.B (SHCSR.MEMFAULTACT);

   --  Functions

   function  "+"   is new SHCSR.FN.Add;
   function  "+"   is new SHCSR.FN.Add_RM;
   function  "-"   is new SHCSR.FN.Clear;
   function  Init  is new SHCSR.FN.Init;

   ---------------------
   --  CFSR Register --
   ---------------------

   package CFSR is

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
            DIVBYZERO   : CFSR.DIVBYZERO.T;
            UNALIGNED   : CFSR.UNALIGNED.T;
            NOCP        : CFSR.NOCP.T;
            INVPC       : CFSR.INVPC.T;
            INVSTATE    : CFSR.INVSTATE.T;
            UNDEFINSTR  : CFSR.UNDEFINSTR.T;
            BFARVALID   : CFSR.BFARVALID.T;
            STKERR      : CFSR.STKERR.T;
            UNSTKERR    : CFSR.UNSTKERR.T;
            IMPRECISERR : CFSR.IMPRECISERR.T;
            PRECISERR   : CFSR.PRECISERR.T;
            IBUSERR     : CFSR.IBUSERR.T;
            MMARVALID   : CFSR.MMARVALID.T;
            MSTKERR     : CFSR.MSTKERR.T;
            MUNSTKERR   : CFSR.MUNSTKERR.T;
            DACCVIOL    : CFSR.DACCVIOL.T;
            IACCVIOL    : CFSR.IACCVIOL.T;
         end record;

      for T use
         record
            DIVBYZERO   at 0 range DIVBYZERO.R'First .. DIVBYZERO.R'Last;
            UNALIGNED   at 0 range UNALIGNED.R'First .. UNALIGNED.R'Last;
            NOCP        at 0 range NOCP.R'First .. NOCP.R'Last;
            INVPC       at 0 range INVPC.R'First .. INVPC.R'Last;
            INVSTATE    at 0 range INVSTATE.R'First .. INVSTATE.R'Last;
            UNDEFINSTR  at 0 range UNDEFINSTR.R'First .. UNDEFINSTR .R'Last;
            BFARVALID   at 0 range BFARVALID.R'First .. BFARVALID.R'Last;
            STKERR      at 0 range STKERR.R'First .. STKERR.R'Last;
            UNSTKERR    at 0 range UNSTKERR.R'First .. UNSTKERR.R'Last;
            IMPRECISERR at 0 range IMPRECISERR.R'First .. IMPRECISERR.R'Last;
            PRECISERR   at 0 range PRECISERR.R'First .. PRECISERR.R'Last;
            IBUSERR     at 0 range IBUSERR.R'First .. IBUSERR.R'Last;
            MMARVALID   at 0 range MMARVALID.R'First .. MMARVALID.R'Last;
            MSTKERR     at 0 range MSTKERR.R'First .. MSTKERR.R'Last;
            MUNSTKERR   at 0 range MUNSTKERR.R'First .. MUNSTKERR.R'Last;
            DACCVIOL    at 0 range DACCVIOL.R'First .. DACCVIOL.R'Last;
            IACCVIOL    at 0 range IACCVIOL.R'First .. IACCVIOL.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CFSR;

   subtype CFSR_T is CFSR.T;

   --  Field definitions

   function DIVBYZERO   is new CFSR.FN.B (CFSR.DIVBYZERO);
   function UNALIGNED   is new CFSR.FN.B (CFSR.UNALIGNED);
   function NOCP        is new CFSR.FN.B (CFSR.NOCP);
   function INVPC       is new CFSR.FN.B (CFSR.INVPC);
   function INVSTATE    is new CFSR.FN.B (CFSR.INVSTATE);
   function UNDEFINSTR  is new CFSR.FN.B (CFSR.UNDEFINSTR);
   function BFARVALID   is new CFSR.FN.B (CFSR.BFARVALID);
   function STKERR      is new CFSR.FN.B (CFSR.STKERR);
   function UNSTKERR    is new CFSR.FN.B (CFSR.UNSTKERR);
   function IMPRECISERR is new CFSR.FN.B (CFSR.IMPRECISERR);
   function PRECISERR   is new CFSR.FN.B (CFSR.PRECISERR);
   function IBUSERR     is new CFSR.FN.B (CFSR.IBUSERR);
   function MMARVALID   is new CFSR.FN.B (CFSR.MMARVALID);
   function MSTKERR     is new CFSR.FN.B (CFSR.MSTKERR);
   function MUNSTKERR   is new CFSR.FN.B (CFSR.MUNSTKERR);
   function DACCVIOL    is new CFSR.FN.B (CFSR.DACCVIOL);
   function IACCVIOL    is new CFSR.FN.B (CFSR.IACCVIOL);

   --  Functions

   function  "+"   is new CFSR.FN.Add;
   function  "+"   is new CFSR.FN.Add_RM;
   function  "-"   is new CFSR.FN.Clear;
   function  Init  is new CFSR.FN.Init;

   ---------------------
   --  HFSR Register --
   ---------------------

   package HFSR is

      package Tp is new Types (R32);

      package DEBUGEVT is new Bitfield (Tp, 31);
      package FORCED   is new Bitfield (Tp, 30);
      package VECTTBL  is new Bitfield (Tp, 1);

      type T is
         record
            DEBUGEVT : HFSR.DEBUGEVT.T;
            FORCED   : HFSR.FORCED.T;
            VECTTBL  : HFSR.VECTTBL.T;
         end record;

      for T use
         record
            DEBUGEVT at 0 range DEBUGEVT.R'First .. DEBUGEVT.R'Last;
            FORCED   at 0 range FORCED.R'First .. FORCED.R'Last;
            VECTTBL  at 0 range VECTTBL.R'First .. VECTTBL.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end HFSR;

   subtype HFSR_T is HFSR.T;

   --  Field definitions

   function DEBUGEVT is new HFSR.FN.B (HFSR.DEBUGEVT);
   function FORCED   is new HFSR.FN.B (HFSR.FORCED);
   function VECTTBL  is new HFSR.FN.B (HFSR.VECTTBL);

   --  Functions

   function  "+"   is new HFSR.FN.Add;
   function  "+"   is new HFSR.FN.Add_RM;
   function  "-"   is new HFSR.FN.Clear;
   function  Init  is new HFSR.FN.Init;

   ---------------------
   --  DFSR Register --
   ---------------------

   package DFSR is

      package Tp is new Types (R32);

      package EXTERNAL is new Bitfield (Tp, 4);
      package VCATCH   is new Bitfield (Tp, 3);
      package DWTTRAP  is new Bitfield (Tp, 2);
      package BKPT     is new Bitfield (Tp, 1);
      package HALTED   is new Bitfield (Tp, 0);

      type T is
         record
            EXTERNAL : DFSR.EXTERNAL.T;
            VCATCH   : DFSR.VCATCH.T;
            DWTTRAP  : DFSR.DWTTRAP.T;
            BKPT     : DFSR.BKPT.T;
            HALTED   : DFSR.HALTED.T;
         end record;

      for T use
         record
            EXTERNAL at 0 range EXTERNAL.R'First .. EXTERNAL.R'Last;
            VCATCH   at 0 range VCATCH.R'First .. VCATCH.R'Last;
            DWTTRAP  at 0 range DWTTRAP.R'First .. DWTTRAP.R'Last;
            BKPT     at 0 range BKPT.R'First .. BKPT.R'Last;
            HALTED   at 0 range HALTED.R'First .. HALTED.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end DFSR;

   subtype DFSR_T is DFSR.T;

   --  Field definitions

   function EXTERNAL is new DFSR.FN.B (DFSR.EXTERNAL);
   function VCATCH   is new DFSR.FN.B (DFSR.VCATCH);
   function DWTTRAP  is new DFSR.FN.B (DFSR.DWTTRAP);
   function BKPT     is new DFSR.FN.B (DFSR.BKPT);
   function HALTED   is new DFSR.FN.B (DFSR.HALTED);

   --  Functions

   function  "+"   is new DFSR.FN.Add;
   function  "+"   is new DFSR.FN.Add_RM;
   function  "-"   is new DFSR.FN.Clear;
   function  Init  is new DFSR.FN.Init;

   --------------------
   --  PFR0 Register --
   --------------------

   subtype PFR0_T is R32;
   pragma Suppress_Initialization (PFR0_T);
   --  Type definition of hardware register: PFR0

   --------------------
   --  PFR1 Register --
   --------------------

   subtype PFR1_T is R32;
   pragma Suppress_Initialization (PFR1_T);
   --  Type definition of hardware register: PFR1

   --------------------
   --  DFR0 Register --
   --------------------

   subtype DFR0_T is R32;
   pragma Suppress_Initialization (DFR0_T);
   --  Type definition of hardware register: DFR0

   --------------------
   --  AFR0 Register --
   --------------------

   subtype AFR0_T is R32;
   pragma Suppress_Initialization (AFR0_T);
   --  Type definition of hardware register: AFR0

   ---------------------
   --  MMFR0 Register --
   ---------------------

   subtype MMFR0_T is R32;
   pragma Suppress_Initialization (MMFR0_T);
   --  Type definition of hardware register: MMFR0

   ---------------------
   --  MMFR1 Register --
   ---------------------

   subtype MMFR1_T is R32;
   pragma Suppress_Initialization (MMFR1_T);
   --  Type definition of hardware register: MMFR1

   ---------------------
   --  MMFR2 Register --
   ---------------------

   subtype MMFR2_T is R32;
   pragma Suppress_Initialization (MMFR2_T);
   --  Type definition of hardware register: MMFR2

   ---------------------
   --  MMFR3 Register --
   ---------------------

   subtype MMFR3_T is R32;
   pragma Suppress_Initialization (MMFR3_T);
   --  Type definition of hardware register: MMFR3

   ---------------------
   --  ISAR0 Register --
   ---------------------

   subtype ISAR0_T is R32;
   pragma Suppress_Initialization (ISAR0_T);
   --  Type definition of hardware register: ISAR0

   ---------------------
   --  ISAR1 Register --
   ---------------------

   subtype ISAR1_T is R32;
   pragma Suppress_Initialization (ISAR1_T);
   --  Type definition of hardware register: ISAR1

   ---------------------
   --  ISAR2 Register --
   ---------------------

   subtype ISAR2_T is R32;
   pragma Suppress_Initialization (ISAR2_T);
   --  Type definition of hardware register: ISAR2

   ---------------------
   --  ISAR3 Register --
   ---------------------

   subtype ISAR3_T is R32;
   pragma Suppress_Initialization (ISAR3_T);
   --  Type definition of hardware register: ISAR3

   ---------------------
   --  ISAR4 Register --
   ---------------------

   subtype ISAR4_T is R32;
   pragma Suppress_Initialization (ISAR4_T);
   --  Type definition of hardware register: ISAR4

   ---------------------
   --  MMFAR Register --
   ---------------------

   subtype MMFAR_T is R32;
   pragma Suppress_Initialization (MMFAR_T);
   --  Type definition of hardware register: MMFAR

   --------------------
   --  BFAR Register --
   --------------------

   subtype BFAR_T is R32;
   pragma Suppress_Initialization (BFAR_T);
   --  Type definition of hardware register: BFAR

   ---------------------
   --  AFSR Register --
   ---------------------

   package AFSR is

      package Tp is new Types (R32);

      package IMPDEF is new Bitfield (Tp, 0, 32);

      type T is
         record
            IMPDEF : AFSR.IMPDEF.T;
         end record;

      for T use
         record
            IMPDEF at 0 range IMPDEF.R'First .. IMPDEF.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AFSR;

   subtype AFSR_T is AFSR.T;

   --  Field definitions

   function IMPDEF is new AFSR.FN.B (AFSR.IMPDEF);

   --  Functions

   function  "+"   is new AFSR.FN.Add;
   function  "+"   is new AFSR.FN.Add_RM;
   function  "-"   is new AFSR.FN.Clear;
   function  Init  is new AFSR.FN.Init;

   ---------------------
   --  STIR Register --
   ---------------------

   package STIR is

      package Tp is new Types (R32);

      package INTID is new Bitfield (Tp, 0, 9);

      type T is
         record
            INTID : STIR.INTID.T;
         end record;

      for T use
         record
            INTID at 0 range INTID.R'First .. INTID.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end STIR;

   subtype STIR_T is STIR.T;

   --  Field definitions

   function INTID is new STIR.FN.B (STIR.INTID);

   --  Functions

   function  "+"   is new STIR.FN.Add;
   function  "+"   is new STIR.FN.Add_RM;
   function  "-"   is new STIR.FN.Clear;
   function  Init  is new STIR.FN.Init;

   type NVIC_T is
      record
         ICTR          : ICTR_T;
         SYSTICK_CTRL  : SYSTICK_CTRL_T;
         SYSTICK_LOAD  : SYSTICK_LOAD_T;
         SYSTICK_VAL   : SYSTICK_VAL_T;
         SYSTICK_CALIB : SYSTICK_CALIB_T;
         ISER          : ISER_T;
         ICER          : ICER_T;
         ISPR          : ISPR_T;
         ICPR          : ICPR_T;
         IABR          : IABR_T;
         IP            : IP_T;
         CPUID         : CPUID_T;
         ICSR          : ICSR_T;
         VTOR          : VTOR_T;
         AIRCR         : AIRCR_T;
         SCR           : SCR_T;
         CCR           : CCR_T;
         SHP           : SHP_T;
         SHCSR         : SHCSR_T;
         CFSR          : CFSR_T;
         HFSR          : HFSR_T;
         DFSR          : DFSR_T;
         MMFAR         : MMFAR_T;
         BFAR          : BFAR_T;
         AFSR          : AFSR_T;
         PFR0          : PFR0_T;
         PFR1          : PFR1_T;
         DFR0          : DFR0_T;
         AFR0          : AFR0_T;
         MMFR0         : MMFR0_T;
         MMFR1         : MMFR1_T;
         MMFR2         : MMFR2_T;
         MMFR3         : MMFR3_T;
         ISAR0         : ISAR0_T;
         ISAR1         : ISAR1_T;
         ISAR2         : ISAR2_T;
         ISAR3         : ISAR3_T;
         ISAR4         : ISAR4_T;
         STIR          : STIR_T;
         pragma Volatile (ICTR);
         pragma Volatile (SYSTICK_CTRL);
         pragma Volatile (SYSTICK_LOAD);
         pragma Volatile (SYSTICK_VAL);
         pragma Volatile (SYSTICK_CALIB);
         pragma Volatile (ISER);
         pragma Volatile (ICER);
         pragma Volatile (ISPR);
         pragma Volatile (ICPR);
         pragma Volatile (IABR);
         pragma Volatile (IP);
         pragma Volatile (CPUID);
         pragma Volatile (ICSR);
         pragma Volatile (VTOR);
         pragma Volatile (AIRCR);
         pragma Volatile (SCR);
         pragma Volatile (CCR);
         pragma Volatile (SHP);
         pragma Volatile (SHCSR);
         pragma Volatile (CFSR);
         pragma Volatile (HFSR);
         pragma Volatile (DFSR);
         pragma Volatile (MMFAR);
         pragma Volatile (BFAR);
         pragma Volatile (AFSR);
         pragma Volatile (PFR0);
         pragma Volatile (PFR1);
         pragma Volatile (DFR0);
         pragma Volatile (AFR0);
         pragma Volatile (MMFR0);
         pragma Volatile (MMFR1);
         pragma Volatile (MMFR2);
         pragma Volatile (MMFR3);
         pragma Volatile (ISAR0);
         pragma Volatile (ISAR1);
         pragma Volatile (ISAR2);
         pragma Volatile (ISAR3);
         pragma Volatile (ISAR4);
         pragma Volatile (STIR);
      end record;

   for NVIC_T use
      record
         ICTR          at 16#004# range 0 .. 31;
         SYSTICK_CTRL  at 16#010# range 0 .. 31;
         SYSTICK_LOAD  at 16#014# range 0 .. 31;
         SYSTICK_VAL   at 16#018# range 0 .. 31;
         SYSTICK_CALIB at 16#01C# range 0 .. 31;
         ISER          at 16#100# range 0 .. 239;
         ICER          at 16#180# range 0 .. 239;
         ISPR          at 16#200# range 0 .. 239;
         ICPR          at 16#280# range 0 .. 239;
         IABR          at 16#300# range 0 .. 239;
         IP            at 16#400# range 0 .. 240 * 8 - 1;
         CPUID         at 16#D00# range 0 .. 31;
         ICSR          at 16#D04# range 0 .. 31;
         VTOR          at 16#D08# range 0 .. 31;
         AIRCR         at 16#D0C# range 0 .. 31;
         SCR           at 16#D10# range 0 .. 31;
         CCR           at 16#D14# range 0 .. 31;
         SHP           at 16#D18# range 0 .. 12 * 8 - 1;
         SHCSR         at 16#D24# range 0 .. 31;
         CFSR          at 16#D28# range 0 .. 31;
         HFSR          at 16#D2C# range 0 .. 31;
         DFSR          at 16#D30# range 0 .. 31;
         MMFAR         at 16#D34# range 0 .. 31;
         BFAR          at 16#D38# range 0 .. 31;
         AFSR          at 16#D3C# range 0 .. 31;
         PFR0          at 16#D40# range 0 .. 31;
         PFR1          at 16#D44# range 0 .. 31;
         DFR0          at 16#D48# range 0 .. 31;
         AFR0          at 16#D4C# range 0 .. 31;
         MMFR0         at 16#D50# range 0 .. 31;
         MMFR1         at 16#D54# range 0 .. 31;
         MMFR2         at 16#D58# range 0 .. 31;
         MMFR3         at 16#D5C# range 0 .. 31;
         ISAR0         at 16#D60# range 0 .. 31;
         ISAR1         at 16#D64# range 0 .. 31;
         ISAR2         at 16#D68# range 0 .. 31;
         ISAR3         at 16#D6C# range 0 .. 31;
         ISAR4         at 16#D70# range 0 .. 31;
         STIR          at 16#F00# range 0 .. 31;
      end record;

   NVIC : NVIC_T;

   for NVIC'Address use System'To_Address (16#E000_E000#);

end STM32.Registers.NVIC_F103VC;
