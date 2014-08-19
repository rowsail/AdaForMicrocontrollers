--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 L P C . R e g i s t e r s . T 0 _ 2 1 X X                  --
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


--  Package defines LPC21XX family Timer0 related registers

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
--                          LPC.Registers.T0_214X                         --
-----------------------------------------------------------------------------

package LPC.Registers.T0_21XX is

   pragma Preelaborate;

   ------------------
   --  IR Register --
   ------------------

   package IR is

      package Tp is new Types (R8);

      package MR0 is new Bitfield (Tp, 0);
      package MR1 is new Bitfield (Tp, 1);
      package MR2 is new Bitfield (Tp, 2);
      package MR3 is new Bitfield (Tp, 3);
      package CR0 is new Bitfield (Tp, 4);
      package CR1 is new Bitfield (Tp, 5);
      package CR2 is new Bitfield (Tp, 6);
      package CR3 is new Bitfield (Tp, 7);

      type T is
         record
            MR0 : IR.MR0.T;
            MR1 : IR.MR1.T;
            MR2 : IR.MR2.T;
            MR3 : IR.MR3.T;
            CR0 : IR.CR0.T;
            CR1 : IR.CR1.T;
            CR2 : IR.CR2.T;
            CR3 : IR.CR3.T;
         end record;

      for T use
         record
            MR0 at 0 range MR0.R'First .. MR0.R'Last;
            MR1 at 0 range MR1.R'First .. MR1.R'Last;
            MR2 at 0 range MR2.R'First .. MR2.R'Last;
            MR3 at 0 range MR3.R'First .. MR3.R'Last;
            CR0 at 0 range CR0.R'First .. CR0.R'Last;
            CR1 at 0 range CR1.R'First .. CR1.R'Last;
            CR2 at 0 range CR2.R'First .. CR2.R'Last;
            CR3 at 0 range CR3.R'First .. CR3.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end IR;

   subtype IR_T is IR.T;

   --  Field definitions

   function MR0 is new IR.FN.B (IR.MR0);
   function MR1 is new IR.FN.B (IR.MR1);
   function MR2 is new IR.FN.B (IR.MR2);
   function MR3 is new IR.FN.B (IR.MR3);
   function CR0 is new IR.FN.B (IR.CR0);
   function CR1 is new IR.FN.B (IR.CR1);
   function CR2 is new IR.FN.B (IR.CR2);
   function CR3 is new IR.FN.B (IR.CR3);

   --  Functions

   function  "+"   is new IR.FN.Add;
   function  "+"   is new IR.FN.Add_RM;
   function  "-"   is new IR.FN.Clear;
   function  Init  is new IR.FN.Init;

   --  Constant definitions

   function Is_Active is new IR.FN.C (IR.MR0, 2#1#);
   function Reset     is new IR.FN.C (IR.MR0, 2#1#);
   function Is_Active is new IR.FN.C (IR.MR1, 2#1#);
   function Reset     is new IR.FN.C (IR.MR1, 2#1#);
   function Is_Active is new IR.FN.C (IR.MR2, 2#1#);
   function Reset     is new IR.FN.C (IR.MR2, 2#1#);
   function Is_Active is new IR.FN.C (IR.MR3, 2#1#);
   function Reset     is new IR.FN.C (IR.MR3, 2#1#);
   function Is_Active is new IR.FN.C (IR.CR0, 2#1#);
   function Reset     is new IR.FN.C (IR.CR0, 2#1#);
   function Is_Active is new IR.FN.C (IR.CR1, 2#1#);
   function Reset     is new IR.FN.C (IR.CR1, 2#1#);
   function Is_Active is new IR.FN.C (IR.CR2, 2#1#);
   function Reset     is new IR.FN.C (IR.CR2, 2#1#);
   function Is_Active is new IR.FN.C (IR.CR3, 2#1#);
   function Reset     is new IR.FN.C (IR.CR3, 2#1#);

   ---------------------
   --  TCR Register --
   ---------------------

   package TCR is

      package Tp is new Types (R8);

      package CRST is new Bitfield (Tp, 1);
      package CEN  is new Bitfield (Tp, 0);

      type T is
         record
            CRST : TCR.CRST.T;
            CEN  : TCR.CEN.T;
         end record;

      for T use
         record
            CRST at 0 range CRST.R'First .. CRST.R'Last;
            CEN  at 0 range CEN.R'First .. CEN.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end TCR;

   subtype TCR_T is TCR.T;

   --  Field definitions

   function CRST is new TCR.FN.B (TCR.CRST);
   function CEN  is new TCR.FN.B (TCR.CEN);

   --  Functions

   function  "+"   is new TCR.FN.Add;
   function  "+"   is new TCR.FN.Add_RM;
   function  "-"   is new TCR.FN.Clear;
   function  Init  is new TCR.FN.Init;

   --  Constant definitions

   function Off     is new TCR.FN.C (TCR.CRST, 2#0#);
   function On      is new TCR.FN.C (TCR.CRST, 2#1#);
   function Disable is new TCR.FN.C (TCR.CEN, 2#0#);
   function Enable  is new TCR.FN.C (TCR.CEN, 2#1#);

   ------------------
   --  TC Register --
   ------------------

   subtype TC_T is R32;
   pragma Suppress_Initialization (TC_T);

   ------------------
   --  PR Register --
   ------------------

   subtype PR_T is R32;
   pragma Suppress_Initialization (PR_T);

   ------------------
   --  PC Register --
   ------------------

   subtype PC_T is R32;
   pragma Suppress_Initialization (PC_T);

   ---------------------
   --  MCR Register --
   ---------------------

   package MCR is

      package Tp is new Types (R16);

      package MR3S is new Bitfield (Tp, 11);
      package MR3R is new Bitfield (Tp, 10);
      package MR3I is new Bitfield (Tp, 9);
      package MR2S is new Bitfield (Tp, 8);
      package MR2R is new Bitfield (Tp, 7);
      package MR2I is new Bitfield (Tp, 6);
      package MR1S is new Bitfield (Tp, 5);
      package MR1R is new Bitfield (Tp, 4);
      package MR1I is new Bitfield (Tp, 3);
      package MR0S is new Bitfield (Tp, 2);
      package MR0R is new Bitfield (Tp, 1);
      package MR0I is new Bitfield (Tp, 0);

      type T is
         record
            MR3S : MCR.MR3S.T;
            MR3R : MCR.MR3R.T;
            MR3I : MCR.MR3I.T;
            MR2S : MCR.MR2S.T;
            MR2R : MCR.MR2R.T;
            MR2I : MCR.MR2I.T;
            MR1S : MCR.MR1S.T;
            MR1R : MCR.MR1R.T;
            MR1I : MCR.MR1I.T;
            MR0S : MCR.MR0S.T;
            MR0R : MCR.MR0R.T;
            MR0I : MCR.MR0I.T;
         end record;

      for T use
         record
            MR3S at 0 range MR3S.R'First .. MR3S.R'Last;
            MR3R at 0 range MR3R.R'First .. MR3R.R'Last;
            MR3I at 0 range MR3I.R'First .. MR3I.R'Last;
            MR2S at 0 range MR2S.R'First .. MR2S.R'Last;
            MR2R at 0 range MR2R.R'First .. MR2R.R'Last;
            MR2I at 0 range MR2I.R'First .. MR2I.R'Last;
            MR1S at 0 range MR1S.R'First .. MR1S.R'Last;
            MR1R at 0 range MR1R.R'First .. MR1R.R'Last;
            MR1I at 0 range MR1I.R'First .. MR1I.R'Last;
            MR0S at 0 range MR0S.R'First .. MR0S.R'Last;
            MR0R at 0 range MR0R.R'First .. MR0R.R'Last;
            MR0I at 0 range MR0I.R'First .. MR0I.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end MCR;

   subtype MCR_T is MCR.T;

   --  Field definitions

   function MR3S is new MCR.FN.B (MCR.MR3S);
   function MR3R is new MCR.FN.B (MCR.MR3R);
   function MR3I is new MCR.FN.B (MCR.MR3I);
   function MR2S is new MCR.FN.B (MCR.MR2S);
   function MR2R is new MCR.FN.B (MCR.MR2R);
   function MR2I is new MCR.FN.B (MCR.MR2I);
   function MR1S is new MCR.FN.B (MCR.MR1S);
   function MR1R is new MCR.FN.B (MCR.MR1R);
   function MR1I is new MCR.FN.B (MCR.MR1I);
   function MR0S is new MCR.FN.B (MCR.MR0S);
   function MR0R is new MCR.FN.B (MCR.MR0R);
   function MR0I is new MCR.FN.B (MCR.MR0I);

   --  Functions

   function  "+"   is new MCR.FN.Add;
   function  "+"   is new MCR.FN.Add_RM;
   function  "-"   is new MCR.FN.Clear;
   function  Init  is new MCR.FN.Init;

   --  Constant definitions

   function Off                is new MCR.FN.C (MCR.MR3S, 2#0#);
   function Stop_On_Match      is new MCR.FN.C (MCR.MR3S, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR3R, 2#0#);
   function Reset_On_Match     is new MCR.FN.C (MCR.MR3R, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR3I, 2#0#);
   function Interrupt_On_Match is new MCR.FN.C (MCR.MR3I, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR2S, 2#0#);
   function Stop_On_Match      is new MCR.FN.C (MCR.MR2S, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR2R, 2#0#);
   function Reset_On_Match     is new MCR.FN.C (MCR.MR2R, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR2I, 2#0#);
   function Interrupt_On_Match is new MCR.FN.C (MCR.MR2I, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR1S, 2#0#);
   function Stop_On_Match      is new MCR.FN.C (MCR.MR1S, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR1R, 2#0#);
   function Reset_On_Match     is new MCR.FN.C (MCR.MR1R, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR1I, 2#0#);
   function Interrupt_On_Match is new MCR.FN.C (MCR.MR1I, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR0S, 2#0#);
   function Stop_On_Match      is new MCR.FN.C (MCR.MR0S, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR0R, 2#0#);
   function Reset_On_Match     is new MCR.FN.C (MCR.MR0R, 2#1#);
   function Off                is new MCR.FN.C (MCR.MR0I, 2#0#);
   function Interrupt_On_Match is new MCR.FN.C (MCR.MR0I, 2#1#);

   -------------------
   --  MR0 Register --
   -------------------

   subtype MR0_T is R32;
   pragma Suppress_Initialization (MR0_T);

   -------------------
   --  MR1 Register --
   -------------------

   subtype MR1_T is R32;
   pragma Suppress_Initialization (MR1_T);

   -------------------
   --  MR2 Register --
   -------------------

   subtype MR2_T is R32;
   pragma Suppress_Initialization (MR2_T);

   -------------------
   --  MR3 Register --
   -------------------

   subtype MR3_T is R32;
   pragma Suppress_Initialization (MR3_T);

   ---------------------
   --  CCR Register --
   ---------------------

   package CCR is

      package Tp is new Types (R16);

      package CAP3I  is new Bitfield (Tp, 11);
      package CAP3FE is new Bitfield (Tp, 10);
      package CAP3RE is new Bitfield (Tp, 9);
      package CAP2I  is new Bitfield (Tp, 8);
      package CAP2FE is new Bitfield (Tp, 7);
      package CAP2RE is new Bitfield (Tp, 6);
      package CAP1I  is new Bitfield (Tp, 5);
      package CAP1FE is new Bitfield (Tp, 4);
      package CAP1RE is new Bitfield (Tp, 3);
      package CAP0I  is new Bitfield (Tp, 2);
      package CAP0FE is new Bitfield (Tp, 1);
      package CAP0RE is new Bitfield (Tp, 0);

      type T is
         record
            CAP3I  : CCR.CAP3I .T;
            CAP3FE : CCR.CAP3FE.T;
            CAP3RE : CCR.CAP3RE.T;
            CAP2I  : CCR.CAP2I .T;
            CAP2FE : CCR.CAP2FE.T;
            CAP2RE : CCR.CAP2RE.T;
            CAP1I  : CCR.CAP1I .T;
            CAP1FE : CCR.CAP1FE.T;
            CAP1RE : CCR.CAP1RE.T;
            CAP0I  : CCR.CAP0I .T;
            CAP0FE : CCR.CAP0FE.T;
            CAP0RE : CCR.CAP0RE.T;
         end record;

      for T use
         record
            CAP3I  at 0 range CAP3I.R'First .. CAP3I.R'Last;
            CAP3FE at 0 range CAP3FE.R'First .. CAP3FE.R'Last;
            CAP3RE at 0 range CAP3RE.R'First .. CAP3RE.R'Last;
            CAP2I  at 0 range CAP2I.R'First .. CAP2I.R'Last;
            CAP2FE at 0 range CAP2FE.R'First .. CAP2FE.R'Last;
            CAP2RE at 0 range CAP2RE.R'First .. CAP2RE.R'Last;
            CAP1I  at 0 range CAP1I.R'First .. CAP1I.R'Last;
            CAP1FE at 0 range CAP1FE.R'First .. CAP1FE.R'Last;
            CAP1RE at 0 range CAP1RE.R'First .. CAP1RE.R'Last;
            CAP0I  at 0 range CAP0I.R'First .. CAP0I.R'Last;
            CAP0FE at 0 range CAP0FE.R'First .. CAP0FE.R'Last;
            CAP0RE at 0 range CAP0RE.R'First .. CAP0RE.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CCR;

   subtype CCR_T is CCR.T;

   --  Field definitions

   function CAP3I  is new CCR.FN.B (CCR.CAP3I);
   function CAP3FE is new CCR.FN.B (CCR.CAP3FE);
   function CAP3RE is new CCR.FN.B (CCR.CAP3RE);
   function CAP2I  is new CCR.FN.B (CCR.CAP2I);
   function CAP2FE is new CCR.FN.B (CCR.CAP2FE);
   function CAP2RE is new CCR.FN.B (CCR.CAP2RE);
   function CAP1I  is new CCR.FN.B (CCR.CAP1I);
   function CAP1FE is new CCR.FN.B (CCR.CAP1FE);
   function CAP1RE is new CCR.FN.B (CCR.CAP1RE);
   function CAP0I  is new CCR.FN.B (CCR.CAP0I);
   function CAP0FE is new CCR.FN.B (CCR.CAP0FE);
   function CAP0RE is new CCR.FN.B (CCR.CAP0RE);

   --  Functions

   function  "+"   is new CCR.FN.Add;
   function  "+"   is new CCR.FN.Add_RM;
   function  "-"   is new CCR.FN.Clear;
   function  Init  is new CCR.FN.Init;

   --  Constant definitions

   function Off                     is new CCR.FN.C (CCR.CAP3I, 2#0#);
   function Interrupt_On_Capture    is new CCR.FN.C (CCR.CAP3I, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP3FE, 2#0#);
   function Capture_On_Falling_Edge is new CCR.FN.C (CCR.CAP3FE, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP3RE, 2#0#);
   function Capture_On_Rising_Edge  is new CCR.FN.C (CCR.CAP3RE, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP2I, 2#0#);
   function Interrupt_On_Capture    is new CCR.FN.C (CCR.CAP2I, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP2FE, 2#0#);
   function Capture_On_Falling_Edge is new CCR.FN.C (CCR.CAP2FE, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP2RE, 2#0#);
   function Capture_On_Rising_Edge  is new CCR.FN.C (CCR.CAP2RE, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP1I, 2#0#);
   function Interrupt_On_Capture    is new CCR.FN.C (CCR.CAP1I, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP1FE, 2#0#);
   function Capture_On_Falling_Edge is new CCR.FN.C (CCR.CAP1FE, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP1RE, 2#0#);
   function Capture_On_Rising_Edge  is new CCR.FN.C (CCR.CAP1RE, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP0I, 2#0#);
   function Interrupt_On_Capture    is new CCR.FN.C (CCR.CAP0I, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP0FE, 2#0#);
   function Capture_On_Falling_Edge is new CCR.FN.C (CCR.CAP0FE, 2#1#);
   function Off                     is new CCR.FN.C (CCR.CAP0RE, 2#0#);
   function Capture_On_Rising_Edge  is new CCR.FN.C (CCR.CAP0RE, 2#1#);

   -------------------
   --  CR0 Register --
   -------------------

   subtype CR0_T is R32;
   pragma Suppress_Initialization (CR0_T);

   -------------------
   --  CR1 Register --
   -------------------

   subtype CR1_T is R32;
   pragma Suppress_Initialization (CR1_T);

   -------------------
   --  CR2 Register --
   -------------------

   subtype CR2_T is R32;
   pragma Suppress_Initialization (CR2_T);

   -------------------
   --  CR3 Register --
   -------------------

   subtype CR3_T is R32;
   pragma Suppress_Initialization (CR3_T);

   ---------------------
   --  EMR Register --
   ---------------------

   package EMR is

      package Tp is new Types (R16);

      package EMC3 is new Bitfield (Tp, 10, 2);
      package EMC2 is new Bitfield (Tp, 8, 2);
      package EMC1 is new Bitfield (Tp, 6, 2);
      package EMC0 is new Bitfield (Tp, 4, 2);
      package EM3  is new Bitfield (Tp, 3);
      package EM2  is new Bitfield (Tp, 2);
      package EM1  is new Bitfield (Tp, 1);
      package EM0  is new Bitfield (Tp, 0);

      type T is
         record
            EMC3 : EMR.EMC3.T;
            EMC2 : EMR.EMC2.T;
            EMC1 : EMR.EMC1.T;
            EMC0 : EMR.EMC0.T;
            EM3  : EMR.EM3.T;
            EM2  : EMR.EM2.T;
            EM1  : EMR.EM1.T;
            EM0  : EMR.EM0.T;
         end record;

      for T use
         record
            EMC3 at 0 range EMC3.R'First .. EMC3.R'Last;
            EMC2 at 0 range EMC2.R'First .. EMC2.R'Last;
            EMC1 at 0 range EMC1.R'First .. EMC1.R'Last;
            EMC0 at 0 range EMC0.R'First .. EMC0.R'Last;
            EM3  at 0 range EM3.R'First .. EM3.R'Last;
            EM2  at 0 range EM2.R'First .. EM2.R'Last;
            EM1  at 0 range EM1.R'First .. EM1.R'Last;
            EM0  at 0 range EM0.R'First .. EM0.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end EMR;

   subtype EMR_T is EMR.T;

   --  Field definitions

   function EMC3 is new EMR.FN.B (EMR.EMC3);
   function EMC2 is new EMR.FN.B (EMR.EMC2);
   function EMC1 is new EMR.FN.B (EMR.EMC1);
   function EMC0 is new EMR.FN.B (EMR.EMC0);
   function EM3  is new EMR.FN.B (EMR.EM3);
   function EM2  is new EMR.FN.B (EMR.EM2);
   function EM1  is new EMR.FN.B (EMR.EM1);
   function EM0  is new EMR.FN.B (EMR.EM0);

   --  Functions

   function  "+"   is new EMR.FN.Add;
   function  "+"   is new EMR.FN.Add_RM;
   function  "-"   is new EMR.FN.Clear;
   function  Init  is new EMR.FN.Init;

   --  Constant definitions

   function Do_Nothing is new EMR.FN.C (EMR.EMC3, 2#00#);
   function Reset      is new EMR.FN.C (EMR.EMC3, 2#01#);
   function Set        is new EMR.FN.C (EMR.EMC3, 2#10#);
   function Inverse    is new EMR.FN.C (EMR.EMC3, 2#11#);
   function Do_Nothing is new EMR.FN.C (EMR.EMC2, 2#00#);
   function Reset      is new EMR.FN.C (EMR.EMC2, 2#01#);
   function Set        is new EMR.FN.C (EMR.EMC2, 2#10#);
   function Inverse    is new EMR.FN.C (EMR.EMC2, 2#11#);
   function Do_Nothing is new EMR.FN.C (EMR.EMC1, 2#00#);
   function Reset      is new EMR.FN.C (EMR.EMC1, 2#01#);
   function Set        is new EMR.FN.C (EMR.EMC1, 2#10#);
   function Inverse    is new EMR.FN.C (EMR.EMC1, 2#11#);
   function Do_Nothing is new EMR.FN.C (EMR.EMC0, 2#00#);
   function Reset      is new EMR.FN.C (EMR.EMC0, 2#01#);
   function Set        is new EMR.FN.C (EMR.EMC0, 2#10#);
   function Inverse    is new EMR.FN.C (EMR.EMC0, 2#11#);
   function Off is new EMR.FN.C (EMR.EM3, 2#0#);
   function On  is new EMR.FN.C (EMR.EM3, 2#1#);
   function Off is new EMR.FN.C (EMR.EM2, 2#0#);
   function On  is new EMR.FN.C (EMR.EM2, 2#1#);
   function Off is new EMR.FN.C (EMR.EM1, 2#0#);
   function On  is new EMR.FN.C (EMR.EM1, 2#1#);
   function Off is new EMR.FN.C (EMR.EM0, 2#0#);
   function On  is new EMR.FN.C (EMR.EM0, 2#1#);

   ---------------------
   --  CTCR Register --
   ---------------------

   package CTCR is

      package Tp is new Types (R8);

      package INPUT_SEL is new Bitfield (Tp, 2, 2);
      package C_T       is new Bitfield (Tp, 0, 2);

      type T is
         record
            INPUT_SEL : CTCR.INPUT_SEL.T;
            C_T       : CTCR.C_T.T;
         end record;

      for T use
         record
            INPUT_SEL at 0 range INPUT_SEL.R'First .. INPUT_SEL.R'Last;
            C_T       at 0 range C_T.R'First .. C_T.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CTCR;

   subtype CTCR_T is CTCR.T;

   --  Field definitions

   function INPUT_SEL is new CTCR.FN.B (CTCR.INPUT_SEL);
   function C_T       is new CTCR.FN.B (CTCR.C_T);

   --  Functions

   function  "+"   is new CTCR.FN.Add;
   function  "+"   is new CTCR.FN.Add_RM;
   function  "-"   is new CTCR.FN.Clear;
   function  Init  is new CTCR.FN.Init;

   --  Constant definitions

   function Input_CAP0 is new CTCR.FN.C (CTCR.INPUT_SEL, 2#00#);
   function Input_CAP1 is new CTCR.FN.C (CTCR.INPUT_SEL, 2#01#);
   function Input_CAP2 is new CTCR.FN.C (CTCR.INPUT_SEL, 2#10#);
   function Input_CAP3 is new CTCR.FN.C (CTCR.INPUT_SEL, 2#11#);
   function Timer_Mode                is new CTCR.FN.C (CTCR.C_T, 2#00#);
   function Counter_Mode_Rising_Edge  is new CTCR.FN.C (CTCR.C_T, 2#01#);
   function Counter_Mode_Falling_Edge is new CTCR.FN.C (CTCR.C_T, 2#10#);
   function Counter_Mode_Both_Edges   is new CTCR.FN.C (CTCR.C_T, 2#11#);

   --     T0 Registers Collection  --

   type T0_T is
      record
         IR        : IR_T;
         TCR       : TCR_T;
         TC        : TC_T;
         PR        : PR_T;
         PC        : PC_T;
         MCR       : MCR_T;
         MR0       : MR0_T;
         MR1       : MR1_T;
         MR2       : MR2_T;
         MR3       : MR3_T;
         CCR       : CCR_T;
         CR0       : CR0_T;
         CR1       : CR1_T;
         CR2       : CR2_T;
         CR3       : CR3_T;
         EMR       : EMR_T;
         CTCR      : CTCR_T;
         pragma Volatile (IR);
         pragma Volatile (TCR);
         pragma Volatile (TC);
         pragma Volatile (PR);
         pragma Volatile (PC);
         pragma Volatile (MCR);
         pragma Volatile (MR0);
         pragma Volatile (MR1);
         pragma Volatile (MR2);
         pragma Volatile (MR3);
         pragma Volatile (CCR);
         pragma Volatile (CR0);
         pragma Volatile (CR1);
         pragma Volatile (CR2);
         pragma Volatile (CR3);
         pragma Volatile (EMR);
         pragma Volatile (CTCR);
      end record;

   for T0_T use
      record
         IR    at 16#000# range 0 .. 7;
         TCR   at 16#004# range 0 .. 7;
         TC    at 16#008# range 0 .. 31;
         PR    at 16#00C# range 0 .. 31;
         PC    at 16#010# range 0 .. 31;
         MCR   at 16#014# range 0 .. 15;
         MR0   at 16#018# range 0 .. 31;
         MR1   at 16#01C# range 0 .. 31;
         MR2   at 16#020# range 0 .. 31;
         MR3   at 16#024# range 0 .. 31;
         CCR   at 16#028# range 0 .. 15;
         CR0   at 16#02C# range 0 .. 31;
         CR1   at 16#030# range 0 .. 31;
         CR2   at 16#034# range 0 .. 31;
         CR3   at 16#038# range 0 .. 31;
         EMR   at 16#03C# range 0 .. 15;
         CTCR  at 16#070# range 0 .. 7;
      end record;

   T0 : T0_T;

   for T0'Address use System'To_Address (16#E000_4000#);

end LPC.Registers.T0_21XX;
