--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 A R M . R e g i s t e r . R C C _ F 1 0 X X X              --
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

--  Package defines STM32 F10XXX family reset & clock related registers

-------------------------
--  Imported packages  --
-------------------------

with ARM.Register;
use  ARM.Register;

with ARM.Register.Types;
with ARM.Register.Bitfield;
with ARM.Register.Register;

--------------------------------------------------------------------------------
--                            ARM.Register.RCC_F10XXX                         --
--------------------------------------------------------------------------------

package ARM.Register.RCC_F10XXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                         Reset and Clock                             --
   -------------------------------------------------------------------------

   --     RCC Registers Collection  --

   ------------------------
   --  Control Register  --
   ------------------------

   package RCC_CR is

      package Tp is new Types (R32);

      package PLL3RDY is new Bitfield (Tp, 29);
      package PLL3ON  is new Bitfield (Tp, 28);
      package PLL2RDY is new Bitfield (Tp, 27);
      package PLL2ON  is new Bitfield (Tp, 26);
      package PLLRDY  is new Bitfield (Tp, 25);
      package PLLON   is new Bitfield (Tp, 24);
      package CSSON   is new Bitfield (Tp, 19);
      package HSEBYP  is new Bitfield (Tp, 18);
      package HSERDY  is new Bitfield (Tp, 17);
      package HSEON   is new Bitfield (Tp, 16);
      package HSICAL  is new Bitfield (Tp, 8, 8);
      package HSITRIM is new Bitfield (Tp, 3, 5);
      package HSIRDY  is new Bitfield (Tp, 1);
      package HSION   is new Bitfield (Tp, 0);

      --  Main register type

      type T is
         record
            PLL3RDY  : RCC_CR.PLL3RDY.T;
            PLL3ON   : RCC_CR.PLL3ON .T;
            PLL2RDY  : RCC_CR.PLL2RDY.T;
            PLL2ON   : RCC_CR.PLL2ON .T;
            PLLRDY   : RCC_CR.PLLRDY .T;
            PLLON    : RCC_CR.PLLON  .T;
            CSSON    : RCC_CR.CSSON  .T;
            HSEBYP   : RCC_CR.HSEBYP .T;
            HSERDY   : RCC_CR.HSERDY .T;
            HSEON    : RCC_CR.HSEON  .T;
            HSICAL   : RCC_CR.HSICAL .T;
            HSITRIM  : RCC_CR.HSITRIM.T;
            HSIRDY   : RCC_CR.HSIRDY .T;
            HSION    : RCC_CR.HSION  .T;
         end record;

      for T use
         record
            PLL3RDY at 0 range PLL3RDY.F .. PLL3RDY.L;
            PLL3ON  at 0 range PLL3ON .F .. PLL3ON .L;
            PLL2RDY at 0 range PLL2RDY.F .. PLL2RDY.L;
            PLL2ON  at 0 range PLL2ON .F .. PLL2ON .L;
            PLLRDY  at 0 range PLLRDY .F .. PLLRDY .L;
            PLLON   at 0 range PLLON  .F .. PLLON  .L;
            CSSON   at 0 range CSSON  .F .. CSSON  .L;
            HSEBYP  at 0 range HSEBYP .F .. HSEBYP .L;
            HSERDY  at 0 range HSERDY .F .. HSERDY .L;
            HSEON   at 0 range HSEON  .F .. HSEON  .L;
            HSICAL  at 0 range HSICAL .F .. HSICAL .L;
            HSITRIM at 0 range HSITRIM.F .. HSITRIM.L;
            HSIRDY  at 0 range HSIRDY .F .. HSIRDY .L;
            HSION   at 0 range HSION  .F .. HSION  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_CR;

   --  Types definitions

   package CR    is new Register (RCC_CR.T, RCC_CR.Tp, 16#4002_1000#);
   subtype CR_T  is CR.T;
   subtype CR_F  is CR.F;
   

   --  Field definitions

   function PLL3RDY is new CR.B (RCC_CR.PLL3RDY) with Inline_Always;
   function PLL3ON  is new CR.B (RCC_CR.PLL3ON ) with Inline_Always;
   function PLL2RDY is new CR.B (RCC_CR.PLL2RDY) with Inline_Always;
   function PLL2ON  is new CR.B (RCC_CR.PLL2ON ) with Inline_Always;
   function PLLRDY  is new CR.B (RCC_CR.PLLRDY ) with Inline_Always;
   function PLLON   is new CR.B (RCC_CR.PLLON  ) with Inline_Always;
   function CSSON   is new CR.B (RCC_CR.CSSON  ) with Inline_Always;
   function HSEBYP  is new CR.B (RCC_CR.HSEBYP ) with Inline_Always;
   function HSERDY  is new CR.B (RCC_CR.HSERDY ) with Inline_Always;
   function HSEON   is new CR.B (RCC_CR.HSEON  ) with Inline_Always;
   function HSICAL  is new CR.B (RCC_CR.HSICAL ) with Inline_Always;
   function HSITRIM is new CR.B (RCC_CR.HSITRIM) with Inline_Always;
   function HSIRDY  is new CR.B (RCC_CR.HSIRDY ) with Inline_Always;
   function HSION   is new CR.B (RCC_CR.HSION  ) with Inline_Always;

   --  Functions

   function "+"  is new CR.Add      with Inline_Always;
   function "+"  is new CR.Add_F    with Inline_Always;
   function "+"  is new CR.Add_FF   with Inline_Always;
   function "-"  is new CR.Clear    with Inline_Always;
   function "-"  is new CR.Clear_FF with Inline_Always;
   function "="  is new CR.Equal    with Inline_Always;
   function Init is new CR.Init     with Inline_Always;

   --  Constant definitions

   function Oscillator_Not_Bypassed is new CR.C (RCC_CR.HSEBYP, 2#0#) with Inline_Always;
   function Oscillator_Bypassed     is new CR.C (RCC_CR.HSEBYP, 2#1#) with Inline_Always;

   function Oscillator_Not_Ready is new CR.C (RCC_CR.HSERDY, 2#0#) with Inline_Always;
   function Oscillator_Ready     is new CR.C (RCC_CR.HSERDY, 2#1#) with Inline_Always;

   ------------------------------
   --  Configuration Register  --
   ------------------------------

   package  RCC_CFGR is

      package Tp is new Types (R32);

      package MCO      is new Bitfield (Tp, 24, 4);
      package OTGFSPRE is new Bitfield (Tp, 22);
      package PLLMUL   is new Bitfield (Tp, 18, 4);
      package PLLXTPRE is new Bitfield (Tp, 17);
      package PLLSRC   is new Bitfield (Tp, 16);
      package ADCPRE   is new Bitfield (Tp, 14, 2);
      package PPRE2    is new Bitfield (Tp, 11, 3);
      package PPRE1    is new Bitfield (Tp,  8, 3);
      package HPRE     is new Bitfield (Tp,  4, 4);
      package SWS      is new Bitfield (Tp,  2, 2);
      package SW       is new Bitfield (Tp,  0, 2);

      type T is
         record
            MCO      : RCC_CFGR.MCO     .T;
            OTGFSPRE : RCC_CFGR.OTGFSPRE.T;
            PLLMUL   : RCC_CFGR.PLLMUL  .T;
            PLLXTPRE : RCC_CFGR.PLLXTPRE.T;
            PLLSRC   : RCC_CFGR.PLLSRC  .T;
            ADCPRE   : RCC_CFGR.ADCPRE  .T;
            PPRE2    : RCC_CFGR.PPRE2   .T;
            PPRE1    : RCC_CFGR.PPRE1   .T;
            HPRE     : RCC_CFGR.HPRE    .T;
            SWS      : RCC_CFGR.SWS     .T;
            SW       : RCC_CFGR.SW      .T;
         end record;

      for T use
         record
            MCO      at 0 range MCO     .F .. MCO     .L;
            OTGFSPRE at 0 range OTGFSPRE.F .. OTGFSPRE.L;
            PLLMUL   at 0 range PLLMUL  .F .. PLLMUL  .L;
            PLLXTPRE at 0 range PLLXTPRE.F .. PLLXTPRE.L;
            PLLSRC   at 0 range PLLSRC  .F .. PLLSRC  .L;
            ADCPRE   at 0 range ADCPRE  .F .. ADCPRE  .L;
            PPRE2    at 0 range PPRE2   .F .. PPRE2   .L;
            PPRE1    at 0 range PPRE1   .F .. PPRE1   .L;
            HPRE     at 0 range HPRE    .F .. HPRE    .L;
            SWS      at 0 range SWS     .F .. SWS     .L;
            SW       at 0 range SW      .F .. SW      .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_CFGR;

   package CFGR    is new Register (RCC_CFGR.T, RCC_CFGR.Tp, 16#4002_1004#);
   subtype CFGR_T  is CFGR.T;
   subtype CFGR_F  is CFGR.F;
   

   --  Field definitions

   function MCO      is new CFGR.B (RCC_CFGR.MCO     ) with Inline_Always;
   function OTGFSPRE is new CFGR.B (RCC_CFGR.OTGFSPRE) with Inline_Always;
   function PLLMUL   is new CFGR.B (RCC_CFGR.PLLMUL  ) with Inline_Always;
   function PLLXTPRE is new CFGR.B (RCC_CFGR.PLLXTPRE) with Inline_Always;
   function PLLSRC   is new CFGR.B (RCC_CFGR.PLLSRC  ) with Inline_Always;
   function ADCPRE   is new CFGR.B (RCC_CFGR.ADCPRE  ) with Inline_Always;
   function PPRE2    is new CFGR.B (RCC_CFGR.PPRE2   ) with Inline_Always;
   function PPRE1    is new CFGR.B (RCC_CFGR.PPRE1   ) with Inline_Always;
   function HPRE     is new CFGR.B (RCC_CFGR.HPRE    ) with Inline_Always;
   function SWS      is new CFGR.B (RCC_CFGR.SWS     ) with Inline_Always;
   function SW       is new CFGR.B (RCC_CFGR.SW      ) with Inline_Always;

   --  Functions

   function "+"  is new CFGR.Add      with Inline_Always;
   function "+"  is new CFGR.Add_F    with Inline_Always;
   function "+"  is new CFGR.Add_FF   with Inline_Always;
   function "-"  is new CFGR.Clear    with Inline_Always;
   function "-"  is new CFGR.Clear_FF with Inline_Always;
   function "="  is new CFGR.Equal    with Inline_Always;
   function Init is new CFGR.Init     with Inline_Always;

   --  Constant definitions

   function NoClock      is new CFGR.C (RCC_CFGR.MCO, 2#0000#) with Inline_Always;
   function SYSCLK       is new CFGR.C (RCC_CFGR.MCO, 2#0100#) with Inline_Always;
   function HSI          is new CFGR.C (RCC_CFGR.MCO, 2#0101#) with Inline_Always;
   function HSE          is new CFGR.C (RCC_CFGR.MCO, 2#0110#) with Inline_Always;
   function PLLCLK_Div2  is new CFGR.C (RCC_CFGR.MCO, 2#0111#) with Inline_Always;
   function PLL2CLK      is new CFGR.C (RCC_CFGR.MCO, 2#1000#) with Inline_Always;
   function PLL3CLK_Div2 is new CFGR.C (RCC_CFGR.MCO, 2#1001#) with Inline_Always;
   function XT           is new CFGR.C (RCC_CFGR.MCO, 2#1010#) with Inline_Always;
   function PLL3CLK      is new CFGR.C (RCC_CFGR.MCO, 2#1011#) with Inline_Always;

   function PLLVOC_Div3  is new CFGR.C (RCC_CFGR.OTGFSPRE, 0);
   function PLLVCO_Div2  is new CFGR.C (RCC_CFGR.OTGFSPRE, 1);

   function PLLx4        is new CFGR.C (RCC_CFGR.PLLMUL, 2#0010#) with Inline_Always;
   function PLLx5        is new CFGR.C (RCC_CFGR.PLLMUL, 2#0011#) with Inline_Always;
   function PLLx6        is new CFGR.C (RCC_CFGR.PLLMUL, 2#0100#) with Inline_Always;
   function PLLx7        is new CFGR.C (RCC_CFGR.PLLMUL, 2#0101#) with Inline_Always;
   function PLLx8        is new CFGR.C (RCC_CFGR.PLLMUL, 2#0110#) with Inline_Always;
   function PLLx9        is new CFGR.C (RCC_CFGR.PLLMUL, 2#0111#) with Inline_Always;
   function PLLx6_5      is new CFGR.C (RCC_CFGR.PLLMUL, 2#1101#) with Inline_Always;

   function HSI_Div2     is new CFGR.C (RCC_CFGR.PLLSRC, 0);
   function PREDIV1      is new CFGR.C (RCC_CFGR.PLLSRC, 1);

   function Div2         is new CFGR.C (RCC_CFGR.ADCPRE, 2#00#) with Inline_Always;
   function Div4         is new CFGR.C (RCC_CFGR.ADCPRE, 2#01#) with Inline_Always;
   function Div6         is new CFGR.C (RCC_CFGR.ADCPRE, 2#10#) with Inline_Always;
   function Div8         is new CFGR.C (RCC_CFGR.ADCPRE, 2#11#) with Inline_Always;

   function Div1         is new CFGR.C (RCC_CFGR.PPRE2, 2#000#) with Inline_Always;
   function Div2         is new CFGR.C (RCC_CFGR.PPRE2, 2#100#) with Inline_Always;
   function Div4         is new CFGR.C (RCC_CFGR.PPRE2, 2#101#) with Inline_Always;
   function Div8         is new CFGR.C (RCC_CFGR.PPRE2, 2#110#) with Inline_Always;
   function Div16        is new CFGR.C (RCC_CFGR.PPRE2, 2#111#) with Inline_Always;

   function Div1         is new CFGR.C (RCC_CFGR.PPRE1, 2#000#) with Inline_Always;
   function Div2         is new CFGR.C (RCC_CFGR.PPRE1, 2#100#) with Inline_Always;
   function Div4         is new CFGR.C (RCC_CFGR.PPRE1, 2#101#) with Inline_Always;
   function Div8         is new CFGR.C (RCC_CFGR.PPRE1, 2#110#) with Inline_Always;
   function Div16        is new CFGR.C (RCC_CFGR.PPRE1, 2#111#) with Inline_Always;

   function Div1         is new CFGR.C (RCC_CFGR.HPRE, 2#0000#) with Inline_Always;
   function Div2         is new CFGR.C (RCC_CFGR.HPRE, 2#1000#) with Inline_Always;
   function Div4         is new CFGR.C (RCC_CFGR.HPRE, 2#1001#) with Inline_Always;
   function Div8         is new CFGR.C (RCC_CFGR.HPRE, 2#1010#) with Inline_Always;
   function Div16        is new CFGR.C (RCC_CFGR.HPRE, 2#1011#) with Inline_Always;
   function Div64        is new CFGR.C (RCC_CFGR.HPRE, 2#1100#) with Inline_Always;
   function Div128       is new CFGR.C (RCC_CFGR.HPRE, 2#1101#) with Inline_Always;
   function Div256       is new CFGR.C (RCC_CFGR.HPRE, 2#1110#) with Inline_Always;
   function Div512       is new CFGR.C (RCC_CFGR.HPRE, 2#1111#) with Inline_Always;

   function HSI          is new CFGR.C (RCC_CFGR.SWS, 2#00#) with Inline_Always;
   function HSE          is new CFGR.C (RCC_CFGR.SWS, 2#01#) with Inline_Always;
   function PLL          is new CFGR.C (RCC_CFGR.SWS, 2#10#) with Inline_Always;

   function HSI          is new CFGR.C (RCC_CFGR.SW, 2#00#) with Inline_Always;
   function HSE          is new CFGR.C (RCC_CFGR.SW, 2#01#) with Inline_Always;
   function PLL          is new CFGR.C (RCC_CFGR.SW, 2#10#) with Inline_Always;

   --------------------------
   --  Interrupt Register  --
   --------------------------

   package  RCC_CIR is

      package Tp is new Types (R32);

      package CSSC      is new Bitfield (Tp, 23);
      package PLL3RDYC  is new Bitfield (Tp, 22);
      package PLL2RDYC  is new Bitfield (Tp, 21);
      package PLLRDYC   is new Bitfield (Tp, 20);
      package HSERDYC   is new Bitfield (Tp, 19);
      package HSIRDYC   is new Bitfield (Tp, 18);
      package LSERDYC   is new Bitfield (Tp, 17);
      package LSIRDYC   is new Bitfield (Tp, 16);
      package PLL3RDYIE is new Bitfield (Tp, 14);
      package PLL2RDYIE is new Bitfield (Tp, 13);
      package PLLRDYIE  is new Bitfield (Tp, 12);
      package HSERDYIE  is new Bitfield (Tp, 11);
      package HSIRDYIE  is new Bitfield (Tp, 10);
      package LSERDYIE  is new Bitfield (Tp,  9);
      package LSIRDYIE  is new Bitfield (Tp,  8);
      package CSSF      is new Bitfield (Tp,  7);
      package PLL3RDYF  is new Bitfield (Tp,  6);
      package PLL2RDYF  is new Bitfield (Tp,  5);
      package PLLRDYF   is new Bitfield (Tp,  4);
      package HSERDYF   is new Bitfield (Tp,  3);
      package HSIRDYF   is new Bitfield (Tp,  2);
      package LSERDYF   is new Bitfield (Tp,  1);
      package LSIRDYF   is new Bitfield (Tp,  0);

      type T is
         record
            CSSC      : RCC_CIR.CSSC     .T;
            PLL3RDYC  : RCC_CIR.PLL3RDYC .T;
            PLL2RDYC  : RCC_CIR.PLL2RDYC .T;
            PLLRDYC   : RCC_CIR.PLLRDYC  .T;
            HSERDYC   : RCC_CIR.HSERDYC  .T;
            HSIRDYC   : RCC_CIR.HSIRDYC  .T;
            LSERDYC   : RCC_CIR.LSERDYC  .T;
            LSIRDYC   : RCC_CIR.LSIRDYC  .T;
            PLL3RDYIE : RCC_CIR.PLL3RDYIE.T;
            PLL2RDYIE : RCC_CIR.PLL2RDYIE.T;
            PLLRDYIE  : RCC_CIR.PLLRDYIE .T;
            HSERDYIE  : RCC_CIR.HSERDYIE .T;
            HSIRDYIE  : RCC_CIR.HSIRDYIE .T;
            LSERDYIE  : RCC_CIR.LSERDYIE .T;
            LSIRDYIE  : RCC_CIR.LSIRDYIE .T;
            CSSF      : RCC_CIR.CSSF     .T;
            PLL3RDYF  : RCC_CIR.PLL3RDYF .T;
            PLL2RDYF  : RCC_CIR.PLL2RDYF .T;
            PLLRDYF   : RCC_CIR.PLLRDYF  .T;
            HSERDYF   : RCC_CIR.HSERDYF  .T;
            HSIRDYF   : RCC_CIR.HSIRDYF  .T;
            LSERDYF   : RCC_CIR.LSERDYF  .T;
            LSIRDYF   : RCC_CIR.LSIRDYF  .T;
         end record;

      for T use
         record
            CSSC      at 0 range CSSC     .F .. CSSC     .L;
            PLL3RDYC  at 0 range PLL3RDYC .F .. PLL3RDYC .L;
            PLL2RDYC  at 0 range PLL2RDYC .F .. PLL2RDYC .L;
            PLLRDYC   at 0 range PLLRDYC  .F .. PLLRDYC  .L;
            HSERDYC   at 0 range HSERDYC  .F .. HSERDYC  .L;
            HSIRDYC   at 0 range HSIRDYC  .F .. HSIRDYC  .L;
            LSERDYC   at 0 range LSERDYC  .F .. LSERDYC  .L;
            LSIRDYC   at 0 range LSIRDYC  .F .. LSIRDYC  .L;
            PLL3RDYIE at 0 range PLL3RDYIE.F .. PLL3RDYIE.L;
            PLL2RDYIE at 0 range PLL2RDYIE.F .. PLL2RDYIE.L;
            PLLRDYIE  at 0 range PLLRDYIE .F .. PLLRDYIE .L;
            HSERDYIE  at 0 range HSERDYIE .F .. HSERDYIE .L;
            HSIRDYIE  at 0 range HSIRDYIE .F .. HSIRDYIE .L;
            LSERDYIE  at 0 range LSERDYIE .F .. LSERDYIE .L;
            LSIRDYIE  at 0 range LSIRDYIE .F .. LSIRDYIE .L;
            CSSF      at 0 range CSSF     .F .. CSSF     .L;
            PLL3RDYF  at 0 range PLL3RDYF .F .. PLL3RDYF .L;
            PLL2RDYF  at 0 range PLL2RDYF .F .. PLL2RDYF .L;
            PLLRDYF   at 0 range PLLRDYF  .F .. PLLRDYF  .L;
            HSERDYF   at 0 range HSERDYF  .F .. HSERDYF  .L;
            HSIRDYF   at 0 range HSIRDYF  .F .. HSIRDYF  .L;
            LSERDYF   at 0 range LSERDYF  .F .. LSERDYF  .L;
            LSIRDYF   at 0 range LSIRDYF  .F .. LSIRDYF  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_CIR;

   package CIR    is new Register (RCC_CIR.T, RCC_CIR.Tp, 16#4002_1008#);
   subtype CIR_T  is CIR.T;
   subtype CIR_F  is CIR.F;
   

   --  Field definitions

   function CSSC      is new CIR.B (RCC_CIR.CSSC     ) with Inline_Always;
   function PLL3RDYC  is new CIR.B (RCC_CIR.PLL3RDYC ) with Inline_Always;
   function PLL2RDYC  is new CIR.B (RCC_CIR.PLL2RDYC ) with Inline_Always;
   function PLLRDYC   is new CIR.B (RCC_CIR.PLLRDYC  ) with Inline_Always;
   function HSERDYC   is new CIR.B (RCC_CIR.HSERDYC  ) with Inline_Always;
   function HSIRDYC   is new CIR.B (RCC_CIR.HSIRDYC  ) with Inline_Always;
   function LSERDYC   is new CIR.B (RCC_CIR.LSERDYC  ) with Inline_Always;
   function LSIRDYC   is new CIR.B (RCC_CIR.LSIRDYC  ) with Inline_Always;
   function PLL3RDYIE is new CIR.B (RCC_CIR.PLL3RDYIE) with Inline_Always;
   function PLL2RDYIE is new CIR.B (RCC_CIR.PLL2RDYIE) with Inline_Always;
   function PLLRDYIE  is new CIR.B (RCC_CIR.PLLRDYIE ) with Inline_Always;
   function HSERDYIE  is new CIR.B (RCC_CIR.HSERDYIE ) with Inline_Always;
   function HSIRDYIE  is new CIR.B (RCC_CIR.HSIRDYIE ) with Inline_Always;
   function LSERDYIE  is new CIR.B (RCC_CIR.LSERDYIE ) with Inline_Always;
   function LSIRDYIE  is new CIR.B (RCC_CIR.LSIRDYIE ) with Inline_Always;
   function CSSF      is new CIR.B (RCC_CIR.CSSF     ) with Inline_Always;
   function PLL3RDYF  is new CIR.B (RCC_CIR.PLL3RDYF ) with Inline_Always;
   function PLL2RDYF  is new CIR.B (RCC_CIR.PLL2RDYF ) with Inline_Always;
   function PLLRDYF   is new CIR.B (RCC_CIR.PLLRDYF  ) with Inline_Always;
   function HSERDYF   is new CIR.B (RCC_CIR.HSERDYF  ) with Inline_Always;
   function HSIRDYF   is new CIR.B (RCC_CIR.HSIRDYF  ) with Inline_Always;
   function LSERDYF   is new CIR.B (RCC_CIR.LSERDYF  ) with Inline_Always;
   function LSIRDYF   is new CIR.B (RCC_CIR.LSIRDYF  ) with Inline_Always;

   --  Functions

   function "+"  is new CIR.Add      with Inline_Always;
   function "+"  is new CIR.Add_F    with Inline_Always;
   function "+"  is new CIR.Add_FF   with Inline_Always;
   function "-"  is new CIR.Clear    with Inline_Always;
   function "-"  is new CIR.Clear_FF with Inline_Always;
   function "="  is new CIR.Equal    with Inline_Always;
   function Init is new CIR.Init     with Inline_Always;

   --------------------------------------
   --  APB2 Peripheral Reset Register  --
   --------------------------------------

   package  RCC_APB2RSTR is

      package Tp is new Types (R32);

      package USART1RST is new Bitfield (Tp, 14);
      package SPI1RST   is new Bitfield (Tp, 12);
      package TIM1RST   is new Bitfield (Tp, 11);
      package ADC2RST   is new Bitfield (Tp, 10);
      package ADC1RST   is new Bitfield (Tp,  9);
      package IOPERST   is new Bitfield (Tp,  6);
      package IOPDRST   is new Bitfield (Tp,  5);
      package IOPCRST   is new Bitfield (Tp,  4);
      package IOPBRST   is new Bitfield (Tp,  3);
      package IOPARST   is new Bitfield (Tp,  2);
      package AFIORST   is new Bitfield (Tp,  0);

      type T is
         record
            USART1RST : RCC_APB2RSTR.USART1RST.T;
            SPI1RST   : RCC_APB2RSTR.SPI1RST  .T;
            TIM1RST   : RCC_APB2RSTR.TIM1RST  .T;
            ADC2RST   : RCC_APB2RSTR.ADC2RST  .T;
            ADC1RST   : RCC_APB2RSTR.ADC1RST  .T;
            IOPERST   : RCC_APB2RSTR.IOPERST  .T;
            IOPDRST   : RCC_APB2RSTR.IOPDRST  .T;
            IOPCRST   : RCC_APB2RSTR.IOPCRST  .T;
            IOPBRST   : RCC_APB2RSTR.IOPBRST  .T;
            IOPARST   : RCC_APB2RSTR.IOPARST  .T;
            AFIORST   : RCC_APB2RSTR.AFIORST  .T;
         end record;

      for T use
         record
            USART1RST at 0 range USART1RST.F .. USART1RST.L;
            SPI1RST   at 0 range SPI1RST  .F .. SPI1RST  .L;
            TIM1RST   at 0 range TIM1RST  .F .. TIM1RST  .L;
            ADC2RST   at 0 range ADC2RST  .F .. ADC2RST  .L;
            ADC1RST   at 0 range ADC1RST  .F .. ADC1RST  .L;
            IOPERST   at 0 range IOPERST  .F .. IOPERST  .L;
            IOPDRST   at 0 range IOPDRST  .F .. IOPDRST  .L;
            IOPCRST   at 0 range IOPCRST  .F .. IOPCRST  .L;
            IOPBRST   at 0 range IOPBRST  .F .. IOPBRST  .L;
            IOPARST   at 0 range IOPARST  .F .. IOPARST  .L;
            AFIORST   at 0 range AFIORST  .F .. AFIORST  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_APB2RSTR;

   package APB2RSTR    is new Register (RCC_APB2RSTR.T, RCC_APB2RSTR.Tp, 16#4002_100C#);
   subtype APB2RSTR_T  is APB2RSTR.T;
   subtype APB2RSTR_F  is APB2RSTR.F;
   

   --  Field definitions

   function USART1RST is new APB2RSTR.B (RCC_APB2RSTR.USART1RST) with Inline_Always;
   function SPI1RST   is new APB2RSTR.B (RCC_APB2RSTR.SPI1RST  ) with Inline_Always;
   function TIM1RST   is new APB2RSTR.B (RCC_APB2RSTR.TIM1RST  ) with Inline_Always;
   function ADC2RST   is new APB2RSTR.B (RCC_APB2RSTR.ADC2RST  ) with Inline_Always;
   function ADC1RST   is new APB2RSTR.B (RCC_APB2RSTR.ADC1RST  ) with Inline_Always;
   function IOPERST   is new APB2RSTR.B (RCC_APB2RSTR.IOPERST  ) with Inline_Always;
   function IOPDRST   is new APB2RSTR.B (RCC_APB2RSTR.IOPDRST  ) with Inline_Always;
   function IOPCRST   is new APB2RSTR.B (RCC_APB2RSTR.IOPCRST  ) with Inline_Always;
   function IOPBRST   is new APB2RSTR.B (RCC_APB2RSTR.IOPBRST  ) with Inline_Always;
   function IOPARST   is new APB2RSTR.B (RCC_APB2RSTR.IOPARST  ) with Inline_Always;
   function AFIORST   is new APB2RSTR.B (RCC_APB2RSTR.AFIORST  ) with Inline_Always;

   --  Functions

   function "+"  is new APB2RSTR.Add      with Inline_Always;
   function "+"  is new APB2RSTR.Add_F    with Inline_Always;
   function "+"  is new APB2RSTR.Add_FF   with Inline_Always;
   function "-"  is new APB2RSTR.Clear    with Inline_Always;
   function "-"  is new APB2RSTR.Clear_FF with Inline_Always;
   function "="  is new APB2RSTR.Equal    with Inline_Always;
   function Init is new APB2RSTR.Init     with Inline_Always;

   --------------------------------------
   --  APB1 Peripheral Reset Register  --
   --------------------------------------

   package  RCC_APB1RSTR is

      package Tp is new Types (R32);

      package DACRST    is new Bitfield (Tp, 29);
      package PWRRST    is new Bitfield (Tp, 28);
      package BKPRST    is new Bitfield (Tp, 27);
      package CAN2RST   is new Bitfield (Tp, 26);
      package CAN1RST   is new Bitfield (Tp, 25);
      package I2C2RST   is new Bitfield (Tp, 22);
      package I2C1RST   is new Bitfield (Tp, 21);
      package UART5RST  is new Bitfield (Tp, 20);
      package UART4RST  is new Bitfield (Tp, 19);
      package USART3RST is new Bitfield (Tp, 18);
      package USART2RST is new Bitfield (Tp, 17);
      package SPI3RST   is new Bitfield (Tp, 15);
      package SPI2RST   is new Bitfield (Tp, 14);
      package WWDGRST   is new Bitfield (Tp, 11);
      package TIM7RST   is new Bitfield (Tp,  5);
      package TIM6RST   is new Bitfield (Tp,  4);
      package TIM5RST   is new Bitfield (Tp,  3);
      package TIM4RST   is new Bitfield (Tp,  2);
      package TIM3RST   is new Bitfield (Tp,  1);
      package TIM2RST   is new Bitfield (Tp,  0);

      type T is
         record
            DACRST    : RCC_APB1RSTR.DACRST   .T;
            PWRRST    : RCC_APB1RSTR.PWRRST   .T;
            BKPRST    : RCC_APB1RSTR.BKPRST   .T;
            CAN2RST   : RCC_APB1RSTR.CAN2RST  .T;
            CAN1RST   : RCC_APB1RSTR.CAN1RST  .T;
            I2C2RST   : RCC_APB1RSTR.I2C2RST  .T;
            I2C1RST   : RCC_APB1RSTR.I2C1RST  .T;
            UART5RST  : RCC_APB1RSTR.UART5RST .T;
            UART4RST  : RCC_APB1RSTR.UART4RST .T;
            USART3RST : RCC_APB1RSTR.USART3RST.T;
            USART2RST : RCC_APB1RSTR.USART2RST.T;
            SPI3RST   : RCC_APB1RSTR.SPI3RST  .T;
            SPI2RST   : RCC_APB1RSTR.SPI2RST  .T;
            WWDGRST   : RCC_APB1RSTR.WWDGRST  .T;
            TIM7RST   : RCC_APB1RSTR.TIM7RST  .T;
            TIM6RST   : RCC_APB1RSTR.TIM6RST  .T;
            TIM5RST   : RCC_APB1RSTR.TIM5RST  .T;
            TIM4RST   : RCC_APB1RSTR.TIM4RST  .T;
            TIM3RST   : RCC_APB1RSTR.TIM3RST  .T;
            TIM2RST   : RCC_APB1RSTR.TIM2RST  .T;
         end record;

      for T use
         record
            DACRST    at 0 range DACRST   .F .. DACRST   .L;
            PWRRST    at 0 range PWRRST   .F .. PWRRST   .L;
            BKPRST    at 0 range BKPRST   .F .. BKPRST   .L;
            CAN2RST   at 0 range CAN2RST  .F .. CAN2RST  .L;
            CAN1RST   at 0 range CAN1RST  .F .. CAN1RST  .L;
            I2C2RST   at 0 range I2C2RST  .F .. I2C2RST  .L;
            I2C1RST   at 0 range I2C1RST  .F .. I2C1RST  .L;
            UART5RST  at 0 range UART5RST .F .. UART5RST .L;
            UART4RST  at 0 range UART4RST .F .. UART4RST .L;
            USART3RST at 0 range USART3RST.F .. USART3RST.L;
            USART2RST at 0 range USART2RST.F .. USART2RST.L;
            SPI3RST   at 0 range SPI3RST  .F .. SPI3RST  .L;
            SPI2RST   at 0 range SPI2RST  .F .. SPI2RST  .L;
            WWDGRST   at 0 range WWDGRST  .F .. WWDGRST  .L;
            TIM7RST   at 0 range TIM7RST  .F .. TIM7RST  .L;
            TIM6RST   at 0 range TIM6RST  .F .. TIM6RST  .L;
            TIM5RST   at 0 range TIM5RST  .F .. TIM5RST  .L;
            TIM4RST   at 0 range TIM4RST  .F .. TIM4RST  .L;
            TIM3RST   at 0 range TIM3RST  .F .. TIM3RST  .L;
            TIM2RST   at 0 range TIM2RST  .F .. TIM2RST  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_APB1RSTR;

   package APB1RSTR    is new Register (RCC_APB1RSTR.T, RCC_APB1RSTR.Tp, 16#4002_1010#);
   subtype APB1RSTR_T  is APB1RSTR.T;
   subtype APB1RSTR_F  is APB1RSTR.F;
   

   --  Field definitions

   function DACRST    is new APB1RSTR.B (RCC_APB1RSTR.DACRST   ) with Inline_Always;
   function PWRRST    is new APB1RSTR.B (RCC_APB1RSTR.PWRRST   ) with Inline_Always;
   function BKPRST    is new APB1RSTR.B (RCC_APB1RSTR.BKPRST   ) with Inline_Always;
   function CAN2RST   is new APB1RSTR.B (RCC_APB1RSTR.CAN2RST  ) with Inline_Always;
   function CAN1RST   is new APB1RSTR.B (RCC_APB1RSTR.CAN1RST  ) with Inline_Always;
   function I2C2RST   is new APB1RSTR.B (RCC_APB1RSTR.I2C2RST  ) with Inline_Always;
   function I2C1RST   is new APB1RSTR.B (RCC_APB1RSTR.I2C1RST  ) with Inline_Always;
   function UART5RST  is new APB1RSTR.B (RCC_APB1RSTR.UART5RST ) with Inline_Always;
   function UART4RST  is new APB1RSTR.B (RCC_APB1RSTR.UART4RST ) with Inline_Always;
   function USART3RST is new APB1RSTR.B (RCC_APB1RSTR.USART3RST) with Inline_Always;
   function USART2RST is new APB1RSTR.B (RCC_APB1RSTR.USART2RST) with Inline_Always;
   function SPI3RST   is new APB1RSTR.B (RCC_APB1RSTR.SPI3RST  ) with Inline_Always;
   function SPI2RST   is new APB1RSTR.B (RCC_APB1RSTR.SPI2RST  ) with Inline_Always;
   function WWDGRST   is new APB1RSTR.B (RCC_APB1RSTR.WWDGRST  ) with Inline_Always;
   function TIM7RST   is new APB1RSTR.B (RCC_APB1RSTR.TIM7RST  ) with Inline_Always;
   function TIM6RST   is new APB1RSTR.B (RCC_APB1RSTR.TIM6RST  ) with Inline_Always;
   function TIM5RST   is new APB1RSTR.B (RCC_APB1RSTR.TIM5RST  ) with Inline_Always;
   function TIM4RST   is new APB1RSTR.B (RCC_APB1RSTR.TIM4RST  ) with Inline_Always;
   function TIM3RST   is new APB1RSTR.B (RCC_APB1RSTR.TIM3RST  ) with Inline_Always;
   function TIM2RST   is new APB1RSTR.B (RCC_APB1RSTR.TIM2RST  ) with Inline_Always;

   --  Functions

   function "+"  is new APB1RSTR.Add      with Inline_Always;
   function "+"  is new APB1RSTR.Add_F    with Inline_Always;
   function "+"  is new APB1RSTR.Add_FF   with Inline_Always;
   function "-"  is new APB1RSTR.Clear    with Inline_Always;
   function "-"  is new APB1RSTR.Clear_FF with Inline_Always;
   function "="  is new APB1RSTR.Equal    with Inline_Always;
   function Init is new APB1RSTR.Init     with Inline_Always;

   --------------------------------------------
   --  AHB Peripheral Clock Enable Register  --
   --------------------------------------------

   package  RCC_AHBENR is

      package Tp is new Types (R32);

      package ETHMACRXEN is new Bitfield (Tp, 16);
      package ETHMACTXEN is new Bitfield (Tp, 15);
      package ETHMACEN   is new Bitfield (Tp, 14);
      package OTGFSEN    is new Bitfield (Tp, 12);
      package CRCEN      is new Bitfield (Tp,  6);
      package FLITFEN    is new Bitfield (Tp,  4);
      package SRAMEN     is new Bitfield (Tp,  2);
      package DMA2EN     is new Bitfield (Tp,  1);
      package DMA1EN     is new Bitfield (Tp,  0);

      type T is
         record
            ETHMACRXEN : RCC_AHBENR.ETHMACRXEN.T;
            ETHMACTXEN : RCC_AHBENR.ETHMACTXEN.T;
            ETHMACEN   : RCC_AHBENR.ETHMACEN  .T;
            OTGFSEN    : RCC_AHBENR.OTGFSEN   .T;
            CRCEN      : RCC_AHBENR.CRCEN     .T;
            FLITFEN    : RCC_AHBENR.FLITFEN   .T;
            SRAMEN     : RCC_AHBENR.SRAMEN    .T;
            DMA2EN     : RCC_AHBENR.DMA2EN    .T;
            DMA1EN     : RCC_AHBENR.DMA1EN    .T;
         end record;

      for T use
         record
            ETHMACRXEN at 0 range ETHMACRXEN.F .. ETHMACRXEN.L;
            ETHMACTXEN at 0 range ETHMACTXEN.F .. ETHMACTXEN.L;
            ETHMACEN   at 0 range ETHMACEN  .F .. ETHMACEN  .L;
            OTGFSEN    at 0 range OTGFSEN   .F .. OTGFSEN   .L;
            CRCEN      at 0 range CRCEN     .F .. CRCEN     .L;
            FLITFEN    at 0 range FLITFEN   .F .. FLITFEN   .L;
            SRAMEN     at 0 range SRAMEN    .F .. SRAMEN    .L;
            DMA2EN     at 0 range DMA2EN    .F .. DMA2EN    .L;
            DMA1EN     at 0 range DMA1EN    .F .. DMA1EN    .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_AHBENR;

   package AHBENR    is new Register (RCC_AHBENR.T, RCC_AHBENR.Tp, 16#4002_1014#);
   subtype AHBENR_T  is AHBENR.T;
   subtype AHBENR_F  is AHBENR.F;
   

   --  Field definitions

   function ETHMACRXEN is new AHBENR.B (RCC_AHBENR.ETHMACRXEN) with Inline_Always;
   function ETHMACTXEN is new AHBENR.B (RCC_AHBENR.ETHMACTXEN) with Inline_Always;
   function ETHMACEN   is new AHBENR.B (RCC_AHBENR.ETHMACEN  ) with Inline_Always;
   function OTGFSEN    is new AHBENR.B (RCC_AHBENR.OTGFSEN   ) with Inline_Always;
   function CRCEN      is new AHBENR.B (RCC_AHBENR.CRCEN     ) with Inline_Always;
   function FLITFEN    is new AHBENR.B (RCC_AHBENR.FLITFEN   ) with Inline_Always;
   function SRAMEN     is new AHBENR.B (RCC_AHBENR.SRAMEN    ) with Inline_Always;
   function DMA2EN     is new AHBENR.B (RCC_AHBENR.DMA2EN    ) with Inline_Always;
   function DMA1EN     is new AHBENR.B (RCC_AHBENR.DMA1EN    ) with Inline_Always;

   --  Functions

   function "+"  is new AHBENR.Add      with Inline_Always;
   function "+"  is new AHBENR.Add_F    with Inline_Always;
   function "+"  is new AHBENR.Add_FF   with Inline_Always;
   function "-"  is new AHBENR.Clear    with Inline_Always;
   function "-"  is new AHBENR.Clear_FF with Inline_Always;
   function "="  is new AHBENR.Equal    with Inline_Always;
   function Init is new AHBENR.Init     with Inline_Always;

   ---------------------------------------------
   --  APB2 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package  RCC_APB2ENR is

      package Tp is new Types (R32);

      package USART1EN is new Bitfield (Tp, 14);
      package SPI1EN   is new Bitfield (Tp, 12);
      package TIM1EN   is new Bitfield (Tp, 11);
      package ADC2EN   is new Bitfield (Tp, 10);
      package ADC1EN   is new Bitfield (Tp,  9);
      package IOPEEN   is new Bitfield (Tp,  6);
      package IOPDEN   is new Bitfield (Tp,  5);
      package IOPCEN   is new Bitfield (Tp,  4);
      package IOPBEN   is new Bitfield (Tp,  3);
      package IOPAEN   is new Bitfield (Tp,  2);
      package AFIOEN   is new Bitfield (Tp,  0);

      type T is
         record
            USART1EN : RCC_APB2ENR.USART1EN.T;
            SPI1EN   : RCC_APB2ENR.SPI1EN  .T;
            TIM1EN   : RCC_APB2ENR.TIM1EN  .T;
            ADC2EN   : RCC_APB2ENR.ADC2EN  .T;
            ADC1EN   : RCC_APB2ENR.ADC1EN  .T;
            IOPEEN   : RCC_APB2ENR.IOPEEN  .T;
            IOPDEN   : RCC_APB2ENR.IOPDEN  .T;
            IOPCEN   : RCC_APB2ENR.IOPCEN  .T;
            IOPBEN   : RCC_APB2ENR.IOPBEN  .T;
            IOPAEN   : RCC_APB2ENR.IOPAEN  .T;
            AFIOEN   : RCC_APB2ENR.AFIOEN  .T;
         end record;

      for T use
         record
            USART1EN at 0 range USART1EN.F .. USART1EN.L;
            SPI1EN   at 0 range SPI1EN  .F .. SPI1EN  .L;
            TIM1EN   at 0 range TIM1EN  .F .. TIM1EN  .L;
            ADC2EN   at 0 range ADC2EN  .F .. ADC2EN  .L;
            ADC1EN   at 0 range ADC1EN  .F .. ADC1EN  .L;
            IOPEEN   at 0 range IOPEEN  .F .. IOPEEN  .L;
            IOPDEN   at 0 range IOPDEN  .F .. IOPDEN  .L;
            IOPCEN   at 0 range IOPCEN  .F .. IOPCEN  .L;
            IOPBEN   at 0 range IOPBEN  .F .. IOPBEN  .L;
            IOPAEN   at 0 range IOPAEN  .F .. IOPAEN  .L;
            AFIOEN   at 0 range AFIOEN  .F .. AFIOEN  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_APB2ENR;

   package APB2ENR    is new Register (RCC_APB2ENR.T, RCC_APB2ENR.Tp, 16#4002_1018#);
   subtype APB2ENR_T  is APB2ENR.T;
   subtype APB2ENR_F  is APB2ENR.F;
   

   --  Field definitions

   function USART1EN is new APB2ENR.B (RCC_APB2ENR.USART1EN) with Inline_Always;
   function SPI1EN   is new APB2ENR.B (RCC_APB2ENR.SPI1EN  ) with Inline_Always;
   function TIM1EN   is new APB2ENR.B (RCC_APB2ENR.TIM1EN  ) with Inline_Always;
   function ADC2EN   is new APB2ENR.B (RCC_APB2ENR.ADC2EN  ) with Inline_Always;
   function ADC1EN   is new APB2ENR.B (RCC_APB2ENR.ADC1EN  ) with Inline_Always;
   function IOPEEN   is new APB2ENR.B (RCC_APB2ENR.IOPEEN  ) with Inline_Always;
   function IOPDEN   is new APB2ENR.B (RCC_APB2ENR.IOPDEN  ) with Inline_Always;
   function IOPCEN   is new APB2ENR.B (RCC_APB2ENR.IOPCEN  ) with Inline_Always;
   function IOPBEN   is new APB2ENR.B (RCC_APB2ENR.IOPBEN  ) with Inline_Always;
   function IOPAEN   is new APB2ENR.B (RCC_APB2ENR.IOPAEN  ) with Inline_Always;
   function AFIOEN   is new APB2ENR.B (RCC_APB2ENR.AFIOEN  ) with Inline_Always;

   --  Functions

   function "+"  is new APB2ENR.Add      with Inline_Always;
   function "+"  is new APB2ENR.Add_F    with Inline_Always;
   function "+"  is new APB2ENR.Add_FF   with Inline_Always;
   function "-"  is new APB2ENR.Clear    with Inline_Always;
   function "-"  is new APB2ENR.Clear_FF with Inline_Always;
   function "="  is new APB2ENR.Equal    with Inline_Always;
   function Init is new APB2ENR.Init     with Inline_Always;

   ---------------------------------------------
   --  APB1 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package  RCC_APB1ENR is

      package Tp is new Types (R32);

      package DACEN    is new Bitfield (Tp, 29);
      package PWREN    is new Bitfield (Tp, 28);
      package BKPEN    is new Bitfield (Tp, 27);
      package CAN2EN   is new Bitfield (Tp, 26);
      package CAN1EN   is new Bitfield (Tp, 25);
      package I2C2EN   is new Bitfield (Tp, 22);
      package I2C1EN   is new Bitfield (Tp, 21);
      package UART5EN  is new Bitfield (Tp, 20);
      package UART4EN  is new Bitfield (Tp, 19);
      package USART3EN is new Bitfield (Tp, 18);
      package USART2EN is new Bitfield (Tp, 17);
      package SPI3EN   is new Bitfield (Tp, 15);
      package SPI2EN   is new Bitfield (Tp, 14);
      package WWDGEN   is new Bitfield (Tp, 11);
      package TIM7EN   is new Bitfield (Tp,  5);
      package TIM6EN   is new Bitfield (Tp,  4);
      package TIM5EN   is new Bitfield (Tp,  3);
      package TIM4EN   is new Bitfield (Tp,  2);
      package TIM3EN   is new Bitfield (Tp,  1);
      package TIM2EN   is new Bitfield (Tp,  0);

      type T is
         record
            DACEN    : RCC_APB1ENR.DACEN   .T;
            PWREN    : RCC_APB1ENR.PWREN   .T;
            BKPEN    : RCC_APB1ENR.BKPEN   .T;
            CAN2EN   : RCC_APB1ENR.CAN2EN  .T;
            CAN1EN   : RCC_APB1ENR.CAN1EN  .T;
            I2C2EN   : RCC_APB1ENR.I2C2EN  .T;
            I2C1EN   : RCC_APB1ENR.I2C1EN  .T;
            UART5EN  : RCC_APB1ENR.UART5EN .T;
            UART4EN  : RCC_APB1ENR.UART4EN .T;
            USART3EN : RCC_APB1ENR.USART3EN.T;
            USART2EN : RCC_APB1ENR.USART2EN.T;
            SPI3EN   : RCC_APB1ENR.SPI3EN  .T;
            SPI2EN   : RCC_APB1ENR.SPI2EN  .T;
            WWDGEN   : RCC_APB1ENR.WWDGEN  .T;
            TIM7EN   : RCC_APB1ENR.TIM7EN  .T;
            TIM6EN   : RCC_APB1ENR.TIM6EN  .T;
            TIM5EN   : RCC_APB1ENR.TIM5EN  .T;
            TIM4EN   : RCC_APB1ENR.TIM4EN  .T;
            TIM3EN   : RCC_APB1ENR.TIM3EN  .T;
            TIM2EN   : RCC_APB1ENR.TIM2EN  .T;
         end record;

      for T use
         record
            DACEN    at 0 range DACEN   .F .. DACEN   .L;
            PWREN    at 0 range PWREN   .F .. PWREN   .L;
            BKPEN    at 0 range BKPEN   .F .. BKPEN   .L;
            CAN2EN   at 0 range CAN2EN  .F .. CAN2EN  .L;
            CAN1EN   at 0 range CAN1EN  .F .. CAN1EN  .L;
            I2C2EN   at 0 range I2C2EN  .F .. I2C2EN  .L;
            I2C1EN   at 0 range I2C1EN  .F .. I2C1EN  .L;
            UART5EN  at 0 range UART5EN .F .. UART5EN .L;
            UART4EN  at 0 range UART4EN .F .. UART4EN .L;
            USART3EN at 0 range USART3EN.F .. USART3EN.L;
            USART2EN at 0 range USART2EN.F .. USART2EN.L;
            SPI3EN   at 0 range SPI3EN  .F .. SPI3EN  .L;
            SPI2EN   at 0 range SPI2EN  .F .. SPI2EN  .L;
            WWDGEN   at 0 range WWDGEN  .F .. WWDGEN  .L;
            TIM7EN   at 0 range TIM7EN  .F .. TIM7EN  .L;
            TIM6EN   at 0 range TIM6EN  .F .. TIM6EN  .L;
            TIM5EN   at 0 range TIM5EN  .F .. TIM5EN  .L;
            TIM4EN   at 0 range TIM4EN  .F .. TIM4EN  .L;
            TIM3EN   at 0 range TIM3EN  .F .. TIM3EN  .L;
            TIM2EN   at 0 range TIM2EN  .F .. TIM2EN  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_APB1ENR;

   package APB1ENR    is new Register (RCC_APB1ENR.T, RCC_APB1ENR.Tp, 16#4002_101C#);
   subtype APB1ENR_T  is APB1ENR.T;
   subtype APB1ENR_F  is APB1ENR.F;
   

   --  Field definitions

   function DACEN    is new APB1ENR.B (RCC_APB1ENR.DACEN   ) with Inline_Always;
   function PWREN    is new APB1ENR.B (RCC_APB1ENR.PWREN   ) with Inline_Always;
   function BKPEN    is new APB1ENR.B (RCC_APB1ENR.BKPEN   ) with Inline_Always;
   function CAN2EN   is new APB1ENR.B (RCC_APB1ENR.CAN2EN  ) with Inline_Always;
   function CAN1EN   is new APB1ENR.B (RCC_APB1ENR.CAN1EN  ) with Inline_Always;
   function I2C2EN   is new APB1ENR.B (RCC_APB1ENR.I2C2EN  ) with Inline_Always;
   function I2C1EN   is new APB1ENR.B (RCC_APB1ENR.I2C1EN  ) with Inline_Always;
   function UART5EN  is new APB1ENR.B (RCC_APB1ENR.UART5EN ) with Inline_Always;
   function UART4EN  is new APB1ENR.B (RCC_APB1ENR.UART4EN ) with Inline_Always;
   function USART3EN is new APB1ENR.B (RCC_APB1ENR.USART3EN) with Inline_Always;
   function USART2EN is new APB1ENR.B (RCC_APB1ENR.USART2EN) with Inline_Always;
   function SPI3EN   is new APB1ENR.B (RCC_APB1ENR.SPI3EN  ) with Inline_Always;
   function SPI2EN   is new APB1ENR.B (RCC_APB1ENR.SPI2EN  ) with Inline_Always;
   function WWDGEN   is new APB1ENR.B (RCC_APB1ENR.WWDGEN  ) with Inline_Always;
   function TIM7EN   is new APB1ENR.B (RCC_APB1ENR.TIM7EN  ) with Inline_Always;
   function TIM6EN   is new APB1ENR.B (RCC_APB1ENR.TIM6EN  ) with Inline_Always;
   function TIM5EN   is new APB1ENR.B (RCC_APB1ENR.TIM5EN  ) with Inline_Always;
   function TIM4EN   is new APB1ENR.B (RCC_APB1ENR.TIM4EN  ) with Inline_Always;
   function TIM3EN   is new APB1ENR.B (RCC_APB1ENR.TIM3EN  ) with Inline_Always;
   function TIM2EN   is new APB1ENR.B (RCC_APB1ENR.TIM2EN  ) with Inline_Always;

   --  Functions

   function "+"  is new APB1ENR.Add      with Inline_Always;
   function "+"  is new APB1ENR.Add_F    with Inline_Always;
   function "+"  is new APB1ENR.Add_FF   with Inline_Always;
   function "-"  is new APB1ENR.Clear    with Inline_Always;
   function "-"  is new APB1ENR.Clear_FF with Inline_Always;
   function "="  is new APB1ENR.Equal    with Inline_Always;
   function Init is new APB1ENR.Init     with Inline_Always;

   --------------------------------------
   --  Backup Domain Control Register  --
   --------------------------------------

   package  RCC_BDCR is

      package Tp is new Types (R32);

      package BDRTS  is new Bitfield (Tp, 16);
      package RTCEN  is new Bitfield (Tp, 15);
      package RTCSEL is new Bitfield (Tp,  8, 2);
      package LSEBYP is new Bitfield (Tp,  2);
      package LSERDY is new Bitfield (Tp,  1);
      package LSEON  is new Bitfield (Tp,  0);

      type T is
         record
            BDRTS  : RCC_BDCR.BDRTS .T;
            RTCEN  : RCC_BDCR.RTCEN .T;
            RTCSEL : RCC_BDCR.RTCSEL.T;
            LSEBYP : RCC_BDCR.LSEBYP.T;
            LSERDY : RCC_BDCR.LSERDY.T;
            LSEON  : RCC_BDCR.LSEON .T;
         end record;

      for T use
         record
            BDRTS  at 0 range BDRTS .F .. BDRTS .L;
            RTCEN  at 0 range RTCEN .F .. RTCEN .L;
            RTCSEL at 0 range RTCSEL.F .. RTCSEL.L;
            LSEBYP at 0 range LSEBYP.F .. LSEBYP.L;
            LSERDY at 0 range LSERDY.F .. LSERDY.L;
            LSEON  at 0 range LSEON .F .. LSEON .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_BDCR;

   package BDCR    is new Register (RCC_BDCR.T, RCC_BDCR.Tp, 16#4002_1020#);
   subtype BDCR_T  is BDCR.T;
   subtype BDCR_F  is BDCR.F;
   

   --  Field definitions

   function BDRTS  is new BDCR.B (RCC_BDCR.BDRTS ) with Inline_Always;
   function RTCEN  is new BDCR.B (RCC_BDCR.RTCEN ) with Inline_Always;
   function RTCSEL is new BDCR.B (RCC_BDCR.RTCSEL) with Inline_Always;
   function LSEBYP is new BDCR.B (RCC_BDCR.LSEBYP) with Inline_Always;
   function LSERDY is new BDCR.B (RCC_BDCR.LSERDY) with Inline_Always;
   function LSEON  is new BDCR.B (RCC_BDCR.LSEON ) with Inline_Always;

   --  Functions

   function "+"  is new BDCR.Add      with Inline_Always;
   function "+"  is new BDCR.Add_F    with Inline_Always;
   function "+"  is new BDCR.Add_FF   with Inline_Always;
   function "-"  is new BDCR.Clear    with Inline_Always;
   function "-"  is new BDCR.Clear_FF with Inline_Always;
   function "="  is new BDCR.Equal    with Inline_Always;
   function Init is new BDCR.Init     with Inline_Always;

   --  Constant definitions

   function NoClock    is new BDCR.C (RCC_BDCR.RTCSEL, 2#00#) with Inline_Always;
   function LSE        is new BDCR.C (RCC_BDCR.RTCSEL, 2#01#) with Inline_Always;
   function LSI        is new BDCR.C (RCC_BDCR.RTCSEL, 2#10#) with Inline_Always;
   function HSE_Div128 is new BDCR.C (RCC_BDCR.RTCSEL, 2#11#) with Inline_Always;

   -------------------------------
   --  Control/Status Register  --
   -------------------------------

   package  RCC_CSR is

      package Tp is new Types (R32);

      package LPWRRSTF is new Bitfield (Tp, 31);
      package WWDGRSTF is new Bitfield (Tp, 30);
      package IWDGRSTF is new Bitfield (Tp, 29);
      package SFTRSTF  is new Bitfield (Tp, 28);
      package PORRSTF  is new Bitfield (Tp, 27);
      package PINRSTF  is new Bitfield (Tp, 26);
      package RMVF     is new Bitfield (Tp, 24);
      package LSIRDY   is new Bitfield (Tp,  1);
      package LSION    is new Bitfield (Tp,  0);

      type T is
         record
            LPWRRSTF : RCC_CSR.LPWRRSTF.T;
            WWDGRSTF : RCC_CSR.WWDGRSTF.T;
            IWDGRSTF : RCC_CSR.IWDGRSTF.T;
            SFTRSTF  : RCC_CSR.SFTRSTF .T;
            PORRSTF  : RCC_CSR.PORRSTF .T;
            PINRSTF  : RCC_CSR.PINRSTF .T;
            RMVF     : RCC_CSR.RMVF    .T;
            LSIRDY   : RCC_CSR.LSIRDY  .T;
            LSION    : RCC_CSR.LSION   .T;
         end record;

      for T use
         record
            LPWRRSTF at 0 range LPWRRSTF.F .. LPWRRSTF.L;
            WWDGRSTF at 0 range WWDGRSTF.F .. WWDGRSTF.L;
            IWDGRSTF at 0 range IWDGRSTF.F .. IWDGRSTF.L;
            SFTRSTF  at 0 range SFTRSTF .F .. SFTRSTF .L;
            PORRSTF  at 0 range PORRSTF .F .. PORRSTF .L;
            PINRSTF  at 0 range PINRSTF .F .. PINRSTF .L;
            RMVF     at 0 range RMVF    .F .. RMVF    .L;
            LSIRDY   at 0 range LSIRDY  .F .. LSIRDY  .L;
            LSION    at 0 range LSION   .F .. LSION   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_CSR;

   package CSR    is new Register (RCC_CSR.T, RCC_CSR.Tp, 16#4002_1024#);
   subtype CSR_T  is CSR.T;
   subtype CSR_F  is CSR.F;
   

   --  Field definitions

   function LPWRRSTF is new CSR.B (RCC_CSR.LPWRRSTF) with Inline_Always;
   function WWDGRSTF is new CSR.B (RCC_CSR.WWDGRSTF) with Inline_Always;
   function IWDGRSTF is new CSR.B (RCC_CSR.IWDGRSTF) with Inline_Always;
   function SFTRSTF  is new CSR.B (RCC_CSR.SFTRSTF ) with Inline_Always;
   function PORRSTF  is new CSR.B (RCC_CSR.PORRSTF ) with Inline_Always;
   function PINRSTF  is new CSR.B (RCC_CSR.PINRSTF ) with Inline_Always;
   function RMVF     is new CSR.B (RCC_CSR.RMVF    ) with Inline_Always;
   function LSIRDY   is new CSR.B (RCC_CSR.LSIRDY  ) with Inline_Always;
   function LSION    is new CSR.B (RCC_CSR.LSION   ) with Inline_Always;

   --  Functions

   function "+"  is new CSR.Add      with Inline_Always;
   function "+"  is new CSR.Add_F    with Inline_Always;
   function "+"  is new CSR.Add_FF   with Inline_Always;
   function "-"  is new CSR.Clear    with Inline_Always;
   function "-"  is new CSR.Clear_FF with Inline_Always;
   function "="  is new CSR.Equal    with Inline_Always;
   function Init is new CSR.Init     with Inline_Always;

   -------------------------------------------
   --  AHB Peripheral Clock Reset Register  --
   -------------------------------------------

   package  RCC_AHBRSTR is

      package Tp is new Types (R32);

      package ETHMACRST is new Bitfield (Tp, 14);
      package OTGFSRST  is new Bitfield (Tp, 12);

      type T is
         record
            ETHMACRST : RCC_AHBRSTR.ETHMACRST.T;
            OTGFSRST  : RCC_AHBRSTR.OTGFSRST .T;
         end record;

      for T use
         record
            ETHMACRST at 0 range ETHMACRST.F .. ETHMACRST.L;
            OTGFSRST  at 0 range OTGFSRST .F .. OTGFSRST .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_AHBRSTR;

   package AHBRSTR    is new Register (RCC_AHBRSTR.T, RCC_AHBRSTR.Tp, 16#4002_1028#);
   subtype AHBRSTR_T  is AHBRSTR.T;
   subtype AHBRSTR_F  is AHBRSTR.F;
   

   --  Field definitions

   function ETHMACRST is new AHBRSTR.B (RCC_AHBRSTR.ETHMACRST) with Inline_Always;
   function OTGFSRST  is new AHBRSTR.B (RCC_AHBRSTR.OTGFSRST)  with Inline_Always;

   --  Functions

   function "+"  is new AHBRSTR.Add      with Inline_Always;
   function "+"  is new AHBRSTR.Add_F    with Inline_Always;
   function "+"  is new AHBRSTR.Add_FF   with Inline_Always;
   function "-"  is new AHBRSTR.Clear    with Inline_Always;
   function "-"  is new AHBRSTR.Clear_FF with Inline_Always;
   function "="  is new AHBRSTR.Equal    with Inline_Always;
   function Init is new AHBRSTR.Init     with Inline_Always;

   ----------------------------------------
   --  Control Configuration Register 2  --
   ----------------------------------------

   package  RCC_CFGR2 is

      package Tp is new Types (R32);

      package I2S3SRC    is new Bitfield (Tp, 18);
      package I2S2SRC    is new Bitfield (Tp, 17);
      package PREDIV1SRC is new Bitfield (Tp, 16);
      package PLL3MUL    is new Bitfield (Tp, 12, 4);
      package PLL2MUL    is new Bitfield (Tp,  8, 4);
      package PREDIV2    is new Bitfield (Tp,  4, 4);
      package PREDIV1    is new Bitfield (Tp,  0, 4);

      type T is
         record
            I2S3SRC    : RCC_CFGR2.I2S3SRC   .T;
            I2S2SRC    : RCC_CFGR2.I2S2SRC   .T;
            PREDIV1SRC : RCC_CFGR2.PREDIV1SRC.T;
            PLL3MUL    : RCC_CFGR2.PLL3MUL   .T;
            PLL2MUL    : RCC_CFGR2.PLL2MUL   .T;
            PREDIV2    : RCC_CFGR2.PREDIV2   .T;
            PREDIV1    : RCC_CFGR2.PREDIV1   .T;
         end record;

      for T use
         record
            I2S3SRC    at 0 range I2S3SRC   .F .. I2S3SRC   .L;
            I2S2SRC    at 0 range I2S2SRC   .F .. I2S2SRC   .L;
            PREDIV1SRC at 0 range PREDIV1SRC.F .. PREDIV1SRC.L;
            PLL3MUL    at 0 range PLL3MUL   .F .. PLL3MUL   .L;
            PLL2MUL    at 0 range PLL2MUL   .F .. PLL2MUL   .L;
            PREDIV2    at 0 range PREDIV2   .F .. PREDIV2   .L;
            PREDIV1    at 0 range PREDIV1   .F .. PREDIV1   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end  RCC_CFGR2;

   package CFGR2    is new Register (RCC_CFGR2.T, RCC_CFGR2.Tp, 16#4002_102C#);
   subtype CFGR2_T  is CFGR2.T;
   subtype CFGR2_F  is CFGR2.F;
   

   --  Field definitions

   function I2S3SRC    is new CFGR2.B (RCC_CFGR2.I2S3SRC   ) with Inline_Always;
   function I2S2SRC    is new CFGR2.B (RCC_CFGR2.I2S2SRC   ) with Inline_Always;
   function PREDIV1SRC is new CFGR2.B (RCC_CFGR2.PREDIV1SRC) with Inline_Always;
   function PLL3MUL    is new CFGR2.B (RCC_CFGR2.PLL3MUL   ) with Inline_Always;
   function PLL2MUL    is new CFGR2.B (RCC_CFGR2.PLL2MUL   ) with Inline_Always;
   function PREDIV2    is new CFGR2.B (RCC_CFGR2.PREDIV2   ) with Inline_Always;
   function PREDIV1    is new CFGR2.B (RCC_CFGR2.PREDIV1   ) with Inline_Always;

   --  Functions

   function "+"  is new CFGR2.Add      with Inline_Always;
   function "+"  is new CFGR2.Add_F    with Inline_Always;
   function "+"  is new CFGR2.Add_FF   with Inline_Always;
   function "-"  is new CFGR2.Clear    with Inline_Always;
   function "-"  is new CFGR2.Clear_FF with Inline_Always;
   function "="  is new CFGR2.Equal    with Inline_Always;
   function Init is new CFGR2.Init     with Inline_Always;

   --  Constant definitions

   function PLLx8  is new CFGR2.C (RCC_CFGR2.PLL3MUL, 2#0110#) with Inline_Always;
   function PLLx9  is new CFGR2.C (RCC_CFGR2.PLL3MUL, 2#0111#) with Inline_Always;
   function PLLx10 is new CFGR2.C (RCC_CFGR2.PLL3MUL, 2#1000#) with Inline_Always;
   function PLLx11 is new CFGR2.C (RCC_CFGR2.PLL3MUL, 2#1001#) with Inline_Always;
   function PLLx12 is new CFGR2.C (RCC_CFGR2.PLL3MUL, 2#1010#) with Inline_Always;
   function PLLx13 is new CFGR2.C (RCC_CFGR2.PLL3MUL, 2#1011#) with Inline_Always;
   function PLLx14 is new CFGR2.C (RCC_CFGR2.PLL3MUL, 2#1100#) with Inline_Always;
   function PLLx16 is new CFGR2.C (RCC_CFGR2.PLL3MUL, 2#1110#) with Inline_Always;
   function PLLx20 is new CFGR2.C (RCC_CFGR2.PLL3MUL, 2#1111#) with Inline_Always;

   function PLLx8  is new CFGR2.C (RCC_CFGR2.PLL2MUL, 2#0110#) with Inline_Always;
   function PLLx9  is new CFGR2.C (RCC_CFGR2.PLL2MUL, 2#0111#) with Inline_Always;
   function PLLx10 is new CFGR2.C (RCC_CFGR2.PLL2MUL, 2#1000#) with Inline_Always;
   function PLLx11 is new CFGR2.C (RCC_CFGR2.PLL2MUL, 2#1001#) with Inline_Always;
   function PLLx12 is new CFGR2.C (RCC_CFGR2.PLL2MUL, 2#1010#) with Inline_Always;
   function PLLx13 is new CFGR2.C (RCC_CFGR2.PLL2MUL, 2#1011#) with Inline_Always;
   function PLLx14 is new CFGR2.C (RCC_CFGR2.PLL2MUL, 2#1100#) with Inline_Always;
   function PLLx16 is new CFGR2.C (RCC_CFGR2.PLL2MUL, 2#1110#) with Inline_Always;
   function PLLx20 is new CFGR2.C (RCC_CFGR2.PLL2MUL, 2#1111#) with Inline_Always;

   function Div1   is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#0000#) with Inline_Always;
   function Div2   is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#0001#) with Inline_Always;
   function Div3   is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#0010#) with Inline_Always;
   function Div4   is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#0011#) with Inline_Always;
   function Div5   is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#0100#) with Inline_Always;
   function Div6   is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#0101#) with Inline_Always;
   function Div7   is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#0110#) with Inline_Always;
   function Div8   is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#0111#) with Inline_Always;
   function Div9   is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#1000#) with Inline_Always;
   function Div10  is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#1001#) with Inline_Always;
   function Div11  is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#1010#) with Inline_Always;
   function Div12  is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#1011#) with Inline_Always;
   function Div13  is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#1100#) with Inline_Always;
   function Div14  is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#1101#) with Inline_Always;
   function Div15  is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#1110#) with Inline_Always;
   function Div16  is new CFGR2.C (RCC_CFGR2.PREDIV2, 2#1111#) with Inline_Always;

   function Div1   is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#0000#) with Inline_Always;
   function Div2   is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#0001#) with Inline_Always;
   function Div3   is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#0010#) with Inline_Always;
   function Div4   is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#0011#) with Inline_Always;
   function Div5   is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#0100#) with Inline_Always;
   function Div6   is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#0101#) with Inline_Always;
   function Div7   is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#0110#) with Inline_Always;
   function Div8   is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#0111#) with Inline_Always;
   function Div9   is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#1000#) with Inline_Always;
   function Div10  is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#1001#) with Inline_Always;
   function Div11  is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#1010#) with Inline_Always;
   function Div12  is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#1011#) with Inline_Always;
   function Div13  is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#1100#) with Inline_Always;
   function Div14  is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#1101#) with Inline_Always;
   function Div15  is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#1110#) with Inline_Always;
   function Div16  is new CFGR2.C (RCC_CFGR2.PREDIV1, 2#1111#) with Inline_Always;

end ARM.Register.RCC_F10XXX;
