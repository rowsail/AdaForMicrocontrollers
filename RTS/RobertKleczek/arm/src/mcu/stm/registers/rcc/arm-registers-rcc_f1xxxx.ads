--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--               A R M . R e g i s t e r s . R C C _ F 1 X X X X              --
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

--  Package defines STM32 F1XXXX family reset & clock related registers

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
--                           ARM.Registers.RCC_F1XXXX                         --
--------------------------------------------------------------------------------

package ARM.Registers.RCC_F1XXXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                         Reset and Clock                             --
   -------------------------------------------------------------------------

   --     RCC Registers Collection  --

   ---------------------
   --  CR Register --
   ---------------------

   package CR is

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
            PLL3RDY  : CR.PLL3RDY.T;
            PLL3ON   : CR.PLL3ON .T;
            PLL2RDY  : CR.PLL2RDY.T;
            PLL2ON   : CR.PLL2ON .T;
            PLLRDY   : CR.PLLRDY .T;
            PLLON    : CR.PLLON  .T;
            CSSON    : CR.CSSON  .T;
            HSEBYP   : CR.HSEBYP .T;
            HSERDY   : CR.HSERDY .T;
            HSEON    : CR.HSEON  .T;
            HSICAL   : CR.HSICAL .T;
            HSITRIM  : CR.HSITRIM.T;
            HSIRDY   : CR.HSIRDY .T;
            HSION    : CR.HSION  .T;
         end record;

      for T use
         record
            PLL3RDY at 0 range PLL3RDY.R'First .. PLL3RDY.R'Last;
            PLL3ON  at 0 range PLL3ON .R'First .. PLL3ON .R'Last;
            PLL2RDY at 0 range PLL2RDY.R'First .. PLL2RDY.R'Last;
            PLL2ON  at 0 range PLL2ON .R'First .. PLL2ON .R'Last;
            PLLRDY  at 0 range PLLRDY .R'First .. PLLRDY .R'Last;
            PLLON   at 0 range PLLON  .R'First .. PLLON  .R'Last;
            CSSON   at 0 range CSSON  .R'First .. CSSON  .R'Last;
            HSEBYP  at 0 range HSEBYP .R'First .. HSEBYP .R'Last;
            HSERDY  at 0 range HSERDY .R'First .. HSERDY .R'Last;
            HSEON   at 0 range HSEON  .R'First .. HSEON  .R'Last;
            HSICAL  at 0 range HSICAL .R'First .. HSICAL .R'Last;
            HSITRIM at 0 range HSITRIM.R'First .. HSITRIM.R'Last;
            HSIRDY  at 0 range HSIRDY .R'First .. HSIRDY .R'Last;
            HSION   at 0 range HSION  .R'First .. HSION  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CR;

   --  Types definitions

   subtype CR_T is CR.T;

   --  Field definitions

   function PLL3RDY is new CR.FN.B (CR.PLL3RDY);
   function PLL3ON  is new CR.FN.B (CR.PLL3ON);
   function PLL2RDY is new CR.FN.B (CR.PLL2RDY);
   function PLL2ON  is new CR.FN.B (CR.PLL2ON);
   function PLLRDY  is new CR.FN.B (CR.PLLRDY);
   function PLLON   is new CR.FN.B (CR.PLLON);
   function CSSON   is new CR.FN.B (CR.CSSON);
   function HSEBYP  is new CR.FN.B (CR.HSEBYP);
   function HSERDY  is new CR.FN.B (CR.HSERDY);
   function HSEON   is new CR.FN.B (CR.HSEON);
   function HSICAL  is new CR.FN.B (CR.HSICAL);
   function HSITRIM is new CR.FN.B (CR.HSITRIM);
   function HSIRDY  is new CR.FN.B (CR.HSIRDY);
   function HSION   is new CR.FN.B (CR.HSION);

   --  Functions

   function  "+"    is new CR.FN.Add;
   function  "+"    is new CR.FN.Add_RM;
   function  "-"    is new CR.FN.Clear;
   function  Init   is new CR.FN.Init;

   --  Constant definitions

   function Oscillator_Not_Bypassed is new CR.FN.C (CR.HSEBYP, 2#0#);
   function Oscillator_Bypassed     is new CR.FN.C (CR.HSEBYP, 2#1#);

   function Oscillator_Not_Ready is new CR.FN.C (CR.HSERDY, 2#0#);
   function Oscillator_Ready     is new CR.FN.C (CR.HSERDY, 2#1#);
   ---------------------
   --  CFGR Register --
   ---------------------

   package CFGR is

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
            MCO      : CFGR.MCO     .T;
            OTGFSPRE : CFGR.OTGFSPRE.T;
            PLLMUL   : CFGR.PLLMUL  .T;
            PLLXTPRE : CFGR.PLLXTPRE.T;
            PLLSRC   : CFGR.PLLSRC  .T;
            ADCPRE   : CFGR.ADCPRE  .T;
            PPRE2    : CFGR.PPRE2   .T;
            PPRE1    : CFGR.PPRE1   .T;
            HPRE     : CFGR.HPRE    .T;
            SWS      : CFGR.SWS     .T;
            SW       : CFGR.SW      .T;
         end record;

      for T use
         record
            MCO      at 0 range MCO     .R'First .. MCO     .R'Last;
            OTGFSPRE at 0 range OTGFSPRE.R'First .. OTGFSPRE.R'Last;
            PLLMUL   at 0 range PLLMUL  .R'First .. PLLMUL  .R'Last;
            PLLXTPRE at 0 range PLLXTPRE.R'First .. PLLXTPRE.R'Last;
            PLLSRC   at 0 range PLLSRC  .R'First .. PLLSRC  .R'Last;
            ADCPRE   at 0 range ADCPRE  .R'First .. ADCPRE  .R'Last;
            PPRE2    at 0 range PPRE2   .R'First .. PPRE2   .R'Last;
            PPRE1    at 0 range PPRE1   .R'First .. PPRE1   .R'Last;
            HPRE     at 0 range HPRE    .R'First .. HPRE    .R'Last;
            SWS      at 0 range SWS     .R'First .. SWS     .R'Last;
            SW       at 0 range SW      .R'First .. SW      .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CFGR;

   subtype CFGR_T is CFGR.T;

   --  Field definitions

   function MCO      is new CFGR.FN.B (CFGR.MCO);
   function OTGFSPRE is new CFGR.FN.B (CFGR.OTGFSPRE);
   function PLLMUL   is new CFGR.FN.B (CFGR.PLLMUL);
   function PLLXTPRE is new CFGR.FN.B (CFGR.PLLXTPRE);
   function PLLSRC   is new CFGR.FN.B (CFGR.PLLSRC);
   function ADCPRE   is new CFGR.FN.B (CFGR.ADCPRE);
   function PPRE2    is new CFGR.FN.B (CFGR.PPRE2);
   function PPRE1    is new CFGR.FN.B (CFGR.PPRE1);
   function HPRE     is new CFGR.FN.B (CFGR.HPRE);
   function SWS      is new CFGR.FN.B (CFGR.SWS);
   function SW       is new CFGR.FN.B (CFGR.SW);

   --  Functions

   function  "+"   is new CFGR.FN.Add;
   function  "+"   is new CFGR.FN.Add_RM;
   function  "-"   is new CFGR.FN.Clear;
   function  Init  is new CFGR.FN.Init;

   --  Constant definitions

   function NoClock      is new CFGR.FN.C (CFGR.MCO, 2#0000#);
   function SYSCLK       is new CFGR.FN.C (CFGR.MCO, 2#0100#);
   function HSI          is new CFGR.FN.C (CFGR.MCO, 2#0101#);
   function HSE          is new CFGR.FN.C (CFGR.MCO, 2#0110#);
   function PLLCLK_Div2  is new CFGR.FN.C (CFGR.MCO, 2#0111#);
   function PLL2CLK      is new CFGR.FN.C (CFGR.MCO, 2#1000#);
   function PLL3CLK_Div2 is new CFGR.FN.C (CFGR.MCO, 2#1001#);
   function XT           is new CFGR.FN.C (CFGR.MCO, 2#1010#);
   function PLL3CLK      is new CFGR.FN.C (CFGR.MCO, 2#1011#);

   function PLLVOC_Div3  is new CFGR.FN.C (CFGR.OTGFSPRE, 0);
   function PLLVCO_Div2  is new CFGR.FN.C (CFGR.OTGFSPRE, 1);

   function PLLx4        is new CFGR.FN.C (CFGR.PLLMUL, 2#0010#);
   function PLLx5        is new CFGR.FN.C (CFGR.PLLMUL, 2#0011#);
   function PLLx6        is new CFGR.FN.C (CFGR.PLLMUL, 2#0100#);
   function PLLx7        is new CFGR.FN.C (CFGR.PLLMUL, 2#0101#);
   function PLLx8        is new CFGR.FN.C (CFGR.PLLMUL, 2#0110#);
   function PLLx9        is new CFGR.FN.C (CFGR.PLLMUL, 2#0111#);
   function PLLx6_5      is new CFGR.FN.C (CFGR.PLLMUL, 2#1101#);

   function HSI_Div2     is new CFGR.FN.C (CFGR.PLLSRC, 0);
   function PREDIV1      is new CFGR.FN.C (CFGR.PLLSRC, 1);

   function Div2         is new CFGR.FN.C (CFGR.ADCPRE, 2#00#);
   function Div4         is new CFGR.FN.C (CFGR.ADCPRE, 2#01#);
   function Div6         is new CFGR.FN.C (CFGR.ADCPRE, 2#10#);
   function Div8         is new CFGR.FN.C (CFGR.ADCPRE, 2#11#);

   function Div1         is new CFGR.FN.C (CFGR.PPRE2, 2#000#);
   function Div2         is new CFGR.FN.C (CFGR.PPRE2, 2#100#);
   function Div4         is new CFGR.FN.C (CFGR.PPRE2, 2#101#);
   function Div8         is new CFGR.FN.C (CFGR.PPRE2, 2#110#);
   function Div16        is new CFGR.FN.C (CFGR.PPRE2, 2#111#);

   function Div1         is new CFGR.FN.C (CFGR.PPRE1, 2#000#);
   function Div2         is new CFGR.FN.C (CFGR.PPRE1, 2#100#);
   function Div4         is new CFGR.FN.C (CFGR.PPRE1, 2#101#);
   function Div8         is new CFGR.FN.C (CFGR.PPRE1, 2#110#);
   function Div16        is new CFGR.FN.C (CFGR.PPRE1, 2#111#);

   function Div1         is new CFGR.FN.C (CFGR.HPRE, 2#0000#);
   function Div2         is new CFGR.FN.C (CFGR.HPRE, 2#1000#);
   function Div4         is new CFGR.FN.C (CFGR.HPRE, 2#1001#);
   function Div8         is new CFGR.FN.C (CFGR.HPRE, 2#1010#);
   function Div16        is new CFGR.FN.C (CFGR.HPRE, 2#1011#);
   function Div64        is new CFGR.FN.C (CFGR.HPRE, 2#1100#);
   function Div128       is new CFGR.FN.C (CFGR.HPRE, 2#1101#);
   function Div256       is new CFGR.FN.C (CFGR.HPRE, 2#1110#);
   function Div512       is new CFGR.FN.C (CFGR.HPRE, 2#1111#);

   function HSI          is new CFGR.FN.C (CFGR.SWS, 2#00#);
   function HSE          is new CFGR.FN.C (CFGR.SWS, 2#01#);
   function PLL          is new CFGR.FN.C (CFGR.SWS, 2#10#);

   function HSI          is new CFGR.FN.C (CFGR.SW, 2#00#);
   function HSE          is new CFGR.FN.C (CFGR.SW, 2#01#);
   function PLL          is new CFGR.FN.C (CFGR.SW, 2#10#);

   ---------------------
   --  CIR Register --
   ---------------------

   package CIR is

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
            CSSC      : CIR.CSSC     .T;
            PLL3RDYC  : CIR.PLL3RDYC .T;
            PLL2RDYC  : CIR.PLL2RDYC .T;
            PLLRDYC   : CIR.PLLRDYC  .T;
            HSERDYC   : CIR.HSERDYC  .T;
            HSIRDYC   : CIR.HSIRDYC  .T;
            LSERDYC   : CIR.LSERDYC  .T;
            LSIRDYC   : CIR.LSIRDYC  .T;
            PLL3RDYIE : CIR.PLL3RDYIE.T;
            PLL2RDYIE : CIR.PLL2RDYIE.T;
            PLLRDYIE  : CIR.PLLRDYIE .T;
            HSERDYIE  : CIR.HSERDYIE .T;
            HSIRDYIE  : CIR.HSIRDYIE .T;
            LSERDYIE  : CIR.LSERDYIE .T;
            LSIRDYIE  : CIR.LSIRDYIE .T;
            CSSF      : CIR.CSSF     .T;
            PLL3RDYF  : CIR.PLL3RDYF .T;
            PLL2RDYF  : CIR.PLL2RDYF .T;
            PLLRDYF   : CIR.PLLRDYF  .T;
            HSERDYF   : CIR.HSERDYF  .T;
            HSIRDYF   : CIR.HSIRDYF  .T;
            LSERDYF   : CIR.LSERDYF  .T;
            LSIRDYF   : CIR.LSIRDYF  .T;
         end record;

      for T use
         record
            CSSC      at 0 range CSSC     .R'First .. CSSC     .R'Last;
            PLL3RDYC  at 0 range PLL3RDYC .R'First .. PLL3RDYC .R'Last;
            PLL2RDYC  at 0 range PLL2RDYC .R'First .. PLL2RDYC .R'Last;
            PLLRDYC   at 0 range PLLRDYC  .R'First .. PLLRDYC  .R'Last;
            HSERDYC   at 0 range HSERDYC  .R'First .. HSERDYC  .R'Last;
            HSIRDYC   at 0 range HSIRDYC  .R'First .. HSIRDYC  .R'Last;
            LSERDYC   at 0 range LSERDYC  .R'First .. LSERDYC  .R'Last;
            LSIRDYC   at 0 range LSIRDYC  .R'First .. LSIRDYC  .R'Last;
            PLL3RDYIE at 0 range PLL3RDYIE.R'First .. PLL3RDYIE.R'Last;
            PLL2RDYIE at 0 range PLL2RDYIE.R'First .. PLL2RDYIE.R'Last;
            PLLRDYIE  at 0 range PLLRDYIE .R'First .. PLLRDYIE .R'Last;
            HSERDYIE  at 0 range HSERDYIE .R'First .. HSERDYIE .R'Last;
            HSIRDYIE  at 0 range HSIRDYIE .R'First .. HSIRDYIE .R'Last;
            LSERDYIE  at 0 range LSERDYIE .R'First .. LSERDYIE .R'Last;
            LSIRDYIE  at 0 range LSIRDYIE .R'First .. LSIRDYIE .R'Last;
            CSSF      at 0 range CSSF     .R'First .. CSSF     .R'Last;
            PLL3RDYF  at 0 range PLL3RDYF .R'First .. PLL3RDYF .R'Last;
            PLL2RDYF  at 0 range PLL2RDYF .R'First .. PLL2RDYF .R'Last;
            PLLRDYF   at 0 range PLLRDYF  .R'First .. PLLRDYF  .R'Last;
            HSERDYF   at 0 range HSERDYF  .R'First .. HSERDYF  .R'Last;
            HSIRDYF   at 0 range HSIRDYF  .R'First .. HSIRDYF  .R'Last;
            LSERDYF   at 0 range LSERDYF  .R'First .. LSERDYF  .R'Last;
            LSIRDYF   at 0 range LSIRDYF  .R'First .. LSIRDYF  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CIR;

   subtype CIR_T is CIR.T;

   --  Field definitions

   function CSSC      is new CIR.FN.B (CIR.CSSC);
   function PLL3RDYC  is new CIR.FN.B (CIR.PLL3RDYC);
   function PLL2RDYC  is new CIR.FN.B (CIR.PLL2RDYC);
   function PLLRDYC   is new CIR.FN.B (CIR.PLLRDYC);
   function HSERDYC   is new CIR.FN.B (CIR.HSERDYC);
   function HSIRDYC   is new CIR.FN.B (CIR.HSIRDYC);
   function LSERDYC   is new CIR.FN.B (CIR.LSERDYC);
   function LSIRDYC   is new CIR.FN.B (CIR.LSIRDYC);
   function PLL3RDYIE is new CIR.FN.B (CIR.PLL3RDYIE);
   function PLL2RDYIE is new CIR.FN.B (CIR.PLL2RDYIE);
   function PLLRDYIE  is new CIR.FN.B (CIR.PLLRDYIE);
   function HSERDYIE  is new CIR.FN.B (CIR.HSERDYIE);
   function HSIRDYIE  is new CIR.FN.B (CIR.HSIRDYIE);
   function LSERDYIE  is new CIR.FN.B (CIR.LSERDYIE);
   function LSIRDYIE  is new CIR.FN.B (CIR.LSIRDYIE);
   function CSSF      is new CIR.FN.B (CIR.CSSF);
   function PLL3RDYF  is new CIR.FN.B (CIR.PLL3RDYF);
   function PLL2RDYF  is new CIR.FN.B (CIR.PLL2RDYF);
   function PLLRDYF   is new CIR.FN.B (CIR.PLLRDYF);
   function HSERDYF   is new CIR.FN.B (CIR.HSERDYF);
   function HSIRDYF   is new CIR.FN.B (CIR.HSIRDYF);
   function LSERDYF   is new CIR.FN.B (CIR.LSERDYF);
   function LSIRDYF   is new CIR.FN.B (CIR.LSIRDYF);

   --  Functions

   function  "+"   is new CIR.FN.Add;
   function  "+"   is new CIR.FN.Add_RM;
   function  "-"   is new CIR.FN.Clear;
   function  Init  is new CIR.FN.Init;

   ---------------------
   --  APB2RSTR Register --
   ---------------------

   package APB2RSTR is

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
            USART1RST : APB2RSTR.USART1RST.T;
            SPI1RST   : APB2RSTR.SPI1RST  .T;
            TIM1RST   : APB2RSTR.TIM1RST  .T;
            ADC2RST   : APB2RSTR.ADC2RST  .T;
            ADC1RST   : APB2RSTR.ADC1RST  .T;
            IOPERST   : APB2RSTR.IOPERST  .T;
            IOPDRST   : APB2RSTR.IOPDRST  .T;
            IOPCRST   : APB2RSTR.IOPCRST  .T;
            IOPBRST   : APB2RSTR.IOPBRST  .T;
            IOPARST   : APB2RSTR.IOPARST  .T;
            AFIORST   : APB2RSTR.AFIORST  .T;
         end record;

      for T use
         record
            USART1RST at 0 range USART1RST.R'First .. USART1RST.R'Last;
            SPI1RST   at 0 range SPI1RST  .R'First .. SPI1RST  .R'Last;
            TIM1RST   at 0 range TIM1RST  .R'First .. TIM1RST  .R'Last;
            ADC2RST   at 0 range ADC2RST  .R'First .. ADC2RST  .R'Last;
            ADC1RST   at 0 range ADC1RST  .R'First .. ADC1RST  .R'Last;
            IOPERST   at 0 range IOPERST  .R'First .. IOPERST  .R'Last;
            IOPDRST   at 0 range IOPDRST  .R'First .. IOPDRST  .R'Last;
            IOPCRST   at 0 range IOPCRST  .R'First .. IOPCRST  .R'Last;
            IOPBRST   at 0 range IOPBRST  .R'First .. IOPBRST  .R'Last;
            IOPARST   at 0 range IOPARST  .R'First .. IOPARST  .R'Last;
            AFIORST   at 0 range AFIORST  .R'First .. AFIORST  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB2RSTR;

   subtype APB2RSTR_T is APB2RSTR.T;

   --  Field definitions

   function USART1RST is new APB2RSTR.FN.B (APB2RSTR.USART1RST);
   function SPI1RST   is new APB2RSTR.FN.B (APB2RSTR.SPI1RST);
   function TIM1RST   is new APB2RSTR.FN.B (APB2RSTR.TIM1RST);
   function ADC2RST   is new APB2RSTR.FN.B (APB2RSTR.ADC2RST);
   function ADC1RST   is new APB2RSTR.FN.B (APB2RSTR.ADC1RST);
   function IOPERST   is new APB2RSTR.FN.B (APB2RSTR.IOPERST);
   function IOPDRST   is new APB2RSTR.FN.B (APB2RSTR.IOPDRST);
   function IOPCRST   is new APB2RSTR.FN.B (APB2RSTR.IOPCRST);
   function IOPBRST   is new APB2RSTR.FN.B (APB2RSTR.IOPBRST);
   function IOPARST   is new APB2RSTR.FN.B (APB2RSTR.IOPARST);
   function AFIORST   is new APB2RSTR.FN.B (APB2RSTR.AFIORST);

   --  Functions

   function  "+"   is new APB2RSTR.FN.Add;
   function  "+"   is new APB2RSTR.FN.Add_RM;
   function  "-"   is new APB2RSTR.FN.Clear;
   function  Init  is new APB2RSTR.FN.Init;

   ---------------------
   --  APB1RSTR Register --
   ---------------------

   package APB1RSTR is

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
            DACRST    : APB1RSTR.DACRST   .T;
            PWRRST    : APB1RSTR.PWRRST   .T;
            BKPRST    : APB1RSTR.BKPRST   .T;
            CAN2RST   : APB1RSTR.CAN2RST  .T;
            CAN1RST   : APB1RSTR.CAN1RST  .T;
            I2C2RST   : APB1RSTR.I2C2RST  .T;
            I2C1RST   : APB1RSTR.I2C1RST  .T;
            UART5RST  : APB1RSTR.UART5RST .T;
            UART4RST  : APB1RSTR.UART4RST .T;
            USART3RST : APB1RSTR.USART3RST.T;
            USART2RST : APB1RSTR.USART2RST.T;
            SPI3RST   : APB1RSTR.SPI3RST  .T;
            SPI2RST   : APB1RSTR.SPI2RST  .T;
            WWDGRST   : APB1RSTR.WWDGRST  .T;
            TIM7RST   : APB1RSTR.TIM7RST  .T;
            TIM6RST   : APB1RSTR.TIM6RST  .T;
            TIM5RST   : APB1RSTR.TIM5RST  .T;
            TIM4RST   : APB1RSTR.TIM4RST  .T;
            TIM3RST   : APB1RSTR.TIM3RST  .T;
            TIM2RST   : APB1RSTR.TIM2RST  .T;
         end record;

      for T use
         record
            DACRST    at 0 range DACRST   .R'First .. DACRST   .R'Last;
            PWRRST    at 0 range PWRRST   .R'First .. PWRRST   .R'Last;
            BKPRST    at 0 range BKPRST   .R'First .. BKPRST   .R'Last;
            CAN2RST   at 0 range CAN2RST  .R'First .. CAN2RST  .R'Last;
            CAN1RST   at 0 range CAN1RST  .R'First .. CAN1RST  .R'Last;
            I2C2RST   at 0 range I2C2RST  .R'First .. I2C2RST  .R'Last;
            I2C1RST   at 0 range I2C1RST  .R'First .. I2C1RST  .R'Last;
            UART5RST  at 0 range UART5RST .R'First .. UART5RST .R'Last;
            UART4RST  at 0 range UART4RST .R'First .. UART4RST .R'Last;
            USART3RST at 0 range USART3RST.R'First .. USART3RST.R'Last;
            USART2RST at 0 range USART2RST.R'First .. USART2RST.R'Last;
            SPI3RST   at 0 range SPI3RST  .R'First .. SPI3RST  .R'Last;
            SPI2RST   at 0 range SPI2RST  .R'First .. SPI2RST  .R'Last;
            WWDGRST   at 0 range WWDGRST  .R'First .. WWDGRST  .R'Last;
            TIM7RST   at 0 range TIM7RST  .R'First .. TIM7RST  .R'Last;
            TIM6RST   at 0 range TIM6RST  .R'First .. TIM6RST  .R'Last;
            TIM5RST   at 0 range TIM5RST  .R'First .. TIM5RST  .R'Last;
            TIM4RST   at 0 range TIM4RST  .R'First .. TIM4RST  .R'Last;
            TIM3RST   at 0 range TIM3RST  .R'First .. TIM3RST  .R'Last;
            TIM2RST   at 0 range TIM2RST  .R'First .. TIM2RST  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB1RSTR;

   subtype APB1RSTR_T is APB1RSTR.T;

   --  Field definitions

   function DACRST    is new APB1RSTR.FN.B (APB1RSTR.DACRST);
   function PWRRST    is new APB1RSTR.FN.B (APB1RSTR.PWRRST);
   function BKPRST    is new APB1RSTR.FN.B (APB1RSTR.BKPRST);
   function CAN2RST   is new APB1RSTR.FN.B (APB1RSTR.CAN2RST);
   function CAN1RST   is new APB1RSTR.FN.B (APB1RSTR.CAN1RST);
   function I2C2RST   is new APB1RSTR.FN.B (APB1RSTR.I2C2RST);
   function I2C1RST   is new APB1RSTR.FN.B (APB1RSTR.I2C1RST);
   function UART5RST  is new APB1RSTR.FN.B (APB1RSTR.UART5RST);
   function UART4RST  is new APB1RSTR.FN.B (APB1RSTR.UART4RST);
   function USART3RST is new APB1RSTR.FN.B (APB1RSTR.USART3RST);
   function USART2RST is new APB1RSTR.FN.B (APB1RSTR.USART2RST);
   function SPI3RST   is new APB1RSTR.FN.B (APB1RSTR.SPI3RST);
   function SPI2RST   is new APB1RSTR.FN.B (APB1RSTR.SPI2RST);
   function WWDGRST   is new APB1RSTR.FN.B (APB1RSTR.WWDGRST);
   function TIM7RST   is new APB1RSTR.FN.B (APB1RSTR.TIM7RST);
   function TIM6RST   is new APB1RSTR.FN.B (APB1RSTR.TIM6RST);
   function TIM5RST   is new APB1RSTR.FN.B (APB1RSTR.TIM5RST);
   function TIM4RST   is new APB1RSTR.FN.B (APB1RSTR.TIM4RST);
   function TIM3RST   is new APB1RSTR.FN.B (APB1RSTR.TIM3RST);
   function TIM2RST   is new APB1RSTR.FN.B (APB1RSTR.TIM2RST);

   --  Functions

   function  "+"   is new APB1RSTR.FN.Add;
   function  "+"   is new APB1RSTR.FN.Add_RM;
   function  "-"   is new APB1RSTR.FN.Clear;
   function  Init  is new APB1RSTR.FN.Init;

   ---------------------
   --  AHBENR Register --
   ---------------------

   package AHBENR is

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
            ETHMACRXEN : AHBENR.ETHMACRXEN.T;
            ETHMACTXEN : AHBENR.ETHMACTXEN.T;
            ETHMACEN   : AHBENR.ETHMACEN  .T;
            OTGFSEN    : AHBENR.OTGFSEN   .T;
            CRCEN      : AHBENR.CRCEN     .T;
            FLITFEN    : AHBENR.FLITFEN   .T;
            SRAMEN     : AHBENR.SRAMEN    .T;
            DMA2EN     : AHBENR.DMA2EN    .T;
            DMA1EN     : AHBENR.DMA1EN    .T;
         end record;

      for T use
         record
            ETHMACRXEN at 0 range ETHMACRXEN.R'First .. ETHMACRXEN.R'Last;
            ETHMACTXEN at 0 range ETHMACTXEN.R'First .. ETHMACTXEN.R'Last;
            ETHMACEN   at 0 range ETHMACEN  .R'First .. ETHMACEN  .R'Last;
            OTGFSEN    at 0 range OTGFSEN   .R'First .. OTGFSEN   .R'Last;
            CRCEN      at 0 range CRCEN     .R'First .. CRCEN     .R'Last;
            FLITFEN    at 0 range FLITFEN   .R'First .. FLITFEN   .R'Last;
            SRAMEN     at 0 range SRAMEN    .R'First .. SRAMEN    .R'Last;
            DMA2EN     at 0 range DMA2EN    .R'First .. DMA2EN    .R'Last;
            DMA1EN     at 0 range DMA1EN    .R'First .. DMA1EN    .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHBENR;

   subtype AHBENR_T is AHBENR.T;

   --  Field definitions

   function ETHMACRXEN is new AHBENR.FN.B (AHBENR.ETHMACRXEN);
   function ETHMACTXEN is new AHBENR.FN.B (AHBENR.ETHMACTXEN);
   function ETHMACEN   is new AHBENR.FN.B (AHBENR.ETHMACEN);
   function OTGFSEN    is new AHBENR.FN.B (AHBENR.OTGFSEN);
   function CRCEN      is new AHBENR.FN.B (AHBENR.CRCEN);
   function FLITFEN    is new AHBENR.FN.B (AHBENR.FLITFEN);
   function SRAMEN     is new AHBENR.FN.B (AHBENR.SRAMEN);
   function DMA2EN     is new AHBENR.FN.B (AHBENR.DMA2EN);
   function DMA1EN     is new AHBENR.FN.B (AHBENR.DMA1EN);

   --  Functions

   function  "+"   is new AHBENR.FN.Add;
   function  "+"   is new AHBENR.FN.Add_RM;
   function  "-"   is new AHBENR.FN.Clear;
   function  Init  is new AHBENR.FN.Init;

   ---------------------
   --  APB2ENR Register --
   ---------------------

   package APB2ENR is

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
            USART1EN : APB2ENR.USART1EN.T;
            SPI1EN   : APB2ENR.SPI1EN  .T;
            TIM1EN   : APB2ENR.TIM1EN  .T;
            ADC2EN   : APB2ENR.ADC2EN  .T;
            ADC1EN   : APB2ENR.ADC1EN  .T;
            IOPEEN   : APB2ENR.IOPEEN  .T;
            IOPDEN   : APB2ENR.IOPDEN  .T;
            IOPCEN   : APB2ENR.IOPCEN  .T;
            IOPBEN   : APB2ENR.IOPBEN  .T;
            IOPAEN   : APB2ENR.IOPAEN  .T;
            AFIOEN   : APB2ENR.AFIOEN  .T;
         end record;

      for T use
         record
            USART1EN at 0 range USART1EN.R'First .. USART1EN.R'Last;
            SPI1EN   at 0 range SPI1EN  .R'First .. SPI1EN  .R'Last;
            TIM1EN   at 0 range TIM1EN  .R'First .. TIM1EN  .R'Last;
            ADC2EN   at 0 range ADC2EN  .R'First .. ADC2EN  .R'Last;
            ADC1EN   at 0 range ADC1EN  .R'First .. ADC1EN  .R'Last;
            IOPEEN   at 0 range IOPEEN  .R'First .. IOPEEN  .R'Last;
            IOPDEN   at 0 range IOPDEN  .R'First .. IOPDEN  .R'Last;
            IOPCEN   at 0 range IOPCEN  .R'First .. IOPCEN  .R'Last;
            IOPBEN   at 0 range IOPBEN  .R'First .. IOPBEN  .R'Last;
            IOPAEN   at 0 range IOPAEN  .R'First .. IOPAEN  .R'Last;
            AFIOEN   at 0 range AFIOEN  .R'First .. AFIOEN  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB2ENR;

   subtype APB2ENR_T is APB2ENR.T;

   --  Field definitions

   function USART1EN is new APB2ENR.FN.B (APB2ENR.USART1EN);
   function SPI1EN   is new APB2ENR.FN.B (APB2ENR.SPI1EN);
   function TIM1EN   is new APB2ENR.FN.B (APB2ENR.TIM1EN);
   function ADC2EN   is new APB2ENR.FN.B (APB2ENR.ADC2EN);
   function ADC1EN   is new APB2ENR.FN.B (APB2ENR.ADC1EN);
   function IOPEEN   is new APB2ENR.FN.B (APB2ENR.IOPEEN);
   function IOPDEN   is new APB2ENR.FN.B (APB2ENR.IOPDEN);
   function IOPCEN   is new APB2ENR.FN.B (APB2ENR.IOPCEN);
   function IOPBEN   is new APB2ENR.FN.B (APB2ENR.IOPBEN);
   function IOPAEN   is new APB2ENR.FN.B (APB2ENR.IOPAEN);
   function AFIOEN   is new APB2ENR.FN.B (APB2ENR.AFIOEN);

   --  Functions

   function  "+"   is new APB2ENR.FN.Add;
   function  "+"   is new APB2ENR.FN.Add_RM;
   function  "-"   is new APB2ENR.FN.Clear;
   function  Init  is new APB2ENR.FN.Init;

   ---------------------
   --  APB1ENR Register --
   ---------------------

   package APB1ENR is

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
            DACEN    : APB1ENR.DACEN   .T;
            PWREN    : APB1ENR.PWREN   .T;
            BKPEN    : APB1ENR.BKPEN   .T;
            CAN2EN   : APB1ENR.CAN2EN  .T;
            CAN1EN   : APB1ENR.CAN1EN  .T;
            I2C2EN   : APB1ENR.I2C2EN  .T;
            I2C1EN   : APB1ENR.I2C1EN  .T;
            UART5EN  : APB1ENR.UART5EN .T;
            UART4EN  : APB1ENR.UART4EN .T;
            USART3EN : APB1ENR.USART3EN.T;
            USART2EN : APB1ENR.USART2EN.T;
            SPI3EN   : APB1ENR.SPI3EN  .T;
            SPI2EN   : APB1ENR.SPI2EN  .T;
            WWDGEN   : APB1ENR.WWDGEN  .T;
            TIM7EN   : APB1ENR.TIM7EN  .T;
            TIM6EN   : APB1ENR.TIM6EN  .T;
            TIM5EN   : APB1ENR.TIM5EN  .T;
            TIM4EN   : APB1ENR.TIM4EN  .T;
            TIM3EN   : APB1ENR.TIM3EN  .T;
            TIM2EN   : APB1ENR.TIM2EN  .T;
         end record;

      for T use
         record
            DACEN    at 0 range DACEN   .R'First .. DACEN   .R'Last;
            PWREN    at 0 range PWREN   .R'First .. PWREN   .R'Last;
            BKPEN    at 0 range BKPEN   .R'First .. BKPEN   .R'Last;
            CAN2EN   at 0 range CAN2EN  .R'First .. CAN2EN  .R'Last;
            CAN1EN   at 0 range CAN1EN  .R'First .. CAN1EN  .R'Last;
            I2C2EN   at 0 range I2C2EN  .R'First .. I2C2EN  .R'Last;
            I2C1EN   at 0 range I2C1EN  .R'First .. I2C1EN  .R'Last;
            UART5EN  at 0 range UART5EN .R'First .. UART5EN .R'Last;
            UART4EN  at 0 range UART4EN .R'First .. UART4EN .R'Last;
            USART3EN at 0 range USART3EN.R'First .. USART3EN.R'Last;
            USART2EN at 0 range USART2EN.R'First .. USART2EN.R'Last;
            SPI3EN   at 0 range SPI3EN  .R'First .. SPI3EN  .R'Last;
            SPI2EN   at 0 range SPI2EN  .R'First .. SPI2EN  .R'Last;
            WWDGEN   at 0 range WWDGEN  .R'First .. WWDGEN  .R'Last;
            TIM7EN   at 0 range TIM7EN  .R'First .. TIM7EN  .R'Last;
            TIM6EN   at 0 range TIM6EN  .R'First .. TIM6EN  .R'Last;
            TIM5EN   at 0 range TIM5EN  .R'First .. TIM5EN  .R'Last;
            TIM4EN   at 0 range TIM4EN  .R'First .. TIM4EN  .R'Last;
            TIM3EN   at 0 range TIM3EN  .R'First .. TIM3EN  .R'Last;
            TIM2EN   at 0 range TIM2EN  .R'First .. TIM2EN  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB1ENR;

   subtype APB1ENR_T is APB1ENR.T;

   --  Field definitions

   function DACEN    is new APB1ENR.FN.B (APB1ENR.DACEN);
   function PWREN    is new APB1ENR.FN.B (APB1ENR.PWREN);
   function BKPEN    is new APB1ENR.FN.B (APB1ENR.BKPEN);
   function CAN2EN   is new APB1ENR.FN.B (APB1ENR.CAN2EN);
   function CAN1EN   is new APB1ENR.FN.B (APB1ENR.CAN1EN);
   function I2C2EN   is new APB1ENR.FN.B (APB1ENR.I2C2EN);
   function I2C1EN   is new APB1ENR.FN.B (APB1ENR.I2C1EN);
   function UART5EN  is new APB1ENR.FN.B (APB1ENR.UART5EN);
   function UART4EN  is new APB1ENR.FN.B (APB1ENR.UART4EN);
   function USART3EN is new APB1ENR.FN.B (APB1ENR.USART3EN);
   function USART2EN is new APB1ENR.FN.B (APB1ENR.USART2EN);
   function SPI3EN   is new APB1ENR.FN.B (APB1ENR.SPI3EN);
   function SPI2EN   is new APB1ENR.FN.B (APB1ENR.SPI2EN);
   function WWDGEN   is new APB1ENR.FN.B (APB1ENR.WWDGEN);
   function TIM7EN   is new APB1ENR.FN.B (APB1ENR.TIM7EN);
   function TIM6EN   is new APB1ENR.FN.B (APB1ENR.TIM6EN);
   function TIM5EN   is new APB1ENR.FN.B (APB1ENR.TIM5EN);
   function TIM4EN   is new APB1ENR.FN.B (APB1ENR.TIM4EN);
   function TIM3EN   is new APB1ENR.FN.B (APB1ENR.TIM3EN);
   function TIM2EN   is new APB1ENR.FN.B (APB1ENR.TIM2EN);

   --  Functions

   function  "+"   is new APB1ENR.FN.Add;
   function  "+"   is new APB1ENR.FN.Add_RM;
   function  "-"   is new APB1ENR.FN.Clear;
   function  Init  is new APB1ENR.FN.Init;

   ---------------------
   --  BDCR Register --
   ---------------------

   package BDCR is

      package Tp is new Types (R32);

      package BDRTS  is new Bitfield (Tp, 16);
      package RTCEN  is new Bitfield (Tp, 15);
      package RTCSEL is new Bitfield (Tp,  8, 2);
      package LSEBYP is new Bitfield (Tp,  2);
      package LSERDY is new Bitfield (Tp,  1);
      package LSEON  is new Bitfield (Tp,  0);

      type T is
         record
            BDRTS  : BDCR.BDRTS .T;
            RTCEN  : BDCR.RTCEN .T;
            RTCSEL : BDCR.RTCSEL.T;
            LSEBYP : BDCR.LSEBYP.T;
            LSERDY : BDCR.LSERDY.T;
            LSEON  : BDCR.LSEON .T;
         end record;

      for T use
         record
            BDRTS  at 0 range BDRTS .R'First .. BDRTS .R'Last;
            RTCEN  at 0 range RTCEN .R'First .. RTCEN .R'Last;
            RTCSEL at 0 range RTCSEL.R'First .. RTCSEL.R'Last;
            LSEBYP at 0 range LSEBYP.R'First .. LSEBYP.R'Last;
            LSERDY at 0 range LSERDY.R'First .. LSERDY.R'Last;
            LSEON  at 0 range LSEON .R'First .. LSEON .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end BDCR;

   subtype BDCR_T is BDCR.T;

   --  Field definitions

   function BDRTS  is new BDCR.FN.B (BDCR.BDRTS);
   function RTCEN  is new BDCR.FN.B (BDCR.RTCEN);
   function RTCSEL is new BDCR.FN.B (BDCR.RTCSEL);
   function LSEBYP is new BDCR.FN.B (BDCR.LSEBYP);
   function LSERDY is new BDCR.FN.B (BDCR.LSERDY);
   function LSEON  is new BDCR.FN.B (BDCR.LSEON);

   --  Functions

   function  "+"   is new BDCR.FN.Add;
   function  "+"   is new BDCR.FN.Add_RM;
   function  "-"   is new BDCR.FN.Clear;
   function  Init  is new BDCR.FN.Init;

   --  Constant definitions

   function NoClock    is new BDCR.FN.C (BDCR.RTCSEL, 2#00#);
   function LSE        is new BDCR.FN.C (BDCR.RTCSEL, 2#01#);
   function LSI        is new BDCR.FN.C (BDCR.RTCSEL, 2#10#);
   function HSE_Div128 is new BDCR.FN.C (BDCR.RTCSEL, 2#11#);

   ---------------------
   --  CSR Register --
   ---------------------

   package CSR is

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
            LPWRRSTF : CSR.LPWRRSTF.T;
            WWDGRSTF : CSR.WWDGRSTF.T;
            IWDGRSTF : CSR.IWDGRSTF.T;
            SFTRSTF  : CSR.SFTRSTF .T;
            PORRSTF  : CSR.PORRSTF .T;
            PINRSTF  : CSR.PINRSTF .T;
            RMVF     : CSR.RMVF    .T;
            LSIRDY   : CSR.LSIRDY  .T;
            LSION    : CSR.LSION   .T;
         end record;

      for T use
         record
            LPWRRSTF at 0 range LPWRRSTF.R'First .. LPWRRSTF.R'Last;
            WWDGRSTF at 0 range WWDGRSTF.R'First .. WWDGRSTF.R'Last;
            IWDGRSTF at 0 range IWDGRSTF.R'First .. IWDGRSTF.R'Last;
            SFTRSTF  at 0 range SFTRSTF .R'First .. SFTRSTF .R'Last;
            PORRSTF  at 0 range PORRSTF .R'First .. PORRSTF .R'Last;
            PINRSTF  at 0 range PINRSTF .R'First .. PINRSTF .R'Last;
            RMVF     at 0 range RMVF    .R'First .. RMVF    .R'Last;
            LSIRDY   at 0 range LSIRDY  .R'First .. LSIRDY  .R'Last;
            LSION    at 0 range LSION   .R'First .. LSION   .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CSR;

   subtype CSR_T is CSR.T;

   --  Field definitions

   function LPWRRSTF is new CSR.FN.B (CSR.LPWRRSTF);
   function WWDGRSTF is new CSR.FN.B (CSR.WWDGRSTF);
   function IWDGRSTF is new CSR.FN.B (CSR.IWDGRSTF);
   function SFTRSTF  is new CSR.FN.B (CSR.SFTRSTF);
   function PORRSTF  is new CSR.FN.B (CSR.PORRSTF);
   function PINRSTF  is new CSR.FN.B (CSR.PINRSTF);
   function RMVF     is new CSR.FN.B (CSR.RMVF);
   function LSIRDY   is new CSR.FN.B (CSR.LSIRDY);
   function LSION    is new CSR.FN.B (CSR.LSION);

   --  Functions

   function  "+"   is new CSR.FN.Add;
   function  "+"   is new CSR.FN.Add_RM;
   function  "-"   is new CSR.FN.Clear;
   function  Init  is new CSR.FN.Init;

   ---------------------
   --  AHBRSTR Register --
   ---------------------

   package AHBRSTR is

      package Tp is new Types (R32);

      package ETHMACRST is new Bitfield (Tp, 14);
      package OTGFSRST  is new Bitfield (Tp, 12);

      type T is
         record
            ETHMACRST : AHBRSTR.ETHMACRST.T;
            OTGFSRST  : AHBRSTR.OTGFSRST .T;
         end record;

      for T use
         record
            ETHMACRST at 0 range ETHMACRST.R'First .. ETHMACRST.R'Last;
            OTGFSRST  at 0 range OTGFSRST .R'First .. OTGFSRST .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHBRSTR;

   subtype AHBRSTR_T is AHBRSTR.T;

   --  Field definitions

   function ETHMACRST is new AHBRSTR.FN.B (AHBRSTR.ETHMACRST);
   function OTGFSRST  is new AHBRSTR.FN.B (AHBRSTR.OTGFSRST);

   --  Functions

   function  "+"   is new AHBRSTR.FN.Add;
   function  "+"   is new AHBRSTR.FN.Add_RM;
   function  "-"   is new AHBRSTR.FN.Clear;
   function  Init  is new AHBRSTR.FN.Init;

   ---------------------
   --  CFGR2 Register --
   ---------------------

   package CFGR2 is

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
            I2S3SRC    : CFGR2.I2S3SRC   .T;
            I2S2SRC    : CFGR2.I2S2SRC   .T;
            PREDIV1SRC : CFGR2.PREDIV1SRC.T;
            PLL3MUL    : CFGR2.PLL3MUL   .T;
            PLL2MUL    : CFGR2.PLL2MUL   .T;
            PREDIV2    : CFGR2.PREDIV2   .T;
            PREDIV1    : CFGR2.PREDIV1   .T;
         end record;

      for T use
         record
            I2S3SRC    at 0 range I2S3SRC   .R'First .. I2S3SRC   .R'Last;
            I2S2SRC    at 0 range I2S2SRC   .R'First .. I2S2SRC   .R'Last;
            PREDIV1SRC at 0 range PREDIV1SRC.R'First .. PREDIV1SRC.R'Last;
            PLL3MUL    at 0 range PLL3MUL   .R'First .. PLL3MUL   .R'Last;
            PLL2MUL    at 0 range PLL2MUL   .R'First .. PLL2MUL   .R'Last;
            PREDIV2    at 0 range PREDIV2   .R'First .. PREDIV2   .R'Last;
            PREDIV1    at 0 range PREDIV1   .R'First .. PREDIV1   .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CFGR2;

   subtype CFGR2_T is CFGR2.T;

   --  Field definitions

   function I2S3SRC    is new CFGR2.FN.B (CFGR2.I2S3SRC);
   function I2S2SRC    is new CFGR2.FN.B (CFGR2.I2S2SRC);
   function PREDIV1SRC is new CFGR2.FN.B (CFGR2.PREDIV1SRC);
   function PLL3MUL    is new CFGR2.FN.B (CFGR2.PLL3MUL);
   function PLL2MUL    is new CFGR2.FN.B (CFGR2.PLL2MUL);
   function PREDIV2    is new CFGR2.FN.B (CFGR2.PREDIV2);
   function PREDIV1    is new CFGR2.FN.B (CFGR2.PREDIV1);

   --  Functions

   function  "+"   is new CFGR2.FN.Add;
   function  "+"   is new CFGR2.FN.Add_RM;
   function  "-"   is new CFGR2.FN.Clear;
   function  Init  is new CFGR2.FN.Init;

   --  Constant definitions

   function PLLx8  is new CFGR2.FN.C (CFGR2.PLL3MUL, 2#0110#);
   function PLLx9  is new CFGR2.FN.C (CFGR2.PLL3MUL, 2#0111#);
   function PLLx10 is new CFGR2.FN.C (CFGR2.PLL3MUL, 2#1000#);
   function PLLx11 is new CFGR2.FN.C (CFGR2.PLL3MUL, 2#1001#);
   function PLLx12 is new CFGR2.FN.C (CFGR2.PLL3MUL, 2#1010#);
   function PLLx13 is new CFGR2.FN.C (CFGR2.PLL3MUL, 2#1011#);
   function PLLx14 is new CFGR2.FN.C (CFGR2.PLL3MUL, 2#1100#);
   function PLLx16 is new CFGR2.FN.C (CFGR2.PLL3MUL, 2#1110#);
   function PLLx20 is new CFGR2.FN.C (CFGR2.PLL3MUL, 2#1111#);

   function PLLx8  is new CFGR2.FN.C (CFGR2.PLL2MUL, 2#0110#);
   function PLLx9  is new CFGR2.FN.C (CFGR2.PLL2MUL, 2#0111#);
   function PLLx10 is new CFGR2.FN.C (CFGR2.PLL2MUL, 2#1000#);
   function PLLx11 is new CFGR2.FN.C (CFGR2.PLL2MUL, 2#1001#);
   function PLLx12 is new CFGR2.FN.C (CFGR2.PLL2MUL, 2#1010#);
   function PLLx13 is new CFGR2.FN.C (CFGR2.PLL2MUL, 2#1011#);
   function PLLx14 is new CFGR2.FN.C (CFGR2.PLL2MUL, 2#1100#);
   function PLLx16 is new CFGR2.FN.C (CFGR2.PLL2MUL, 2#1110#);
   function PLLx20 is new CFGR2.FN.C (CFGR2.PLL2MUL, 2#1111#);

   function Div1   is new CFGR2.FN.C (CFGR2.PREDIV2, 2#0000#);
   function Div2   is new CFGR2.FN.C (CFGR2.PREDIV2, 2#0001#);
   function Div3   is new CFGR2.FN.C (CFGR2.PREDIV2, 2#0010#);
   function Div4   is new CFGR2.FN.C (CFGR2.PREDIV2, 2#0011#);
   function Div5   is new CFGR2.FN.C (CFGR2.PREDIV2, 2#0100#);
   function Div6   is new CFGR2.FN.C (CFGR2.PREDIV2, 2#0101#);
   function Div7   is new CFGR2.FN.C (CFGR2.PREDIV2, 2#0110#);
   function Div8   is new CFGR2.FN.C (CFGR2.PREDIV2, 2#0111#);
   function Div9   is new CFGR2.FN.C (CFGR2.PREDIV2, 2#1000#);
   function Div10  is new CFGR2.FN.C (CFGR2.PREDIV2, 2#1001#);
   function Div11  is new CFGR2.FN.C (CFGR2.PREDIV2, 2#1010#);
   function Div12  is new CFGR2.FN.C (CFGR2.PREDIV2, 2#1011#);
   function Div13  is new CFGR2.FN.C (CFGR2.PREDIV2, 2#1100#);
   function Div14  is new CFGR2.FN.C (CFGR2.PREDIV2, 2#1101#);
   function Div15  is new CFGR2.FN.C (CFGR2.PREDIV2, 2#1110#);
   function Div16  is new CFGR2.FN.C (CFGR2.PREDIV2, 2#1111#);

   function Div1   is new CFGR2.FN.C (CFGR2.PREDIV1, 2#0000#);
   function Div2   is new CFGR2.FN.C (CFGR2.PREDIV1, 2#0001#);
   function Div3   is new CFGR2.FN.C (CFGR2.PREDIV1, 2#0010#);
   function Div4   is new CFGR2.FN.C (CFGR2.PREDIV1, 2#0011#);
   function Div5   is new CFGR2.FN.C (CFGR2.PREDIV1, 2#0100#);
   function Div6   is new CFGR2.FN.C (CFGR2.PREDIV1, 2#0101#);
   function Div7   is new CFGR2.FN.C (CFGR2.PREDIV1, 2#0110#);
   function Div8   is new CFGR2.FN.C (CFGR2.PREDIV1, 2#0111#);
   function Div9   is new CFGR2.FN.C (CFGR2.PREDIV1, 2#1000#);
   function Div10  is new CFGR2.FN.C (CFGR2.PREDIV1, 2#1001#);
   function Div11  is new CFGR2.FN.C (CFGR2.PREDIV1, 2#1010#);
   function Div12  is new CFGR2.FN.C (CFGR2.PREDIV1, 2#1011#);
   function Div13  is new CFGR2.FN.C (CFGR2.PREDIV1, 2#1100#);
   function Div14  is new CFGR2.FN.C (CFGR2.PREDIV1, 2#1101#);
   function Div15  is new CFGR2.FN.C (CFGR2.PREDIV1, 2#1110#);
   function Div16  is new CFGR2.FN.C (CFGR2.PREDIV1, 2#1111#);

   --------------------------------------------------------------------------
   --                         Register definition                          --
   --------------------------------------------------------------------------

   type RCC_T is
      record
         CR       : CR_T;
         CFGR     : CFGR_T;
         CIR      : CIR_T;
         APB2RSTR : APB2RSTR_T;
         APB1RSTR : APB1RSTR_T;
         AHBENR   : AHBENR_T;
         APB2ENR  : APB2ENR_T;
         APB1ENR  : APB1ENR_T;
         BDCR     : BDCR_T;
         CSR      : CSR_T;
         AHBRSTR  : AHBRSTR_T;
         CFGR2    : CFGR2_T;
         pragma Volatile (CR);
         pragma Volatile (CFGR);
         pragma Volatile (CIR);
         pragma Volatile (APB2RSTR);
         pragma Volatile (APB1RSTR);
         pragma Volatile (AHBENR);
         pragma Volatile (APB2ENR);
         pragma Volatile (APB1ENR);
         pragma Volatile (BDCR);
         pragma Volatile (CSR);
         pragma Volatile (AHBRSTR);
         pragma Volatile (CFGR2);
      end record;

   for RCC_T use
      record
         CR       at 16#00# range 0 .. 31;
         CFGR     at 16#04# range 0 .. 31;
         CIR      at 16#08# range 0 .. 31;
         APB2RSTR at 16#0C# range 0 .. 31;
         APB1RSTR at 16#10# range 0 .. 31;
         AHBENR   at 16#14# range 0 .. 31;
         APB2ENR  at 16#18# range 0 .. 31;
         APB1ENR  at 16#1C# range 0 .. 31;
         BDCR     at 16#20# range 0 .. 31;
         CSR      at 16#24# range 0 .. 31;
         AHBRSTR  at 16#28# range 0 .. 31;
         CFGR2    at 16#2C# range 0 .. 31;
      end record;

   RCC : RCC_T;

   for RCC'Address use System'To_Address (16#4002_1000#);

end ARM.Registers.RCC_F1XXXX;
