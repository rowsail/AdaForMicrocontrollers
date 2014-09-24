--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--               A R M . R e g i s t e r s . R C C _ F 3 7 X X X              --
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

--  Package defines STM32 F37XXX family reset & clock related registers

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
--                           ARM.Registers.RCC_F37XXX                         --
--------------------------------------------------------------------------------

package ARM.Registers.RCC_F37XXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                         Reset and Clock                             --
   -------------------------------------------------------------------------

   --     RCC Registers Collection  --

   ------------------------
   --  Control Register  --
   ------------------------

   package CR is

      package Tp is new Types (R32);

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

   ------------------------------
   --  Configuration Register  --
   ------------------------------

   package CFGR is

      package Tp is new Types (R32);

      package SDPRE    is new Bitfield (Tp, 27, 5);
      package MCO      is new Bitfield (Tp, 24, 3);
      package USBPRE   is new Bitfield (Tp, 22);
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
            SDPRE    : CFGR.SDPRE   .T;
            MCO      : CFGR.MCO     .T;
            USBPRE   : CFGR.USBPRE  .T;
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
            SDPRE    at 0 range SDPRE   .R'First .. SDPRE   .R'Last;
            MCO      at 0 range MCO     .R'First .. MCO     .R'Last;
            USBPRE   at 0 range USBPRE  .R'First .. USBPRE  .R'Last;
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

   function SDPRE    is new CFGR.FN.B (CFGR.SDPRE);
   function MCO      is new CFGR.FN.B (CFGR.MCO);
   function USBPRE   is new CFGR.FN.B (CFGR.USBPRE);
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

   function Div2         is new CFGR.FN.C (CFGR.SDPRE, 2#10000#);
   function Div4         is new CFGR.FN.C (CFGR.SDPRE, 2#10001#);
   function Div6         is new CFGR.FN.C (CFGR.SDPRE, 2#10010#);
   function Div8         is new CFGR.FN.C (CFGR.SDPRE, 2#10011#);
   function Div10        is new CFGR.FN.C (CFGR.SDPRE, 2#10100#);
   function Div12        is new CFGR.FN.C (CFGR.SDPRE, 2#10101#);
   function Div14        is new CFGR.FN.C (CFGR.SDPRE, 2#10110#);
   function Div16        is new CFGR.FN.C (CFGR.SDPRE, 2#10111#);
   function Div20        is new CFGR.FN.C (CFGR.SDPRE, 2#11000#);
   function Div24        is new CFGR.FN.C (CFGR.SDPRE, 2#11001#);
   function Div28        is new CFGR.FN.C (CFGR.SDPRE, 2#11010#);
   function Div32        is new CFGR.FN.C (CFGR.SDPRE, 2#11011#);
   function Div36        is new CFGR.FN.C (CFGR.SDPRE, 2#11100#);
   function Div40        is new CFGR.FN.C (CFGR.SDPRE, 2#11101#);
   function Div44        is new CFGR.FN.C (CFGR.SDPRE, 2#11110#);
   function Div48        is new CFGR.FN.C (CFGR.SDPRE, 2#11111#);

   function NoClock               is new CFGR.FN.C (CFGR.MCO, 2#000#);
   function LSI_Clock_Selected    is new CFGR.FN.C (CFGR.MCO, 2#010#);
   function LSE_Clock_Selected    is new CFGR.FN.C (CFGR.MCO, 2#011#);
   function System_Clock_Selected is new CFGR.FN.C (CFGR.MCO, 2#100#);
   function HSI_Clock_Selected    is new CFGR.FN.C (CFGR.MCO, 2#101#);
   function HSE_Clock_Selected    is new CFGR.FN.C (CFGR.MCO, 2#110#);
   function PLL_Clock_Div_By_2    is new CFGR.FN.C (CFGR.MCO, 2#111#);

   function PLL_Clock_Div_By_1_5  is new CFGR.FN.C (CFGR.USBPRE, 2#0#);
   function PLL_Clock_Not_Divided is new CFGR.FN.C (CFGR.USBPRE, 2#1#);

   function PLLx2   is new CFGR.FN.C (CFGR.PLLMUL, 2#0000#);
   function PLLx3   is new CFGR.FN.C (CFGR.PLLMUL, 2#0001#);
   function PLLx4   is new CFGR.FN.C (CFGR.PLLMUL, 2#0010#);
   function PLLx5   is new CFGR.FN.C (CFGR.PLLMUL, 2#0011#);
   function PLLx6   is new CFGR.FN.C (CFGR.PLLMUL, 2#0100#);
   function PLLx7   is new CFGR.FN.C (CFGR.PLLMUL, 2#0101#);
   function PLLx8   is new CFGR.FN.C (CFGR.PLLMUL, 2#0110#);
   function PLLx9   is new CFGR.FN.C (CFGR.PLLMUL, 2#0111#);
   function PLLx10  is new CFGR.FN.C (CFGR.PLLMUL, 2#1000#);
   function PLLx11  is new CFGR.FN.C (CFGR.PLLMUL, 2#1001#);
   function PLLx12  is new CFGR.FN.C (CFGR.PLLMUL, 2#1010#);
   function PLLx13  is new CFGR.FN.C (CFGR.PLLMUL, 2#1011#);
   function PLLx14  is new CFGR.FN.C (CFGR.PLLMUL, 2#1100#);
   function PLLx15  is new CFGR.FN.C (CFGR.PLLMUL, 2#1101#);
   function PLLx16  is new CFGR.FN.C (CFGR.PLLMUL, 2#1110#);

   function HSE_Not_Divided  is new CFGR.FN.C (CFGR.PLLXTPRE, 2#0#);
   function HSE_Divided_By_2 is new CFGR.FN.C (CFGR.PLLXTPRE, 2#1#);

   function HSI_Div2       is new CFGR.FN.C (CFGR.PLLSRC, 2#0#);
   function HES_DIV_PREDIV is new CFGR.FN.C (CFGR.PLLSRC, 2#1#);

   function PCLK_Divided_By_2 is new CFGR.FN.C (CFGR.ADCPRE, 2#00#);
   function PCLK_Divided_By_4 is new CFGR.FN.C (CFGR.ADCPRE, 2#01#);
   function PCLK_Divided_By_6 is new CFGR.FN.C (CFGR.ADCPRE, 2#10#);
   function PCLK_Divided_By_8 is new CFGR.FN.C (CFGR.ADCPRE, 2#11#);

   function AHB_Clock_Not_Divided   is new CFGR.FN.C (CFGR.PPRE2, 2#000#);
   function AHB_Clock_Divided_By_2  is new CFGR.FN.C (CFGR.PPRE2, 2#100#);
   function AHB_Clock_Divided_By_4  is new CFGR.FN.C (CFGR.PPRE2, 2#101#);
   function AHB_Clock_Divided_By_8  is new CFGR.FN.C (CFGR.PPRE2, 2#110#);
   function AHB_Clock_Divided_By_16 is new CFGR.FN.C (CFGR.PPRE2, 2#111#);

   function APB1_Clock_Not_Divided   is new CFGR.FN.C (CFGR.PPRE1, 2#000#);
   function APB1_Clock_Divided_By_2  is new CFGR.FN.C (CFGR.PPRE1, 2#100#);
   function APB1_Clock_Divided_By_4  is new CFGR.FN.C (CFGR.PPRE1, 2#101#);
   function APB1_Clock_Divided_By_8  is new CFGR.FN.C (CFGR.PPRE1, 2#110#);
   function APB1_Clock_Divided_By_16 is new CFGR.FN.C (CFGR.PPRE1, 2#111#);

   function SYSCLK_Not_Divided    is new CFGR.FN.C (CFGR.HPRE, 2#0000#);
   function SYSCLK_Divided_By_2   is new CFGR.FN.C (CFGR.HPRE, 2#1000#);
   function SYSCLK_Divided_By_4   is new CFGR.FN.C (CFGR.HPRE, 2#1001#);
   function SYSCLK_Divided_By_8   is new CFGR.FN.C (CFGR.HPRE, 2#1010#);
   function SYSCLK_Divided_By_16  is new CFGR.FN.C (CFGR.HPRE, 2#1011#);
   function SYSCLK_Divided_By_64  is new CFGR.FN.C (CFGR.HPRE, 2#1100#);
   function SYSCLK_Divided_By_128 is new CFGR.FN.C (CFGR.HPRE, 2#1101#);
   function SYSCLK_Divided_By_256 is new CFGR.FN.C (CFGR.HPRE, 2#1110#);
   function SYSCLK_Divided_By_512 is new CFGR.FN.C (CFGR.HPRE, 2#1111#);

   function HSI_Oscillator_Used_As_System_Clock is new CFGR.FN.C (CFGR.SWS, 2#00#);
   function HSE_Oscillator_Used_As_System_Clock is new CFGR.FN.C (CFGR.SWS, 2#01#);
   function PLLUsed_As_System_Clock             is new CFGR.FN.C (CFGR.SWS, 2#10#);

   function HSI_Selected_As_System_Clock is new CFGR.FN.C (CFGR.SW, 2#00#);
   function HSE_Selected_As_System_Clock is new CFGR.FN.C (CFGR.SW, 2#01#);
   function PLL_Selected_As_System_Clock is new CFGR.FN.C (CFGR.SW, 2#10#);

   --------------------------
   --  Interrupt Register  --
   --------------------------

   package CIR is

      package Tp is new Types (R32);

      package CSSC      is new Bitfield (Tp, 23);
      package PLLRDYC   is new Bitfield (Tp, 20);
      package HSERDYC   is new Bitfield (Tp, 19);
      package HSIRDYC   is new Bitfield (Tp, 18);
      package LSERDYC   is new Bitfield (Tp, 17);
      package LSIRDYC   is new Bitfield (Tp, 16);
      package PLLRDYIE  is new Bitfield (Tp, 12);
      package HSERDYIE  is new Bitfield (Tp, 11);
      package HSIRDYIE  is new Bitfield (Tp, 10);
      package LSERDYIE  is new Bitfield (Tp,  9);
      package LSIRDYIE  is new Bitfield (Tp,  8);
      package CSSF      is new Bitfield (Tp,  7);
      package PLLRDYF   is new Bitfield (Tp,  4);
      package HSERDYF   is new Bitfield (Tp,  3);
      package HSIRDYF   is new Bitfield (Tp,  2);
      package LSERDYF   is new Bitfield (Tp,  1);
      package LSIRDYF   is new Bitfield (Tp,  0);

      type T is
         record
            CSSC      : CIR.CSSC     .T;
            PLLRDYC   : CIR.PLLRDYC  .T;
            HSERDYC   : CIR.HSERDYC  .T;
            HSIRDYC   : CIR.HSIRDYC  .T;
            LSERDYC   : CIR.LSERDYC  .T;
            LSIRDYC   : CIR.LSIRDYC  .T;
            PLLRDYIE  : CIR.PLLRDYIE .T;
            HSERDYIE  : CIR.HSERDYIE .T;
            HSIRDYIE  : CIR.HSIRDYIE .T;
            LSERDYIE  : CIR.LSERDYIE .T;
            LSIRDYIE  : CIR.LSIRDYIE .T;
            CSSF      : CIR.CSSF     .T;
            PLLRDYF   : CIR.PLLRDYF  .T;
            HSERDYF   : CIR.HSERDYF  .T;
            HSIRDYF   : CIR.HSIRDYF  .T;
            LSERDYF   : CIR.LSERDYF  .T;
            LSIRDYF   : CIR.LSIRDYF  .T;
         end record;

      for T use
         record
            CSSC      at 0 range CSSC     .R'First .. CSSC     .R'Last;
            PLLRDYC   at 0 range PLLRDYC  .R'First .. PLLRDYC  .R'Last;
            HSERDYC   at 0 range HSERDYC  .R'First .. HSERDYC  .R'Last;
            HSIRDYC   at 0 range HSIRDYC  .R'First .. HSIRDYC  .R'Last;
            LSERDYC   at 0 range LSERDYC  .R'First .. LSERDYC  .R'Last;
            LSIRDYC   at 0 range LSIRDYC  .R'First .. LSIRDYC  .R'Last;
            PLLRDYIE  at 0 range PLLRDYIE .R'First .. PLLRDYIE .R'Last;
            HSERDYIE  at 0 range HSERDYIE .R'First .. HSERDYIE .R'Last;
            HSIRDYIE  at 0 range HSIRDYIE .R'First .. HSIRDYIE .R'Last;
            LSERDYIE  at 0 range LSERDYIE .R'First .. LSERDYIE .R'Last;
            LSIRDYIE  at 0 range LSIRDYIE .R'First .. LSIRDYIE .R'Last;
            CSSF      at 0 range CSSF     .R'First .. CSSF     .R'Last;
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
   function PLLRDYC   is new CIR.FN.B (CIR.PLLRDYC);
   function HSERDYC   is new CIR.FN.B (CIR.HSERDYC);
   function HSIRDYC   is new CIR.FN.B (CIR.HSIRDYC);
   function LSERDYC   is new CIR.FN.B (CIR.LSERDYC);
   function LSIRDYC   is new CIR.FN.B (CIR.LSIRDYC);
   function PLLRDYIE  is new CIR.FN.B (CIR.PLLRDYIE);
   function HSERDYIE  is new CIR.FN.B (CIR.HSERDYIE);
   function HSIRDYIE  is new CIR.FN.B (CIR.HSIRDYIE);
   function LSERDYIE  is new CIR.FN.B (CIR.LSERDYIE);
   function LSIRDYIE  is new CIR.FN.B (CIR.LSIRDYIE);
   function CSSF      is new CIR.FN.B (CIR.CSSF);
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

   --------------------------------------
   --  APB2 Peripheral Reset Register  --
   --------------------------------------

   package APB2RSTR is

      package Tp is new Types (R32);

      package SDA3RST   is new Bitfield (Tp, 26);
      package SDA2RST   is new Bitfield (Tp, 25);
      package SDA1RST   is new Bitfield (Tp, 24);
      package TIM19RST  is new Bitfield (Tp, 19);
      package TIM17RST  is new Bitfield (Tp, 18);
      package TIM16RST  is new Bitfield (Tp, 17);
      package TIM15RST  is new Bitfield (Tp, 16);
      package USART1RST is new Bitfield (Tp, 14);
      package SPI1RST   is new Bitfield (Tp, 12);
      package ADCRST    is new Bitfield (Tp, 9);
      package SYSCFGRST is new Bitfield (Tp, 0);

      type T is
         record
            SDA3RST   : APB2RSTR.SDA3RST  .T;
            SDA2RST   : APB2RSTR.SDA2RST  .T;
            SDA1RST   : APB2RSTR.SDA1RST  .T;
            TIM19RST  : APB2RSTR.TIM19RST .T;
            TIM17RST  : APB2RSTR.TIM17RST .T;
            TIM16RST  : APB2RSTR.TIM16RST .T;
            TIM15RST  : APB2RSTR.TIM15RST .T;
            USART1RST : APB2RSTR.USART1RST.T;
            SPI1RST   : APB2RSTR.SPI1RST  .T;
            ADCRST    : APB2RSTR.ADCRST   .T;
            SYSCFGRST : APB2RSTR.SYSCFGRST.T;
         end record;

      for T use
         record
            SDA3RST   at 0 range SDA3RST  .R'First .. SDA3RST  .R'Last;
            SDA2RST   at 0 range SDA2RST  .R'First .. SDA2RST  .R'Last;
            SDA1RST   at 0 range SDA1RST  .R'First .. SDA1RST  .R'Last;
            TIM19RST  at 0 range TIM19RST .R'First .. TIM19RST .R'Last;
            TIM17RST  at 0 range TIM17RST .R'First .. TIM17RST .R'Last;
            TIM16RST  at 0 range TIM16RST .R'First .. TIM16RST .R'Last;
            TIM15RST  at 0 range TIM15RST .R'First .. TIM15RST .R'Last;
            USART1RST at 0 range USART1RST.R'First .. USART1RST.R'Last;
            SPI1RST   at 0 range SPI1RST  .R'First .. SPI1RST  .R'Last;
            ADCRST    at 0 range ADCRST   .R'First .. ADCRST   .R'Last;
            SYSCFGRST at 0 range SYSCFGRST.R'First .. SYSCFGRST.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB2RSTR;

   subtype APB2RSTR_T is APB2RSTR.T;

   --  Field definitions

   function SDA3RST   is new APB2RSTR.FN.B (APB2RSTR.SDA3RST  );
   function SDA2RST   is new APB2RSTR.FN.B (APB2RSTR.SDA2RST  );
   function SDA1RST   is new APB2RSTR.FN.B (APB2RSTR.SDA1RST  );
   function TIM19RST  is new APB2RSTR.FN.B (APB2RSTR.TIM19RST );
   function TIM17RST  is new APB2RSTR.FN.B (APB2RSTR.TIM17RST );
   function TIM16RST  is new APB2RSTR.FN.B (APB2RSTR.TIM16RST );
   function TIM15RST  is new APB2RSTR.FN.B (APB2RSTR.TIM15RST );
   function USART1RST is new APB2RSTR.FN.B (APB2RSTR.USART1RST);
   function SPI1RST   is new APB2RSTR.FN.B (APB2RSTR.SPI1RST  );
   function ADCRST    is new APB2RSTR.FN.B (APB2RSTR.ADCRST   );
   function SYSCFGRST is new APB2RSTR.FN.B (APB2RSTR.SYSCFGRST);

   --  Functions

   function  "+"   is new APB2RSTR.FN.Add;
   function  "+"   is new APB2RSTR.FN.Add_RM;
   function  "-"   is new APB2RSTR.FN.Clear;
   function  Init  is new APB2RSTR.FN.Init;

   --  Constant definitions

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.SDA3RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.SDA3RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.SDA2RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.SDA2RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.SDA1RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.SDA1RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.TIM19RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.TIM19RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.TIM17RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.TIM17RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.TIM16RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.TIM16RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.TIM15RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.TIM15RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.USART1RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.USART1RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.SPI1RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.SPI1RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.ADCRST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.ADCRST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.SYSCFGRST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.SYSCFGRST, 2#1#);

   --------------------------------------
   --  APB1 Peripheral Reset Register  --
   --------------------------------------

   package APB1RSTR is

      package Tp is new Types (R32);

      package CECRST    is new Bitfield (Tp, 30);
      package DAC1RST   is new Bitfield (Tp, 29);
      package PWRRST    is new Bitfield (Tp, 28);
      package DAC2RST   is new Bitfield (Tp, 26);
      package CANRST    is new Bitfield (Tp, 25);
      package USBRST    is new Bitfield (Tp, 23);
      package I2C2RST   is new Bitfield (Tp, 22);
      package I2C1RST   is new Bitfield (Tp, 21);
      package USART3RST is new Bitfield (Tp, 18);
      package USART2RST is new Bitfield (Tp, 17);
      package SPI3RST   is new Bitfield (Tp, 15);
      package SPI2RST   is new Bitfield (Tp, 14);
      package WWDGRST   is new Bitfield (Tp, 11);
      package TIM18RST  is new Bitfield (Tp, 9);
      package TIM14RST  is new Bitfield (Tp, 8);
      package TIM13RST  is new Bitfield (Tp, 7);
      package TIM12RST  is new Bitfield (Tp, 6);
      package TIM7RST   is new Bitfield (Tp, 5);
      package TIM6RST   is new Bitfield (Tp, 4);
      package TIM5RST   is new Bitfield (Tp, 3);
      package TIM4RST   is new Bitfield (Tp, 2);
      package TIM3RST   is new Bitfield (Tp, 1);
      package TIM2RST   is new Bitfield (Tp, 0);

      type T is
         record
            CECRST    : APB1RSTR.CECRST   .T;
            DAC1RST   : APB1RSTR.DAC1RST  .T;
            PWRRST    : APB1RSTR.PWRRST   .T;
            DAC2RST   : APB1RSTR.DAC2RST  .T;
            CANRST    : APB1RSTR.CANRST   .T;
            USBRST    : APB1RSTR.USBRST   .T;
            I2C2RST   : APB1RSTR.I2C2RST  .T;
            I2C1RST   : APB1RSTR.I2C1RST  .T;
            USART3RST : APB1RSTR.USART3RST.T;
            USART2RST : APB1RSTR.USART2RST.T;
            SPI3RST   : APB1RSTR.SPI3RST  .T;
            SPI2RST   : APB1RSTR.SPI2RST  .T;
            WWDGRST   : APB1RSTR.WWDGRST  .T;
            TIM18RST  : APB1RSTR.TIM18RST .T;
            TIM14RST  : APB1RSTR.TIM14RST .T;
            TIM13RST  : APB1RSTR.TIM13RST .T;
            TIM12RST  : APB1RSTR.TIM12RST .T;
            TIM7RST   : APB1RSTR.TIM7RST  .T;
            TIM6RST   : APB1RSTR.TIM6RST  .T;
            TIM5RST   : APB1RSTR.TIM5RST  .T;
            TIM4RST   : APB1RSTR.TIM4RST  .T;
            TIM3RST   : APB1RSTR.TIM3RST  .T;
            TIM2RST   : APB1RSTR.TIM2RST  .T;
         end record;

      for T use
         record
            CECRST    at 0 range CECRST   .R'First .. CECRST   .R'Last;
            DAC1RST   at 0 range DAC1RST  .R'First .. DAC1RST  .R'Last;
            PWRRST    at 0 range PWRRST   .R'First .. PWRRST   .R'Last;
            DAC2RST   at 0 range DAC2RST  .R'First .. DAC2RST  .R'Last;
            CANRST    at 0 range CANRST   .R'First .. CANRST   .R'Last;
            USBRST    at 0 range USBRST   .R'First .. USBRST   .R'Last;
            I2C2RST   at 0 range I2C2RST  .R'First .. I2C2RST  .R'Last;
            I2C1RST   at 0 range I2C1RST  .R'First .. I2C1RST  .R'Last;
            USART3RST at 0 range USART3RST.R'First .. USART3RST.R'Last;
            USART2RST at 0 range USART2RST.R'First .. USART2RST.R'Last;
            SPI3RST   at 0 range SPI3RST  .R'First .. SPI3RST  .R'Last;
            SPI2RST   at 0 range SPI2RST  .R'First .. SPI2RST  .R'Last;
            WWDGRST   at 0 range WWDGRST  .R'First .. WWDGRST  .R'Last;
            TIM18RST  at 0 range TIM18RST .R'First .. TIM18RST .R'Last;
            TIM14RST  at 0 range TIM14RST .R'First .. TIM14RST .R'Last;
            TIM13RST  at 0 range TIM13RST .R'First .. TIM13RST .R'Last;
            TIM12RST  at 0 range TIM12RST .R'First .. TIM12RST .R'Last;
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

   function CECRST    is new APB1RSTR.FN.B (APB1RSTR.CECRST   );
   function DAC1RST   is new APB1RSTR.FN.B (APB1RSTR.DAC1RST  );
   function PWRRST    is new APB1RSTR.FN.B (APB1RSTR.PWRRST   );
   function DAC2RST   is new APB1RSTR.FN.B (APB1RSTR.DAC2RST  );
   function CANRST    is new APB1RSTR.FN.B (APB1RSTR.CANRST   );
   function USBRST    is new APB1RSTR.FN.B (APB1RSTR.USBRST   );
   function I2C2RST   is new APB1RSTR.FN.B (APB1RSTR.I2C2RST  );
   function I2C1RST   is new APB1RSTR.FN.B (APB1RSTR.I2C1RST  );
   function USART3RST is new APB1RSTR.FN.B (APB1RSTR.USART3RST);
   function USART2RST is new APB1RSTR.FN.B (APB1RSTR.USART2RST);
   function SPI3RST   is new APB1RSTR.FN.B (APB1RSTR.SPI3RST  );
   function SPI2RST   is new APB1RSTR.FN.B (APB1RSTR.SPI2RST  );
   function WWDGRST   is new APB1RSTR.FN.B (APB1RSTR.WWDGRST  );
   function TIM18RST  is new APB1RSTR.FN.B (APB1RSTR.TIM18RST );
   function TIM14RST  is new APB1RSTR.FN.B (APB1RSTR.TIM14RST );
   function TIM13RST  is new APB1RSTR.FN.B (APB1RSTR.TIM13RST );
   function TIM12RST  is new APB1RSTR.FN.B (APB1RSTR.TIM12RST );
   function TIM7RST   is new APB1RSTR.FN.B (APB1RSTR.TIM7RST  );
   function TIM6RST   is new APB1RSTR.FN.B (APB1RSTR.TIM6RST  );
   function TIM5RST   is new APB1RSTR.FN.B (APB1RSTR.TIM5RST  );
   function TIM4RST   is new APB1RSTR.FN.B (APB1RSTR.TIM4RST  );
   function TIM3RST   is new APB1RSTR.FN.B (APB1RSTR.TIM3RST  );
   function TIM2RST   is new APB1RSTR.FN.B (APB1RSTR.TIM2RST  );

   --  Functions

   function  "+"   is new APB1RSTR.FN.Add;
   function  "+"   is new APB1RSTR.FN.Add_RM;
   function  "-"   is new APB1RSTR.FN.Clear;
   function  Init  is new APB1RSTR.FN.Init;

   --  Constant definitions

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.CECRST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.CECRST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.DAC1RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.DAC1RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.PWRRST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.PWRRST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.DAC2RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.DAC2RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.CANRST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.CANRST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.USBRST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.USBRST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.I2C2RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.I2C2RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.I2C1RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.I2C1RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.USART3RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.USART3RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.USART2RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.USART2RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.SPI3RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.SPI3RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.SPI2RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.SPI2RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.WWDGRST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.WWDGRST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.TIM18RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.TIM18RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.TIM14RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.TIM14RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.TIM13RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.TIM13RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.TIM12RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.TIM12RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.TIM7RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.TIM7RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.TIM6RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.TIM6RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.TIM5RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.TIM5RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.TIM4RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.TIM4RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.TIM3RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.TIM3RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.TIM2RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.TIM2RST, 2#1#);

   --------------------------------------------
   --  AHB Peripheral Clock Enable Register  --
   --------------------------------------------


   package AHBENR is

      package Tp is new Types (R32);

      package TSCEN   is new Bitfield (Tp, 24);
      package IOPFEN  is new Bitfield (Tp, 22);
      package IOPEEN  is new Bitfield (Tp, 21);
      package IOPDEN  is new Bitfield (Tp, 20);
      package IOPCEN  is new Bitfield (Tp, 19);
      package IOPBEN  is new Bitfield (Tp, 18);
      package IOPAEN  is new Bitfield (Tp, 17);
      package CRCEN   is new Bitfield (Tp, 6);
      package FLITFEN is new Bitfield (Tp, 4);
      package SRAMEN  is new Bitfield (Tp, 2);
      package DMA2EN  is new Bitfield (Tp, 1);
      package DMAEN   is new Bitfield (Tp, 0);

      type T is
         record
            TSCEN   : AHBENR.TSCEN  .T;
            IOPFEN  : AHBENR.IOPFEN .T;
            IOPEEN  : AHBENR.IOPEEN .T;
            IOPDEN  : AHBENR.IOPDEN .T;
            IOPCEN  : AHBENR.IOPCEN .T;
            IOPBEN  : AHBENR.IOPBEN .T;
            IOPAEN  : AHBENR.IOPAEN .T;
            CRCEN   : AHBENR.CRCEN  .T;
            FLITFEN : AHBENR.FLITFEN.T;
            SRAMEN  : AHBENR.SRAMEN .T;
            DMA2EN  : AHBENR.DMA2EN .T;
            DMAEN   : AHBENR.DMAEN  .T;
         end record;

      for T use
         record
            TSCEN   at 0 range TSCEN  .R'First .. TSCEN  .R'Last;
            IOPFEN  at 0 range IOPFEN .R'First .. IOPFEN .R'Last;
            IOPEEN  at 0 range IOPEEN .R'First .. IOPEEN .R'Last;
            IOPDEN  at 0 range IOPDEN .R'First .. IOPDEN .R'Last;
            IOPCEN  at 0 range IOPCEN .R'First .. IOPCEN .R'Last;
            IOPBEN  at 0 range IOPBEN .R'First .. IOPBEN .R'Last;
            IOPAEN  at 0 range IOPAEN .R'First .. IOPAEN .R'Last;
            CRCEN   at 0 range CRCEN  .R'First .. CRCEN  .R'Last;
            FLITFEN at 0 range FLITFEN.R'First .. FLITFEN.R'Last;
            SRAMEN  at 0 range SRAMEN .R'First .. SRAMEN .R'Last;
            DMA2EN  at 0 range DMA2EN .R'First .. DMA2EN .R'Last;
            DMAEN   at 0 range DMAEN  .R'First .. DMAEN  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHBENR;

   subtype AHBENR_T is AHBENR.T;

   --  Field definitions

   function TSCEN   is new AHBENR.FN.B (AHBENR.TSCEN  );
   function IOPFEN  is new AHBENR.FN.B (AHBENR.IOPFEN );
   function IOPEEN  is new AHBENR.FN.B (AHBENR.IOPEEN );
   function IOPDEN  is new AHBENR.FN.B (AHBENR.IOPDEN );
   function IOPCEN  is new AHBENR.FN.B (AHBENR.IOPCEN );
   function IOPBEN  is new AHBENR.FN.B (AHBENR.IOPBEN );
   function IOPAEN  is new AHBENR.FN.B (AHBENR.IOPAEN );
   function CRCEN   is new AHBENR.FN.B (AHBENR.CRCEN  );
   function FLITFEN is new AHBENR.FN.B (AHBENR.FLITFEN);
   function SRAMEN  is new AHBENR.FN.B (AHBENR.SRAMEN );
   function DMA2EN  is new AHBENR.FN.B (AHBENR.DMA2EN );
   function DMAEN   is new AHBENR.FN.B (AHBENR.DMAEN  );

   --  Functions

   function  "+"   is new AHBENR.FN.Add;
   function  "+"   is new AHBENR.FN.Add_RM;
   function  "-"   is new AHBENR.FN.Clear;
   function  Init  is new AHBENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.TSCEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.TSCEN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.IOPFEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.IOPFEN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.IOPEEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.IOPEEN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.IOPDEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.IOPDEN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.IOPCEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.IOPCEN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.IOPBEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.IOPBEN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.IOPAEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.IOPAEN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.CRCEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.CRCEN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.FLITFEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.FLITFEN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.SRAMEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.SRAMEN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.DMA2EN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.DMA2EN, 2#1#);

   function Clock_Disabled is new AHBENR.FN.C (AHBENR.DMAEN, 2#0#);
   function Clock_Enabled  is new AHBENR.FN.C (AHBENR.DMAEN, 2#1#);


   ---------------------------------------------
   --  APB2 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package APB2ENR is

      package Tp is new Types (R32);

      package SDA3EN   is new Bitfield (Tp, 26);
      package SDA2EN   is new Bitfield (Tp, 25);
      package SDA1EN   is new Bitfield (Tp, 24);
      package TIM19EN  is new Bitfield (Tp, 19);
      package TIM17EN  is new Bitfield (Tp, 18);
      package TIM16EN  is new Bitfield (Tp, 17);
      package TIM15EN  is new Bitfield (Tp, 16);
      package USART1EN is new Bitfield (Tp, 14);
      package SPI1EN   is new Bitfield (Tp, 12);
      package ADCEN    is new Bitfield (Tp, 9);
      package SYSCFGEN is new Bitfield (Tp, 0);

      type T is
         record
            SDA3EN   : APB2ENR.SDA3EN  .T;
            SDA2EN   : APB2ENR.SDA2EN  .T;
            SDA1EN   : APB2ENR.SDA1EN  .T;
            TIM19EN  : APB2ENR.TIM19EN .T;
            TIM17EN  : APB2ENR.TIM17EN .T;
            TIM16EN  : APB2ENR.TIM16EN .T;
            TIM15EN  : APB2ENR.TIM15EN .T;
            USART1EN : APB2ENR.USART1EN.T;
            SPI1EN   : APB2ENR.SPI1EN  .T;
            ADCEN    : APB2ENR.ADCEN   .T;
            SYSCFGEN : APB2ENR.SYSCFGEN.T;
         end record;

      for T use
         record
            SDA3EN   at 0 range SDA3EN  .R'First .. SDA3EN  .R'Last;
            SDA2EN   at 0 range SDA2EN  .R'First .. SDA2EN  .R'Last;
            SDA1EN   at 0 range SDA1EN  .R'First .. SDA1EN  .R'Last;
            TIM19EN  at 0 range TIM19EN .R'First .. TIM19EN .R'Last;
            TIM17EN  at 0 range TIM17EN .R'First .. TIM17EN .R'Last;
            TIM16EN  at 0 range TIM16EN .R'First .. TIM16EN .R'Last;
            TIM15EN  at 0 range TIM15EN .R'First .. TIM15EN .R'Last;
            USART1EN at 0 range USART1EN.R'First .. USART1EN.R'Last;
            SPI1EN   at 0 range SPI1EN  .R'First .. SPI1EN  .R'Last;
            ADCEN    at 0 range ADCEN   .R'First .. ADCEN   .R'Last;
            SYSCFGEN at 0 range SYSCFGEN.R'First .. SYSCFGEN.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB2ENR;

   subtype APB2ENR_T is APB2ENR.T;

   --  Field definitions

   function SDA3EN   is new APB2ENR.FN.B (APB2ENR.SDA3EN  );
   function SDA2EN   is new APB2ENR.FN.B (APB2ENR.SDA2EN  );
   function SDA1EN   is new APB2ENR.FN.B (APB2ENR.SDA1EN  );
   function TIM19EN  is new APB2ENR.FN.B (APB2ENR.TIM19EN );
   function TIM17EN  is new APB2ENR.FN.B (APB2ENR.TIM17EN );
   function TIM16EN  is new APB2ENR.FN.B (APB2ENR.TIM16EN );
   function TIM15EN  is new APB2ENR.FN.B (APB2ENR.TIM15EN );
   function USART1EN is new APB2ENR.FN.B (APB2ENR.USART1EN);
   function SPI1EN   is new APB2ENR.FN.B (APB2ENR.SPI1EN  );
   function ADCEN    is new APB2ENR.FN.B (APB2ENR.ADCEN   );
   function SYSCFGEN is new APB2ENR.FN.B (APB2ENR.SYSCFGEN);

   --  Functions

   function  "+"   is new APB2ENR.FN.Add;
   function  "+"   is new APB2ENR.FN.Add_RM;
   function  "-"   is new APB2ENR.FN.Clear;
   function  Init  is new APB2ENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.SDA3EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.SDA3EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.SDA2EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.SDA2EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.SDA1EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.SDA1EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.TIM19EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.TIM19EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.TIM17EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.TIM17EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.TIM16EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.TIM16EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.TIM15EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.TIM15EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.USART1EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.USART1EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.SPI1EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.SPI1EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.ADCEN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.ADCEN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.SYSCFGEN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.SYSCFGEN, 2#1#);

   ---------------------------------------------
   --  APB1 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package APB1ENR is

      package Tp is new Types (R32);

      package CECEN    is new Bitfield (Tp, 30);
      package DAC1EN   is new Bitfield (Tp, 29);
      package PWREN    is new Bitfield (Tp, 28);
      package DAC2EN   is new Bitfield (Tp, 26);
      package CANEN    is new Bitfield (Tp, 25);
      package USBEN    is new Bitfield (Tp, 23);
      package I2C2EN   is new Bitfield (Tp, 22);
      package I2C1EN   is new Bitfield (Tp, 21);
      package USART3EN is new Bitfield (Tp, 18);
      package USART2EN is new Bitfield (Tp, 17);
      package SPI3EN   is new Bitfield (Tp, 15);
      package SPI2EN   is new Bitfield (Tp, 14);
      package WWDGEN   is new Bitfield (Tp, 11);
      package TIM18EN  is new Bitfield (Tp, 9);
      package TIM14EN  is new Bitfield (Tp, 8);
      package TIM13EN  is new Bitfield (Tp, 7);
      package TIM12EN  is new Bitfield (Tp, 6);
      package TIM7EN   is new Bitfield (Tp, 5);
      package TIM6EN   is new Bitfield (Tp, 4);
      package TIM5EN   is new Bitfield (Tp, 3);
      package TIM4EN   is new Bitfield (Tp, 2);
      package TIM3EN   is new Bitfield (Tp, 1);
      package TIM2EN   is new Bitfield (Tp, 0);

      type T is
         record
            CECEN    : APB1ENR.CECEN   .T;
            DAC1EN   : APB1ENR.DAC1EN  .T;
            PWREN    : APB1ENR.PWREN   .T;
            DAC2EN   : APB1ENR.DAC2EN  .T;
            CANEN    : APB1ENR.CANEN   .T;
            USBEN    : APB1ENR.USBEN   .T;
            I2C2EN   : APB1ENR.I2C2EN  .T;
            I2C1EN   : APB1ENR.I2C1EN  .T;
            USART3EN : APB1ENR.USART3EN.T;
            USART2EN : APB1ENR.USART2EN.T;
            SPI3EN   : APB1ENR.SPI3EN  .T;
            SPI2EN   : APB1ENR.SPI2EN  .T;
            WWDGEN   : APB1ENR.WWDGEN  .T;
            TIM18EN  : APB1ENR.TIM18EN .T;
            TIM14EN  : APB1ENR.TIM14EN .T;
            TIM13EN  : APB1ENR.TIM13EN .T;
            TIM12EN  : APB1ENR.TIM12EN .T;
            TIM7EN   : APB1ENR.TIM7EN  .T;
            TIM6EN   : APB1ENR.TIM6EN  .T;
            TIM5EN   : APB1ENR.TIM5EN  .T;
            TIM4EN   : APB1ENR.TIM4EN  .T;
            TIM3EN   : APB1ENR.TIM3EN  .T;
            TIM2EN   : APB1ENR.TIM2EN  .T;
         end record;

      for T use
         record
            CECEN    at 0 range CECEN   .R'First .. CECEN   .R'Last;
            DAC1EN   at 0 range DAC1EN  .R'First .. DAC1EN  .R'Last;
            PWREN    at 0 range PWREN   .R'First .. PWREN   .R'Last;
            DAC2EN   at 0 range DAC2EN  .R'First .. DAC2EN  .R'Last;
            CANEN    at 0 range CANEN   .R'First .. CANEN   .R'Last;
            USBEN    at 0 range USBEN   .R'First .. USBEN   .R'Last;
            I2C2EN   at 0 range I2C2EN  .R'First .. I2C2EN  .R'Last;
            I2C1EN   at 0 range I2C1EN  .R'First .. I2C1EN  .R'Last;
            USART3EN at 0 range USART3EN.R'First .. USART3EN.R'Last;
            USART2EN at 0 range USART2EN.R'First .. USART2EN.R'Last;
            SPI3EN   at 0 range SPI3EN  .R'First .. SPI3EN  .R'Last;
            SPI2EN   at 0 range SPI2EN  .R'First .. SPI2EN  .R'Last;
            WWDGEN   at 0 range WWDGEN  .R'First .. WWDGEN  .R'Last;
            TIM18EN  at 0 range TIM18EN .R'First .. TIM18EN .R'Last;
            TIM14EN  at 0 range TIM14EN .R'First .. TIM14EN .R'Last;
            TIM13EN  at 0 range TIM13EN .R'First .. TIM13EN .R'Last;
            TIM12EN  at 0 range TIM12EN .R'First .. TIM12EN .R'Last;
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

   function CECEN    is new APB1ENR.FN.B (APB1ENR.CECEN   );
   function DAC1EN   is new APB1ENR.FN.B (APB1ENR.DAC1EN  );
   function PWREN    is new APB1ENR.FN.B (APB1ENR.PWREN   );
   function DAC2EN   is new APB1ENR.FN.B (APB1ENR.DAC2EN  );
   function CANEN    is new APB1ENR.FN.B (APB1ENR.CANEN   );
   function USBEN    is new APB1ENR.FN.B (APB1ENR.USBEN   );
   function I2C2EN   is new APB1ENR.FN.B (APB1ENR.I2C2EN  );
   function I2C1EN   is new APB1ENR.FN.B (APB1ENR.I2C1EN  );
   function USART3EN is new APB1ENR.FN.B (APB1ENR.USART3EN);
   function USART2EN is new APB1ENR.FN.B (APB1ENR.USART2EN);
   function SPI3EN   is new APB1ENR.FN.B (APB1ENR.SPI3EN  );
   function SPI2EN   is new APB1ENR.FN.B (APB1ENR.SPI2EN  );
   function WWDGEN   is new APB1ENR.FN.B (APB1ENR.WWDGEN  );
   function TIM18EN  is new APB1ENR.FN.B (APB1ENR.TIM18EN );
   function TIM14EN  is new APB1ENR.FN.B (APB1ENR.TIM14EN );
   function TIM13EN  is new APB1ENR.FN.B (APB1ENR.TIM13EN );
   function TIM12EN  is new APB1ENR.FN.B (APB1ENR.TIM12EN );
   function TIM7EN   is new APB1ENR.FN.B (APB1ENR.TIM7EN  );
   function TIM6EN   is new APB1ENR.FN.B (APB1ENR.TIM6EN  );
   function TIM5EN   is new APB1ENR.FN.B (APB1ENR.TIM5EN  );
   function TIM4EN   is new APB1ENR.FN.B (APB1ENR.TIM4EN  );
   function TIM3EN   is new APB1ENR.FN.B (APB1ENR.TIM3EN  );
   function TIM2EN   is new APB1ENR.FN.B (APB1ENR.TIM2EN  );

   --  Functions

   function  "+"   is new APB1ENR.FN.Add;
   function  "+"   is new APB1ENR.FN.Add_RM;
   function  "-"   is new APB1ENR.FN.Clear;
   function  Init  is new APB1ENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.CECEN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.CECEN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.DAC1EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.DAC1EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.PWREN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.PWREN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.DAC2EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.DAC2EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.CANEN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.CANEN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.USBEN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.USBEN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.I2C2EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.I2C2EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.I2C1EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.I2C1EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.USART3EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.USART3EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.USART2EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.USART2EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.SPI3EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.SPI3EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.SPI2EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.SPI2EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.WWDGEN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.WWDGEN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.TIM18EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.TIM18EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.TIM14EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.TIM14EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.TIM13EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.TIM13EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.TIM12EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.TIM12EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.TIM7EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.TIM7EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.TIM6EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.TIM6EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.TIM5EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.TIM5EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.TIM4EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.TIM4EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.TIM3EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.TIM3EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.TIM2EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.TIM2EN, 2#1#);

   --------------------------------------
   --  Backup Domain Control Register  --
   --------------------------------------

   package BDCR is

      package Tp is new Types (R32);

      package BDRTS  is new Bitfield (Tp, 16);
      package RTCEN  is new Bitfield (Tp, 15);
      package RTCSEL is new Bitfield (Tp,  8, 2);
      package LSEDRV is new Bitfield (Tp,  3, 2);
      package LSEBYP is new Bitfield (Tp,  2);
      package LSERDY is new Bitfield (Tp,  1);
      package LSEON  is new Bitfield (Tp,  0);

      type T is
         record
            BDRTS  : BDCR.BDRTS .T;
            RTCEN  : BDCR.RTCEN .T;
            RTCSEL : BDCR.RTCSEL.T;
            LSEDRV : BDCR.LSEDRV.T;
            LSEBYP : BDCR.LSEBYP.T;
            LSERDY : BDCR.LSERDY.T;
            LSEON  : BDCR.LSEON .T;
         end record;

      for T use
         record
            BDRTS  at 0 range BDRTS .R'First .. BDRTS .R'Last;
            RTCEN  at 0 range RTCEN .R'First .. RTCEN .R'Last;
            RTCSEL at 0 range RTCSEL.R'First .. RTCSEL.R'Last;
            LSEDRV at 0 range LSEDRV.R'First .. LSEDRV.R'Last;
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
   function LSEDRV is new BDCR.FN.B (BDCR.LSEDRV);
   function LSEBYP is new BDCR.FN.B (BDCR.LSEBYP);
   function LSERDY is new BDCR.FN.B (BDCR.LSERDY);
   function LSEON  is new BDCR.FN.B (BDCR.LSEON);

   --  Functions

   function  "+"   is new BDCR.FN.Add;
   function  "+"   is new BDCR.FN.Add_RM;
   function  "-"   is new BDCR.FN.Clear;
   function  Init  is new BDCR.FN.Init;

   --  Constant definitions

   function Does_Not_Reset is new BDCR.FN.C (BDCR.BDRTS, 2#0#);
   function Resets         is new BDCR.FN.C (BDCR.BDRTS, 2#1#);

   function Clock_Disabled is new BDCR.FN.C (BDCR.RTCEN, 2#0#);
   function Clock_Enabled  is new BDCR.FN.C (BDCR.RTCEN, 2#1#);

   function NoClock                      is new BDCR.FN.C (BDCR.RTCSEL, 2#00#);
   function LSE_Oscillator               is new BDCR.FN.C (BDCR.RTCSEL, 2#01#);
   function LSI_Oscillator               is new BDCR.FN.C (BDCR.RTCSEL, 2#10#);
   function HSE_Oscillator_Divided_By_32 is new BDCR.FN.C (BDCR.RTCSEL, 2#11#);

   function XTAL_Mode_Low_Driving         is new BDCR.FN.C (BDCR.LSEDRV, 2#00#);
   function XTAL_Mode_Medium_Driving      is new BDCR.FN.C (BDCR.LSEDRV, 2#01#);
   function XTAL_Mode_Medium_High_Driving is new BDCR.FN.C (BDCR.LSEDRV, 2#10#);
   function XTAL_Mode_Higher_Driving      is new BDCR.FN.C (BDCR.LSEDRV, 2#11#);

   function LSE_Oscillator_Not_Bypassed is new BDCR.FN.C (BDCR.LSEBYP, 2#0#);
   function LSE_Oscillator_Bypassed     is new BDCR.FN.C (BDCR.LSEBYP, 2#1#);

   function LSE_Oscillator_Not_Ready is new BDCR.FN.C (BDCR.LSERDY, 2#0#);
   function LSE_Oscillator_Ready     is new BDCR.FN.C (BDCR.LSERDY, 2#1#);

   function LSE_Oscillator_Off is new BDCR.FN.C (BDCR.LSEON, 2#0#);
   function LSE_Oscillator_On  is new BDCR.FN.C (BDCR.LSEON, 2#1#);

   -------------------------------
   --  Control/Status Register  --
   -------------------------------

   package CSR is

      package Tp is new Types (R32);

      package LPWRRSTF is new Bitfield (Tp, 31);
      package WWDGRSTF is new Bitfield (Tp, 30);
      package IWDGRSTF is new Bitfield (Tp, 29);
      package SFTRSTF  is new Bitfield (Tp, 28);
      package PORRSTF  is new Bitfield (Tp, 27);
      package PINRSTF  is new Bitfield (Tp, 26);
      package OBLRSTF  is new Bitfield (Tp, 25);
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
            OBLRSTF  : CSR.OBLRSTF .T;
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
            OBLRSTF  at 0 range OBLRSTF .R'First .. OBLRSTF .R'Last;
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
   function OBLRSTF  is new CSR.FN.B (CSR.OBLRSTF);
   function RMVF     is new CSR.FN.B (CSR.RMVF);
   function LSIRDY   is new CSR.FN.B (CSR.LSIRDY);
   function LSION    is new CSR.FN.B (CSR.LSION);

   --  Functions

   function  "+"   is new CSR.FN.Add;
   function  "+"   is new CSR.FN.Add_RM;
   function  "-"   is new CSR.FN.Clear;
   function  Init  is new CSR.FN.Init;

   function No_LowPower_Management_Reset_Occurred is new CSR.FN.C (CSR.LPWRRSTF, 2#0#);
   function LowPower_Management_Reset_Occurred    is new CSR.FN.C (CSR.LPWRRSTF, 2#1#);

   function No_Window_Watchdog_Reset_Occurred is new CSR.FN.C (CSR.WWDGRSTF, 2#0#);
   function Window_Watchdog_Reset_Occurred    is new CSR.FN.C (CSR.WWDGRSTF, 2#1#);

   function No_Watchdog_Reset_Occurred is new CSR.FN.C (CSR.IWDGRSTF, 2#0#);
   function Watchdog_Reset_Occurred    is new CSR.FN.C (CSR.IWDGRSTF, 2#1#);

   function No_Software_Reset_Occurred is new CSR.FN.C (CSR.SFTRSTF, 2#0#);
   function Software_Reset_Occurred    is new CSR.FN.C (CSR.SFTRSTF, 2#1#);

   function No_POR_PDR_Reset_Ocurred is new CSR.FN.C (CSR.PORRSTF, 2#0#);
   function POR_PDR_Reset_Ocurred    is new CSR.FN.C (CSR.PORRSTF, 2#1#);

   function No_Reset_From_NRST_Pin_Occurred is new CSR.FN.C (CSR.PINRSTF, 2#0#);
   function Reset_From_NRST_Pin_Occurred    is new CSR.FN.C (CSR.PINRSTF, 2#1#);

   function No_Reset_From_OBL_Ocurred is new CSR.FN.C (CSR.OBLRSTF, 2#0#);
   function Reset_From_OBL_Ocurred    is new CSR.FN.C (CSR.OBLRSTF, 2#1#);

   function Clear_Flag is new CSR.FN.C (CSR.RMVF, 2#1#);

   function LSI_Oscillator_Not_Ready is new CSR.FN.C (CSR.LSIRDY, 2#0#);
   function LSI_Oscillator_Ready     is new CSR.FN.C (CSR.LSIRDY, 2#1#);

   function LSI_Oscillator_Off is new CSR.FN.C (CSR.LSION, 2#0#);
   function LSI_Oscillator_On  is new CSR.FN.C (CSR.LSION, 2#1#);


   -------------------------------------------
   --  AHB Peripheral Clock Reset Register  --
   -------------------------------------------

   package AHBRSTR is

      package Tp is new Types (R32);

      package TSCRST   is new Bitfield (Tp, 24);
      package IOPFRST  is new Bitfield (Tp, 22);
      package IOPERST  is new Bitfield (Tp, 21);
      package IOPDRST  is new Bitfield (Tp, 20);
      package IOPCRST  is new Bitfield (Tp, 19);
      package IOPBRST  is new Bitfield (Tp, 18);
      package IOPARST  is new Bitfield (Tp, 17);

      type T is
         record
            TSCRST   : AHBRSTR.TSCRST  .T;
            IOPFRST  : AHBRSTR.IOPFRST .T;
            IOPERST  : AHBRSTR.IOPERST .T;
            IOPDRST  : AHBRSTR.IOPDRST .T;
            IOPCRST  : AHBRSTR.IOPCRST .T;
            IOPBRST  : AHBRSTR.IOPBRST .T;
            IOPARST  : AHBRSTR.IOPARST .T;
         end record;

      for T use
         record
            TSCRST   at 0 range TSCRST  .R'First .. TSCRST  .R'Last;
            IOPFRST  at 0 range IOPFRST .R'First .. IOPFRST .R'Last;
            IOPERST  at 0 range IOPERST .R'First .. IOPERST .R'Last;
            IOPDRST  at 0 range IOPDRST .R'First .. IOPDRST .R'Last;
            IOPCRST  at 0 range IOPCRST .R'First .. IOPCRST .R'Last;
            IOPBRST  at 0 range IOPBRST .R'First .. IOPBRST .R'Last;
            IOPARST  at 0 range IOPARST .R'First .. IOPARST .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHBRSTR;

   subtype AHBRSTR_T is AHBRSTR.T;

   --  Field definitions

   function TSCRST   is new AHBRSTR.FN.B (AHBRSTR.TSCRST  );
   function IOPFRST  is new AHBRSTR.FN.B (AHBRSTR.IOPFRST );
   function IOPERST  is new AHBRSTR.FN.B (AHBRSTR.IOPERST );
   function IOPDRST  is new AHBRSTR.FN.B (AHBRSTR.IOPDRST );
   function IOPCRST  is new AHBRSTR.FN.B (AHBRSTR.IOPCRST );
   function IOPBRST  is new AHBRSTR.FN.B (AHBRSTR.IOPBRST );
   function IOPARST  is new AHBRSTR.FN.B (AHBRSTR.IOPARST );

   --  Functions

   function  "+"   is new AHBRSTR.FN.Add;
   function  "+"   is new AHBRSTR.FN.Add_RM;
   function  "-"   is new AHBRSTR.FN.Clear;
   function  Init  is new AHBRSTR.FN.Init;

   --  Constant definitions

   function Does_Not_Reset is new AHBRSTR.FN.C (AHBRSTR.TSCRST, 2#0#);
   function Resets         is new AHBRSTR.FN.C (AHBRSTR.TSCRST, 2#1#);

   function Does_Not_Reset is new AHBRSTR.FN.C (AHBRSTR.IOPFRST, 2#0#);
   function Resets         is new AHBRSTR.FN.C (AHBRSTR.IOPFRST, 2#1#);

   function Does_Not_Reset is new AHBRSTR.FN.C (AHBRSTR.IOPERST, 2#0#);
   function Resets         is new AHBRSTR.FN.C (AHBRSTR.IOPERST, 2#1#);

   function Does_Not_Reset is new AHBRSTR.FN.C (AHBRSTR.IOPDRST, 2#0#);
   function Resets         is new AHBRSTR.FN.C (AHBRSTR.IOPDRST, 2#1#);

   function Does_Not_Reset is new AHBRSTR.FN.C (AHBRSTR.IOPCRST, 2#0#);
   function Resets         is new AHBRSTR.FN.C (AHBRSTR.IOPCRST, 2#1#);

   function Does_Not_Reset is new AHBRSTR.FN.C (AHBRSTR.IOPBRST, 2#0#);
   function Resets         is new AHBRSTR.FN.C (AHBRSTR.IOPBRST, 2#1#);

   function Does_Not_Reset is new AHBRSTR.FN.C (AHBRSTR.IOPARST, 2#0#);
   function Resets         is new AHBRSTR.FN.C (AHBRSTR.IOPARST, 2#1#);

   --------------------------------
   --  Configuration Register 2  --
   --------------------------------

   package CFGR2 is

      package Tp is new Types (R32);

      package PREDIV    is new Bitfield (Tp,  0, 4);

      type T is
         record
            PREDIV    : CFGR2.PREDIV.T;
         end record;

      for T use
         record
            PREDIV    at 0 range PREDIV.R'First .. PREDIV.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CFGR2;

   subtype CFGR2_T is CFGR2.T;

   --  Field definitions

   function PREDIV    is new CFGR2.FN.B (CFGR2.PREDIV);

   --  Functions

   function  "+"   is new CFGR2.FN.Add;
   function  "+"   is new CFGR2.FN.Add_RM;
   function  "-"   is new CFGR2.FN.Clear;
   function  Init  is new CFGR2.FN.Init;

   --  Constant definitions

   function HSE_Input_To_PLL_Not_Divided    is new CFGR2.FN.C (CFGR2.PREDIV, 2#0000#);
   function HSE_Input_To_PLL_Divided_By_2   is new CFGR2.FN.C (CFGR2.PREDIV, 2#0001#);
   function HSE_Input_To_PLL_Divided_By_3   is new CFGR2.FN.C (CFGR2.PREDIV, 2#0010#);
   function HSE_Input_To_PLL_Divided_By_4   is new CFGR2.FN.C (CFGR2.PREDIV, 2#0011#);
   function HSE_Input_To_PLL_Divided_By_5   is new CFGR2.FN.C (CFGR2.PREDIV, 2#0100#);
   function HSE_Input_To_PLL_Divided_By_6   is new CFGR2.FN.C (CFGR2.PREDIV, 2#0101#);
   function HSE_Input_To_PLL_Divided_By_7   is new CFGR2.FN.C (CFGR2.PREDIV, 2#0110#);
   function HSE_Input_To_PLL_Divided_By_8   is new CFGR2.FN.C (CFGR2.PREDIV, 2#0111#);
   function HSE_Input_To_PLL_Divided_By_9   is new CFGR2.FN.C (CFGR2.PREDIV, 2#1000#);
   function HSE_Input_To_PLL_Divided_By_10  is new CFGR2.FN.C (CFGR2.PREDIV, 2#1001#);
   function HSE_Input_To_PLL_Divided_By_11  is new CFGR2.FN.C (CFGR2.PREDIV, 2#1010#);
   function HSE_Input_To_PLL_Divided_By_12  is new CFGR2.FN.C (CFGR2.PREDIV, 2#1011#);
   function HSE_Input_To_PLL_Divided_By_13  is new CFGR2.FN.C (CFGR2.PREDIV, 2#1100#);
   function HSE_Input_To_PLL_Divided_By_14  is new CFGR2.FN.C (CFGR2.PREDIV, 2#1101#);
   function HSE_Input_To_PLL_Divided_By_15  is new CFGR2.FN.C (CFGR2.PREDIV, 2#1110#);
   function HSE_Input_To_PLL_Divided_By_16  is new CFGR2.FN.C (CFGR2.PREDIV, 2#1111#);

   --------------------------------
   --  Configuration Register 3  --
   --------------------------------

   package CFGR3 is

      package Tp is new Types (R32);

      package USART3SW is new Bitfield (Tp, 18, 2);
      package USART2SW is new Bitfield (Tp, 16, 2);
      package CECSW    is new Bitfield (Tp, 6);
      package I2C2SW   is new Bitfield (Tp, 5);
      package I2C1SW   is new Bitfield (Tp, 4);
      package USART1SW is new Bitfield (Tp, 0, 2);

      type T is
         record
            USART3SW : CFGR3.USART3SW.T;
            USART2SW : CFGR3.USART2SW.T;
            CECSW    : CFGR3.CECSW   .T;
            I2C2SW   : CFGR3.I2C2SW  .T;
            I2C1SW   : CFGR3.I2C1SW  .T;
            USART1SW : CFGR3.USART1SW.T;
         end record;

      for T use
         record
            USART3SW at 0 range USART3SW.R'First .. USART3SW.R'Last;
            USART2SW at 0 range USART2SW.R'First .. USART2SW.R'Last;
            CECSW    at 0 range CECSW   .R'First .. CECSW   .R'Last;
            I2C2SW   at 0 range I2C2SW  .R'First .. I2C2SW  .R'Last;
            I2C1SW   at 0 range I2C1SW  .R'First .. I2C1SW  .R'Last;
            USART1SW at 0 range USART1SW.R'First .. USART1SW.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CFGR3;

   subtype CFGR3_T is CFGR3.T;

   --  Field definitions

   function USART3SW is new CFGR3.FN.B (CFGR3.USART3SW);
   function USART2SW is new CFGR3.FN.B (CFGR3.USART2SW);
   function CECSW    is new CFGR3.FN.B (CFGR3.CECSW   );
   function I2C2SW   is new CFGR3.FN.B (CFGR3.I2C2SW  );
   function I2C1SW   is new CFGR3.FN.B (CFGR3.I2C1SW  );
   function USART1SW is new CFGR3.FN.B (CFGR3.USART1SW);

   --  Functions

   function  "+"   is new CFGR3.FN.Add;
   function  "+"   is new CFGR3.FN.Add_RM;
   function  "-"   is new CFGR3.FN.Clear;
   function  Init  is new CFGR3.FN.Init;

   --  Constant definitions

   function PCLK_Selected         is new CFGR3.FN.C (CFGR3.USART3SW, 2#00#);
   function System_Clock_Selected is new CFGR3.FN.C (CFGR3.USART3SW, 2#01#);
   function LSE_Clock_Selected    is new CFGR3.FN.C (CFGR3.USART3SW, 2#10#);
   function HSI_Clock_Selected    is new CFGR3.FN.C (CFGR3.USART3SW, 2#11#);

   function PCLK_Selected         is new CFGR3.FN.C (CFGR3.USART2SW, 2#00#);
   function System_Clock_Selected is new CFGR3.FN.C (CFGR3.USART2SW, 2#01#);
   function LSE_Clock_Selected    is new CFGR3.FN.C (CFGR3.USART2SW, 2#10#);
   function HSI_Clock_Selected    is new CFGR3.FN.C (CFGR3.USART2SW, 2#11#);

   function HSI_Clock_Divided_By_244 is new CFGR3.FN.C (CFGR3.CECSW, 2#0#);
   function LSE_Clock                is new CFGR3.FN.C (CFGR3.CECSW, 2#1#);

   function HSI_Clock_Selected    is new CFGR3.FN.C (CFGR3.I2C2SW, 2#0#);
   function System_Clock_Selected is new CFGR3.FN.C (CFGR3.I2C2SW, 2#1#);

   function HSI_Clock_Selected    is new CFGR3.FN.C (CFGR3.I2C1SW, 2#0#);
   function System_Clock_Selected is new CFGR3.FN.C (CFGR3.I2C1SW, 2#1#);

   function PCLK_Selected         is new CFGR3.FN.C (CFGR3.USART1SW, 2#00#);
   function System_Clock_Selected is new CFGR3.FN.C (CFGR3.USART1SW, 2#01#);
   function LSE_Clock_Selected    is new CFGR3.FN.C (CFGR3.USART1SW, 2#10#);
   function HSI_Clock_Selected    is new CFGR3.FN.C (CFGR3.USART1SW, 2#11#);

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
         CFGR3    : CFGR3_T;
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
         pragma Volatile (CFGR3);
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
         CFGR3    at 16#30# range 0 .. 31;
      end record;

   RCC : RCC_T;

   for RCC'Address use System'To_Address (16#4002_1000#);

end ARM.Registers.RCC_F37XXX;
