--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 A R M . R e g i s t e r . R C C _ F 3 7 X X X              --
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

with ARM.Register;
use  ARM.Register;

with ARM.Register.Types;
with ARM.Register.Bitfield;
with ARM.Register.Register;

--------------------------------------------------------------------------------
--                            ARM.Register.RCC_F37XXX                         --
--------------------------------------------------------------------------------

package ARM.Register.RCC_F37XXX is

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

   end RCC_CR;

   --  Types definitions

   package CR    is new Register (RCC_CR.T, RCC_CR.Tp, 16#4002_1000#);
   subtype CR_T  is CR.T;
   subtype CR_F  is CR.F;
   

   --  Field definitions

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

   package RCC_CFGR is

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
            SDPRE    : RCC_CFGR.SDPRE   .T;
            MCO      : RCC_CFGR.MCO     .T;
            USBPRE   : RCC_CFGR.USBPRE  .T;
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
            SDPRE    at 0 range SDPRE   .F .. SDPRE   .L;
            MCO      at 0 range MCO     .F .. MCO     .L;
            USBPRE   at 0 range USBPRE  .F .. USBPRE  .L;
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

   end RCC_CFGR;

   package CFGR    is new Register (RCC_CFGR.T, RCC_CFGR.Tp, 16#4002_1004#);
   subtype CFGR_T  is CFGR.T;
   subtype CFGR_F  is CFGR.F;
   

   --  Field definitions

   function SDPRE    is new CFGR.B (RCC_CFGR.SDPRE   ) with Inline_Always;
   function MCO      is new CFGR.B (RCC_CFGR.MCO     ) with Inline_Always;
   function USBPRE   is new CFGR.B (RCC_CFGR.USBPRE  ) with Inline_Always;
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

   function Div2         is new CFGR.C (RCC_CFGR.SDPRE, 2#10000#) with Inline_Always;
   function Div4         is new CFGR.C (RCC_CFGR.SDPRE, 2#10001#) with Inline_Always;
   function Div6         is new CFGR.C (RCC_CFGR.SDPRE, 2#10010#) with Inline_Always;
   function Div8         is new CFGR.C (RCC_CFGR.SDPRE, 2#10011#) with Inline_Always;
   function Div10        is new CFGR.C (RCC_CFGR.SDPRE, 2#10100#) with Inline_Always;
   function Div12        is new CFGR.C (RCC_CFGR.SDPRE, 2#10101#) with Inline_Always;
   function Div14        is new CFGR.C (RCC_CFGR.SDPRE, 2#10110#) with Inline_Always;
   function Div16        is new CFGR.C (RCC_CFGR.SDPRE, 2#10111#) with Inline_Always;
   function Div20        is new CFGR.C (RCC_CFGR.SDPRE, 2#11000#) with Inline_Always;
   function Div24        is new CFGR.C (RCC_CFGR.SDPRE, 2#11001#) with Inline_Always;
   function Div28        is new CFGR.C (RCC_CFGR.SDPRE, 2#11010#) with Inline_Always;
   function Div32        is new CFGR.C (RCC_CFGR.SDPRE, 2#11011#) with Inline_Always;
   function Div36        is new CFGR.C (RCC_CFGR.SDPRE, 2#11100#) with Inline_Always;
   function Div40        is new CFGR.C (RCC_CFGR.SDPRE, 2#11101#) with Inline_Always;
   function Div44        is new CFGR.C (RCC_CFGR.SDPRE, 2#11110#) with Inline_Always;
   function Div48        is new CFGR.C (RCC_CFGR.SDPRE, 2#11111#) with Inline_Always;

   function NoClock               is new CFGR.C (RCC_CFGR.MCO, 2#000#) with Inline_Always;
   function LSI_Clock_Selected    is new CFGR.C (RCC_CFGR.MCO, 2#010#) with Inline_Always;
   function LSE_Clock_Selected    is new CFGR.C (RCC_CFGR.MCO, 2#011#) with Inline_Always;
   function System_Clock_Selected is new CFGR.C (RCC_CFGR.MCO, 2#100#) with Inline_Always;
   function HSI_Clock_Selected    is new CFGR.C (RCC_CFGR.MCO, 2#101#) with Inline_Always;
   function HSE_Clock_Selected    is new CFGR.C (RCC_CFGR.MCO, 2#110#) with Inline_Always;
   function PLL_Clock_Div_By_2    is new CFGR.C (RCC_CFGR.MCO, 2#111#) with Inline_Always;

   function PLL_Clock_Div_By_1_5  is new CFGR.C (RCC_CFGR.USBPRE, 2#0#) with Inline_Always;
   function PLL_Clock_Not_Divided is new CFGR.C (RCC_CFGR.USBPRE, 2#1#) with Inline_Always;

   function PLLx2   is new CFGR.C (RCC_CFGR.PLLMUL, 2#0000#) with Inline_Always;
   function PLLx3   is new CFGR.C (RCC_CFGR.PLLMUL, 2#0001#) with Inline_Always;
   function PLLx4   is new CFGR.C (RCC_CFGR.PLLMUL, 2#0010#) with Inline_Always;
   function PLLx5   is new CFGR.C (RCC_CFGR.PLLMUL, 2#0011#) with Inline_Always;
   function PLLx6   is new CFGR.C (RCC_CFGR.PLLMUL, 2#0100#) with Inline_Always;
   function PLLx7   is new CFGR.C (RCC_CFGR.PLLMUL, 2#0101#) with Inline_Always;
   function PLLx8   is new CFGR.C (RCC_CFGR.PLLMUL, 2#0110#) with Inline_Always;
   function PLLx9   is new CFGR.C (RCC_CFGR.PLLMUL, 2#0111#) with Inline_Always;
   function PLLx10  is new CFGR.C (RCC_CFGR.PLLMUL, 2#1000#) with Inline_Always;
   function PLLx11  is new CFGR.C (RCC_CFGR.PLLMUL, 2#1001#) with Inline_Always;
   function PLLx12  is new CFGR.C (RCC_CFGR.PLLMUL, 2#1010#) with Inline_Always;
   function PLLx13  is new CFGR.C (RCC_CFGR.PLLMUL, 2#1011#) with Inline_Always;
   function PLLx14  is new CFGR.C (RCC_CFGR.PLLMUL, 2#1100#) with Inline_Always;
   function PLLx15  is new CFGR.C (RCC_CFGR.PLLMUL, 2#1101#) with Inline_Always;
   function PLLx16  is new CFGR.C (RCC_CFGR.PLLMUL, 2#1110#) with Inline_Always;

   function HSE_Not_Divided  is new CFGR.C (RCC_CFGR.PLLXTPRE, 2#0#) with Inline_Always;
   function HSE_Divided_By_2 is new CFGR.C (RCC_CFGR.PLLXTPRE, 2#1#) with Inline_Always;

   function HSI_Div2       is new CFGR.C (RCC_CFGR.PLLSRC, 2#0#) with Inline_Always;
   function HES_DIV_PREDIV is new CFGR.C (RCC_CFGR.PLLSRC, 2#1#) with Inline_Always;

   function PCLK_Divided_By_2 is new CFGR.C (RCC_CFGR.ADCPRE, 2#00#) with Inline_Always;
   function PCLK_Divided_By_4 is new CFGR.C (RCC_CFGR.ADCPRE, 2#01#) with Inline_Always;
   function PCLK_Divided_By_6 is new CFGR.C (RCC_CFGR.ADCPRE, 2#10#) with Inline_Always;
   function PCLK_Divided_By_8 is new CFGR.C (RCC_CFGR.ADCPRE, 2#11#) with Inline_Always;

   function AHB_Clock_Not_Divided   is new CFGR.C (RCC_CFGR.PPRE2, 2#000#) with Inline_Always;
   function AHB_Clock_Divided_By_2  is new CFGR.C (RCC_CFGR.PPRE2, 2#100#) with Inline_Always;
   function AHB_Clock_Divided_By_4  is new CFGR.C (RCC_CFGR.PPRE2, 2#101#) with Inline_Always;
   function AHB_Clock_Divided_By_8  is new CFGR.C (RCC_CFGR.PPRE2, 2#110#) with Inline_Always;
   function AHB_Clock_Divided_By_16 is new CFGR.C (RCC_CFGR.PPRE2, 2#111#) with Inline_Always;

   function APB1_Clock_Not_Divided   is new CFGR.C (RCC_CFGR.PPRE1, 2#000#) with Inline_Always;
   function APB1_Clock_Divided_By_2  is new CFGR.C (RCC_CFGR.PPRE1, 2#100#) with Inline_Always;
   function APB1_Clock_Divided_By_4  is new CFGR.C (RCC_CFGR.PPRE1, 2#101#) with Inline_Always;
   function APB1_Clock_Divided_By_8  is new CFGR.C (RCC_CFGR.PPRE1, 2#110#) with Inline_Always;
   function APB1_Clock_Divided_By_16 is new CFGR.C (RCC_CFGR.PPRE1, 2#111#) with Inline_Always;

   function SYSCLK_Not_Divided    is new CFGR.C (RCC_CFGR.HPRE, 2#0000#) with Inline_Always;
   function SYSCLK_Divided_By_2   is new CFGR.C (RCC_CFGR.HPRE, 2#1000#) with Inline_Always;
   function SYSCLK_Divided_By_4   is new CFGR.C (RCC_CFGR.HPRE, 2#1001#) with Inline_Always;
   function SYSCLK_Divided_By_8   is new CFGR.C (RCC_CFGR.HPRE, 2#1010#) with Inline_Always;
   function SYSCLK_Divided_By_16  is new CFGR.C (RCC_CFGR.HPRE, 2#1011#) with Inline_Always;
   function SYSCLK_Divided_By_64  is new CFGR.C (RCC_CFGR.HPRE, 2#1100#) with Inline_Always;
   function SYSCLK_Divided_By_128 is new CFGR.C (RCC_CFGR.HPRE, 2#1101#) with Inline_Always;
   function SYSCLK_Divided_By_256 is new CFGR.C (RCC_CFGR.HPRE, 2#1110#) with Inline_Always;
   function SYSCLK_Divided_By_512 is new CFGR.C (RCC_CFGR.HPRE, 2#1111#) with Inline_Always;

   function HSI_Oscillator_Used_As_System_Clock is new CFGR.C (RCC_CFGR.SWS, 2#00#) with Inline_Always;
   function HSE_Oscillator_Used_As_System_Clock is new CFGR.C (RCC_CFGR.SWS, 2#01#) with Inline_Always;
   function PLLUsed_As_System_Clock             is new CFGR.C (RCC_CFGR.SWS, 2#10#) with Inline_Always;

   function HSI_Selected_As_System_Clock is new CFGR.C (RCC_CFGR.SW, 2#00#) with Inline_Always;
   function HSE_Selected_As_System_Clock is new CFGR.C (RCC_CFGR.SW, 2#01#) with Inline_Always;
   function PLL_Selected_As_System_Clock is new CFGR.C (RCC_CFGR.SW, 2#10#) with Inline_Always;

   --------------------------
   --  Interrupt Register  --
   --------------------------

   package RCC_CIR is

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
            CSSC      : RCC_CIR.CSSC     .T;
            PLLRDYC   : RCC_CIR.PLLRDYC  .T;
            HSERDYC   : RCC_CIR.HSERDYC  .T;
            HSIRDYC   : RCC_CIR.HSIRDYC  .T;
            LSERDYC   : RCC_CIR.LSERDYC  .T;
            LSIRDYC   : RCC_CIR.LSIRDYC  .T;
            PLLRDYIE  : RCC_CIR.PLLRDYIE .T;
            HSERDYIE  : RCC_CIR.HSERDYIE .T;
            HSIRDYIE  : RCC_CIR.HSIRDYIE .T;
            LSERDYIE  : RCC_CIR.LSERDYIE .T;
            LSIRDYIE  : RCC_CIR.LSIRDYIE .T;
            CSSF      : RCC_CIR.CSSF     .T;
            PLLRDYF   : RCC_CIR.PLLRDYF  .T;
            HSERDYF   : RCC_CIR.HSERDYF  .T;
            HSIRDYF   : RCC_CIR.HSIRDYF  .T;
            LSERDYF   : RCC_CIR.LSERDYF  .T;
            LSIRDYF   : RCC_CIR.LSIRDYF  .T;
         end record;

      for T use
         record
            CSSC      at 0 range CSSC     .F .. CSSC     .L;
            PLLRDYC   at 0 range PLLRDYC  .F .. PLLRDYC  .L;
            HSERDYC   at 0 range HSERDYC  .F .. HSERDYC  .L;
            HSIRDYC   at 0 range HSIRDYC  .F .. HSIRDYC  .L;
            LSERDYC   at 0 range LSERDYC  .F .. LSERDYC  .L;
            LSIRDYC   at 0 range LSIRDYC  .F .. LSIRDYC  .L;
            PLLRDYIE  at 0 range PLLRDYIE .F .. PLLRDYIE .L;
            HSERDYIE  at 0 range HSERDYIE .F .. HSERDYIE .L;
            HSIRDYIE  at 0 range HSIRDYIE .F .. HSIRDYIE .L;
            LSERDYIE  at 0 range LSERDYIE .F .. LSERDYIE .L;
            LSIRDYIE  at 0 range LSIRDYIE .F .. LSIRDYIE .L;
            CSSF      at 0 range CSSF     .F .. CSSF     .L;
            PLLRDYF   at 0 range PLLRDYF  .F .. PLLRDYF  .L;
            HSERDYF   at 0 range HSERDYF  .F .. HSERDYF  .L;
            HSIRDYF   at 0 range HSIRDYF  .F .. HSIRDYF  .L;
            LSERDYF   at 0 range LSERDYF  .F .. LSERDYF  .L;
            LSIRDYF   at 0 range LSIRDYF  .F .. LSIRDYF  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_CIR;

   package CIR    is new Register (RCC_CIR.T, RCC_CIR.Tp, 16#4002_1008#);
   subtype CIR_T  is CIR.T;
   subtype CIR_F  is CIR.F;
   

   --  Field definitions

   function CSSC      is new CIR.B (RCC_CIR.CSSC    ) with Inline_Always;
   function PLLRDYC   is new CIR.B (RCC_CIR.PLLRDYC ) with Inline_Always;
   function HSERDYC   is new CIR.B (RCC_CIR.HSERDYC ) with Inline_Always;
   function HSIRDYC   is new CIR.B (RCC_CIR.HSIRDYC ) with Inline_Always;
   function LSERDYC   is new CIR.B (RCC_CIR.LSERDYC ) with Inline_Always;
   function LSIRDYC   is new CIR.B (RCC_CIR.LSIRDYC ) with Inline_Always;
   function PLLRDYIE  is new CIR.B (RCC_CIR.PLLRDYIE) with Inline_Always;
   function HSERDYIE  is new CIR.B (RCC_CIR.HSERDYIE) with Inline_Always;
   function HSIRDYIE  is new CIR.B (RCC_CIR.HSIRDYIE) with Inline_Always;
   function LSERDYIE  is new CIR.B (RCC_CIR.LSERDYIE) with Inline_Always;
   function LSIRDYIE  is new CIR.B (RCC_CIR.LSIRDYIE) with Inline_Always;
   function CSSF      is new CIR.B (RCC_CIR.CSSF    ) with Inline_Always;
   function PLLRDYF   is new CIR.B (RCC_CIR.PLLRDYF ) with Inline_Always;
   function HSERDYF   is new CIR.B (RCC_CIR.HSERDYF ) with Inline_Always;
   function HSIRDYF   is new CIR.B (RCC_CIR.HSIRDYF ) with Inline_Always;
   function LSERDYF   is new CIR.B (RCC_CIR.LSERDYF ) with Inline_Always;
   function LSIRDYF   is new CIR.B (RCC_CIR.LSIRDYF ) with Inline_Always;

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

   package RCC_APB2RSTR is

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
            SDA3RST   : RCC_APB2RSTR.SDA3RST  .T;
            SDA2RST   : RCC_APB2RSTR.SDA2RST  .T;
            SDA1RST   : RCC_APB2RSTR.SDA1RST  .T;
            TIM19RST  : RCC_APB2RSTR.TIM19RST .T;
            TIM17RST  : RCC_APB2RSTR.TIM17RST .T;
            TIM16RST  : RCC_APB2RSTR.TIM16RST .T;
            TIM15RST  : RCC_APB2RSTR.TIM15RST .T;
            USART1RST : RCC_APB2RSTR.USART1RST.T;
            SPI1RST   : RCC_APB2RSTR.SPI1RST  .T;
            ADCRST    : RCC_APB2RSTR.ADCRST   .T;
            SYSCFGRST : RCC_APB2RSTR.SYSCFGRST.T;
         end record;

      for T use
         record
            SDA3RST   at 0 range SDA3RST  .F .. SDA3RST  .L;
            SDA2RST   at 0 range SDA2RST  .F .. SDA2RST  .L;
            SDA1RST   at 0 range SDA1RST  .F .. SDA1RST  .L;
            TIM19RST  at 0 range TIM19RST .F .. TIM19RST .L;
            TIM17RST  at 0 range TIM17RST .F .. TIM17RST .L;
            TIM16RST  at 0 range TIM16RST .F .. TIM16RST .L;
            TIM15RST  at 0 range TIM15RST .F .. TIM15RST .L;
            USART1RST at 0 range USART1RST.F .. USART1RST.L;
            SPI1RST   at 0 range SPI1RST  .F .. SPI1RST  .L;
            ADCRST    at 0 range ADCRST   .F .. ADCRST   .L;
            SYSCFGRST at 0 range SYSCFGRST.F .. SYSCFGRST.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_APB2RSTR;

   package APB2RSTR    is new Register (RCC_APB2RSTR.T, RCC_APB2RSTR.Tp, 16#4002_100C#);
   subtype APB2RSTR_T  is APB2RSTR.T;
   subtype APB2RSTR_F  is APB2RSTR.F;
   

   --  Field definitions

   function SDA3RST   is new APB2RSTR.B (RCC_APB2RSTR.SDA3RST  ) with Inline_Always;
   function SDA2RST   is new APB2RSTR.B (RCC_APB2RSTR.SDA2RST  ) with Inline_Always;
   function SDA1RST   is new APB2RSTR.B (RCC_APB2RSTR.SDA1RST  ) with Inline_Always;
   function TIM19RST  is new APB2RSTR.B (RCC_APB2RSTR.TIM19RST ) with Inline_Always;
   function TIM17RST  is new APB2RSTR.B (RCC_APB2RSTR.TIM17RST ) with Inline_Always;
   function TIM16RST  is new APB2RSTR.B (RCC_APB2RSTR.TIM16RST ) with Inline_Always;
   function TIM15RST  is new APB2RSTR.B (RCC_APB2RSTR.TIM15RST ) with Inline_Always;
   function USART1RST is new APB2RSTR.B (RCC_APB2RSTR.USART1RST) with Inline_Always;
   function SPI1RST   is new APB2RSTR.B (RCC_APB2RSTR.SPI1RST  ) with Inline_Always;
   function ADCRST    is new APB2RSTR.B (RCC_APB2RSTR.ADCRST   ) with Inline_Always;
   function SYSCFGRST is new APB2RSTR.B (RCC_APB2RSTR.SYSCFGRST) with Inline_Always;

   --  Functions

   function "+"  is new APB2RSTR.Add      with Inline_Always;
   function "+"  is new APB2RSTR.Add_F    with Inline_Always;
   function "+"  is new APB2RSTR.Add_FF   with Inline_Always;
   function "-"  is new APB2RSTR.Clear    with Inline_Always;
   function "-"  is new APB2RSTR.Clear_FF with Inline_Always;
   function "="  is new APB2RSTR.Equal    with Inline_Always;
   function Init is new APB2RSTR.Init     with Inline_Always;

   --  Constant definitions

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.SDA3RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.SDA3RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.SDA2RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.SDA2RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.SDA1RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.SDA1RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.TIM19RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.TIM19RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.TIM17RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.TIM17RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.TIM16RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.TIM16RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.TIM15RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.TIM15RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.USART1RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.USART1RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.SPI1RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.SPI1RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.ADCRST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.ADCRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.SYSCFGRST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.SYSCFGRST, 2#1#) with Inline_Always;

   --------------------------------------
   --  APB1 Peripheral Reset Register  --
   --------------------------------------

   package RCC_APB1RSTR is

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
            CECRST    : RCC_APB1RSTR.CECRST   .T;
            DAC1RST   : RCC_APB1RSTR.DAC1RST  .T;
            PWRRST    : RCC_APB1RSTR.PWRRST   .T;
            DAC2RST   : RCC_APB1RSTR.DAC2RST  .T;
            CANRST    : RCC_APB1RSTR.CANRST   .T;
            USBRST    : RCC_APB1RSTR.USBRST   .T;
            I2C2RST   : RCC_APB1RSTR.I2C2RST  .T;
            I2C1RST   : RCC_APB1RSTR.I2C1RST  .T;
            USART3RST : RCC_APB1RSTR.USART3RST.T;
            USART2RST : RCC_APB1RSTR.USART2RST.T;
            SPI3RST   : RCC_APB1RSTR.SPI3RST  .T;
            SPI2RST   : RCC_APB1RSTR.SPI2RST  .T;
            WWDGRST   : RCC_APB1RSTR.WWDGRST  .T;
            TIM18RST  : RCC_APB1RSTR.TIM18RST .T;
            TIM14RST  : RCC_APB1RSTR.TIM14RST .T;
            TIM13RST  : RCC_APB1RSTR.TIM13RST .T;
            TIM12RST  : RCC_APB1RSTR.TIM12RST .T;
            TIM7RST   : RCC_APB1RSTR.TIM7RST  .T;
            TIM6RST   : RCC_APB1RSTR.TIM6RST  .T;
            TIM5RST   : RCC_APB1RSTR.TIM5RST  .T;
            TIM4RST   : RCC_APB1RSTR.TIM4RST  .T;
            TIM3RST   : RCC_APB1RSTR.TIM3RST  .T;
            TIM2RST   : RCC_APB1RSTR.TIM2RST  .T;
         end record;

      for T use
         record
            CECRST    at 0 range CECRST   .F .. CECRST   .L;
            DAC1RST   at 0 range DAC1RST  .F .. DAC1RST  .L;
            PWRRST    at 0 range PWRRST   .F .. PWRRST   .L;
            DAC2RST   at 0 range DAC2RST  .F .. DAC2RST  .L;
            CANRST    at 0 range CANRST   .F .. CANRST   .L;
            USBRST    at 0 range USBRST   .F .. USBRST   .L;
            I2C2RST   at 0 range I2C2RST  .F .. I2C2RST  .L;
            I2C1RST   at 0 range I2C1RST  .F .. I2C1RST  .L;
            USART3RST at 0 range USART3RST.F .. USART3RST.L;
            USART2RST at 0 range USART2RST.F .. USART2RST.L;
            SPI3RST   at 0 range SPI3RST  .F .. SPI3RST  .L;
            SPI2RST   at 0 range SPI2RST  .F .. SPI2RST  .L;
            WWDGRST   at 0 range WWDGRST  .F .. WWDGRST  .L;
            TIM18RST  at 0 range TIM18RST .F .. TIM18RST .L;
            TIM14RST  at 0 range TIM14RST .F .. TIM14RST .L;
            TIM13RST  at 0 range TIM13RST .F .. TIM13RST .L;
            TIM12RST  at 0 range TIM12RST .F .. TIM12RST .L;
            TIM7RST   at 0 range TIM7RST  .F .. TIM7RST  .L;
            TIM6RST   at 0 range TIM6RST  .F .. TIM6RST  .L;
            TIM5RST   at 0 range TIM5RST  .F .. TIM5RST  .L;
            TIM4RST   at 0 range TIM4RST  .F .. TIM4RST  .L;
            TIM3RST   at 0 range TIM3RST  .F .. TIM3RST  .L;
            TIM2RST   at 0 range TIM2RST  .F .. TIM2RST  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_APB1RSTR;

   package APB1RSTR    is new Register (RCC_APB1RSTR.T, RCC_APB1RSTR.Tp, 16#4002_1010#);
   subtype APB1RSTR_T  is APB1RSTR.T;
   subtype APB1RSTR_F  is APB1RSTR.F;
   

   --  Field definitions

   function CECRST    is new APB1RSTR.B (RCC_APB1RSTR.CECRST   ) with Inline_Always;
   function DAC1RST   is new APB1RSTR.B (RCC_APB1RSTR.DAC1RST  ) with Inline_Always;
   function PWRRST    is new APB1RSTR.B (RCC_APB1RSTR.PWRRST   ) with Inline_Always;
   function DAC2RST   is new APB1RSTR.B (RCC_APB1RSTR.DAC2RST  ) with Inline_Always;
   function CANRST    is new APB1RSTR.B (RCC_APB1RSTR.CANRST   ) with Inline_Always;
   function USBRST    is new APB1RSTR.B (RCC_APB1RSTR.USBRST   ) with Inline_Always;
   function I2C2RST   is new APB1RSTR.B (RCC_APB1RSTR.I2C2RST  ) with Inline_Always;
   function I2C1RST   is new APB1RSTR.B (RCC_APB1RSTR.I2C1RST  ) with Inline_Always;
   function USART3RST is new APB1RSTR.B (RCC_APB1RSTR.USART3RST) with Inline_Always;
   function USART2RST is new APB1RSTR.B (RCC_APB1RSTR.USART2RST) with Inline_Always;
   function SPI3RST   is new APB1RSTR.B (RCC_APB1RSTR.SPI3RST  ) with Inline_Always;
   function SPI2RST   is new APB1RSTR.B (RCC_APB1RSTR.SPI2RST  ) with Inline_Always;
   function WWDGRST   is new APB1RSTR.B (RCC_APB1RSTR.WWDGRST  ) with Inline_Always;
   function TIM18RST  is new APB1RSTR.B (RCC_APB1RSTR.TIM18RST ) with Inline_Always;
   function TIM14RST  is new APB1RSTR.B (RCC_APB1RSTR.TIM14RST ) with Inline_Always;
   function TIM13RST  is new APB1RSTR.B (RCC_APB1RSTR.TIM13RST ) with Inline_Always;
   function TIM12RST  is new APB1RSTR.B (RCC_APB1RSTR.TIM12RST ) with Inline_Always;
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

   --  Constant definitions

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.CECRST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.CECRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.DAC1RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.DAC1RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.PWRRST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.PWRRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.DAC2RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.DAC2RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.CANRST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.CANRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.USBRST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.USBRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.I2C2RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.I2C2RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.I2C1RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.I2C1RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.USART3RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.USART3RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.USART2RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.USART2RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.SPI3RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.SPI3RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.SPI2RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.SPI2RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.WWDGRST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.WWDGRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.TIM18RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.TIM18RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.TIM14RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.TIM14RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.TIM13RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.TIM13RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.TIM12RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.TIM12RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.TIM7RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.TIM7RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.TIM6RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.TIM6RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.TIM5RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.TIM5RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.TIM4RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.TIM4RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.TIM3RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.TIM3RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.TIM2RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.TIM2RST, 2#1#) with Inline_Always;

   --------------------------------------------
   --  AHB Peripheral Clock Enable Register  --
   --------------------------------------------


   package RCC_AHBENR is

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
            TSCEN   : RCC_AHBENR.TSCEN  .T;
            IOPFEN  : RCC_AHBENR.IOPFEN .T;
            IOPEEN  : RCC_AHBENR.IOPEEN .T;
            IOPDEN  : RCC_AHBENR.IOPDEN .T;
            IOPCEN  : RCC_AHBENR.IOPCEN .T;
            IOPBEN  : RCC_AHBENR.IOPBEN .T;
            IOPAEN  : RCC_AHBENR.IOPAEN .T;
            CRCEN   : RCC_AHBENR.CRCEN  .T;
            FLITFEN : RCC_AHBENR.FLITFEN.T;
            SRAMEN  : RCC_AHBENR.SRAMEN .T;
            DMA2EN  : RCC_AHBENR.DMA2EN .T;
            DMAEN   : RCC_AHBENR.DMAEN  .T;
         end record;

      for T use
         record
            TSCEN   at 0 range TSCEN  .F .. TSCEN  .L;
            IOPFEN  at 0 range IOPFEN .F .. IOPFEN .L;
            IOPEEN  at 0 range IOPEEN .F .. IOPEEN .L;
            IOPDEN  at 0 range IOPDEN .F .. IOPDEN .L;
            IOPCEN  at 0 range IOPCEN .F .. IOPCEN .L;
            IOPBEN  at 0 range IOPBEN .F .. IOPBEN .L;
            IOPAEN  at 0 range IOPAEN .F .. IOPAEN .L;
            CRCEN   at 0 range CRCEN  .F .. CRCEN  .L;
            FLITFEN at 0 range FLITFEN.F .. FLITFEN.L;
            SRAMEN  at 0 range SRAMEN .F .. SRAMEN .L;
            DMA2EN  at 0 range DMA2EN .F .. DMA2EN .L;
            DMAEN   at 0 range DMAEN  .F .. DMAEN  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHBENR;

   package AHBENR    is new Register (RCC_AHBENR.T, RCC_AHBENR.Tp, 16#4002_1014#);
   subtype AHBENR_T  is AHBENR.T;
   subtype AHBENR_F  is AHBENR.F;
   

   --  Field definitions

   function TSCEN   is new AHBENR.B (RCC_AHBENR.TSCEN  ) with Inline_Always;
   function IOPFEN  is new AHBENR.B (RCC_AHBENR.IOPFEN ) with Inline_Always;
   function IOPEEN  is new AHBENR.B (RCC_AHBENR.IOPEEN ) with Inline_Always;
   function IOPDEN  is new AHBENR.B (RCC_AHBENR.IOPDEN ) with Inline_Always;
   function IOPCEN  is new AHBENR.B (RCC_AHBENR.IOPCEN ) with Inline_Always;
   function IOPBEN  is new AHBENR.B (RCC_AHBENR.IOPBEN ) with Inline_Always;
   function IOPAEN  is new AHBENR.B (RCC_AHBENR.IOPAEN ) with Inline_Always;
   function CRCEN   is new AHBENR.B (RCC_AHBENR.CRCEN  ) with Inline_Always;
   function FLITFEN is new AHBENR.B (RCC_AHBENR.FLITFEN) with Inline_Always;
   function SRAMEN  is new AHBENR.B (RCC_AHBENR.SRAMEN ) with Inline_Always;
   function DMA2EN  is new AHBENR.B (RCC_AHBENR.DMA2EN ) with Inline_Always;
   function DMAEN   is new AHBENR.B (RCC_AHBENR.DMAEN  ) with Inline_Always;

   --  Functions

   function "+"  is new AHBENR.Add      with Inline_Always;
   function "+"  is new AHBENR.Add_F    with Inline_Always;
   function "+"  is new AHBENR.Add_FF   with Inline_Always;
   function "-"  is new AHBENR.Clear    with Inline_Always;
   function "-"  is new AHBENR.Clear_FF with Inline_Always;
   function "="  is new AHBENR.Equal    with Inline_Always;
   function Init is new AHBENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.TSCEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.TSCEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.IOPFEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.IOPFEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.IOPEEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.IOPEEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.IOPDEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.IOPDEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.IOPCEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.IOPCEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.IOPBEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.IOPBEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.IOPAEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.IOPAEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.CRCEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.CRCEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.FLITFEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.FLITFEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.SRAMEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.SRAMEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.DMA2EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.DMA2EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHBENR.C (RCC_AHBENR.DMAEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHBENR.C (RCC_AHBENR.DMAEN, 2#1#) with Inline_Always;


   ---------------------------------------------
   --  APB2 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package RCC_APB2ENR is

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
            SDA3EN   : RCC_APB2ENR.SDA3EN  .T;
            SDA2EN   : RCC_APB2ENR.SDA2EN  .T;
            SDA1EN   : RCC_APB2ENR.SDA1EN  .T;
            TIM19EN  : RCC_APB2ENR.TIM19EN .T;
            TIM17EN  : RCC_APB2ENR.TIM17EN .T;
            TIM16EN  : RCC_APB2ENR.TIM16EN .T;
            TIM15EN  : RCC_APB2ENR.TIM15EN .T;
            USART1EN : RCC_APB2ENR.USART1EN.T;
            SPI1EN   : RCC_APB2ENR.SPI1EN  .T;
            ADCEN    : RCC_APB2ENR.ADCEN   .T;
            SYSCFGEN : RCC_APB2ENR.SYSCFGEN.T;
         end record;

      for T use
         record
            SDA3EN   at 0 range SDA3EN  .F .. SDA3EN  .L;
            SDA2EN   at 0 range SDA2EN  .F .. SDA2EN  .L;
            SDA1EN   at 0 range SDA1EN  .F .. SDA1EN  .L;
            TIM19EN  at 0 range TIM19EN .F .. TIM19EN .L;
            TIM17EN  at 0 range TIM17EN .F .. TIM17EN .L;
            TIM16EN  at 0 range TIM16EN .F .. TIM16EN .L;
            TIM15EN  at 0 range TIM15EN .F .. TIM15EN .L;
            USART1EN at 0 range USART1EN.F .. USART1EN.L;
            SPI1EN   at 0 range SPI1EN  .F .. SPI1EN  .L;
            ADCEN    at 0 range ADCEN   .F .. ADCEN   .L;
            SYSCFGEN at 0 range SYSCFGEN.F .. SYSCFGEN.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_APB2ENR;

   package APB2ENR    is new Register (RCC_APB2ENR.T, RCC_APB2ENR.Tp, 16#4002_1018#);
   subtype APB2ENR_T  is APB2ENR.T;
   subtype APB2ENR_F  is APB2ENR.F;
   

   --  Field definitions

   function SDA3EN   is new APB2ENR.B (RCC_APB2ENR.SDA3EN  ) with Inline_Always;
   function SDA2EN   is new APB2ENR.B (RCC_APB2ENR.SDA2EN  ) with Inline_Always;
   function SDA1EN   is new APB2ENR.B (RCC_APB2ENR.SDA1EN  ) with Inline_Always;
   function TIM19EN  is new APB2ENR.B (RCC_APB2ENR.TIM19EN ) with Inline_Always;
   function TIM17EN  is new APB2ENR.B (RCC_APB2ENR.TIM17EN ) with Inline_Always;
   function TIM16EN  is new APB2ENR.B (RCC_APB2ENR.TIM16EN ) with Inline_Always;
   function TIM15EN  is new APB2ENR.B (RCC_APB2ENR.TIM15EN ) with Inline_Always;
   function USART1EN is new APB2ENR.B (RCC_APB2ENR.USART1EN) with Inline_Always;
   function SPI1EN   is new APB2ENR.B (RCC_APB2ENR.SPI1EN  ) with Inline_Always;
   function ADCEN    is new APB2ENR.B (RCC_APB2ENR.ADCEN   ) with Inline_Always;
   function SYSCFGEN is new APB2ENR.B (RCC_APB2ENR.SYSCFGEN) with Inline_Always;

   --  Functions

   function "+"  is new APB2ENR.Add      with Inline_Always;
   function "+"  is new APB2ENR.Add_F    with Inline_Always;
   function "+"  is new APB2ENR.Add_FF   with Inline_Always;
   function "-"  is new APB2ENR.Clear    with Inline_Always;
   function "-"  is new APB2ENR.Clear_FF with Inline_Always;
   function "="  is new APB2ENR.Equal    with Inline_Always;
   function Init is new APB2ENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.SDA3EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.SDA3EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.SDA2EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.SDA2EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.SDA1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.SDA1EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.TIM19EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.TIM19EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.TIM17EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.TIM17EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.TIM16EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.TIM16EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.TIM15EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.TIM15EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.USART1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.USART1EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.SPI1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.SPI1EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.ADCEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.ADCEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.SYSCFGEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.SYSCFGEN, 2#1#) with Inline_Always;

   ---------------------------------------------
   --  APB1 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package RCC_APB1ENR is

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
            CECEN    : RCC_APB1ENR.CECEN   .T;
            DAC1EN   : RCC_APB1ENR.DAC1EN  .T;
            PWREN    : RCC_APB1ENR.PWREN   .T;
            DAC2EN   : RCC_APB1ENR.DAC2EN  .T;
            CANEN    : RCC_APB1ENR.CANEN   .T;
            USBEN    : RCC_APB1ENR.USBEN   .T;
            I2C2EN   : RCC_APB1ENR.I2C2EN  .T;
            I2C1EN   : RCC_APB1ENR.I2C1EN  .T;
            USART3EN : RCC_APB1ENR.USART3EN.T;
            USART2EN : RCC_APB1ENR.USART2EN.T;
            SPI3EN   : RCC_APB1ENR.SPI3EN  .T;
            SPI2EN   : RCC_APB1ENR.SPI2EN  .T;
            WWDGEN   : RCC_APB1ENR.WWDGEN  .T;
            TIM18EN  : RCC_APB1ENR.TIM18EN .T;
            TIM14EN  : RCC_APB1ENR.TIM14EN .T;
            TIM13EN  : RCC_APB1ENR.TIM13EN .T;
            TIM12EN  : RCC_APB1ENR.TIM12EN .T;
            TIM7EN   : RCC_APB1ENR.TIM7EN  .T;
            TIM6EN   : RCC_APB1ENR.TIM6EN  .T;
            TIM5EN   : RCC_APB1ENR.TIM5EN  .T;
            TIM4EN   : RCC_APB1ENR.TIM4EN  .T;
            TIM3EN   : RCC_APB1ENR.TIM3EN  .T;
            TIM2EN   : RCC_APB1ENR.TIM2EN  .T;
         end record;

      for T use
         record
            CECEN    at 0 range CECEN   .F .. CECEN   .L;
            DAC1EN   at 0 range DAC1EN  .F .. DAC1EN  .L;
            PWREN    at 0 range PWREN   .F .. PWREN   .L;
            DAC2EN   at 0 range DAC2EN  .F .. DAC2EN  .L;
            CANEN    at 0 range CANEN   .F .. CANEN   .L;
            USBEN    at 0 range USBEN   .F .. USBEN   .L;
            I2C2EN   at 0 range I2C2EN  .F .. I2C2EN  .L;
            I2C1EN   at 0 range I2C1EN  .F .. I2C1EN  .L;
            USART3EN at 0 range USART3EN.F .. USART3EN.L;
            USART2EN at 0 range USART2EN.F .. USART2EN.L;
            SPI3EN   at 0 range SPI3EN  .F .. SPI3EN  .L;
            SPI2EN   at 0 range SPI2EN  .F .. SPI2EN  .L;
            WWDGEN   at 0 range WWDGEN  .F .. WWDGEN  .L;
            TIM18EN  at 0 range TIM18EN .F .. TIM18EN .L;
            TIM14EN  at 0 range TIM14EN .F .. TIM14EN .L;
            TIM13EN  at 0 range TIM13EN .F .. TIM13EN .L;
            TIM12EN  at 0 range TIM12EN .F .. TIM12EN .L;
            TIM7EN   at 0 range TIM7EN  .F .. TIM7EN  .L;
            TIM6EN   at 0 range TIM6EN  .F .. TIM6EN  .L;
            TIM5EN   at 0 range TIM5EN  .F .. TIM5EN  .L;
            TIM4EN   at 0 range TIM4EN  .F .. TIM4EN  .L;
            TIM3EN   at 0 range TIM3EN  .F .. TIM3EN  .L;
            TIM2EN   at 0 range TIM2EN  .F .. TIM2EN  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_APB1ENR;

   package APB1ENR    is new Register (RCC_APB1ENR.T, RCC_APB1ENR.Tp, 16#4002_101C#);
   subtype APB1ENR_T  is APB1ENR.T;
   subtype APB1ENR_F  is APB1ENR.F;
   

   --  Field definitions

   function CECEN    is new APB1ENR.B (RCC_APB1ENR.CECEN   ) with Inline_Always;
   function DAC1EN   is new APB1ENR.B (RCC_APB1ENR.DAC1EN  ) with Inline_Always;
   function PWREN    is new APB1ENR.B (RCC_APB1ENR.PWREN   ) with Inline_Always;
   function DAC2EN   is new APB1ENR.B (RCC_APB1ENR.DAC2EN  ) with Inline_Always;
   function CANEN    is new APB1ENR.B (RCC_APB1ENR.CANEN   ) with Inline_Always;
   function USBEN    is new APB1ENR.B (RCC_APB1ENR.USBEN   ) with Inline_Always;
   function I2C2EN   is new APB1ENR.B (RCC_APB1ENR.I2C2EN  ) with Inline_Always;
   function I2C1EN   is new APB1ENR.B (RCC_APB1ENR.I2C1EN  ) with Inline_Always;
   function USART3EN is new APB1ENR.B (RCC_APB1ENR.USART3EN) with Inline_Always;
   function USART2EN is new APB1ENR.B (RCC_APB1ENR.USART2EN) with Inline_Always;
   function SPI3EN   is new APB1ENR.B (RCC_APB1ENR.SPI3EN  ) with Inline_Always;
   function SPI2EN   is new APB1ENR.B (RCC_APB1ENR.SPI2EN  ) with Inline_Always;
   function WWDGEN   is new APB1ENR.B (RCC_APB1ENR.WWDGEN  ) with Inline_Always;
   function TIM18EN  is new APB1ENR.B (RCC_APB1ENR.TIM18EN ) with Inline_Always;
   function TIM14EN  is new APB1ENR.B (RCC_APB1ENR.TIM14EN ) with Inline_Always;
   function TIM13EN  is new APB1ENR.B (RCC_APB1ENR.TIM13EN ) with Inline_Always;
   function TIM12EN  is new APB1ENR.B (RCC_APB1ENR.TIM12EN ) with Inline_Always;
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

   --  Constant definitions

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.CECEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.CECEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.DAC1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.DAC1EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.PWREN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.PWREN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.DAC2EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.DAC2EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.CANEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.CANEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.USBEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.USBEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.I2C2EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.I2C2EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.I2C1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.I2C1EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.USART3EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.USART3EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.USART2EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.USART2EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.SPI3EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.SPI3EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.SPI2EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.SPI2EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.WWDGEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.WWDGEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.TIM18EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.TIM18EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.TIM14EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.TIM14EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.TIM13EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.TIM13EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.TIM12EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.TIM12EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.TIM7EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.TIM7EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.TIM6EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.TIM6EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.TIM5EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.TIM5EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.TIM4EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.TIM4EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.TIM3EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.TIM3EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.TIM2EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.TIM2EN, 2#1#) with Inline_Always;

   --------------------------------------
   --  Backup Domain Control Register  --
   --------------------------------------

   package RCC_BDCR is

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
            BDRTS  : RCC_BDCR.BDRTS .T;
            RTCEN  : RCC_BDCR.RTCEN .T;
            RTCSEL : RCC_BDCR.RTCSEL.T;
            LSEDRV : RCC_BDCR.LSEDRV.T;
            LSEBYP : RCC_BDCR.LSEBYP.T;
            LSERDY : RCC_BDCR.LSERDY.T;
            LSEON  : RCC_BDCR.LSEON .T;
         end record;

      for T use
         record
            BDRTS  at 0 range BDRTS .F .. BDRTS .L;
            RTCEN  at 0 range RTCEN .F .. RTCEN .L;
            RTCSEL at 0 range RTCSEL.F .. RTCSEL.L;
            LSEDRV at 0 range LSEDRV.F .. LSEDRV.L;
            LSEBYP at 0 range LSEBYP.F .. LSEBYP.L;
            LSERDY at 0 range LSERDY.F .. LSERDY.L;
            LSEON  at 0 range LSEON .F .. LSEON .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_BDCR;

   package BDCR    is new Register (RCC_BDCR.T, RCC_BDCR.Tp, 16#4002_1020#);
   subtype BDCR_T  is BDCR.T;
   subtype BDCR_F  is BDCR.F;
   

   --  Field definitions

   function BDRTS  is new BDCR.B (RCC_BDCR.BDRTS ) with Inline_Always;
   function RTCEN  is new BDCR.B (RCC_BDCR.RTCEN ) with Inline_Always;
   function RTCSEL is new BDCR.B (RCC_BDCR.RTCSEL) with Inline_Always;
   function LSEDRV is new BDCR.B (RCC_BDCR.LSEDRV) with Inline_Always;
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

   function Does_Not_Reset is new BDCR.C (RCC_BDCR.BDRTS, 2#0#) with Inline_Always;
   function Resets         is new BDCR.C (RCC_BDCR.BDRTS, 2#1#) with Inline_Always;

   function Clock_Disabled is new BDCR.C (RCC_BDCR.RTCEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new BDCR.C (RCC_BDCR.RTCEN, 2#1#) with Inline_Always;

   function NoClock                      is new BDCR.C (RCC_BDCR.RTCSEL, 2#00#) with Inline_Always;
   function LSE_Oscillator               is new BDCR.C (RCC_BDCR.RTCSEL, 2#01#) with Inline_Always;
   function LSI_Oscillator               is new BDCR.C (RCC_BDCR.RTCSEL, 2#10#) with Inline_Always;
   function HSE_Oscillator_Divided_By_32 is new BDCR.C (RCC_BDCR.RTCSEL, 2#11#) with Inline_Always;

   function XTAL_Mode_Low_Driving         is new BDCR.C (RCC_BDCR.LSEDRV, 2#00#) with Inline_Always;
   function XTAL_Mode_Medium_Driving      is new BDCR.C (RCC_BDCR.LSEDRV, 2#01#) with Inline_Always;
   function XTAL_Mode_Medium_High_Driving is new BDCR.C (RCC_BDCR.LSEDRV, 2#10#) with Inline_Always;
   function XTAL_Mode_Higher_Driving      is new BDCR.C (RCC_BDCR.LSEDRV, 2#11#) with Inline_Always;

   function LSE_Oscillator_Not_Bypassed is new BDCR.C (RCC_BDCR.LSEBYP, 2#0#) with Inline_Always;
   function LSE_Oscillator_Bypassed     is new BDCR.C (RCC_BDCR.LSEBYP, 2#1#) with Inline_Always;

   function LSE_Oscillator_Not_Ready is new BDCR.C (RCC_BDCR.LSERDY, 2#0#) with Inline_Always;
   function LSE_Oscillator_Ready     is new BDCR.C (RCC_BDCR.LSERDY, 2#1#) with Inline_Always;

   function LSE_Oscillator_Off is new BDCR.C (RCC_BDCR.LSEON, 2#0#) with Inline_Always;
   function LSE_Oscillator_On  is new BDCR.C (RCC_BDCR.LSEON, 2#1#) with Inline_Always;

   -------------------------------
   --  Control/Status Register  --
   -------------------------------

   package RCC_CSR is

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
            LPWRRSTF : RCC_CSR.LPWRRSTF.T;
            WWDGRSTF : RCC_CSR.WWDGRSTF.T;
            IWDGRSTF : RCC_CSR.IWDGRSTF.T;
            SFTRSTF  : RCC_CSR.SFTRSTF .T;
            PORRSTF  : RCC_CSR.PORRSTF .T;
            PINRSTF  : RCC_CSR.PINRSTF .T;
            OBLRSTF  : RCC_CSR.OBLRSTF .T;
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
            OBLRSTF  at 0 range OBLRSTF .F .. OBLRSTF .L;
            RMVF     at 0 range RMVF    .F .. RMVF    .L;
            LSIRDY   at 0 range LSIRDY  .F .. LSIRDY  .L;
            LSION    at 0 range LSION   .F .. LSION   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_CSR;

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
   function OBLRSTF  is new CSR.B (RCC_CSR.OBLRSTF ) with Inline_Always;
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

   function No_LowPower_Management_Reset_Occurred is new CSR.C (RCC_CSR.LPWRRSTF, 2#0#) with Inline_Always;
   function LowPower_Management_Reset_Occurred    is new CSR.C (RCC_CSR.LPWRRSTF, 2#1#) with Inline_Always;

   function No_Window_Watchdog_Reset_Occurred is new CSR.C (RCC_CSR.WWDGRSTF, 2#0#) with Inline_Always;
   function Window_Watchdog_Reset_Occurred    is new CSR.C (RCC_CSR.WWDGRSTF, 2#1#) with Inline_Always;

   function No_Watchdog_Reset_Occurred is new CSR.C (RCC_CSR.IWDGRSTF, 2#0#) with Inline_Always;
   function Watchdog_Reset_Occurred    is new CSR.C (RCC_CSR.IWDGRSTF, 2#1#) with Inline_Always;

   function No_Software_Reset_Occurred is new CSR.C (RCC_CSR.SFTRSTF, 2#0#) with Inline_Always;
   function Software_Reset_Occurred    is new CSR.C (RCC_CSR.SFTRSTF, 2#1#) with Inline_Always;

   function No_POR_PDR_Reset_Ocurred is new CSR.C (RCC_CSR.PORRSTF, 2#0#) with Inline_Always;
   function POR_PDR_Reset_Ocurred    is new CSR.C (RCC_CSR.PORRSTF, 2#1#) with Inline_Always;

   function No_Reset_From_NRST_Pin_Occurred is new CSR.C (RCC_CSR.PINRSTF, 2#0#) with Inline_Always;
   function Reset_From_NRST_Pin_Occurred    is new CSR.C (RCC_CSR.PINRSTF, 2#1#) with Inline_Always;

   function No_Reset_From_OBL_Ocurred is new CSR.C (RCC_CSR.OBLRSTF, 2#0#) with Inline_Always;
   function Reset_From_OBL_Ocurred    is new CSR.C (RCC_CSR.OBLRSTF, 2#1#) with Inline_Always;

   function Clear_Flag is new CSR.C (RCC_CSR.RMVF, 2#1#) with Inline_Always;

   function LSI_Oscillator_Not_Ready is new CSR.C (RCC_CSR.LSIRDY, 2#0#) with Inline_Always;
   function LSI_Oscillator_Ready     is new CSR.C (RCC_CSR.LSIRDY, 2#1#) with Inline_Always;

   function LSI_Oscillator_Off is new CSR.C (RCC_CSR.LSION, 2#0#) with Inline_Always;
   function LSI_Oscillator_On  is new CSR.C (RCC_CSR.LSION, 2#1#) with Inline_Always;


   -------------------------------------------
   --  AHB Peripheral Clock Reset Register  --
   -------------------------------------------

   package RCC_AHBRSTR is

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
            TSCRST   : RCC_AHBRSTR.TSCRST  .T;
            IOPFRST  : RCC_AHBRSTR.IOPFRST .T;
            IOPERST  : RCC_AHBRSTR.IOPERST .T;
            IOPDRST  : RCC_AHBRSTR.IOPDRST .T;
            IOPCRST  : RCC_AHBRSTR.IOPCRST .T;
            IOPBRST  : RCC_AHBRSTR.IOPBRST .T;
            IOPARST  : RCC_AHBRSTR.IOPARST .T;
         end record;

      for T use
         record
            TSCRST   at 0 range TSCRST  .F .. TSCRST  .L;
            IOPFRST  at 0 range IOPFRST .F .. IOPFRST .L;
            IOPERST  at 0 range IOPERST .F .. IOPERST .L;
            IOPDRST  at 0 range IOPDRST .F .. IOPDRST .L;
            IOPCRST  at 0 range IOPCRST .F .. IOPCRST .L;
            IOPBRST  at 0 range IOPBRST .F .. IOPBRST .L;
            IOPARST  at 0 range IOPARST .F .. IOPARST .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHBRSTR;

   package AHBRSTR    is new Register (RCC_AHBRSTR.T, RCC_AHBRSTR.Tp, 16#4002_1028#);
   subtype AHBRSTR_T  is AHBRSTR.T;
   subtype AHBRSTR_F  is AHBRSTR.F;
   

   --  Field definitions

   function TSCRST   is new AHBRSTR.B (RCC_AHBRSTR.TSCRST  ) with Inline_Always;
   function IOPFRST  is new AHBRSTR.B (RCC_AHBRSTR.IOPFRST ) with Inline_Always;
   function IOPERST  is new AHBRSTR.B (RCC_AHBRSTR.IOPERST ) with Inline_Always;
   function IOPDRST  is new AHBRSTR.B (RCC_AHBRSTR.IOPDRST ) with Inline_Always;
   function IOPCRST  is new AHBRSTR.B (RCC_AHBRSTR.IOPCRST ) with Inline_Always;
   function IOPBRST  is new AHBRSTR.B (RCC_AHBRSTR.IOPBRST ) with Inline_Always;
   function IOPARST  is new AHBRSTR.B (RCC_AHBRSTR.IOPARST ) with Inline_Always;

   --  Functions

   function "+"  is new AHBRSTR.Add      with Inline_Always;
   function "+"  is new AHBRSTR.Add_F    with Inline_Always;
   function "+"  is new AHBRSTR.Add_FF   with Inline_Always;
   function "-"  is new AHBRSTR.Clear    with Inline_Always;
   function "-"  is new AHBRSTR.Clear_FF with Inline_Always;
   function "="  is new AHBRSTR.Equal    with Inline_Always;
   function Init is new AHBRSTR.Init     with Inline_Always;

   --  Constant definitions

   function Does_Not_Reset is new AHBRSTR.C (RCC_AHBRSTR.TSCRST, 2#0#) with Inline_Always;
   function Resets         is new AHBRSTR.C (RCC_AHBRSTR.TSCRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHBRSTR.C (RCC_AHBRSTR.IOPFRST, 2#0#) with Inline_Always;
   function Resets         is new AHBRSTR.C (RCC_AHBRSTR.IOPFRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHBRSTR.C (RCC_AHBRSTR.IOPERST, 2#0#) with Inline_Always;
   function Resets         is new AHBRSTR.C (RCC_AHBRSTR.IOPERST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHBRSTR.C (RCC_AHBRSTR.IOPDRST, 2#0#) with Inline_Always;
   function Resets         is new AHBRSTR.C (RCC_AHBRSTR.IOPDRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHBRSTR.C (RCC_AHBRSTR.IOPCRST, 2#0#) with Inline_Always;
   function Resets         is new AHBRSTR.C (RCC_AHBRSTR.IOPCRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHBRSTR.C (RCC_AHBRSTR.IOPBRST, 2#0#) with Inline_Always;
   function Resets         is new AHBRSTR.C (RCC_AHBRSTR.IOPBRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHBRSTR.C (RCC_AHBRSTR.IOPARST, 2#0#) with Inline_Always;
   function Resets         is new AHBRSTR.C (RCC_AHBRSTR.IOPARST, 2#1#) with Inline_Always;

   --------------------------------
   --  Configuration Register 2  --
   --------------------------------

   package RCC_CFGR2 is

      package Tp is new Types (R32);

      package PREDIV    is new Bitfield (Tp,  0, 4);

      type T is
         record
            PREDIV    : RCC_CFGR2.PREDIV.T;
         end record;

      for T use
         record
            PREDIV    at 0 range PREDIV.F .. PREDIV.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_CFGR2;

   package CFGR2    is new Register (RCC_CFGR2.T, RCC_CFGR2.Tp, 16#4002_102C#);
   subtype CFGR2_T  is CFGR2.T;
   subtype CFGR2_F  is CFGR2.F;
   

   --  Field definitions

   function PREDIV    is new CFGR2.B (RCC_CFGR2.PREDIV) with Inline_Always;

   --  Functions

   function "+"  is new CFGR2.Add      with Inline_Always;
   function "+"  is new CFGR2.Add_F    with Inline_Always;
   function "+"  is new CFGR2.Add_FF   with Inline_Always;
   function "-"  is new CFGR2.Clear    with Inline_Always;
   function "-"  is new CFGR2.Clear_FF with Inline_Always;
   function "="  is new CFGR2.Equal    with Inline_Always;
   function Init is new CFGR2.Init     with Inline_Always;

   --  Constant definitions

   function HSE_Input_To_PLL_Not_Divided    is new CFGR2.C (RCC_CFGR2.PREDIV, 2#0000#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_2   is new CFGR2.C (RCC_CFGR2.PREDIV, 2#0001#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_3   is new CFGR2.C (RCC_CFGR2.PREDIV, 2#0010#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_4   is new CFGR2.C (RCC_CFGR2.PREDIV, 2#0011#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_5   is new CFGR2.C (RCC_CFGR2.PREDIV, 2#0100#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_6   is new CFGR2.C (RCC_CFGR2.PREDIV, 2#0101#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_7   is new CFGR2.C (RCC_CFGR2.PREDIV, 2#0110#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_8   is new CFGR2.C (RCC_CFGR2.PREDIV, 2#0111#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_9   is new CFGR2.C (RCC_CFGR2.PREDIV, 2#1000#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_10  is new CFGR2.C (RCC_CFGR2.PREDIV, 2#1001#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_11  is new CFGR2.C (RCC_CFGR2.PREDIV, 2#1010#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_12  is new CFGR2.C (RCC_CFGR2.PREDIV, 2#1011#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_13  is new CFGR2.C (RCC_CFGR2.PREDIV, 2#1100#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_14  is new CFGR2.C (RCC_CFGR2.PREDIV, 2#1101#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_15  is new CFGR2.C (RCC_CFGR2.PREDIV, 2#1110#) with Inline_Always;
   function HSE_Input_To_PLL_Divided_By_16  is new CFGR2.C (RCC_CFGR2.PREDIV, 2#1111#) with Inline_Always;

   --------------------------------
   --  Configuration Register 3  --
   --------------------------------

   package RCC_CFGR3 is

      package Tp is new Types (R32);

      package USART3SW is new Bitfield (Tp, 18, 2);
      package USART2SW is new Bitfield (Tp, 16, 2);
      package CECSW    is new Bitfield (Tp, 6);
      package I2C2SW   is new Bitfield (Tp, 5);
      package I2C1SW   is new Bitfield (Tp, 4);
      package USART1SW is new Bitfield (Tp, 0, 2);

      type T is
         record
            USART3SW : RCC_CFGR3.USART3SW.T;
            USART2SW : RCC_CFGR3.USART2SW.T;
            CECSW    : RCC_CFGR3.CECSW   .T;
            I2C2SW   : RCC_CFGR3.I2C2SW  .T;
            I2C1SW   : RCC_CFGR3.I2C1SW  .T;
            USART1SW : RCC_CFGR3.USART1SW.T;
         end record;

      for T use
         record
            USART3SW at 0 range USART3SW.F .. USART3SW.L;
            USART2SW at 0 range USART2SW.F .. USART2SW.L;
            CECSW    at 0 range CECSW   .F .. CECSW   .L;
            I2C2SW   at 0 range I2C2SW  .F .. I2C2SW  .L;
            I2C1SW   at 0 range I2C1SW  .F .. I2C1SW  .L;
            USART1SW at 0 range USART1SW.F .. USART1SW.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_CFGR3;

   package CFGR3    is new Register (RCC_CFGR3.T, RCC_CFGR3.Tp, 16#4002_1030#);
   subtype CFGR3_T  is CFGR3.T;
   subtype CFGR3_F  is CFGR3.F;
   

   --  Field definitions

   function USART3SW is new CFGR3.B (RCC_CFGR3.USART3SW) with Inline_Always;
   function USART2SW is new CFGR3.B (RCC_CFGR3.USART2SW) with Inline_Always;
   function CECSW    is new CFGR3.B (RCC_CFGR3.CECSW   ) with Inline_Always;
   function I2C2SW   is new CFGR3.B (RCC_CFGR3.I2C2SW  ) with Inline_Always;
   function I2C1SW   is new CFGR3.B (RCC_CFGR3.I2C1SW  ) with Inline_Always;
   function USART1SW is new CFGR3.B (RCC_CFGR3.USART1SW) with Inline_Always;

   --  Functions

   function "+"  is new CFGR3.Add      with Inline_Always;
   function "+"  is new CFGR3.Add_F    with Inline_Always;
   function "+"  is new CFGR3.Add_FF   with Inline_Always;
   function "-"  is new CFGR3.Clear    with Inline_Always;
   function "-"  is new CFGR3.Clear_FF with Inline_Always;
   function "="  is new CFGR3.Equal    with Inline_Always;
   function Init is new CFGR3.Init     with Inline_Always;

   --  Constant definitions

   function PCLK_Selected         is new CFGR3.C (RCC_CFGR3.USART3SW, 2#00#) with Inline_Always;
   function System_Clock_Selected is new CFGR3.C (RCC_CFGR3.USART3SW, 2#01#) with Inline_Always;
   function LSE_Clock_Selected    is new CFGR3.C (RCC_CFGR3.USART3SW, 2#10#) with Inline_Always;
   function HSI_Clock_Selected    is new CFGR3.C (RCC_CFGR3.USART3SW, 2#11#) with Inline_Always;

   function PCLK_Selected         is new CFGR3.C (RCC_CFGR3.USART2SW, 2#00#) with Inline_Always;
   function System_Clock_Selected is new CFGR3.C (RCC_CFGR3.USART2SW, 2#01#) with Inline_Always;
   function LSE_Clock_Selected    is new CFGR3.C (RCC_CFGR3.USART2SW, 2#10#) with Inline_Always;
   function HSI_Clock_Selected    is new CFGR3.C (RCC_CFGR3.USART2SW, 2#11#) with Inline_Always;

   function HSI_Clock_Divided_By_244 is new CFGR3.C (RCC_CFGR3.CECSW, 2#0#) with Inline_Always;
   function LSE_Clock                is new CFGR3.C (RCC_CFGR3.CECSW, 2#1#) with Inline_Always;

   function HSI_Clock_Selected    is new CFGR3.C (RCC_CFGR3.I2C2SW, 2#0#) with Inline_Always;
   function System_Clock_Selected is new CFGR3.C (RCC_CFGR3.I2C2SW, 2#1#) with Inline_Always;

   function HSI_Clock_Selected    is new CFGR3.C (RCC_CFGR3.I2C1SW, 2#0#) with Inline_Always;
   function System_Clock_Selected is new CFGR3.C (RCC_CFGR3.I2C1SW, 2#1#) with Inline_Always;

   function PCLK_Selected         is new CFGR3.C (RCC_CFGR3.USART1SW, 2#00#) with Inline_Always;
   function System_Clock_Selected is new CFGR3.C (RCC_CFGR3.USART1SW, 2#01#) with Inline_Always;
   function LSE_Clock_Selected    is new CFGR3.C (RCC_CFGR3.USART1SW, 2#10#) with Inline_Always;
   function HSI_Clock_Selected    is new CFGR3.C (RCC_CFGR3.USART1SW, 2#11#) with Inline_Always;

end ARM.Register.RCC_F37XXX;
