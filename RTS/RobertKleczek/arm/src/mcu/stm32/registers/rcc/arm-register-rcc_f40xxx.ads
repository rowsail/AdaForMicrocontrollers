--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 A R M . R e g i s t e r . R C C _ F 4 0 X X X              --
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

--  Package defines STM32 F40XXX family reset & clock related registers

--  NEED UPGRADE

-------------------------
--  Imported packages  --
-------------------------

with ARM.Register;
use  ARM.Register;

with ARM.Register.Types;
with ARM.Register.Bitfield;
with ARM.Register.Register;

--------------------------------------------------------------------------------
--                            ARM.Register.RCC_F40XXX                         --
--------------------------------------------------------------------------------

package ARM.Register.RCC_F40XXX is

   pragma Preelaborate;

   --------------------------------------------------------------------------
   --                    Reset and Clock Control                           --
   --------------------------------------------------------------------------

   ----------------------
   --  CR  Register --
   ----------------------

   package RCC_CR is

      package Tp is new Types (R32);

      package PLLI2SRDY is new Bitfield (Tp, 27);
      package PLLI2SON  is new Bitfield (Tp, 26);
      package PLLRDY    is new Bitfield (Tp, 25);
      package PLLON     is new Bitfield (Tp, 24);
      package CSSON     is new Bitfield (Tp, 19);
      package HSEBYP    is new Bitfield (Tp, 18);
      package HSERDY    is new Bitfield (Tp, 17);
      package HSEON     is new Bitfield (Tp, 16);
      package HSICAL    is new Bitfield (Tp, 8, 8);
      package HSITRIM   is new Bitfield (Tp, 3, 5);
      package HSIRDY    is new Bitfield (Tp, 1);
      package HSION     is new Bitfield (Tp, 0);

      type T is
         record
            PLLI2SRDY : RCC_CR.PLLI2SRDY.T;
            PLLI2SON  : RCC_CR.PLLI2SON .T;
            PLLRDY    : RCC_CR.PLLRDY   .T;
            PLLON     : RCC_CR.PLLON    .T;
            CSSON     : RCC_CR.CSSON    .T;
            HSEBYP    : RCC_CR.HSEBYP   .T;
            HSERDY    : RCC_CR.HSERDY   .T;
            HSEON     : RCC_CR.HSEON    .T;
            HSICAL    : RCC_CR.HSICAL   .T;
            HSITRIM   : RCC_CR.HSITRIM  .T;
            HSIRDY    : RCC_CR.HSIRDY   .T;
            HSION     : RCC_CR.HSION    .T;
         end record;

      for T use
         record
            PLLI2SRDY at 0 range PLLI2SRDY.F .. PLLI2SRDY.L;
            PLLI2SON  at 0 range PLLI2SON .F .. PLLI2SON .L;
            PLLRDY    at 0 range PLLRDY   .F .. PLLRDY   .L;
            PLLON     at 0 range PLLON    .F .. PLLON    .L;
            CSSON     at 0 range CSSON    .F .. CSSON    .L;
            HSEBYP    at 0 range HSEBYP   .F .. HSEBYP   .L;
            HSERDY    at 0 range HSERDY   .F .. HSERDY   .L;
            HSEON     at 0 range HSEON    .F .. HSEON    .L;
            HSICAL    at 0 range HSICAL   .F .. HSICAL   .L;
            HSITRIM   at 0 range HSITRIM  .F .. HSITRIM  .L;
            HSIRDY    at 0 range HSIRDY   .F .. HSIRDY   .L;
            HSION     at 0 range HSION    .F .. HSION    .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_CR;

   package CR is new Register (RCC_CR.T, RCC_CR.Tp, 16#4002_3800#, 16#0000_0083#);
   subtype CR_T  is CR.T;
   subtype CR_F  is CR.F;
   

   --  Field definitions

   function PLLI2SRDY is new CR.B (RCC_CR.PLLI2SRDY) with Inline_Always;
   function PLLI2SON  is new CR.B (RCC_CR.PLLI2SON ) with Inline_Always;
   function PLLRDY    is new CR.B (RCC_CR.PLLRDY   ) with Inline_Always;
   function PLLON     is new CR.B (RCC_CR.PLLON    ) with Inline_Always;
   function CSSON     is new CR.B (RCC_CR.CSSON    ) with Inline_Always;
   function HSEBYP    is new CR.B (RCC_CR.HSEBYP   ) with Inline_Always;
   function HSERDY    is new CR.B (RCC_CR.HSERDY   ) with Inline_Always;
   function HSEON     is new CR.B (RCC_CR.HSEON    ) with Inline_Always;
   function HSICAL    is new CR.B (RCC_CR.HSICAL   ) with Inline_Always;
   function HSITRIM   is new CR.B (RCC_CR.HSITRIM  ) with Inline_Always;
   function HSIRDY    is new CR.B (RCC_CR.HSIRDY   ) with Inline_Always;
   function HSION     is new CR.B (RCC_CR.HSION    ) with Inline_Always;

   --  Functions

   function "+"  is new CR.Add      with Inline_Always;
   function "+"  is new CR.Add_F    with Inline_Always;
   function "+"  is new CR.Add_FF   with Inline_Always;
   function "-"  is new CR.Clear    with Inline_Always;
   function "-"  is new CR.Clear_FF with Inline_Always;
   function "="  is new CR.Equal    with Inline_Always;
   function Init is new CR.Init     with Inline_Always;

   --  Constant definitions

   function PLLI2S_Unlocked is new CR.C (RCC_CR.PLLI2SRDY, 2#0#) with Inline_Always;
   function PLLI2S_Locked   is new CR.C (RCC_CR.PLLI2SRDY, 2#1#) with Inline_Always;

   function PLLI2S_Off is new CR.C (RCC_CR.PLLI2SON, 2#0#) with Inline_Always;
   function PLLI2S_On  is new CR.C (RCC_CR.PLLI2SON, 2#1#) with Inline_Always;

   function PLL_Unlocked is new CR.C (RCC_CR.PLLRDY, 2#0#) with Inline_Always;
   function PLL_Locked   is new CR.C (RCC_CR.PLLRDY, 2#1#) with Inline_Always;

   function PLL_Off is new CR.C (RCC_CR.PLLON, 2#0#) with Inline_Always;
   function PLL_On  is new CR.C (RCC_CR.PLLON, 2#1#) with Inline_Always;

   function Clock_Security_System_Off is new CR.C (RCC_CR.CSSON, 2#0#) with Inline_Always;
   function Clock_Security_System_On  is new CR.C (RCC_CR.CSSON, 2#1#) with Inline_Always;

   function Oscillator_Not_Bypassed is new CR.C (RCC_CR.HSEBYP, 2#0#) with Inline_Always;
   function Oscillator_Bypassed     is new CR.C (RCC_CR.HSEBYP, 2#1#) with Inline_Always;

   function Oscillator_Not_Ready is new CR.C (RCC_CR.HSERDY, 2#0#) with Inline_Always;
   function Oscillator_Ready     is new CR.C (RCC_CR.HSERDY, 2#1#) with Inline_Always;

   function Oscillator_Off is new CR.C (RCC_CR.HSEON, 2#0#) with Inline_Always;
   function Oscillator_On  is new CR.C (RCC_CR.HSEON, 2#1#) with Inline_Always;

   function Oscillator_Not_Ready is new CR.C (RCC_CR.HSIRDY, 2#0#) with Inline_Always;
   function Oscillator_Ready     is new CR.C (RCC_CR.HSIRDY, 2#1#) with Inline_Always;

   function Oscillator_Off is new CR.C (RCC_CR.HSION, 2#0#) with Inline_Always;
   function Oscillator_On  is new CR.C (RCC_CR.HSION, 2#1#) with Inline_Always;

   -----------------------------------
   --  PLL Configuration  Register  --
   -----------------------------------

   package RCC_PLLCFGR is

      package Tp is new Types (R32);

      package PLLQ   is new Bitfield (Tp, 24, 4);
      package PLLSRC is new Bitfield (Tp, 22);
      package PLLP   is new Bitfield (Tp, 16, 2);
      package PLLN   is new Bitfield (Tp, 6, 9);
      package PLLM   is new Bitfield (Tp, 0, 6);

      type T is
         record
            PLLQ   : RCC_PLLCFGR.PLLQ  .T;
            PLLSRC : RCC_PLLCFGR.PLLSRC.T;
            PLLP   : RCC_PLLCFGR.PLLP  .T;
            PLLN   : RCC_PLLCFGR.PLLN  .T;
            PLLM   : RCC_PLLCFGR.PLLM  .T;
         end record;

      for T use
         record
            PLLQ   at 0 range PLLQ  .F .. PLLQ  .L;
            PLLSRC at 0 range PLLSRC.F .. PLLSRC.L;
            PLLP   at 0 range PLLP  .F .. PLLP  .L;
            PLLN   at 0 range PLLN  .F .. PLLN  .L;
            PLLM   at 0 range PLLM  .F .. PLLM  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_PLLCFGR;

   package PLLCFGR    is new Register (RCC_PLLCFGR.T, RCC_PLLCFGR.Tp, 16#4002_3804#, 16#2400_3010#);
   subtype PLLCFGR_T  is PLLCFGR.T;
   subtype PLLCFGR_F  is PLLCFGR.F;
   

   --  Field definitions

   function PLLQ   is new PLLCFGR.B (RCC_PLLCFGR.PLLQ  ) with Inline_Always;
   function PLLSRC is new PLLCFGR.B (RCC_PLLCFGR.PLLSRC) with Inline_Always;
   function PLLP   is new PLLCFGR.B (RCC_PLLCFGR.PLLP  ) with Inline_Always;
   function PLLN   is new PLLCFGR.B (RCC_PLLCFGR.PLLN  ) with Inline_Always;
   function PLLM   is new PLLCFGR.B (RCC_PLLCFGR.PLLM  ) with Inline_Always;

   --  Functions

   function "+"  is new PLLCFGR.Add      with Inline_Always;
   function "+"  is new PLLCFGR.Add_F    with Inline_Always;
   function "+"  is new PLLCFGR.Add_FF   with Inline_Always;
   function "-"  is new PLLCFGR.Clear    with Inline_Always;
   function "-"  is new PLLCFGR.Clear_FF with Inline_Always;
   function "="  is new PLLCFGR.Equal    with Inline_Always;
   function Init is new PLLCFGR.Init     with Inline_Always;

   --  Constant definitions

   function HSI_Clock is new PLLCFGR.C (RCC_PLLCFGR.PLLSRC, 2#0#) with Inline_Always;
   function HSE_Clock is new PLLCFGR.C (RCC_PLLCFGR.PLLSRC, 2#1#) with Inline_Always;

   function Division_Factor_2 is new PLLCFGR.C (RCC_PLLCFGR.PLLP, 2#00#) with Inline_Always;
   function Division_Factor_4 is new PLLCFGR.C (RCC_PLLCFGR.PLLP, 2#01#) with Inline_Always;
   function Division_Factor_6 is new PLLCFGR.C (RCC_PLLCFGR.PLLP, 2#10#) with Inline_Always;
   function Division_Factor_8 is new PLLCFGR.C (RCC_PLLCFGR.PLLP, 2#11#) with Inline_Always;

   ------------------------------
   --  Configuration Register  --
   ------------------------------

   package RCC_CFGR is

      package Tp is new Types (R32);

      package MCO2    is new Bitfield (Tp, 30, 2);
      package MCO2PRE is new Bitfield (Tp, 27, 3);
      package MCO1PRE is new Bitfield (Tp, 24, 3);
      package I2SSCR  is new Bitfield (Tp, 23);
      package MCO1    is new Bitfield (Tp, 21, 2);
      package RTCPRE  is new Bitfield (Tp, 16, 5);
      package PPRE2   is new Bitfield (Tp, 13, 3);
      package PPRE1   is new Bitfield (Tp, 10, 3);
      package HPRE    is new Bitfield (Tp, 4, 4);
      package SWS     is new Bitfield (Tp, 2, 2);
      package SW      is new Bitfield (Tp, 0, 2);

      type T is
         record
            MCO2    : RCC_CFGR .MCO2   . T;
            MCO2PRE : RCC_CFGR .MCO2PRE. T;
            MCO1PRE : RCC_CFGR .MCO1PRE. T;
            I2SSCR  : RCC_CFGR .I2SSCR . T;
            MCO1    : RCC_CFGR .MCO1   . T;
            RTCPRE  : RCC_CFGR .RTCPRE . T;
            PPRE2   : RCC_CFGR .PPRE2  . T;
            PPRE1   : RCC_CFGR .PPRE1  . T;
            HPRE    : RCC_CFGR .HPRE   . T;
            SWS     : RCC_CFGR .SWS    . T;
            SW      : RCC_CFGR .SW     . T;
         end record;

      for T use
         record
            MCO2    at 0 range MCO2   .F .. MCO2   .L;
            MCO2PRE at 0 range MCO2PRE.F .. MCO2PRE.L;
            MCO1PRE at 0 range MCO1PRE.F .. MCO1PRE.L;
            I2SSCR  at 0 range I2SSCR .F .. I2SSCR .L;
            MCO1    at 0 range MCO1   .F .. MCO1   .L;
            RTCPRE  at 0 range RTCPRE .F .. RTCPRE .L;
            PPRE2   at 0 range PPRE2  .F .. PPRE2  .L;
            PPRE1   at 0 range PPRE1  .F .. PPRE1  .L;
            HPRE    at 0 range HPRE   .F .. HPRE   .L;
            SWS     at 0 range SWS    .F .. SWS    .L;
            SW      at 0 range SW     .F .. SW     .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_CFGR;

   package CFGR    is new Register (RCC_CFGR.T, RCC_CFGR.Tp, 16#4002_3808#);
   subtype CFGR_T  is CFGR.T;
   subtype CFGR_F  is CFGR.F;
   

   --  Field definitions

   function MCO2    is new CFGR.B (RCC_CFGR.MCO2   ) with Inline_Always;
   function MCO2PRE is new CFGR.B (RCC_CFGR.MCO2PRE) with Inline_Always;
   function MCO1PRE is new CFGR.B (RCC_CFGR.MCO1PRE) with Inline_Always;
   function I2SSCR  is new CFGR.B (RCC_CFGR.I2SSCR ) with Inline_Always;
   function MCO1    is new CFGR.B (RCC_CFGR.MCO1   ) with Inline_Always;
   function RTCPRE  is new CFGR.B (RCC_CFGR.RTCPRE ) with Inline_Always;
   function PPRE2   is new CFGR.B (RCC_CFGR.PPRE2  ) with Inline_Always;
   function PPRE1   is new CFGR.B (RCC_CFGR.PPRE1  ) with Inline_Always;
   function HPRE    is new CFGR.B (RCC_CFGR.HPRE   ) with Inline_Always;
   function SWS     is new CFGR.B (RCC_CFGR.SWS    ) with Inline_Always;
   function SW      is new CFGR.B (RCC_CFGR.SW     ) with Inline_Always;

   --  Functions

   function "+"  is new CFGR.Add      with Inline_Always;
   function "+"  is new CFGR.Add_F    with Inline_Always;
   function "+"  is new CFGR.Add_FF   with Inline_Always;
   function "-"  is new CFGR.Clear    with Inline_Always;
   function "-"  is new CFGR.Clear_FF with Inline_Always;
   function "="  is new CFGR.Equal    with Inline_Always;
   function Init is new CFGR.Init     with Inline_Always;


   --  Constant definitions

   function System_Clock         is new CFGR.C (RCC_CFGR.MCO2, 2#00#) with Inline_Always;
   function PLLI2S_Clock         is new CFGR.C (RCC_CFGR.MCO2, 2#01#) with Inline_Always;
   function HSE_Oscillator_Clock is new CFGR.C (RCC_CFGR.MCO2, 2#10#) with Inline_Always;
   function PLL_Clock            is new CFGR.C (RCC_CFGR.MCO2, 2#11#) with Inline_Always;

   function Not_Divided  is new CFGR.C (RCC_CFGR.MCO2PRE, 2#000#) with Inline_Always;
   function Divided_By_2 is new CFGR.C (RCC_CFGR.MCO2PRE, 2#100#) with Inline_Always;
   function Divided_By_3 is new CFGR.C (RCC_CFGR.MCO2PRE, 2#101#) with Inline_Always;
   function Divided_By_4 is new CFGR.C (RCC_CFGR.MCO2PRE, 2#110#) with Inline_Always;
   function Divided_By_5 is new CFGR.C (RCC_CFGR.MCO2PRE, 2#111#) with Inline_Always;

   function Not_Divided  is new CFGR.C (RCC_CFGR.MCO1PRE, 2#000#) with Inline_Always;
   function Divided_By_2 is new CFGR.C (RCC_CFGR.MCO1PRE, 2#100#) with Inline_Always;
   function Divided_By_3 is new CFGR.C (RCC_CFGR.MCO1PRE, 2#101#) with Inline_Always;
   function Divided_By_4 is new CFGR.C (RCC_CFGR.MCO1PRE, 2#110#) with Inline_Always;
   function Divided_By_5 is new CFGR.C (RCC_CFGR.MCO1PRE, 2#111#) with Inline_Always;

   function PLLI2S_Clock                          is new CFGR.C (RCC_CFGR.I2SSCR, 2#0#) with Inline_Always;
   function External_Clock_Mapped_On_The_I2S_CKIN is new CFGR.C (RCC_CFGR.I2SSCR, 2#1#) with Inline_Always;

   function HSI_Clock_Selected            is new CFGR.C (RCC_CFGR.MCO1, 2#00#) with Inline_Always;
   function LSE_Oscillator_Selected       is new CFGR.C (RCC_CFGR.MCO1, 2#01#) with Inline_Always;
   function HSE_Oscillator_Clock_Selected is new CFGR.C (RCC_CFGR.MCO1, 2#10#) with Inline_Always;
   function PLL_Clock_Selected            is new CFGR.C (RCC_CFGR.MCO1, 2#11#) with Inline_Always;

   function Not_Divided   is new CFGR.C (RCC_CFGR.PPRE2, 2#000#) with Inline_Always;
   function Divided_By_2  is new CFGR.C (RCC_CFGR.PPRE2, 2#100#) with Inline_Always;
   function Divided_By_4  is new CFGR.C (RCC_CFGR.PPRE2, 2#101#) with Inline_Always;
   function Divided_By_8  is new CFGR.C (RCC_CFGR.PPRE2, 2#110#) with Inline_Always;
   function Divided_By_16 is new CFGR.C (RCC_CFGR.PPRE2, 2#111#) with Inline_Always;

   function Not_Divided   is new CFGR.C (RCC_CFGR.PPRE1, 2#000#) with Inline_Always;
   function Divided_By_2  is new CFGR.C (RCC_CFGR.PPRE1, 2#100#) with Inline_Always;
   function Divided_By_4  is new CFGR.C (RCC_CFGR.PPRE1, 2#101#) with Inline_Always;
   function Divided_By_8  is new CFGR.C (RCC_CFGR.PPRE1, 2#110#) with Inline_Always;
   function Divided_By_16 is new CFGR.C (RCC_CFGR.PPRE1, 2#111#) with Inline_Always;

   function System_Clock_Not_Divided    is new CFGR.C (RCC_CFGR.HPRE, 2#0000#) with Inline_Always;
   function System_Clock_Divided_By_2   is new CFGR.C (RCC_CFGR.HPRE, 2#1000#) with Inline_Always;
   function System_Clock_Divided_By_4   is new CFGR.C (RCC_CFGR.HPRE, 2#1001#) with Inline_Always;
   function System_Clock_Divided_By_8   is new CFGR.C (RCC_CFGR.HPRE, 2#1010#) with Inline_Always;
   function System_Clock_Divided_By_16  is new CFGR.C (RCC_CFGR.HPRE, 2#1011#) with Inline_Always;
   function System_Clock_Divided_By_64  is new CFGR.C (RCC_CFGR.HPRE, 2#1100#) with Inline_Always;
   function System_Clock_Divided_By_128 is new CFGR.C (RCC_CFGR.HPRE, 2#1101#) with Inline_Always;
   function System_Clock_Divided_By_256 is new CFGR.C (RCC_CFGR.HPRE, 2#1110#) with Inline_Always;
   function System_Clock_Divided_By_512 is new CFGR.C (RCC_CFGR.HPRE, 2#1111#) with Inline_Always;

   function HSI_Oscillator_Selected is new CFGR.C (RCC_CFGR.SWS, 2#00#) with Inline_Always;
   function HSE_Oscillator_Selected is new CFGR.C (RCC_CFGR.SWS, 2#01#) with Inline_Always;
   function PLL_Selected            is new CFGR.C (RCC_CFGR.SWS, 2#10#) with Inline_Always;

   function HSI_Oscillator_Selected is new CFGR.C (RCC_CFGR.SW, 2#00#) with Inline_Always;
   function HSE_Oscillator_Selected is new CFGR.C (RCC_CFGR.SW, 2#01#) with Inline_Always;
   function PLL_Selected            is new CFGR.C (RCC_CFGR.SW, 2#10#) with Inline_Always;

   --------------------------
   --  Interrupt Register  --
   --------------------------

   package RCC_CIR is

      package Tp is new Types (R32);

      package CSSC        is new Bitfield (Tp, 23);
      package PLLI2SRDYC  is new Bitfield (Tp, 21);
      package PLLRDYC     is new Bitfield (Tp, 20);
      package HSERDYC     is new Bitfield (Tp, 19);
      package HSIRDYC     is new Bitfield (Tp, 18);
      package LSERDYC     is new Bitfield (Tp, 17);
      package LSIRDYC     is new Bitfield (Tp, 16);
      package PLLI2SRDYIE is new Bitfield (Tp, 13);
      package PLLRDYIE    is new Bitfield (Tp, 12);
      package HSERDYIE    is new Bitfield (Tp, 11);
      package HSIRDYIE    is new Bitfield (Tp, 10);
      package LSERDYIE    is new Bitfield (Tp, 9);
      package LSIRDYIE    is new Bitfield (Tp, 8);
      package CSSF        is new Bitfield (Tp, 7);
      package PLLI2SRDYF  is new Bitfield (Tp, 5);
      package PLLRDYF     is new Bitfield (Tp, 4);
      package HSERDYF     is new Bitfield (Tp, 3);
      package HSIRDYF     is new Bitfield (Tp, 2);
      package LSERDYF     is new Bitfield (Tp, 1);
      package LSIRDYF     is new Bitfield (Tp, 0);

      type T is
         record
            CSSC        : RCC_CIR.CSSC       .T;
            PLLI2SRDYC  : RCC_CIR.PLLI2SRDYC .T;
            PLLRDYC     : RCC_CIR.PLLRDYC    .T;
            HSERDYC     : RCC_CIR.HSERDYC    .T;
            HSIRDYC     : RCC_CIR.HSIRDYC    .T;
            LSERDYC     : RCC_CIR.LSERDYC    .T;
            LSIRDYC     : RCC_CIR.LSIRDYC    .T;
            PLLI2SRDYIE : RCC_CIR.PLLI2SRDYIE.T;
            PLLRDYIE    : RCC_CIR.PLLRDYIE   .T;
            HSERDYIE    : RCC_CIR.HSERDYIE   .T;
            HSIRDYIE    : RCC_CIR.HSIRDYIE   .T;
            LSERDYIE    : RCC_CIR.LSERDYIE   .T;
            LSIRDYIE    : RCC_CIR.LSIRDYIE   .T;
            CSSF        : RCC_CIR.CSSF       .T;
            PLLI2SRDYF  : RCC_CIR.PLLI2SRDYF .T;
            PLLRDYF     : RCC_CIR.PLLRDYF    .T;
            HSERDYF     : RCC_CIR.HSERDYF    .T;
            HSIRDYF     : RCC_CIR.HSIRDYF    .T;
            LSERDYF     : RCC_CIR.LSERDYF    .T;
            LSIRDYF     : RCC_CIR.LSIRDYF    .T;
         end record;

      for T use
         record
            CSSC        at 0 range CSSC       .F .. CSSC       .L;
            PLLI2SRDYC  at 0 range PLLI2SRDYC .F .. PLLI2SRDYC .L;
            PLLRDYC     at 0 range PLLRDYC    .F .. PLLRDYC    .L;
            HSERDYC     at 0 range HSERDYC    .F .. HSERDYC    .L;
            HSIRDYC     at 0 range HSIRDYC    .F .. HSIRDYC    .L;
            LSERDYC     at 0 range LSERDYC    .F .. LSERDYC    .L;
            LSIRDYC     at 0 range LSIRDYC    .F .. LSIRDYC    .L;
            PLLI2SRDYIE at 0 range PLLI2SRDYIE.F .. PLLI2SRDYIE.L;
            PLLRDYIE    at 0 range PLLRDYIE   .F .. PLLRDYIE   .L;
            HSERDYIE    at 0 range HSERDYIE   .F .. HSERDYIE   .L;
            HSIRDYIE    at 0 range HSIRDYIE   .F .. HSIRDYIE   .L;
            LSERDYIE    at 0 range LSERDYIE   .F .. LSERDYIE   .L;
            LSIRDYIE    at 0 range LSIRDYIE   .F .. LSIRDYIE   .L;
            CSSF        at 0 range CSSF       .F .. CSSF       .L;
            PLLI2SRDYF  at 0 range PLLI2SRDYF .F .. PLLI2SRDYF .L;
            PLLRDYF     at 0 range PLLRDYF    .F .. PLLRDYF    .L;
            HSERDYF     at 0 range HSERDYF    .F .. HSERDYF    .L;
            HSIRDYF     at 0 range HSIRDYF    .F .. HSIRDYF    .L;
            LSERDYF     at 0 range LSERDYF    .F .. LSERDYF    .L;
            LSIRDYF     at 0 range LSIRDYF    .F .. LSIRDYF    .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_CIR;

   package CIR    is new Register (RCC_CIR.T, RCC_CIR.Tp, 16#4002_380C#);
   subtype CIR_T  is CIR.T;
   subtype CIR_F  is CIR.F;
   

   --  Field definitions

   function CSSC        is new CIR.B (RCC_CIR.CSSC       ) with Inline_Always;
   function PLLI2SRDYC  is new CIR.B (RCC_CIR.PLLI2SRDYC ) with Inline_Always;
   function PLLRDYC     is new CIR.B (RCC_CIR.PLLRDYC    ) with Inline_Always;
   function HSERDYC     is new CIR.B (RCC_CIR.HSERDYC    ) with Inline_Always;
   function HSIRDYC     is new CIR.B (RCC_CIR.HSIRDYC    ) with Inline_Always;
   function LSERDYC     is new CIR.B (RCC_CIR.LSERDYC    ) with Inline_Always;
   function LSIRDYC     is new CIR.B (RCC_CIR.LSIRDYC    ) with Inline_Always;
   function PLLI2SRDYIE is new CIR.B (RCC_CIR.PLLI2SRDYIE) with Inline_Always;
   function PLLRDYIE    is new CIR.B (RCC_CIR.PLLRDYIE   ) with Inline_Always;
   function HSERDYIE    is new CIR.B (RCC_CIR.HSERDYIE   ) with Inline_Always;
   function HSIRDYIE    is new CIR.B (RCC_CIR.HSIRDYIE   ) with Inline_Always;
   function LSERDYIE    is new CIR.B (RCC_CIR.LSERDYIE   ) with Inline_Always;
   function LSIRDYIE    is new CIR.B (RCC_CIR.LSIRDYIE   ) with Inline_Always;
   function CSSF        is new CIR.B (RCC_CIR.CSSF       ) with Inline_Always;
   function PLLI2SRDYF  is new CIR.B (RCC_CIR.PLLI2SRDYF ) with Inline_Always;
   function PLLRDYF     is new CIR.B (RCC_CIR.PLLRDYF    ) with Inline_Always;
   function HSERDYF     is new CIR.B (RCC_CIR.HSERDYF    ) with Inline_Always;
   function HSIRDYF     is new CIR.B (RCC_CIR.HSIRDYF    ) with Inline_Always;
   function LSERDYF     is new CIR.B (RCC_CIR.LSERDYF    ) with Inline_Always;
   function LSIRDYF     is new CIR.B (RCC_CIR.LSIRDYF    ) with Inline_Always;

   --  Functions

   function "+"  is new CIR.Add      with Inline_Always;
   function "+"  is new CIR.Add_F    with Inline_Always;
   function "+"  is new CIR.Add_FF   with Inline_Always;
   function "-"  is new CIR.Clear    with Inline_Always;
   function "-"  is new CIR.Clear_FF with Inline_Always;
   function "="  is new CIR.Equal    with Inline_Always;
   function Init is new CIR.Init     with Inline_Always;

   --  Constant definitions

   function Clear_Flag is new CIR.C (RCC_CIR.CSSC, 2#1#) with Inline_Always;
   function Clear_Flag is new CIR.C (RCC_CIR.PLLI2SRDYC, 2#1#) with Inline_Always;
   function Clear_Flag is new CIR.C (RCC_CIR.PLLRDYC, 2#1#) with Inline_Always;
   function Clear_Flag is new CIR.C (RCC_CIR.HSERDYC, 2#1#) with Inline_Always;
   function Clear_Flag is new CIR.C (RCC_CIR.HSIRDYC, 2#1#) with Inline_Always;
   function Clear_Flag is new CIR.C (RCC_CIR.LSERDYC, 2#1#) with Inline_Always;
   function Clear_Flag is new CIR.C (RCC_CIR.LSIRDYC, 2#1#) with Inline_Always;

   function Lock_Interrupt_Disabled is new CIR.C (RCC_CIR.PLLI2SRDYIE, 2#0#) with Inline_Always;
   function Lock_Interrupt_Enabled  is new CIR.C (RCC_CIR.PLLI2SRDYIE, 2#1#) with Inline_Always;

   function Lock_Interrupt_Disabled is new CIR.C (RCC_CIR.PLLRDYIE, 2#0#) with Inline_Always;
   function Lock_Interrupt_Enabled  is new CIR.C (RCC_CIR.PLLRDYIE, 2#1#) with Inline_Always;

   function Lock_Interrupt_Disabled is new CIR.C (RCC_CIR.HSERDYIE, 2#0#) with Inline_Always;
   function Lock_Interrupt_Enabled  is new CIR.C (RCC_CIR.HSERDYIE, 2#1#) with Inline_Always;

   function Lock_Interrupt_Disabled is new CIR.C (RCC_CIR.HSIRDYIE, 2#0#) with Inline_Always;
   function Lock_Interrupt_Enabled  is new CIR.C (RCC_CIR.HSIRDYIE, 2#1#) with Inline_Always;

   function Lock_Interrupt_Disabled is new CIR.C (RCC_CIR.LSERDYIE, 2#0#) with Inline_Always;
   function Lock_Interrupt_Enabled  is new CIR.C (RCC_CIR.LSERDYIE, 2#1#) with Inline_Always;

   function Lock_Interrupt_Disabled is new CIR.C (RCC_CIR.LSIRDYIE, 2#0#) with Inline_Always;
   function Lock_Interrupt_Enabled  is new CIR.C (RCC_CIR.LSIRDYIE, 2#1#) with Inline_Always;


   function No_Clock_Security is new CIR.C (RCC_CIR.CSSF, 2#0#) with Inline_Always;
   function Clock_Security    is new CIR.C (RCC_CIR.CSSF, 2#1#) with Inline_Always;

   function No_Clock_Security is new CIR.C (RCC_CIR.PLLI2SRDYF, 2#0#) with Inline_Always;
   function Clock_Security    is new CIR.C (RCC_CIR.PLLI2SRDYF, 2#1#) with Inline_Always;

   function No_Clock_Security is new CIR.C (RCC_CIR.PLLRDYF, 2#0#) with Inline_Always;
   function Clock_Security    is new CIR.C (RCC_CIR.PLLRDYF, 2#1#) with Inline_Always;

   function No_Clock_Security is new CIR.C (RCC_CIR.HSERDYF, 2#0#) with Inline_Always;
   function Clock_Security    is new CIR.C (RCC_CIR.HSERDYF, 2#1#) with Inline_Always;

   function No_Clock_Security is new CIR.C (RCC_CIR.HSIRDYF, 2#0#) with Inline_Always;
   function Clock_Security    is new CIR.C (RCC_CIR.HSIRDYF, 2#1#) with Inline_Always;

   function No_Clock_Security is new CIR.C (RCC_CIR.LSERDYF, 2#0#) with Inline_Always;
   function Clock_Security    is new CIR.C (RCC_CIR.LSERDYF, 2#1#) with Inline_Always;

   function No_Clock_Security is new CIR.C (RCC_CIR.LSIRDYF, 2#0#) with Inline_Always;
   function Clock_Security    is new CIR.C (RCC_CIR.LSIRDYF, 2#1#) with Inline_Always;

   --------------------------------------
   --  AHB1 Peripheral Reset Register  --
   --------------------------------------

   package RCC_AHB1RSTR is

      package Tp is new Types (R32);

      package OTGHSRST  is new Bitfield (Tp, 29);
      package ETHMACRST is new Bitfield (Tp, 25);
      package DMA2RST   is new Bitfield (Tp, 22);
      package DMA1RST   is new Bitfield (Tp, 21);
      package CRCRST    is new Bitfield (Tp, 12);
      package GPIOIRST  is new Bitfield (Tp, 8);
      package GPIOHRST  is new Bitfield (Tp, 7);
      package GPIOGRST  is new Bitfield (Tp, 6);
      package GPIOFRST  is new Bitfield (Tp, 5);
      package GPIOERST  is new Bitfield (Tp, 4);
      package GPIODRST  is new Bitfield (Tp, 3);
      package GPIOCRST  is new Bitfield (Tp, 2);
      package GPIOBRST  is new Bitfield (Tp, 1);
      package GPIOARST  is new Bitfield (Tp, 0);

      type T is
         record
            OTGHSRST  : RCC_AHB1RSTR .OTGHSRST .T;
            ETHMACRST : RCC_AHB1RSTR .ETHMACRST.T;
            DMA2RST   : RCC_AHB1RSTR .DMA2RST  .T;
            DMA1RST   : RCC_AHB1RSTR .DMA1RST  .T;
            CRCRST    : RCC_AHB1RSTR .CRCRST   .T;
            GPIOIRST  : RCC_AHB1RSTR .GPIOIRST .T;
            GPIOHRST  : RCC_AHB1RSTR .GPIOHRST .T;
            GPIOGRST  : RCC_AHB1RSTR .GPIOGRST .T;
            GPIOFRST  : RCC_AHB1RSTR .GPIOFRST .T;
            GPIOERST  : RCC_AHB1RSTR .GPIOERST .T;
            GPIODRST  : RCC_AHB1RSTR .GPIODRST .T;
            GPIOCRST  : RCC_AHB1RSTR .GPIOCRST .T;
            GPIOBRST  : RCC_AHB1RSTR .GPIOBRST .T;
            GPIOARST  : RCC_AHB1RSTR .GPIOARST .T;
         end record;

      for T use
         record
            OTGHSRST  at 0 range OTGHSRST .F .. OTGHSRST .L;
            ETHMACRST at 0 range ETHMACRST.F .. ETHMACRST.L;
            DMA2RST   at 0 range DMA2RST  .F .. DMA2RST  .L;
            DMA1RST   at 0 range DMA1RST  .F .. DMA1RST  .L;
            CRCRST    at 0 range CRCRST   .F .. CRCRST   .L;
            GPIOIRST  at 0 range GPIOIRST .F .. GPIOIRST .L;
            GPIOHRST  at 0 range GPIOHRST .F .. GPIOHRST .L;
            GPIOGRST  at 0 range GPIOGRST .F .. GPIOGRST .L;
            GPIOFRST  at 0 range GPIOFRST .F .. GPIOFRST .L;
            GPIOERST  at 0 range GPIOERST .F .. GPIOERST .L;
            GPIODRST  at 0 range GPIODRST .F .. GPIODRST .L;
            GPIOCRST  at 0 range GPIOCRST .F .. GPIOCRST .L;
            GPIOBRST  at 0 range GPIOBRST .F .. GPIOBRST .L;
            GPIOARST  at 0 range GPIOARST .F .. GPIOARST .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHB1RSTR;

   package AHB1RSTR    is new Register (RCC_AHB1RSTR.T, RCC_AHB1RSTR.Tp, 16#4002_3810#);
   subtype AHB1RSTR_T  is AHB1RSTR.T;
   subtype AHB1RSTR_F  is AHB1RSTR.F;
   

   --  Field definitions

   function OTGHSRST  is new AHB1RSTR.B (RCC_AHB1RSTR.OTGHSRST ) with Inline_Always;
   function ETHMACRST is new AHB1RSTR.B (RCC_AHB1RSTR.ETHMACRST) with Inline_Always;
   function DMA2RST   is new AHB1RSTR.B (RCC_AHB1RSTR.DMA2RST  ) with Inline_Always;
   function DMA1RST   is new AHB1RSTR.B (RCC_AHB1RSTR.DMA1RST  ) with Inline_Always;
   function CRCRST    is new AHB1RSTR.B (RCC_AHB1RSTR.CRCRST   ) with Inline_Always;
   function GPIOIRST  is new AHB1RSTR.B (RCC_AHB1RSTR.GPIOIRST ) with Inline_Always;
   function GPIOHRST  is new AHB1RSTR.B (RCC_AHB1RSTR.GPIOHRST ) with Inline_Always;
   function GPIOGRST  is new AHB1RSTR.B (RCC_AHB1RSTR.GPIOGRST ) with Inline_Always;
   function GPIOFRST  is new AHB1RSTR.B (RCC_AHB1RSTR.GPIOFRST ) with Inline_Always;
   function GPIOERST  is new AHB1RSTR.B (RCC_AHB1RSTR.GPIOERST ) with Inline_Always;
   function GPIODRST  is new AHB1RSTR.B (RCC_AHB1RSTR.GPIODRST ) with Inline_Always;
   function GPIOCRST  is new AHB1RSTR.B (RCC_AHB1RSTR.GPIOCRST ) with Inline_Always;
   function GPIOBRST  is new AHB1RSTR.B (RCC_AHB1RSTR.GPIOBRST ) with Inline_Always;
   function GPIOARST  is new AHB1RSTR.B (RCC_AHB1RSTR.GPIOARST ) with Inline_Always;

   --  Functions

   function "+"  is new AHB1RSTR.Add      with Inline_Always;
   function "+"  is new AHB1RSTR.Add_F    with Inline_Always;
   function "+"  is new AHB1RSTR.Add_FF   with Inline_Always;
   function "-"  is new AHB1RSTR.Clear    with Inline_Always;
   function "-"  is new AHB1RSTR.Clear_FF with Inline_Always;
   function "="  is new AHB1RSTR.Equal    with Inline_Always;
   function Init is new AHB1RSTR.Init     with Inline_Always;

   --  Constant definitions

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.OTGHSRST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.OTGHSRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.ETHMACRST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.ETHMACRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.DMA2RST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.DMA2RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.DMA1RST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.DMA1RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.CRCRST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.CRCRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOIRST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOIRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOHRST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOHRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOGRST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOGRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOFRST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOFRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOERST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOERST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.GPIODRST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.GPIODRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOCRST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOCRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOBRST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOBRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOARST, 2#0#) with Inline_Always;
   function Resets         is new AHB1RSTR.C (RCC_AHB1RSTR.GPIOARST, 2#1#) with Inline_Always;

   --------------------------------------
   --  AHB2 Peripheral Reset Register  --
   --------------------------------------

   package RCC_AHB2RSTR is

      package Tp is new Types (R32);

      package OTGFSRST is new Bitfield (Tp, 7);
      package RNGRST   is new Bitfield (Tp, 6);
      package HASHRST  is new Bitfield (Tp, 5);
      package CRYPRST  is new Bitfield (Tp, 4);
      package DCMIRST  is new Bitfield (Tp, 0);

      type T is
         record
            OTGFSRST : RCC_AHB2RSTR.OTGFSRST.T;
            RNGRST   : RCC_AHB2RSTR.RNGRST  .T;
            HASHRST  : RCC_AHB2RSTR.HASHRST .T;
            CRYPRST  : RCC_AHB2RSTR.CRYPRST .T;
            DCMIRST  : RCC_AHB2RSTR.DCMIRST .T;
         end record;

      for T use
         record
            OTGFSRST at 0 range OTGFSRST.F .. OTGFSRST.L;
            RNGRST   at 0 range RNGRST  .F .. RNGRST  .L;
            HASHRST  at 0 range HASHRST .F .. HASHRST .L;
            CRYPRST  at 0 range CRYPRST .F .. CRYPRST .L;
            DCMIRST  at 0 range DCMIRST .F .. DCMIRST .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHB2RSTR;

   package AHB2RSTR    is new Register (RCC_AHB2RSTR.T, RCC_AHB2RSTR.Tp, 16#4002_3814#);
   subtype AHB2RSTR_T  is AHB2RSTR.T;
   subtype AHB2RSTR_F  is AHB2RSTR.F;
   

   --  Field definitions

   function OTGFSRST is new AHB2RSTR.B (RCC_AHB2RSTR.OTGFSRST) with Inline_Always;
   function RNGRST   is new AHB2RSTR.B (RCC_AHB2RSTR.RNGRST  ) with Inline_Always;
   function HASHRST  is new AHB2RSTR.B (RCC_AHB2RSTR.HASHRST ) with Inline_Always;
   function CRYPRST  is new AHB2RSTR.B (RCC_AHB2RSTR.CRYPRST ) with Inline_Always;
   function DCMIRST  is new AHB2RSTR.B (RCC_AHB2RSTR.DCMIRST ) with Inline_Always;

   --  Functions

   function "+"  is new AHB2RSTR.Add      with Inline_Always;
   function "+"  is new AHB2RSTR.Add_F    with Inline_Always;
   function "+"  is new AHB2RSTR.Add_FF   with Inline_Always;
   function "-"  is new AHB2RSTR.Clear    with Inline_Always;
   function "-"  is new AHB2RSTR.Clear_FF with Inline_Always;
   function "="  is new AHB2RSTR.Equal    with Inline_Always;
   function Init is new AHB2RSTR.Init     with Inline_Always;

   --  Constant definitions

   function Does_Not_Reset is new AHB2RSTR.C (RCC_AHB2RSTR.OTGFSRST, 2#0#) with Inline_Always;
   function Resets         is new AHB2RSTR.C (RCC_AHB2RSTR.OTGFSRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB2RSTR.C (RCC_AHB2RSTR.RNGRST, 2#0#) with Inline_Always;
   function Resets         is new AHB2RSTR.C (RCC_AHB2RSTR.RNGRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB2RSTR.C (RCC_AHB2RSTR.HASHRST, 2#0#) with Inline_Always;
   function Resets         is new AHB2RSTR.C (RCC_AHB2RSTR.HASHRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB2RSTR.C (RCC_AHB2RSTR.CRYPRST, 2#0#) with Inline_Always;
   function Resets         is new AHB2RSTR.C (RCC_AHB2RSTR.CRYPRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new AHB2RSTR.C (RCC_AHB2RSTR.DCMIRST, 2#0#) with Inline_Always;
   function Resets         is new AHB2RSTR.C (RCC_AHB2RSTR.DCMIRST, 2#1#) with Inline_Always;

   --------------------------------------
   --  AHB3 Peripheral Reset Register  --
   --------------------------------------

   package RCC_AHB3RSTR is

      package Tp is new Types (R32);

      package FSMCRST is new Bitfield (Tp, 0);

      type T is
         record
            FSMCRST : RCC_AHB3RSTR.FSMCRST.T;
         end record;

      for T use
         record
            FSMCRST at 0 range FSMCRST.F .. FSMCRST.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHB3RSTR;

   package AHB3RSTR    is new Register (RCC_AHB3RSTR.T, RCC_AHB3RSTR.Tp, 16#4002_3818#);
   subtype AHB3RSTR_T  is AHB3RSTR.T;
   subtype AHB3RSTR_F  is AHB3RSTR.F;
   

   --  Field definitions

   function FSMCRST is new AHB3RSTR.B (RCC_AHB3RSTR.FSMCRST) with Inline_Always;

   --  Functions

   function "+"  is new AHB3RSTR.Add      with Inline_Always;
   function "+"  is new AHB3RSTR.Add_F    with Inline_Always;
   function "+"  is new AHB3RSTR.Add_FF   with Inline_Always;
   function "-"  is new AHB3RSTR.Clear    with Inline_Always;
   function "-"  is new AHB3RSTR.Clear_FF with Inline_Always;
   function "="  is new AHB3RSTR.Equal    with Inline_Always;
   function Init is new AHB3RSTR.Init     with Inline_Always;

   --  Constant definitions

   function Does_Not_Reset is new AHB3RSTR.C (RCC_AHB3RSTR.FSMCRST, 2#0#) with Inline_Always;
   function Resets         is new AHB3RSTR.C (RCC_AHB3RSTR.FSMCRST, 2#1#) with Inline_Always;

   --------------------------------------
   --  APB1 Peripheral Reset Register  --
   --------------------------------------

   package RCC_APB1RSTR is

      package Tp is new Types (R32);

      package DACRST   is new Bitfield (Tp, 29);
      package PWRRST   is new Bitfield (Tp, 28);
      package CAN2RST  is new Bitfield (Tp, 26);
      package CAN1RST  is new Bitfield (Tp, 25);
      package I2C3RST  is new Bitfield (Tp, 23);
      package I2C2RST  is new Bitfield (Tp, 22);
      package I2C1RST  is new Bitfield (Tp, 21);
      package UART5RST is new Bitfield (Tp, 20);
      package UART4RST is new Bitfield (Tp, 19);
      package UART3RST is new Bitfield (Tp, 18);
      package UART2RST is new Bitfield (Tp, 17);
      package SPI3RST  is new Bitfield (Tp, 15);
      package SPI2RST  is new Bitfield (Tp, 14);
      package WWDGRST  is new Bitfield (Tp, 11);
      package TIM14RST is new Bitfield (Tp, 8);
      package TIM13RST is new Bitfield (Tp, 7);
      package TIM12RST is new Bitfield (Tp, 6);
      package TIM7RST  is new Bitfield (Tp, 5);
      package TIM6RST  is new Bitfield (Tp, 4);
      package TIM5RST  is new Bitfield (Tp, 3);
      package TIM4RST  is new Bitfield (Tp, 2);
      package TIM3RST  is new Bitfield (Tp, 1);
      package TIM2RST  is new Bitfield (Tp, 0);

      type T is
         record
            DACRST   : RCC_APB1RSTR.DACRST  .T;
            PWRRST   : RCC_APB1RSTR.PWRRST  .T;
            CAN2RST  : RCC_APB1RSTR.CAN2RST .T;
            CAN1RST  : RCC_APB1RSTR.CAN1RST .T;
            I2C3RST  : RCC_APB1RSTR.I2C3RST .T;
            I2C2RST  : RCC_APB1RSTR.I2C2RST .T;
            I2C1RST  : RCC_APB1RSTR.I2C1RST .T;
            UART5RST : RCC_APB1RSTR.UART5RST.T;
            UART4RST : RCC_APB1RSTR.UART4RST.T;
            UART3RST : RCC_APB1RSTR.UART3RST.T;
            UART2RST : RCC_APB1RSTR.UART2RST.T;
            SPI3RST  : RCC_APB1RSTR.SPI3RST .T;
            SPI2RST  : RCC_APB1RSTR.SPI2RST .T;
            WWDGRST  : RCC_APB1RSTR.WWDGRST .T;
            TIM14RST : RCC_APB1RSTR.TIM14RST.T;
            TIM13RST : RCC_APB1RSTR.TIM13RST.T;
            TIM12RST : RCC_APB1RSTR.TIM12RST.T;
            TIM7RST  : RCC_APB1RSTR.TIM7RST .T;
            TIM6RST  : RCC_APB1RSTR.TIM6RST .T;
            TIM5RST  : RCC_APB1RSTR.TIM5RST .T;
            TIM4RST  : RCC_APB1RSTR.TIM4RST .T;
            TIM3RST  : RCC_APB1RSTR.TIM3RST .T;
            TIM2RST  : RCC_APB1RSTR.TIM2RST .T;
         end record;

      for T use
         record
            DACRST   at 0 range DACRST  .F .. DACRST  .L;
            PWRRST   at 0 range PWRRST  .F .. PWRRST  .L;
            CAN2RST  at 0 range CAN2RST .F .. CAN2RST .L;
            CAN1RST  at 0 range CAN1RST .F .. CAN1RST .L;
            I2C3RST  at 0 range I2C3RST .F .. I2C3RST .L;
            I2C2RST  at 0 range I2C2RST .F .. I2C2RST .L;
            I2C1RST  at 0 range I2C1RST .F .. I2C1RST .L;
            UART5RST at 0 range UART5RST.F .. UART5RST.L;
            UART4RST at 0 range UART4RST.F .. UART4RST.L;
            UART3RST at 0 range UART3RST.F .. UART3RST.L;
            UART2RST at 0 range UART2RST.F .. UART2RST.L;
            SPI3RST  at 0 range SPI3RST .F .. SPI3RST .L;
            SPI2RST  at 0 range SPI2RST .F .. SPI2RST .L;
            WWDGRST  at 0 range WWDGRST .F .. WWDGRST .L;
            TIM14RST at 0 range TIM14RST.F .. TIM14RST.L;
            TIM13RST at 0 range TIM13RST.F .. TIM13RST.L;
            TIM12RST at 0 range TIM12RST.F .. TIM12RST.L;
            TIM7RST  at 0 range TIM7RST .F .. TIM7RST .L;
            TIM6RST  at 0 range TIM6RST .F .. TIM6RST .L;
            TIM5RST  at 0 range TIM5RST .F .. TIM5RST .L;
            TIM4RST  at 0 range TIM4RST .F .. TIM4RST .L;
            TIM3RST  at 0 range TIM3RST .F .. TIM3RST .L;
            TIM2RST  at 0 range TIM2RST .F .. TIM2RST .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_APB1RSTR;

   package APB1RSTR    is new Register (RCC_APB1RSTR.T, RCC_APB1RSTR.Tp, 16#4002_3820#);
   subtype APB1RSTR_T  is APB1RSTR.T;
   subtype APB1RSTR_F  is APB1RSTR.F;
   

   --  Field definitions

   function DACRST   is new APB1RSTR.B (RCC_APB1RSTR.DACRST  ) with Inline_Always;
   function PWRRST   is new APB1RSTR.B (RCC_APB1RSTR.PWRRST  ) with Inline_Always;
   function CAN2RST  is new APB1RSTR.B (RCC_APB1RSTR.CAN2RST ) with Inline_Always;
   function CAN1RST  is new APB1RSTR.B (RCC_APB1RSTR.CAN1RST ) with Inline_Always;
   function I2C3RST  is new APB1RSTR.B (RCC_APB1RSTR.I2C3RST ) with Inline_Always;
   function I2C2RST  is new APB1RSTR.B (RCC_APB1RSTR.I2C2RST ) with Inline_Always;
   function I2C1RST  is new APB1RSTR.B (RCC_APB1RSTR.I2C1RST ) with Inline_Always;
   function UART5RST is new APB1RSTR.B (RCC_APB1RSTR.UART5RST) with Inline_Always;
   function UART4RST is new APB1RSTR.B (RCC_APB1RSTR.UART4RST) with Inline_Always;
   function UART3RST is new APB1RSTR.B (RCC_APB1RSTR.UART3RST) with Inline_Always;
   function UART2RST is new APB1RSTR.B (RCC_APB1RSTR.UART2RST) with Inline_Always;
   function SPI3RST  is new APB1RSTR.B (RCC_APB1RSTR.SPI3RST ) with Inline_Always;
   function SPI2RST  is new APB1RSTR.B (RCC_APB1RSTR.SPI2RST ) with Inline_Always;
   function WWDGRST  is new APB1RSTR.B (RCC_APB1RSTR.WWDGRST ) with Inline_Always;
   function TIM14RST is new APB1RSTR.B (RCC_APB1RSTR.TIM14RST) with Inline_Always;
   function TIM13RST is new APB1RSTR.B (RCC_APB1RSTR.TIM13RST) with Inline_Always;
   function TIM12RST is new APB1RSTR.B (RCC_APB1RSTR.TIM12RST) with Inline_Always;
   function TIM7RST  is new APB1RSTR.B (RCC_APB1RSTR.TIM7RST ) with Inline_Always;
   function TIM6RST  is new APB1RSTR.B (RCC_APB1RSTR.TIM6RST ) with Inline_Always;
   function TIM5RST  is new APB1RSTR.B (RCC_APB1RSTR.TIM5RST ) with Inline_Always;
   function TIM4RST  is new APB1RSTR.B (RCC_APB1RSTR.TIM4RST ) with Inline_Always;
   function TIM3RST  is new APB1RSTR.B (RCC_APB1RSTR.TIM3RST ) with Inline_Always;
   function TIM2RST  is new APB1RSTR.B (RCC_APB1RSTR.TIM2RST ) with Inline_Always;

   --  Functions

   function "+"  is new APB1RSTR.Add      with Inline_Always;
   function "+"  is new APB1RSTR.Add_F    with Inline_Always;
   function "+"  is new APB1RSTR.Add_FF   with Inline_Always;
   function "-"  is new APB1RSTR.Clear    with Inline_Always;
   function "-"  is new APB1RSTR.Clear_FF with Inline_Always;
   function "="  is new APB1RSTR.Equal    with Inline_Always;
   function Init is new APB1RSTR.Init     with Inline_Always;

   --  Constant definitions

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.DACRST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.DACRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.PWRRST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.PWRRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.CAN2RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.CAN2RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.CAN1RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.CAN1RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.I2C3RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.I2C3RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.I2C2RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.I2C2RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.I2C1RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.I2C1RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.UART5RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.UART5RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.UART4RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.UART4RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.UART3RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.UART3RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.UART2RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.UART2RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.SPI3RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.SPI3RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.SPI2RST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.SPI2RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB1RSTR.C (RCC_APB1RSTR.WWDGRST, 2#0#) with Inline_Always;
   function Resets         is new APB1RSTR.C (RCC_APB1RSTR.WWDGRST, 2#1#) with Inline_Always;

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

   --------------------------------------
   --  APB2 Peripheral Reset Register  --
   --------------------------------------

   package RCC_APB2RSTR is

      package Tp is new Types (R32);

      package TIM11RST  is new Bitfield (Tp, 18);
      package TIM10RST  is new Bitfield (Tp, 17);
      package TIM9RST   is new Bitfield (Tp, 16);
      package SYSCFGRST is new Bitfield (Tp, 14);
      package SPI1RST   is new Bitfield (Tp, 12);
      package SDIORST   is new Bitfield (Tp, 11);
      package ADCRST    is new Bitfield (Tp, 8);
      package USART6RST is new Bitfield (Tp, 5);
      package USART1RST is new Bitfield (Tp, 4);
      package TIM8RST   is new Bitfield (Tp, 1);
      package TIM1RST   is new Bitfield (Tp, 0);

      type T is
         record
            TIM11RST  : RCC_APB2RSTR.TIM11RST .T;
            TIM10RST  : RCC_APB2RSTR.TIM10RST .T;
            TIM9RST   : RCC_APB2RSTR.TIM9RST  .T;
            SYSCFGRST : RCC_APB2RSTR.SYSCFGRST.T;
            SPI1RST   : RCC_APB2RSTR.SPI1RST  .T;
            SDIORST   : RCC_APB2RSTR.SDIORST  .T;
            ADCRST    : RCC_APB2RSTR.ADCRST   .T;
            USART6RST : RCC_APB2RSTR.USART6RST.T;
            USART1RST : RCC_APB2RSTR.USART1RST.T;
            TIM8RST   : RCC_APB2RSTR.TIM8RST  .T;
            TIM1RST   : RCC_APB2RSTR.TIM1RST  .T;
         end record;

      for T use
         record
            TIM11RST  at 0 range TIM11RST .F .. TIM11RST .L;
            TIM10RST  at 0 range TIM10RST .F .. TIM10RST .L;
            TIM9RST   at 0 range TIM9RST  .F .. TIM9RST  .L;
            SYSCFGRST at 0 range SYSCFGRST.F .. SYSCFGRST.L;
            SPI1RST   at 0 range SPI1RST  .F .. SPI1RST  .L;
            SDIORST   at 0 range SDIORST  .F .. SDIORST  .L;
            ADCRST    at 0 range ADCRST   .F .. ADCRST   .L;
            USART6RST at 0 range USART6RST.F .. USART6RST.L;
            USART1RST at 0 range USART1RST.F .. USART1RST.L;
            TIM8RST   at 0 range TIM8RST  .F .. TIM8RST  .L;
            TIM1RST   at 0 range TIM1RST  .F .. TIM1RST  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_APB2RSTR;

   package APB2RSTR    is new Register (RCC_APB2RSTR.T, RCC_APB2RSTR.Tp, 16#4002_3824#);
   subtype APB2RSTR_T  is APB2RSTR.T;
   subtype APB2RSTR_F  is APB2RSTR.F;
   

   --  Field definitions

   function TIM11RST  is new APB2RSTR.B (RCC_APB2RSTR.TIM11RST ) with Inline_Always;
   function TIM10RST  is new APB2RSTR.B (RCC_APB2RSTR.TIM10RST ) with Inline_Always;
   function TIM9RST   is new APB2RSTR.B (RCC_APB2RSTR.TIM9RST  ) with Inline_Always;
   function SYSCFGRST is new APB2RSTR.B (RCC_APB2RSTR.SYSCFGRST) with Inline_Always;
   function SPI1RST   is new APB2RSTR.B (RCC_APB2RSTR.SPI1RST  ) with Inline_Always;
   function SDIORST   is new APB2RSTR.B (RCC_APB2RSTR.SDIORST  ) with Inline_Always;
   function ADCRST    is new APB2RSTR.B (RCC_APB2RSTR.ADCRST   ) with Inline_Always;
   function USART6RST is new APB2RSTR.B (RCC_APB2RSTR.USART6RST) with Inline_Always;
   function USART1RST is new APB2RSTR.B (RCC_APB2RSTR.USART1RST) with Inline_Always;
   function TIM8RST   is new APB2RSTR.B (RCC_APB2RSTR.TIM8RST  ) with Inline_Always;
   function TIM1RST   is new APB2RSTR.B (RCC_APB2RSTR.TIM1RST  ) with Inline_Always;

   --  Functions

   function "+"  is new APB2RSTR.Add      with Inline_Always;
   function "+"  is new APB2RSTR.Add_F    with Inline_Always;
   function "+"  is new APB2RSTR.Add_FF   with Inline_Always;
   function "-"  is new APB2RSTR.Clear    with Inline_Always;
   function "-"  is new APB2RSTR.Clear_FF with Inline_Always;
   function "="  is new APB2RSTR.Equal    with Inline_Always;
   function Init is new APB2RSTR.Init     with Inline_Always;

   --  Constant definitions

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.TIM11RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.TIM11RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.TIM10RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.TIM10RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.TIM9RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.TIM9RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.SYSCFGRST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.SYSCFGRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.SPI1RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.SPI1RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.SDIORST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.SDIORST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.ADCRST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.ADCRST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.USART6RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.USART6RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.USART1RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.USART1RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.TIM8RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.TIM8RST, 2#1#) with Inline_Always;

   function Does_Not_Reset is new APB2RSTR.C (RCC_APB2RSTR.TIM1RST, 2#0#) with Inline_Always;
   function Resets         is new APB2RSTR.C (RCC_APB2RSTR.TIM1RST, 2#1#) with Inline_Always;

   --------------------------------------------
   --  AHB1 Peipheral Clock Enable Register  --
   --------------------------------------------

   package RCC_AHB1ENR is

      package Tp is new Types (R32);

      package OTGHSULPIEN  is new Bitfield (Tp, 30);
      package OTGHSEN      is new Bitfield (Tp, 29);
      package ETHMACPTPEN  is new Bitfield (Tp, 28);
      package ETHMACRXEN   is new Bitfield (Tp, 27);
      package ETHMACTXEN   is new Bitfield (Tp, 26);
      package ETHMACEN     is new Bitfield (Tp, 25);
      package DMA2EN       is new Bitfield (Tp, 22);
      package DMA1EN       is new Bitfield (Tp, 21);
      package CCMDATARAMEN is new Bitfield (Tp, 20);
      package BKPSRAMEN    is new Bitfield (Tp, 18);
      package CRCEN        is new Bitfield (Tp, 12);
      package GPIOIEN      is new Bitfield (Tp, 8);
      package GPIOHEN      is new Bitfield (Tp, 7);
      package GPIOGEN      is new Bitfield (Tp, 6);
      package GPIOFEN      is new Bitfield (Tp, 5);
      package GPIOEEN      is new Bitfield (Tp, 4);
      package GPIODEN      is new Bitfield (Tp, 3);
      package GPIOCEN      is new Bitfield (Tp, 2);
      package GPIOBEN      is new Bitfield (Tp, 1);
      package GPIOAEN      is new Bitfield (Tp, 0);

      type T is
         record
            OTGHSULPIEN  : RCC_AHB1ENR .OTGHSULPIEN . T;
            OTGHSEN      : RCC_AHB1ENR .OTGHSEN     . T;
            ETHMACPTPEN  : RCC_AHB1ENR .ETHMACPTPEN . T;
            ETHMACRXEN   : RCC_AHB1ENR .ETHMACRXEN  . T;
            ETHMACTXEN   : RCC_AHB1ENR .ETHMACTXEN  . T;
            ETHMACEN     : RCC_AHB1ENR .ETHMACEN    . T;
            DMA2EN       : RCC_AHB1ENR .DMA2EN      . T;
            DMA1EN       : RCC_AHB1ENR .DMA1EN      . T;
            CCMDATARAMEN : RCC_AHB1ENR .CCMDATARAMEN. T;
            BKPSRAMEN    : RCC_AHB1ENR .BKPSRAMEN   . T;
            CRCEN        : RCC_AHB1ENR .CRCEN       . T;
            GPIOIEN      : RCC_AHB1ENR .GPIOIEN     . T;
            GPIOHEN      : RCC_AHB1ENR .GPIOHEN     . T;
            GPIOGEN      : RCC_AHB1ENR .GPIOGEN     . T;
            GPIOFEN      : RCC_AHB1ENR .GPIOFEN     . T;
            GPIOEEN      : RCC_AHB1ENR .GPIOEEN     . T;
            GPIODEN      : RCC_AHB1ENR .GPIODEN     . T;
            GPIOCEN      : RCC_AHB1ENR .GPIOCEN     . T;
            GPIOBEN      : RCC_AHB1ENR .GPIOBEN     . T;
            GPIOAEN      : RCC_AHB1ENR .GPIOAEN     . T;
         end record;

      for T use
         record
            OTGHSULPIEN  at 0 range OTGHSULPIEN .F .. OTGHSULPIEN .L;
            OTGHSEN      at 0 range OTGHSEN     .F .. OTGHSEN     .L;
            ETHMACPTPEN  at 0 range ETHMACPTPEN .F .. ETHMACPTPEN .L;
            ETHMACRXEN   at 0 range ETHMACRXEN  .F .. ETHMACRXEN  .L;
            ETHMACTXEN   at 0 range ETHMACTXEN  .F .. ETHMACTXEN  .L;
            ETHMACEN     at 0 range ETHMACEN    .F .. ETHMACEN    .L;
            DMA2EN       at 0 range DMA2EN      .F .. DMA2EN      .L;
            DMA1EN       at 0 range DMA1EN      .F .. DMA1EN      .L;
            CCMDATARAMEN at 0 range CCMDATARAMEN.F .. CCMDATARAMEN.L;
            BKPSRAMEN    at 0 range BKPSRAMEN   .F .. BKPSRAMEN   .L;
            CRCEN        at 0 range CRCEN       .F .. CRCEN       .L;
            GPIOIEN      at 0 range GPIOIEN     .F .. GPIOIEN     .L;
            GPIOHEN      at 0 range GPIOHEN     .F .. GPIOHEN     .L;
            GPIOGEN      at 0 range GPIOGEN     .F .. GPIOGEN     .L;
            GPIOFEN      at 0 range GPIOFEN     .F .. GPIOFEN     .L;
            GPIOEEN      at 0 range GPIOEEN     .F .. GPIOEEN     .L;
            GPIODEN      at 0 range GPIODEN     .F .. GPIODEN     .L;
            GPIOCEN      at 0 range GPIOCEN     .F .. GPIOCEN     .L;
            GPIOBEN      at 0 range GPIOBEN     .F .. GPIOBEN     .L;
            GPIOAEN      at 0 range GPIOAEN     .F .. GPIOAEN     .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHB1ENR;

   package AHB1ENR    is new Register (RCC_AHB1ENR.T, RCC_AHB1ENR.Tp, 16#4002_3830#);
   subtype AHB1ENR_T  is AHB1ENR.T;
   subtype AHB1ENR_F  is AHB1ENR.F;
   

   --  Field definitions

   function OTGHSULPIEN  is new AHB1ENR.B (RCC_AHB1ENR.OTGHSULPIEN ) with Inline_Always;
   function OTGHSEN      is new AHB1ENR.B (RCC_AHB1ENR.OTGHSEN     ) with Inline_Always;
   function ETHMACPTPEN  is new AHB1ENR.B (RCC_AHB1ENR.ETHMACPTPEN ) with Inline_Always;
   function ETHMACRXEN   is new AHB1ENR.B (RCC_AHB1ENR.ETHMACRXEN  ) with Inline_Always;
   function ETHMACTXEN   is new AHB1ENR.B (RCC_AHB1ENR.ETHMACTXEN  ) with Inline_Always;
   function ETHMACEN     is new AHB1ENR.B (RCC_AHB1ENR.ETHMACEN    ) with Inline_Always;
   function DMA2EN       is new AHB1ENR.B (RCC_AHB1ENR.DMA2EN      ) with Inline_Always;
   function DMA1EN       is new AHB1ENR.B (RCC_AHB1ENR.DMA1EN      ) with Inline_Always;
   function CCMDATARAMEN is new AHB1ENR.B (RCC_AHB1ENR.CCMDATARAMEN) with Inline_Always;
   function BKPSRAMEN    is new AHB1ENR.B (RCC_AHB1ENR.BKPSRAMEN   ) with Inline_Always;
   function CRCEN        is new AHB1ENR.B (RCC_AHB1ENR.CRCEN       ) with Inline_Always;
   function GPIOIEN      is new AHB1ENR.B (RCC_AHB1ENR.GPIOIEN     ) with Inline_Always;
   function GPIOHEN      is new AHB1ENR.B (RCC_AHB1ENR.GPIOHEN     ) with Inline_Always;
   function GPIOGEN      is new AHB1ENR.B (RCC_AHB1ENR.GPIOGEN     ) with Inline_Always;
   function GPIOFEN      is new AHB1ENR.B (RCC_AHB1ENR.GPIOFEN     ) with Inline_Always;
   function GPIOEEN      is new AHB1ENR.B (RCC_AHB1ENR.GPIOEEN     ) with Inline_Always;
   function GPIODEN      is new AHB1ENR.B (RCC_AHB1ENR.GPIODEN     ) with Inline_Always;
   function GPIOCEN      is new AHB1ENR.B (RCC_AHB1ENR.GPIOCEN     ) with Inline_Always;
   function GPIOBEN      is new AHB1ENR.B (RCC_AHB1ENR.GPIOBEN     ) with Inline_Always;
   function GPIOAEN      is new AHB1ENR.B (RCC_AHB1ENR.GPIOAEN     ) with Inline_Always;

   --  Functions

   function "+"  is new AHB1ENR.Add      with Inline_Always;
   function "+"  is new AHB1ENR.Add_F    with Inline_Always;
   function "+"  is new AHB1ENR.Add_FF   with Inline_Always;
   function "-"  is new AHB1ENR.Clear    with Inline_Always;
   function "-"  is new AHB1ENR.Clear_FF with Inline_Always;
   function "="  is new AHB1ENR.Equal    with Inline_Always;
   function Init is new AHB1ENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.OTGHSULPIEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.OTGHSULPIEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.OTGHSEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.OTGHSEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.ETHMACPTPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.ETHMACPTPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.ETHMACRXEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.ETHMACRXEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.ETHMACTXEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.ETHMACTXEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.ETHMACEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.ETHMACEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.DMA2EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.DMA2EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.DMA1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.DMA1EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.CCMDATARAMEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.CCMDATARAMEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.BKPSRAMEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.BKPSRAMEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.CRCEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.CRCEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.GPIOIEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.GPIOIEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.GPIOHEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.GPIOHEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.GPIOGEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.GPIOGEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.GPIOFEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.GPIOFEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.GPIOEEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.GPIOEEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.GPIODEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.GPIODEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.GPIOCEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.GPIOCEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.GPIOBEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.GPIOBEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1ENR.C (RCC_AHB1ENR.GPIOAEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1ENR.C (RCC_AHB1ENR.GPIOAEN, 2#1#) with Inline_Always;

   ---------------------------------------------
   --  AHB2 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package RCC_AHB2ENR is

      package Tp is new Types (R32);

      package OTGFSEN is new Bitfield (Tp, 7);
      package RNGEN   is new Bitfield (Tp, 6);
      package HASHEN  is new Bitfield (Tp, 5);
      package CRYPEN  is new Bitfield (Tp, 4);
      package DCMIEN  is new Bitfield (Tp, 0);

      type T is
         record
            OTGFSEN : RCC_AHB2ENR.OTGFSEN.T;
            RNGEN   : RCC_AHB2ENR.RNGEN  .T;
            HASHEN  : RCC_AHB2ENR.HASHEN .T;
            CRYPEN  : RCC_AHB2ENR.CRYPEN .T;
            DCMIEN  : RCC_AHB2ENR.DCMIEN .T;
         end record;

      for T use
         record
            OTGFSEN at 0 range OTGFSEN.F .. OTGFSEN.L;
            RNGEN   at 0 range RNGEN  .F .. RNGEN  .L;
            HASHEN  at 0 range HASHEN .F .. HASHEN .L;
            CRYPEN  at 0 range CRYPEN .F .. CRYPEN .L;
            DCMIEN  at 0 range DCMIEN .F .. DCMIEN .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHB2ENR;

   package AHB2ENR    is new Register (RCC_AHB2ENR.T, RCC_AHB2ENR.Tp, 16#4002_3834#);
   subtype AHB2ENR_T  is AHB2ENR.T;
   subtype AHB2ENR_F  is AHB2ENR.F;
   

   --  Field definitions

   function OTGFSEN is new AHB2ENR.B (RCC_AHB2ENR.OTGFSEN) with Inline_Always;
   function RNGEN   is new AHB2ENR.B (RCC_AHB2ENR.RNGEN  ) with Inline_Always;
   function HASHEN  is new AHB2ENR.B (RCC_AHB2ENR.HASHEN ) with Inline_Always;
   function CRYPEN  is new AHB2ENR.B (RCC_AHB2ENR.CRYPEN ) with Inline_Always;
   function DCMIEN  is new AHB2ENR.B (RCC_AHB2ENR.DCMIEN ) with Inline_Always;

   --  Functions

   function "+"  is new AHB2ENR.Add      with Inline_Always;
   function "+"  is new AHB2ENR.Add_F    with Inline_Always;
   function "+"  is new AHB2ENR.Add_FF   with Inline_Always;
   function "-"  is new AHB2ENR.Clear    with Inline_Always;
   function "-"  is new AHB2ENR.Clear_FF with Inline_Always;
   function "="  is new AHB2ENR.Equal    with Inline_Always;
   function Init is new AHB2ENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new AHB2ENR.C (RCC_AHB2ENR.OTGFSEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB2ENR.C (RCC_AHB2ENR.OTGFSEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB2ENR.C (RCC_AHB2ENR.RNGEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB2ENR.C (RCC_AHB2ENR.RNGEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB2ENR.C (RCC_AHB2ENR.HASHEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB2ENR.C (RCC_AHB2ENR.HASHEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB2ENR.C (RCC_AHB2ENR.CRYPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB2ENR.C (RCC_AHB2ENR.CRYPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB2ENR.C (RCC_AHB2ENR.DCMIEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB2ENR.C (RCC_AHB2ENR.DCMIEN, 2#1#) with Inline_Always;

   ---------------------------------------------
   --  AHB3 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package RCC_AHB3ENR is

      package Tp is new Types (R32);

      package FSMCEN is new Bitfield (Tp, 0);

      type T is
         record
            FSMCEN : RCC_AHB3ENR.FSMCEN.T;
         end record;

      for T use
         record
            FSMCEN at 0 range FSMCEN.F .. FSMCEN.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHB3ENR;

   package AHB3ENR    is new Register (RCC_AHB3ENR.T, RCC_AHB3ENR.Tp, 16#4002_3838#);
   subtype AHB3ENR_T  is AHB3ENR.T;
   subtype AHB3ENR_F  is AHB3ENR.F;
   

   --  Field definitions

   function FSMCEN is new AHB3ENR.B (RCC_AHB3ENR.FSMCEN) with Inline_Always;

   --  Functions

   function "+"  is new AHB3ENR.Add      with Inline_Always;
   function "+"  is new AHB3ENR.Add_F    with Inline_Always;
   function "+"  is new AHB3ENR.Add_FF   with Inline_Always;
   function "-"  is new AHB3ENR.Clear    with Inline_Always;
   function "-"  is new AHB3ENR.Clear_FF with Inline_Always;
   function "="  is new AHB3ENR.Equal    with Inline_Always;
   function Init is new AHB3ENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new AHB3ENR.C (RCC_AHB3ENR.FSMCEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB3ENR.C (RCC_AHB3ENR.FSMCEN, 2#1#) with Inline_Always;

   ---------------------------------------------
   --  APB1 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package RCC_APB1ENR is

      package Tp is new Types (R32);

      package DACEN    is new Bitfield (Tp, 29);
      package PWREN    is new Bitfield (Tp, 28);
      package CAN2EN   is new Bitfield (Tp, 26);
      package CAN1EN   is new Bitfield (Tp, 25);
      package I2C3EN   is new Bitfield (Tp, 23);
      package I2C2EN   is new Bitfield (Tp, 22);
      package I2C1EN   is new Bitfield (Tp, 21);
      package UART5EN  is new Bitfield (Tp, 20);
      package UART4EN  is new Bitfield (Tp, 19);
      package USART3EN is new Bitfield (Tp, 18);
      package USART2EN is new Bitfield (Tp, 17);
      package SPI3EN   is new Bitfield (Tp, 15);
      package SPI2EN   is new Bitfield (Tp, 14);
      package WWDGEN   is new Bitfield (Tp, 11);
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
            DACEN    : RCC_APB1ENR.DACEN   .T;
            PWREN    : RCC_APB1ENR.PWREN   .T;
            CAN2EN   : RCC_APB1ENR.CAN2EN  .T;
            CAN1EN   : RCC_APB1ENR.CAN1EN  .T;
            I2C3EN   : RCC_APB1ENR.I2C3EN  .T;
            I2C2EN   : RCC_APB1ENR.I2C2EN  .T;
            I2C1EN   : RCC_APB1ENR.I2C1EN  .T;
            UART5EN  : RCC_APB1ENR.UART5EN .T;
            UART4EN  : RCC_APB1ENR.UART4EN .T;
            USART3EN : RCC_APB1ENR.USART3EN.T;
            USART2EN : RCC_APB1ENR.USART2EN.T;
            SPI3EN   : RCC_APB1ENR.SPI3EN  .T;
            SPI2EN   : RCC_APB1ENR.SPI2EN  .T;
            WWDGEN   : RCC_APB1ENR.WWDGEN  .T;
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
            DACEN    at 0 range DACEN   .F .. DACEN   .L;
            PWREN    at 0 range PWREN   .F .. PWREN   .L;
            CAN2EN   at 0 range CAN2EN  .F .. CAN2EN  .L;
            CAN1EN   at 0 range CAN1EN  .F .. CAN1EN  .L;
            I2C3EN   at 0 range I2C3EN  .F .. I2C3EN  .L;
            I2C2EN   at 0 range I2C2EN  .F .. I2C2EN  .L;
            I2C1EN   at 0 range I2C1EN  .F .. I2C1EN  .L;
            UART5EN  at 0 range UART5EN .F .. UART5EN .L;
            UART4EN  at 0 range UART4EN .F .. UART4EN .L;
            USART3EN at 0 range USART3EN.F .. USART3EN.L;
            USART2EN at 0 range USART2EN.F .. USART2EN.L;
            SPI3EN   at 0 range SPI3EN  .F .. SPI3EN  .L;
            SPI2EN   at 0 range SPI2EN  .F .. SPI2EN  .L;
            WWDGEN   at 0 range WWDGEN  .F .. WWDGEN  .L;
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

   package APB1ENR     is new Register (RCC_APB1ENR.T, RCC_APB1ENR.Tp, 16#4002_3840#);
   subtype APB1ENR_T   is APB1ENR.T;
   subtype APB1ENR_F   is APB1ENR.F;

   --  Field definitions

   function DACEN    is new APB1ENR.B (RCC_APB1ENR.DACEN   ) with Inline_Always;
   function PWREN    is new APB1ENR.B (RCC_APB1ENR.PWREN   ) with Inline_Always;
   function CAN2EN   is new APB1ENR.B (RCC_APB1ENR.CAN2EN  ) with Inline_Always;
   function CAN1EN   is new APB1ENR.B (RCC_APB1ENR.CAN1EN  ) with Inline_Always;
   function I2C3EN   is new APB1ENR.B (RCC_APB1ENR.I2C3EN  ) with Inline_Always;
   function I2C2EN   is new APB1ENR.B (RCC_APB1ENR.I2C2EN  ) with Inline_Always;
   function I2C1EN   is new APB1ENR.B (RCC_APB1ENR.I2C1EN  ) with Inline_Always;
   function UART5EN  is new APB1ENR.B (RCC_APB1ENR.UART5EN ) with Inline_Always;
   function UART4EN  is new APB1ENR.B (RCC_APB1ENR.UART4EN ) with Inline_Always;
   function USART3EN is new APB1ENR.B (RCC_APB1ENR.USART3EN) with Inline_Always;
   function USART2EN is new APB1ENR.B (RCC_APB1ENR.USART2EN) with Inline_Always;
   function SPI3EN   is new APB1ENR.B (RCC_APB1ENR.SPI3EN  ) with Inline_Always;
   function SPI2EN   is new APB1ENR.B (RCC_APB1ENR.SPI2EN  ) with Inline_Always;
   function WWDGEN   is new APB1ENR.B (RCC_APB1ENR.WWDGEN  ) with Inline_Always;
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

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.DACEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.DACEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.PWREN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.PWREN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.CAN2EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.CAN2EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.CAN1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.CAN1EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.I2C3EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.I2C3EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.I2C2EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.I2C2EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.I2C1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.I2C1EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.UART5EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.UART5EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1ENR.C (RCC_APB1ENR.UART4EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1ENR.C (RCC_APB1ENR.UART4EN, 2#1#) with Inline_Always;

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

   ---------------------------------------------
   --  APB2 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package RCC_APB2ENR is

      package Tp is new Types (R32);

      package TIM11EN  is new Bitfield (Tp, 18);
      package TIM10EN  is new Bitfield (Tp, 17);
      package TIM9EN   is new Bitfield (Tp, 16);
      package SYSCFGEN is new Bitfield (Tp, 14);
      package SPI1EN   is new Bitfield (Tp, 12);
      package SDIOEN   is new Bitfield (Tp, 11);
      package ADCEN    is new Bitfield (Tp, 8);
      package USART6EN is new Bitfield (Tp, 5);
      package USART1EN is new Bitfield (Tp, 4);
      package TIM8EN   is new Bitfield (Tp, 1);
      package TIM1EN   is new Bitfield (Tp, 0);

      type T is
         record
            TIM11EN  : RCC_APB2ENR.TIM11EN .T;
            TIM10EN  : RCC_APB2ENR.TIM10EN .T;
            TIM9EN   : RCC_APB2ENR.TIM9EN  .T;
            SYSCFGEN : RCC_APB2ENR.SYSCFGEN.T;
            SPI1EN   : RCC_APB2ENR.SPI1EN  .T;
            SDIOEN   : RCC_APB2ENR.SDIOEN  .T;
            ADCEN    : RCC_APB2ENR.ADCEN   .T;
            USART6EN : RCC_APB2ENR.USART6EN.T;
            USART1EN : RCC_APB2ENR.USART1EN.T;
            TIM8EN   : RCC_APB2ENR.TIM8EN  .T;
            TIM1EN   : RCC_APB2ENR.TIM1EN  .T;
         end record;

      for T use
         record
            TIM11EN  at 0 range TIM11EN .F .. TIM11EN .L;
            TIM10EN  at 0 range TIM10EN .F .. TIM10EN .L;
            TIM9EN   at 0 range TIM9EN  .F .. TIM9EN  .L;
            SYSCFGEN at 0 range SYSCFGEN.F .. SYSCFGEN.L;
            SPI1EN   at 0 range SPI1EN  .F .. SPI1EN  .L;
            SDIOEN   at 0 range SDIOEN  .F .. SDIOEN  .L;
            ADCEN    at 0 range ADCEN   .F .. ADCEN   .L;
            USART6EN at 0 range USART6EN.F .. USART6EN.L;
            USART1EN at 0 range USART1EN.F .. USART1EN.L;
            TIM8EN   at 0 range TIM8EN  .F .. TIM8EN  .L;
            TIM1EN   at 0 range TIM1EN  .F .. TIM1EN  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_APB2ENR;

   package APB2ENR    is new Register (RCC_APB2ENR.T, RCC_APB2ENR.Tp, 16#4002_3844#);
   subtype APB2ENR_T  is APB2ENR.T;
   subtype APB2ENR_F  is APB2ENR.F;
   

   --  Field definitions

   function TIM11EN  is new APB2ENR.B (RCC_APB2ENR.TIM11EN ) with Inline_Always;
   function TIM10EN  is new APB2ENR.B (RCC_APB2ENR.TIM10EN ) with Inline_Always;
   function TIM9EN   is new APB2ENR.B (RCC_APB2ENR.TIM9EN  ) with Inline_Always;
   function SYSCFGEN is new APB2ENR.B (RCC_APB2ENR.SYSCFGEN) with Inline_Always;
   function SPI1EN   is new APB2ENR.B (RCC_APB2ENR.SPI1EN  ) with Inline_Always;
   function SDIOEN   is new APB2ENR.B (RCC_APB2ENR.SDIOEN  ) with Inline_Always;
   function ADCEN    is new APB2ENR.B (RCC_APB2ENR.ADCEN   ) with Inline_Always;
   function USART6EN is new APB2ENR.B (RCC_APB2ENR.USART6EN) with Inline_Always;
   function USART1EN is new APB2ENR.B (RCC_APB2ENR.USART1EN) with Inline_Always;
   function TIM8EN   is new APB2ENR.B (RCC_APB2ENR.TIM8EN  ) with Inline_Always;
   function TIM1EN   is new APB2ENR.B (RCC_APB2ENR.TIM1EN  ) with Inline_Always;

   --  Functions

   function "+"  is new APB2ENR.Add      with Inline_Always;
   function "+"  is new APB2ENR.Add_F    with Inline_Always;
   function "+"  is new APB2ENR.Add_FF   with Inline_Always;
   function "-"  is new APB2ENR.Clear    with Inline_Always;
   function "-"  is new APB2ENR.Clear_FF with Inline_Always;
   function "="  is new APB2ENR.Equal    with Inline_Always;
   function Init is new APB2ENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.TIM11EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.TIM11EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.TIM10EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.TIM10EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.TIM9EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.TIM9EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.SYSCFGEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.SYSCFGEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.SPI1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.SPI1EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.SDIOEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.SDIOEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.ADCEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.ADCEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.USART6EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.USART6EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.USART1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.USART1EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.TIM8EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.TIM8EN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2ENR.C (RCC_APB2ENR.TIM1EN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2ENR.C (RCC_APB2ENR.TIM1EN, 2#1#) with Inline_Always;

   ---------------------------------------------------------------
   --  AHB1 Peipheral Clock Enabled In Low Power Mode Register  --
   ---------------------------------------------------------------

   package RCC_AHB1LPENR is

      package Tp is new Types (R32);

      package OTGHSULPILPEN  is new Bitfield (Tp, 30);
      package OTGHSLPEN      is new Bitfield (Tp, 29);
      package ETHMACPTPLPEN  is new Bitfield (Tp, 28);
      package ETHMACRXLPEN   is new Bitfield (Tp, 27);
      package ETHMACTXLPEN   is new Bitfield (Tp, 26);
      package ETHMACLPEN     is new Bitfield (Tp, 25);
      package DMA2LPEN       is new Bitfield (Tp, 22);
      package DMA1LPEN       is new Bitfield (Tp, 21);
      package CCMDATARAMLPEN is new Bitfield (Tp, 20);
      package BKPSRAMLPEN    is new Bitfield (Tp, 18);
      package CRCLPEN        is new Bitfield (Tp, 12);
      package GPIOILPEN      is new Bitfield (Tp, 8);
      package GPIOHLPEN      is new Bitfield (Tp, 7);
      package GPIOGLPEN      is new Bitfield (Tp, 6);
      package GPIOFLPEN      is new Bitfield (Tp, 5);
      package GPIOELPEN      is new Bitfield (Tp, 4);
      package GPIODLPEN      is new Bitfield (Tp, 3);
      package GPIOCLPEN      is new Bitfield (Tp, 2);
      package GPIOBLPEN      is new Bitfield (Tp, 1);
      package GPIOALPEN      is new Bitfield (Tp, 0);

      type T is
         record
            OTGHSULPILPEN  : RCC_AHB1LPENR.OTGHSULPILPEN .T;
            OTGHSLPEN      : RCC_AHB1LPENR.OTGHSLPEN     .T;
            ETHMACPTPLPEN  : RCC_AHB1LPENR.ETHMACPTPLPEN .T;
            ETHMACRXLPEN   : RCC_AHB1LPENR.ETHMACRXLPEN  .T;
            ETHMACTXLPEN   : RCC_AHB1LPENR.ETHMACTXLPEN  .T;
            ETHMACLPEN     : RCC_AHB1LPENR.ETHMACLPEN    .T;
            DMA2LPEN       : RCC_AHB1LPENR.DMA2LPEN      .T;
            DMA1LPEN       : RCC_AHB1LPENR.DMA1LPEN      .T;
            CCMDATARAMLPEN : RCC_AHB1LPENR.CCMDATARAMLPEN.T;
            BKPSRAMLPEN    : RCC_AHB1LPENR.BKPSRAMLPEN   .T;
            CRCLPEN        : RCC_AHB1LPENR.CRCLPEN       .T;
            GPIOILPEN      : RCC_AHB1LPENR.GPIOILPEN     .T;
            GPIOHLPEN      : RCC_AHB1LPENR.GPIOHLPEN     .T;
            GPIOGLPEN      : RCC_AHB1LPENR.GPIOGLPEN     .T;
            GPIOFLPEN      : RCC_AHB1LPENR.GPIOFLPEN     .T;
            GPIOELPEN      : RCC_AHB1LPENR.GPIOELPEN     .T;
            GPIODLPEN      : RCC_AHB1LPENR.GPIODLPEN     .T;
            GPIOCLPEN      : RCC_AHB1LPENR.GPIOCLPEN     .T;
            GPIOBLPEN      : RCC_AHB1LPENR.GPIOBLPEN     .T;
            GPIOALPEN      : RCC_AHB1LPENR.GPIOALPEN     .T;
         end record;

      for T use
         record
            OTGHSULPILPEN  at 0 range OTGHSULPILPEN .F .. OTGHSULPILPEN .L;
            OTGHSLPEN      at 0 range OTGHSLPEN     .F .. OTGHSLPEN     .L;
            ETHMACPTPLPEN  at 0 range ETHMACPTPLPEN .F .. ETHMACPTPLPEN .L;
            ETHMACRXLPEN   at 0 range ETHMACRXLPEN  .F .. ETHMACRXLPEN  .L;
            ETHMACTXLPEN   at 0 range ETHMACTXLPEN  .F .. ETHMACTXLPEN  .L;
            ETHMACLPEN     at 0 range ETHMACLPEN    .F .. ETHMACLPEN    .L;
            DMA2LPEN       at 0 range DMA2LPEN      .F .. DMA2LPEN      .L;
            DMA1LPEN       at 0 range DMA1LPEN      .F .. DMA1LPEN      .L;
            CCMDATARAMLPEN at 0 range CCMDATARAMLPEN.F .. CCMDATARAMLPEN.L;
            BKPSRAMLPEN    at 0 range BKPSRAMLPEN   .F .. BKPSRAMLPEN   .L;
            CRCLPEN        at 0 range CRCLPEN       .F .. CRCLPEN       .L;
            GPIOILPEN      at 0 range GPIOILPEN     .F .. GPIOILPEN     .L;
            GPIOHLPEN      at 0 range GPIOHLPEN     .F .. GPIOHLPEN     .L;
            GPIOGLPEN      at 0 range GPIOGLPEN     .F .. GPIOGLPEN     .L;
            GPIOFLPEN      at 0 range GPIOFLPEN     .F .. GPIOFLPEN     .L;
            GPIOELPEN      at 0 range GPIOELPEN     .F .. GPIOELPEN     .L;
            GPIODLPEN      at 0 range GPIODLPEN     .F .. GPIODLPEN     .L;
            GPIOCLPEN      at 0 range GPIOCLPEN     .F .. GPIOCLPEN     .L;
            GPIOBLPEN      at 0 range GPIOBLPEN     .F .. GPIOBLPEN     .L;
            GPIOALPEN      at 0 range GPIOALPEN     .F .. GPIOALPEN     .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHB1LPENR;

   package AHB1LPENR    is new Register (RCC_AHB1LPENR.T, RCC_AHB1LPENR.Tp, 16#4002_3850#);
   subtype AHB1LPENR_T  is AHB1LPENR.T;
   subtype AHB1LPENR_F  is AHB1LPENR.F;
   

   --  Field definitions

   function OTGHSULPILPEN  is new AHB1LPENR.B (RCC_AHB1LPENR.OTGHSULPILPEN ) with Inline_Always;
   function OTGHSLPEN      is new AHB1LPENR.B (RCC_AHB1LPENR.OTGHSLPEN     ) with Inline_Always;
   function ETHMACPTPLPEN  is new AHB1LPENR.B (RCC_AHB1LPENR.ETHMACPTPLPEN ) with Inline_Always;
   function ETHMACRXLPEN   is new AHB1LPENR.B (RCC_AHB1LPENR.ETHMACRXLPEN  ) with Inline_Always;
   function ETHMACTXLPEN   is new AHB1LPENR.B (RCC_AHB1LPENR.ETHMACTXLPEN  ) with Inline_Always;
   function ETHMACLPEN     is new AHB1LPENR.B (RCC_AHB1LPENR.ETHMACLPEN    ) with Inline_Always;
   function DMA2LPEN       is new AHB1LPENR.B (RCC_AHB1LPENR.DMA2LPEN      ) with Inline_Always;
   function DMA1LPEN       is new AHB1LPENR.B (RCC_AHB1LPENR.DMA1LPEN      ) with Inline_Always;
   function CCMDATARAMLPEN is new AHB1LPENR.B (RCC_AHB1LPENR.CCMDATARAMLPEN) with Inline_Always;
   function BKPSRAMLPEN    is new AHB1LPENR.B (RCC_AHB1LPENR.BKPSRAMLPEN   ) with Inline_Always;
   function CRCLPEN        is new AHB1LPENR.B (RCC_AHB1LPENR.CRCLPEN       ) with Inline_Always;
   function GPIOILPEN      is new AHB1LPENR.B (RCC_AHB1LPENR.GPIOILPEN     ) with Inline_Always;
   function GPIOHLPEN      is new AHB1LPENR.B (RCC_AHB1LPENR.GPIOHLPEN     ) with Inline_Always;
   function GPIOGLPEN      is new AHB1LPENR.B (RCC_AHB1LPENR.GPIOGLPEN     ) with Inline_Always;
   function GPIOFLPEN      is new AHB1LPENR.B (RCC_AHB1LPENR.GPIOFLPEN     ) with Inline_Always;
   function GPIOELPEN      is new AHB1LPENR.B (RCC_AHB1LPENR.GPIOELPEN     ) with Inline_Always;
   function GPIODLPEN      is new AHB1LPENR.B (RCC_AHB1LPENR.GPIODLPEN     ) with Inline_Always;
   function GPIOCLPEN      is new AHB1LPENR.B (RCC_AHB1LPENR.GPIOCLPEN     ) with Inline_Always;
   function GPIOBLPEN      is new AHB1LPENR.B (RCC_AHB1LPENR.GPIOBLPEN     ) with Inline_Always;
   function GPIOALPEN      is new AHB1LPENR.B (RCC_AHB1LPENR.GPIOALPEN     ) with Inline_Always;

   --  Functions

   function "+"  is new AHB1LPENR.Add      with Inline_Always;
   function "+"  is new AHB1LPENR.Add_F    with Inline_Always;
   function "+"  is new AHB1LPENR.Add_FF   with Inline_Always;
   function "-"  is new AHB1LPENR.Clear    with Inline_Always;
   function "-"  is new AHB1LPENR.Clear_FF with Inline_Always;
   function "="  is new AHB1LPENR.Equal    with Inline_Always;
   function Init is new AHB1LPENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.OTGHSULPILPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.OTGHSULPILPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.OTGHSLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.OTGHSLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.ETHMACPTPLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.ETHMACPTPLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.ETHMACRXLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.ETHMACRXLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.ETHMACTXLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.ETHMACTXLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.ETHMACLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.ETHMACLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.DMA2LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.DMA2LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.DMA1LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.DMA1LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.CCMDATARAMLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.CCMDATARAMLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.BKPSRAMLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.BKPSRAMLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.CRCLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.CRCLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOILPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOILPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOHLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOHLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOGLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOGLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOFLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOFLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOELPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOELPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.GPIODLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.GPIODLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOCLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOCLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOBLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOBLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOALPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB1LPENR.C (RCC_AHB1LPENR.GPIOALPEN, 2#1#) with Inline_Always;


   ----------------------------------------------------------------
   --  AHB2 Peripheral Clock Enabled In Low Power Mode Register  --
   ----------------------------------------------------------------

   package RCC_AHB2LPENR is

      package Tp is new Types (R32);

      package OTGFSLPEN is new Bitfield (Tp, 7);
      package RNGLPEN   is new Bitfield (Tp, 6);
      package HASHLPEN  is new Bitfield (Tp, 5);
      package CRYPLPEN  is new Bitfield (Tp, 4);
      package DCMILPEN  is new Bitfield (Tp, 0);

      type T is
         record
            OTGFSLPEN : RCC_AHB2LPENR.OTGFSLPEN.T;
            RNGLPEN   : RCC_AHB2LPENR.RNGLPEN  .T;
            HASHLPEN  : RCC_AHB2LPENR.HASHLPEN .T;
            CRYPLPEN  : RCC_AHB2LPENR.CRYPLPEN .T;
            DCMILPEN  : RCC_AHB2LPENR.DCMILPEN .T;
         end record;

      for T use
         record
            OTGFSLPEN at 0 range OTGFSLPEN.F .. OTGFSLPEN.L;
            RNGLPEN   at 0 range RNGLPEN  .F .. RNGLPEN  .L;
            HASHLPEN  at 0 range HASHLPEN .F .. HASHLPEN .L;
            CRYPLPEN  at 0 range CRYPLPEN .F .. CRYPLPEN .L;
            DCMILPEN  at 0 range DCMILPEN .F .. DCMILPEN .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHB2LPENR;

   package AHB2LPENR    is new Register (RCC_AHB2LPENR.T, RCC_AHB2LPENR.Tp, 16#4002_3854#);
   subtype AHB2LPENR_T  is AHB2LPENR.T;
   subtype AHB2LPENR_F  is AHB2LPENR.F;
   

   --  Field definitions

   function OTGFSLPEN is new AHB2LPENR.B (RCC_AHB2LPENR.OTGFSLPEN) with Inline_Always;
   function RNGLPEN   is new AHB2LPENR.B (RCC_AHB2LPENR.RNGLPEN  ) with Inline_Always;
   function HASHLPEN  is new AHB2LPENR.B (RCC_AHB2LPENR.HASHLPEN ) with Inline_Always;
   function CRYPLPEN  is new AHB2LPENR.B (RCC_AHB2LPENR.CRYPLPEN ) with Inline_Always;
   function DCMILPEN  is new AHB2LPENR.B (RCC_AHB2LPENR.DCMILPEN ) with Inline_Always;

   --  Functions

   function "+"  is new AHB2LPENR.Add      with Inline_Always;
   function "+"  is new AHB2LPENR.Add_F    with Inline_Always;
   function "+"  is new AHB2LPENR.Add_FF   with Inline_Always;
   function "-"  is new AHB2LPENR.Clear    with Inline_Always;
   function "-"  is new AHB2LPENR.Clear_FF with Inline_Always;
   function "="  is new AHB2LPENR.Equal    with Inline_Always;
   function Init is new AHB2LPENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new AHB2LPENR.C (RCC_AHB2LPENR.OTGFSLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB2LPENR.C (RCC_AHB2LPENR.OTGFSLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB2LPENR.C (RCC_AHB2LPENR.RNGLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB2LPENR.C (RCC_AHB2LPENR.RNGLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB2LPENR.C (RCC_AHB2LPENR.HASHLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB2LPENR.C (RCC_AHB2LPENR.HASHLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB2LPENR.C (RCC_AHB2LPENR.CRYPLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB2LPENR.C (RCC_AHB2LPENR.CRYPLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new AHB2LPENR.C (RCC_AHB2LPENR.DCMILPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB2LPENR.C (RCC_AHB2LPENR.DCMILPEN, 2#1#) with Inline_Always;

   ----------------------------------------------------------------
   --  AHB3 Peripheral Clock Enabled In Low Power Mode Register  --
   ----------------------------------------------------------------

   package RCC_AHB3LPENR is

      package Tp is new Types (R32);

      package FSMCLPEN is new Bitfield (Tp, 0);

      type T is
         record
            FSMCLPEN : RCC_AHB3LPENR.FSMCLPEN.T;
         end record;

      for T use
         record
            FSMCLPEN at 0 range FSMCLPEN.F .. FSMCLPEN.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_AHB3LPENR;

   package AHB3LPENR    is new Register (RCC_AHB3LPENR.T, RCC_AHB3LPENR.Tp, 16#4002_3858#);
   subtype AHB3LPENR_T  is AHB3LPENR.T;
   subtype AHB3LPENR_F  is AHB3LPENR.F;
   

   --  Field definitions

   function FSMCLPEN is new AHB3LPENR.B (RCC_AHB3LPENR.FSMCLPEN) with Inline_Always;

   --  Functions

   function "+"   is new AHB3LPENR.Add      with Inline_Always;
   function "+"   is new AHB3LPENR.Add_F    with Inline_Always;
   function "+"   is new AHB3LPENR.Add_FF   with Inline_Always;
   function "-"   is new AHB3LPENR.Clear    with Inline_Always;
   function "-"   is new AHB3LPENR.Clear_FF with Inline_Always;
   function Init  is new AHB3LPENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new AHB3LPENR.C (RCC_AHB3LPENR.FSMCLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new AHB3LPENR.C (RCC_AHB3LPENR.FSMCLPEN, 2#1#) with Inline_Always;

   ----------------------------------------------------------------
   --  APB1 Peripheral Clock Enabled In Low Power Mode Register  --
   ----------------------------------------------------------------

   package RCC_APB1LPENR is

      package Tp is new Types (R32);

      package DACLPEN    is new Bitfield (Tp, 29);
      package PWRLPEN    is new Bitfield (Tp, 28);
      package CAN2LPEN   is new Bitfield (Tp, 26);
      package CAN1LPEN   is new Bitfield (Tp, 25);
      package I2C3LPEN   is new Bitfield (Tp, 23);
      package I2C2LPEN   is new Bitfield (Tp, 22);
      package I2C1LPEN   is new Bitfield (Tp, 21);
      package UART5LPEN  is new Bitfield (Tp, 20);
      package UART4LPEN  is new Bitfield (Tp, 19);
      package USART3LPEN is new Bitfield (Tp, 18);
      package USART2LPEN is new Bitfield (Tp, 17);
      package SPI3LPEN   is new Bitfield (Tp, 15);
      package SPI2LPEN   is new Bitfield (Tp, 14);
      package WWDGLPEN   is new Bitfield (Tp, 11);
      package TIM14LPEN  is new Bitfield (Tp, 8);
      package TIM13LPEN  is new Bitfield (Tp, 7);
      package TIM12LPEN  is new Bitfield (Tp, 6);
      package TIM7LPEN   is new Bitfield (Tp, 5);
      package TIM6LPEN   is new Bitfield (Tp, 4);
      package TIM5LPEN   is new Bitfield (Tp, 3);
      package TIM4LPEN   is new Bitfield (Tp, 2);
      package TIM3LPEN   is new Bitfield (Tp, 1);
      package TIM2LPEN   is new Bitfield (Tp, 0);

      type T is
         record
            DACLPEN    : RCC_APB1LPENR.DACLPEN  .T;
            PWRLPEN    : RCC_APB1LPENR.PWRLPEN  .T;
            CAN2LPEN   : RCC_APB1LPENR.CAN2LPEN .T;
            CAN1LPEN   : RCC_APB1LPENR.CAN1LPEN .T;
            I2C3LPEN   : RCC_APB1LPENR.I2C3LPEN .T;
            I2C2LPEN   : RCC_APB1LPENR.I2C2LPEN .T;
            I2C1LPEN   : RCC_APB1LPENR.I2C1LPEN .T;
            UART5LPEN  : RCC_APB1LPENR.UART5LPEN.T;
            UART4LPEN  : RCC_APB1LPENR.UART4LPEN.T;
            USART3LPEN : RCC_APB1LPENR.USART3LPEN.T;
            USART2LPEN : RCC_APB1LPENR.USART2LPEN.T;
            SPI3LPEN   : RCC_APB1LPENR.SPI3LPEN .T;
            SPI2LPEN   : RCC_APB1LPENR.SPI2LPEN .T;
            WWDGLPEN   : RCC_APB1LPENR.WWDGLPEN .T;
            TIM14LPEN  : RCC_APB1LPENR.TIM14LPEN.T;
            TIM13LPEN  : RCC_APB1LPENR.TIM13LPEN.T;
            TIM12LPEN  : RCC_APB1LPENR.TIM12LPEN.T;
            TIM7LPEN   : RCC_APB1LPENR.TIM7LPEN .T;
            TIM6LPEN   : RCC_APB1LPENR.TIM6LPEN .T;
            TIM5LPEN   : RCC_APB1LPENR.TIM5LPEN .T;
            TIM4LPEN   : RCC_APB1LPENR.TIM4LPEN .T;
            TIM3LPEN   : RCC_APB1LPENR.TIM3LPEN .T;
            TIM2LPEN   : RCC_APB1LPENR.TIM2LPEN .T;
         end record;

      for T use
         record
            DACLPEN    at 0 range DACLPEN  .F .. DACLPEN  .L;
            PWRLPEN    at 0 range PWRLPEN  .F .. PWRLPEN  .L;
            CAN2LPEN   at 0 range CAN2LPEN .F .. CAN2LPEN .L;
            CAN1LPEN   at 0 range CAN1LPEN .F .. CAN1LPEN .L;
            I2C3LPEN   at 0 range I2C3LPEN .F .. I2C3LPEN .L;
            I2C2LPEN   at 0 range I2C2LPEN .F .. I2C2LPEN .L;
            I2C1LPEN   at 0 range I2C1LPEN .F .. I2C1LPEN .L;
            UART5LPEN  at 0 range UART5LPEN.F .. UART5LPEN.L;
            UART4LPEN  at 0 range UART4LPEN.F .. UART4LPEN.L;
            USART3LPEN at 0 range USART3LPEN.F .. USART3LPEN.L;
            USART2LPEN at 0 range USART2LPEN.F .. USART2LPEN.L;
            SPI3LPEN   at 0 range SPI3LPEN .F .. SPI3LPEN .L;
            SPI2LPEN   at 0 range SPI2LPEN .F .. SPI2LPEN .L;
            WWDGLPEN   at 0 range WWDGLPEN .F .. WWDGLPEN .L;
            TIM14LPEN  at 0 range TIM14LPEN.F .. TIM14LPEN.L;
            TIM13LPEN  at 0 range TIM13LPEN.F .. TIM13LPEN.L;
            TIM12LPEN  at 0 range TIM12LPEN.F .. TIM12LPEN.L;
            TIM7LPEN   at 0 range TIM7LPEN .F .. TIM7LPEN .L;
            TIM6LPEN   at 0 range TIM6LPEN .F .. TIM6LPEN .L;
            TIM5LPEN   at 0 range TIM5LPEN .F .. TIM5LPEN .L;
            TIM4LPEN   at 0 range TIM4LPEN .F .. TIM4LPEN .L;
            TIM3LPEN   at 0 range TIM3LPEN .F .. TIM3LPEN .L;
            TIM2LPEN   at 0 range TIM2LPEN .F .. TIM2LPEN .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_APB1LPENR;

   package APB1LPENR    is new Register (RCC_APB1LPENR.T, RCC_APB1LPENR.Tp, 16#4002_3860#);
   subtype APB1LPENR_T  is APB1LPENR.T;
   subtype APB1LPENR_F  is APB1LPENR.F;
   

   --  Field definitions

   function DACLPEN    is new APB1LPENR.B (RCC_APB1LPENR.DACLPEN   ) with Inline_Always;
   function PWRLPEN    is new APB1LPENR.B (RCC_APB1LPENR.PWRLPEN   ) with Inline_Always;
   function CAN2LPEN   is new APB1LPENR.B (RCC_APB1LPENR.CAN2LPEN  ) with Inline_Always;
   function CAN1LPEN   is new APB1LPENR.B (RCC_APB1LPENR.CAN1LPEN  ) with Inline_Always;
   function I2C3LPEN   is new APB1LPENR.B (RCC_APB1LPENR.I2C3LPEN  ) with Inline_Always;
   function I2C2LPEN   is new APB1LPENR.B (RCC_APB1LPENR.I2C2LPEN  ) with Inline_Always;
   function I2C1LPEN   is new APB1LPENR.B (RCC_APB1LPENR.I2C1LPEN  ) with Inline_Always;
   function UART5LPEN  is new APB1LPENR.B (RCC_APB1LPENR.UART5LPEN ) with Inline_Always;
   function UART4LPEN  is new APB1LPENR.B (RCC_APB1LPENR.UART4LPEN ) with Inline_Always;
   function USART3LPEN is new APB1LPENR.B (RCC_APB1LPENR.USART3LPEN) with Inline_Always;
   function USART2LPEN is new APB1LPENR.B (RCC_APB1LPENR.USART2LPEN) with Inline_Always;
   function SPI3LPEN   is new APB1LPENR.B (RCC_APB1LPENR.SPI3LPEN  ) with Inline_Always;
   function SPI2LPEN   is new APB1LPENR.B (RCC_APB1LPENR.SPI2LPEN  ) with Inline_Always;
   function WWDGLPEN   is new APB1LPENR.B (RCC_APB1LPENR.WWDGLPEN  ) with Inline_Always;
   function TIM14LPEN  is new APB1LPENR.B (RCC_APB1LPENR.TIM14LPEN ) with Inline_Always;
   function TIM13LPEN  is new APB1LPENR.B (RCC_APB1LPENR.TIM13LPEN ) with Inline_Always;
   function TIM12LPEN  is new APB1LPENR.B (RCC_APB1LPENR.TIM12LPEN ) with Inline_Always;
   function TIM7LPEN   is new APB1LPENR.B (RCC_APB1LPENR.TIM7LPEN  ) with Inline_Always;
   function TIM6LPEN   is new APB1LPENR.B (RCC_APB1LPENR.TIM6LPEN  ) with Inline_Always;
   function TIM5LPEN   is new APB1LPENR.B (RCC_APB1LPENR.TIM5LPEN  ) with Inline_Always;
   function TIM4LPEN   is new APB1LPENR.B (RCC_APB1LPENR.TIM4LPEN  ) with Inline_Always;
   function TIM3LPEN   is new APB1LPENR.B (RCC_APB1LPENR.TIM3LPEN  ) with Inline_Always;
   function TIM2LPEN   is new APB1LPENR.B (RCC_APB1LPENR.TIM2LPEN  ) with Inline_Always;

   --  Functions

   function "+"  is new APB1LPENR.Add      with Inline_Always;
   function "+"  is new APB1LPENR.Add_F    with Inline_Always;
   function "+"  is new APB1LPENR.Add_FF   with Inline_Always;
   function "-"  is new APB1LPENR.Clear    with Inline_Always;
   function "-"  is new APB1LPENR.Clear_FF with Inline_Always;
   function "="  is new APB1LPENR.Equal    with Inline_Always;
   function Init is new APB1LPENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.DACLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.DACLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.PWRLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.PWRLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.CAN2LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.CAN2LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.CAN1LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.CAN1LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.I2C3LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.I2C3LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.I2C2LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.I2C2LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.I2C1LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.I2C1LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.UART5LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.UART5LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.UART4LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.UART4LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.USART3LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.USART3LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.USART2LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.USART2LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.SPI3LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.SPI3LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.SPI2LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.SPI2LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.WWDGLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.WWDGLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.TIM14LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.TIM14LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.TIM13LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.TIM13LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.TIM12LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.TIM12LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.TIM7LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.TIM7LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.TIM6LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.TIM6LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.TIM5LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.TIM5LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.TIM4LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.TIM4LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.TIM3LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.TIM3LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB1LPENR.C (RCC_APB1LPENR.TIM2LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB1LPENR.C (RCC_APB1LPENR.TIM2LPEN, 2#1#) with Inline_Always;

   ----------------------------------------------------------------
   --  APB2 Peripheral Clock Enabled In Low Power Mode Register  --
   ----------------------------------------------------------------

   package RCC_APB2LPENR is

      package Tp is new Types (R32);

      package TIM11LPEN  is new Bitfield (Tp, 18);
      package TIM10LPEN  is new Bitfield (Tp, 17);
      package TIM9LPEN   is new Bitfield (Tp, 16);
      package SYSCFGLPEN is new Bitfield (Tp, 14);
      package SPI1LPEN   is new Bitfield (Tp, 12);
      package SDIOLPEN   is new Bitfield (Tp, 11);
      package ADCLPEN    is new Bitfield (Tp, 8);
      package USART6LPEN is new Bitfield (Tp, 5);
      package USART1LPEN is new Bitfield (Tp, 4);
      package TIM8LPEN   is new Bitfield (Tp, 1);
      package TIM1LPEN   is new Bitfield (Tp, 0);

      type T is
         record
            TIM11LPEN  : RCC_APB2LPENR.TIM11LPEN .T;
            TIM10LPEN  : RCC_APB2LPENR.TIM10LPEN .T;
            TIM9LPEN   : RCC_APB2LPENR.TIM9LPEN  .T;
            SYSCFGLPEN : RCC_APB2LPENR.SYSCFGLPEN.T;
            SPI1LPEN   : RCC_APB2LPENR.SPI1LPEN  .T;
            SDIOLPEN   : RCC_APB2LPENR.SDIOLPEN  .T;
            ADCLPEN    : RCC_APB2LPENR.ADCLPEN   .T;
            USART6LPEN : RCC_APB2LPENR.USART6LPEN.T;
            USART1LPEN : RCC_APB2LPENR.USART1LPEN.T;
            TIM8LPEN   : RCC_APB2LPENR.TIM8LPEN  .T;
            TIM1LPEN   : RCC_APB2LPENR.TIM1LPEN  .T;
         end record;

      for T use
         record
            TIM11LPEN  at 0 range TIM11LPEN .F .. TIM11LPEN .L;
            TIM10LPEN  at 0 range TIM10LPEN .F .. TIM10LPEN .L;
            TIM9LPEN   at 0 range TIM9LPEN  .F .. TIM9LPEN  .L;
            SYSCFGLPEN at 0 range SYSCFGLPEN.F .. SYSCFGLPEN.L;
            SPI1LPEN   at 0 range SPI1LPEN  .F .. SPI1LPEN  .L;
            SDIOLPEN   at 0 range SDIOLPEN  .F .. SDIOLPEN  .L;
            ADCLPEN    at 0 range ADCLPEN   .F .. ADCLPEN   .L;
            USART6LPEN at 0 range USART6LPEN.F .. USART6LPEN.L;
            USART1LPEN at 0 range USART1LPEN.F .. USART1LPEN.L;
            TIM8LPEN   at 0 range TIM8LPEN  .F .. TIM8LPEN  .L;
            TIM1LPEN   at 0 range TIM1LPEN  .F .. TIM1LPEN  .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_APB2LPENR;

   package APB2LPENR    is new Register (RCC_APB2LPENR.T, RCC_APB2LPENR.Tp, 16#4002_3864#);
   subtype APB2LPENR_T  is APB2LPENR.T;
   subtype APB2LPENR_F  is APB2LPENR.F;
   

   --  Field definitions

   function TIM11LPEN  is new APB2LPENR.B (RCC_APB2LPENR.TIM11LPEN ) with Inline_Always;
   function TIM10LPEN  is new APB2LPENR.B (RCC_APB2LPENR.TIM10LPEN ) with Inline_Always;
   function TIM9LPEN   is new APB2LPENR.B (RCC_APB2LPENR.TIM9LPEN  ) with Inline_Always;
   function SYSCFGLPEN is new APB2LPENR.B (RCC_APB2LPENR.SYSCFGLPEN) with Inline_Always;
   function SPI1LPEN   is new APB2LPENR.B (RCC_APB2LPENR.SPI1LPEN  ) with Inline_Always;
   function SDIOLPEN   is new APB2LPENR.B (RCC_APB2LPENR.SDIOLPEN  ) with Inline_Always;
   function ADCLPEN    is new APB2LPENR.B (RCC_APB2LPENR.ADCLPEN   ) with Inline_Always;
   function USART6LPEN is new APB2LPENR.B (RCC_APB2LPENR.USART6LPEN) with Inline_Always;
   function USART1LPEN is new APB2LPENR.B (RCC_APB2LPENR.USART1LPEN) with Inline_Always;
   function TIM8LPEN   is new APB2LPENR.B (RCC_APB2LPENR.TIM8LPEN  ) with Inline_Always;
   function TIM1LPEN   is new APB2LPENR.B (RCC_APB2LPENR.TIM1LPEN  ) with Inline_Always;

   --  Functions

   function "+"  is new APB2LPENR.Add      with Inline_Always;
   function "+"  is new APB2LPENR.Add_F    with Inline_Always;
   function "+"  is new APB2LPENR.Add_FF   with Inline_Always;
   function "-"  is new APB2LPENR.Clear    with Inline_Always;
   function "-"  is new APB2LPENR.Clear_FF with Inline_Always;
   function "="  is new APB2LPENR.Equal    with Inline_Always;
   function Init is new APB2LPENR.Init     with Inline_Always;

   --  Constant definitions

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.TIM11LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.TIM11LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.TIM10LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.TIM10LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.TIM9LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.TIM9LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.SYSCFGLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.SYSCFGLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.SPI1LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.SPI1LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.SDIOLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.SDIOLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.ADCLPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.ADCLPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.USART6LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.USART6LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.USART1LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.USART1LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.TIM8LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.TIM8LPEN, 2#1#) with Inline_Always;

   function Clock_Disabled is new APB2LPENR.C (RCC_APB2LPENR.TIM1LPEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new APB2LPENR.C (RCC_APB2LPENR.TIM1LPEN, 2#1#) with Inline_Always;

   --------------------------------------
   --  Backup Domain Control Register  --
   --------------------------------------

   package RCC_BDCR is

      package Tp is new Types (R32);

      package BDRST  is new Bitfield (Tp, 16);
      package RTCEN  is new Bitfield (Tp, 15);
      package RTCSEL is new Bitfield (Tp, 8, 2);
      package LSEBYP is new Bitfield (Tp, 2);
      package LSERDY is new Bitfield (Tp, 1);
      package LSEON  is new Bitfield (Tp, 0);

      type T is
         record
            BDRST  : RCC_BDCR.BDRST .T;
            RTCEN  : RCC_BDCR.RTCEN .T;
            RTCSEL : RCC_BDCR.RTCSEL.T;
            LSEBYP : RCC_BDCR.LSEBYP.T;
            LSERDY : RCC_BDCR.LSERDY.T;
            LSEON  : RCC_BDCR.LSEON .T;
         end record;

      for T use
         record
            BDRST  at 0 range BDRST .F .. BDRST .L;
            RTCEN  at 0 range RTCEN .F .. RTCEN .L;
            RTCSEL at 0 range RTCSEL.F .. RTCSEL.L;
            LSEBYP at 0 range LSEBYP.F .. LSEBYP.L;
            LSERDY at 0 range LSERDY.F .. LSERDY.L;
            LSEON  at 0 range LSEON .F .. LSEON .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_BDCR;

   package BDCR    is new ARM.Register.Register (RCC_BDCR.T, RCC_BDCR.Tp, 16#4002_3870#);
   subtype BDCR_T  is BDCR.T;
   subtype BDCR_F  is BDCR.F;
   

   --  Field definitions

   function BDRST  is new BDCR.B (RCC_BDCR.BDRST ) with Inline_Always;
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

   function Reset_Not_Activated         is new BDCR.C (RCC_BDCR.BDRST, 2#0#) with Inline_Always;
   function Resets_Entire_Backup_Domain is new BDCR.C (RCC_BDCR.BDRST, 2#1#) with Inline_Always;

   function Clock_Disabled is new BDCR.C (RCC_BDCR.RTCEN, 2#0#) with Inline_Always;
   function Clock_Enabled  is new BDCR.C (RCC_BDCR.RTCEN, 2#1#) with Inline_Always;

   function No_Clock       is new BDCR.C (RCC_BDCR.RTCSEL, 2#00#) with Inline_Always;
   function LSE_Oscillator is new BDCR.C (RCC_BDCR.RTCSEL, 2#01#) with Inline_Always;
   function LSI_Oscillator is new BDCR.C (RCC_BDCR.RTCSEL, 2#10#) with Inline_Always;
   function HSE_Oscillator is new BDCR.C (RCC_BDCR.RTCSEL, 2#11#) with Inline_Always;

   function Oscillator_Not_Bypassed is new BDCR.C (RCC_BDCR.LSEBYP, 2#0#) with Inline_Always;
   function Oscillator_Bypassed     is new BDCR.C (RCC_BDCR.LSEBYP, 2#1#) with Inline_Always;

   function Clock_Not_Ready is new BDCR.C (RCC_BDCR.LSERDY, 2#0#) with Inline_Always;
   function Clock_Ready     is new BDCR.C (RCC_BDCR.LSERDY, 2#1#) with Inline_Always;

   function Clock_Off is new BDCR.C (RCC_BDCR.LSEON, 2#0#) with Inline_Always;
   function Clock_On  is new BDCR.C (RCC_BDCR.LSEON, 2#1#) with Inline_Always;

   ---------------------------------------
   --  Clock Control & Status Register  --
   ---------------------------------------

   package RCC_CSR is

      package Tp is new Types (R32);

      package LPWRRSTF is new Bitfield (Tp, 31);
      package WWDGRSTF is new Bitfield (Tp, 30);
      package IWDGRSTF is new Bitfield (Tp, 29);
      package SFTRSTF  is new Bitfield (Tp, 28);
      package PORRSTF  is new Bitfield (Tp, 27);
      package PINRSTF  is new Bitfield (Tp, 26);
      package BORRSTF  is new Bitfield (Tp, 25);
      package RMVF     is new Bitfield (Tp, 24);
      package LSIRDY   is new Bitfield (Tp, 1);
      package LSION    is new Bitfield (Tp, 0);

      type T is
         record
            LPWRRSTF : RCC_CSR.LPWRRSTF.T;
            WWDGRSTF : RCC_CSR.WWDGRSTF.T;
            IWDGRSTF : RCC_CSR.IWDGRSTF.T;
            SFTRSTF  : RCC_CSR.SFTRSTF .T;
            PORRSTF  : RCC_CSR.PORRSTF .T;
            PINRSTF  : RCC_CSR.PINRSTF .T;
            BORRSTF  : RCC_CSR.BORRSTF .T;
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
            BORRSTF  at 0 range BORRSTF .F .. BORRSTF .L;
            RMVF     at 0 range RMVF    .F .. RMVF    .L;
            LSIRDY   at 0 range LSIRDY  .F .. LSIRDY  .L;
            LSION    at 0 range LSION   .F .. LSION   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_CSR;

   package CSR    is new Register (RCC_CSR.T, RCC_CSR.Tp, 16#4002_3874#);
   subtype CSR_T  is RCC_CSR.T;
   subtype CSR_F  is CSR.F;
   

   --  Field definitions

   function LPWRRSTF is new CSR.B (RCC_CSR.LPWRRSTF) with Inline_Always;
   function WWDGRSTF is new CSR.B (RCC_CSR.WWDGRSTF) with Inline_Always;
   function IWDGRSTF is new CSR.B (RCC_CSR.IWDGRSTF) with Inline_Always;
   function SFTRSTF  is new CSR.B (RCC_CSR.SFTRSTF ) with Inline_Always;
   function PORRSTF  is new CSR.B (RCC_CSR.PORRSTF ) with Inline_Always;
   function PINRSTF  is new CSR.B (RCC_CSR.PINRSTF ) with Inline_Always;
   function BORRSTF  is new CSR.B (RCC_CSR.BORRSTF ) with Inline_Always;
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

   --  Constant definitions

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

   function No_POR_PDR_Or_BOR_Reset_Ocurred is new CSR.C (RCC_CSR.BORRSTF, 2#0#) with Inline_Always;
   function POR_PDR_Or_BOR_Reset_Ocurred    is new CSR.C (RCC_CSR.BORRSTF, 2#1#) with Inline_Always;

   function Clear_Flag is new CSR.C (RCC_CSR.RMVF, 2#1#) with Inline_Always;

   function Oscillator_Not_Ready is new CSR.C (RCC_CSR.LSIRDY, 2#0#) with Inline_Always;
   function Oscillator_Ready     is new CSR.C (RCC_CSR.LSIRDY, 2#1#) with Inline_Always;

   function Oscillator_Off is new CSR.C (RCC_CSR.LSION, 2#0#) with Inline_Always;
   function Oscillator_On  is new CSR.C (RCC_CSR.LSION, 2#1#) with Inline_Always;

   -------------------------------------------------
   --  Spread Spectrum Clock Generation Register  --
   -------------------------------------------------

   package RCC_SSCGR is

      package Tp is new Types (R32);

      package SSCGEN    is new Bitfield (Tp, 31);
      package SPREADSEL is new Bitfield (Tp, 30);
      package INCSTEP   is new Bitfield (Tp, 13, 15);
      package MODPER    is new Bitfield (Tp, 0, 13);

      type T is
         record
            SSCGEN    : RCC_SSCGR.SSCGEN   .T;
            SPREADSEL : RCC_SSCGR.SPREADSEL.T;
            INCSTEP   : RCC_SSCGR.INCSTEP  .T;
            MODPER    : RCC_SSCGR.MODPER   .T;
         end record;

      for T use
         record
            SSCGEN    at 0 range SSCGEN   .F .. SSCGEN   .L;
            SPREADSEL at 0 range SPREADSEL.F .. SPREADSEL.L;
            INCSTEP   at 0 range INCSTEP  .F .. INCSTEP  .L;
            MODPER    at 0 range MODPER   .F .. MODPER   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_SSCGR;

   package SSCGR    is new Register (RCC_SSCGR.T, RCC_SSCGR.Tp, 16#4002_3880#);
   subtype SSCGR_T  is SSCGR.T;
   subtype SSCGR_F  is SSCGR.F;
   

   --  Field definitions

   function SSCGEN    is new SSCGR.B (RCC_SSCGR.SSCGEN   ) with Inline_Always;
   function SPREADSEL is new SSCGR.B (RCC_SSCGR.SPREADSEL) with Inline_Always;
   function INCSTEP   is new SSCGR.B (RCC_SSCGR.INCSTEP  ) with Inline_Always;
   function MODPER    is new SSCGR.B (RCC_SSCGR.MODPER   ) with Inline_Always;

   --  Functions

   function "+"  is new SSCGR.Add      with Inline_Always;
   function "+"  is new SSCGR.Add_F    with Inline_Always;
   function "+"  is new SSCGR.Add_FF   with Inline_Always;
   function "-"  is new SSCGR.Clear    with Inline_Always;
   function "-"  is new SSCGR.Clear_FF with Inline_Always;
   function "="  is new SSCGR.Equal    with Inline_Always;
   function Init is new SSCGR.Init     with Inline_Always;

   --  Constant definitions

   function SSM_Disable is new SSCGR.C (RCC_SSCGR.SSCGEN, 2#0#) with Inline_Always;
   function SSM_Enable  is new SSCGR.C (RCC_SSCGR.SSCGEN, 2#1#) with Inline_Always;

   function Center_Spread is new SSCGR.C (RCC_SSCGR.SPREADSEL, 2#0#) with Inline_Always;
   function Down_Spread   is new SSCGR.C (RCC_SSCGR.SPREADSEL, 2#1#) with Inline_Always;

   --------------------------------------
   --  PLL I2S Configuration Register  --
   --------------------------------------

   package RCC_PLLI2SCFGR is

      package Tp is new Types (R32);

      package PLLI2SR is new Bitfield (Tp, 28, 3);
      package PLLI2SN is new Bitfield (Tp, 6, 9);

      type T is
         record
            PLLI2SR : RCC_PLLI2SCFGR.PLLI2SR.T;
            PLLI2SN : RCC_PLLI2SCFGR.PLLI2SN.T;
         end record;

      for T use
         record
            PLLI2SR at 0 range PLLI2SR.F .. PLLI2SR.L;
            PLLI2SN at 0 range PLLI2SN.F .. PLLI2SN.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end RCC_PLLI2SCFGR;

   package PLLI2SCFGR    is new Register (RCC_PLLI2SCFGR.T, RCC_PLLI2SCFGR.Tp, 16#4002_3884#);
   subtype PLLI2SCFGR_T  is PLLI2SCFGR.T;
   subtype PLLI2SCFGR_F  is PLLI2SCFGR.F;
   

   --  Field definitions

   function PLLI2SR is new PLLI2SCFGR.B (RCC_PLLI2SCFGR.PLLI2SR) with Inline_Always;
   function PLLI2SN is new PLLI2SCFGR.B (RCC_PLLI2SCFGR.PLLI2SN) with Inline_Always;

   --  Functions

   function "+"  is new PLLI2SCFGR.Add      with Inline_Always;
   function "+"  is new PLLI2SCFGR.Add_F    with Inline_Always;
   function "+"  is new PLLI2SCFGR.Add_FF   with Inline_Always;
   function "-"  is new PLLI2SCFGR.Clear    with Inline_Always;
   function "-"  is new PLLI2SCFGR.Clear_FF with Inline_Always;
   function "="  is new PLLI2SCFGR.Equal    with Inline_Always;
   function Init is new PLLI2SCFGR.Init     with Inline_Always;

end ARM.Register.RCC_F40XXX;
