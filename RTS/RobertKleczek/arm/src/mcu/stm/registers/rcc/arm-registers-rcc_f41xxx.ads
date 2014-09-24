--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--               A R M . R e g i s t e r s . R C C _ F 4 1 X X X              --
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

--  Package defines STM32 F41XXX family reset & clock related registers

--  NEED UPGRADE

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
--                           ARM.Registers.RCC_F41XXX                         --
--------------------------------------------------------------------------------

package ARM.Registers.RCC_F41XXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                         Reset and Clock                             --
   -------------------------------------------------------------------------

   --     RCC Registers Collection  --

   ----------------------
   --  CR  Register --
   ----------------------

   package CR is

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
            PLLI2SRDY : CR.PLLI2SRDY.T;
            PLLI2SON  : CR.PLLI2SON .T;
            PLLRDY    : CR.PLLRDY   .T;
            PLLON     : CR.PLLON    .T;
            CSSON     : CR.CSSON    .T;
            HSEBYP    : CR.HSEBYP   .T;
            HSERDY    : CR.HSERDY   .T;
            HSEON     : CR.HSEON    .T;
            HSICAL    : CR.HSICAL   .T;
            HSITRIM   : CR.HSITRIM  .T;
            HSIRDY    : CR.HSIRDY   .T;
            HSION     : CR.HSION    .T;
         end record;

      for T use
         record
            PLLI2SRDY at 0 range PLLI2SRDY.R'First .. PLLI2SRDY.R'Last;
            PLLI2SON  at 0 range PLLI2SON .R'First .. PLLI2SON .R'Last;
            PLLRDY    at 0 range PLLRDY   .R'First .. PLLRDY   .R'Last;
            PLLON     at 0 range PLLON    .R'First .. PLLON    .R'Last;
            CSSON     at 0 range CSSON    .R'First .. CSSON    .R'Last;
            HSEBYP    at 0 range HSEBYP   .R'First .. HSEBYP   .R'Last;
            HSERDY    at 0 range HSERDY   .R'First .. HSERDY   .R'Last;
            HSEON     at 0 range HSEON    .R'First .. HSEON    .R'Last;
            HSICAL    at 0 range HSICAL   .R'First .. HSICAL   .R'Last;
            HSITRIM   at 0 range HSITRIM  .R'First .. HSITRIM  .R'Last;
            HSIRDY    at 0 range HSIRDY   .R'First .. HSIRDY   .R'Last;
            HSION     at 0 range HSION    .R'First .. HSION    .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CR;

   subtype CR_T is CR.T;

   --  Field definitions

   function PLLI2SRDY is new CR.FN.B (CR.PLLI2SRDY);
   function PLLI2SON  is new CR.FN.B (CR.PLLI2SON );
   function PLLRDY    is new CR.FN.B (CR.PLLRDY   );
   function PLLON     is new CR.FN.B (CR.PLLON    );
   function CSSON     is new CR.FN.B (CR.CSSON    );
   function HSEBYP    is new CR.FN.B (CR.HSEBYP   );
   function HSERDY    is new CR.FN.B (CR.HSERDY   );
   function HSEON     is new CR.FN.B (CR.HSEON    );
   function HSICAL    is new CR.FN.B (CR.HSICAL   );
   function HSITRIM   is new CR.FN.B (CR.HSITRIM  );
   function HSIRDY    is new CR.FN.B (CR.HSIRDY   );
   function HSION     is new CR.FN.B (CR.HSION    );

   --  Functions

   function  "+"   is new CR.FN.Add;
   function  "+"   is new CR.FN.Add_RM;
   function  "-"   is new CR.FN.Clear;
   function  Init  is new CR.FN.Init;

   --  Constant definitions

   function PLLI2S_Unlocked is new CR.FN.C (CR.PLLI2SRDY, 2#0#);
   function PLLI2S_Locked   is new CR.FN.C (CR.PLLI2SRDY, 2#1#);

   function PLLI2S_Off is new CR.FN.C (CR.PLLI2SON, 2#0#);
   function PLLI2S_On  is new CR.FN.C (CR.PLLI2SON, 2#1#);

   function PLL_Unlocked is new CR.FN.C (CR.PLLRDY, 2#0#);
   function PLL_Locked   is new CR.FN.C (CR.PLLRDY, 2#1#);

   function PLL_Off is new CR.FN.C (CR.PLLRDY, 2#0#);
   function PLL_On  is new CR.FN.C (CR.PLLRDY, 2#1#);

   function Clock_Security_System_Off is new CR.FN.C (CR.CSSON, 2#0#);
   function Clock_Security_System_On  is new CR.FN.C (CR.CSSON, 2#1#);

   function Oscillator_Not_Bypassed is new CR.FN.C (CR.HSEBYP, 2#0#);
   function Oscillator_Bypassed     is new CR.FN.C (CR.HSEBYP, 2#1#);

   function Oscillator_Not_Ready is new CR.FN.C (CR.HSERDY, 2#0#);
   function Oscillator_Ready     is new CR.FN.C (CR.HSERDY, 2#1#);

   function Oscillator_Off is new CR.FN.C (CR.HSEON, 2#0#);
   function Oscillator_On  is new CR.FN.C (CR.HSEON, 2#1#);

   function Oscillator_Not_Ready is new CR.FN.C (CR.HSIRDY, 2#0#);
   function Oscillator_Ready     is new CR.FN.C (CR.HSIRDY, 2#1#);

   function Oscillator_Off is new CR.FN.C (CR.HSION, 2#0#);
   function Oscillator_On  is new CR.FN.C (CR.HSION, 2#1#);

   -----------------------------------
   --  PLL Configuration  Register  --
   -----------------------------------

   package PLLCFGR is

      package Tp is new Types (R32);

      package PLLQ   is new Bitfield (Tp, 24, 4);
      package PLLSRC is new Bitfield (Tp, 22);
      package PLLP   is new Bitfield (Tp, 16, 2);
      package PLLN   is new Bitfield (Tp, 6, 9);
      package PLLM   is new Bitfield (Tp, 0, 6);

      type T is
         record
            PLLQ   : PLLCFGR.PLLQ  .T;
            PLLSRC : PLLCFGR.PLLSRC.T;
            PLLP   : PLLCFGR.PLLP  .T;
            PLLN   : PLLCFGR.PLLN  .T;
            PLLM   : PLLCFGR.PLLM  .T;
         end record;

      for T use
         record
            PLLQ   at 0 range PLLQ  .R'First .. PLLQ  .R'Last;
            PLLSRC at 0 range PLLSRC.R'First .. PLLSRC.R'Last;
            PLLP   at 0 range PLLP  .R'First .. PLLP  .R'Last;
            PLLN   at 0 range PLLN  .R'First .. PLLN  .R'Last;
            PLLM   at 0 range PLLM  .R'First .. PLLM  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PLLCFGR;

   subtype PLLCFGR_T is PLLCFGR.T;

   --  Field definitions

   function PLLQ   is new PLLCFGR.FN.B (PLLCFGR.PLLQ  );
   function PLLSRC is new PLLCFGR.FN.B (PLLCFGR.PLLSRC);
   function PLLP   is new PLLCFGR.FN.B (PLLCFGR.PLLP  );
   function PLLN   is new PLLCFGR.FN.B (PLLCFGR.PLLN  );
   function PLLM   is new PLLCFGR.FN.B (PLLCFGR.PLLM  );

   --  Functions

   function  "+"   is new PLLCFGR.FN.Add;
   function  "+"   is new PLLCFGR.FN.Add_RM;
   function  "-"   is new PLLCFGR.FN.Clear;
   function  Init  is new PLLCFGR.FN.Init;

   --  Constant definitions

   function HSI_Clock_Selected             is new PLLCFGR.FN.C (PLLCFGR.PLLSRC, 2#0#);
   function HSE_Osclillator_Clock_Selected is new PLLCFGR.FN.C (PLLCFGR.PLLSRC, 2#1#);

   function Division_Factor_2 is new PLLCFGR.FN.C (PLLCFGR.PLLP, 2#00#);
   function Division_Factor_4 is new PLLCFGR.FN.C (PLLCFGR.PLLP, 2#01#);
   function Division_Factor_6 is new PLLCFGR.FN.C (PLLCFGR.PLLP, 2#10#);
   function Division_Factor_8 is new PLLCFGR.FN.C (PLLCFGR.PLLP, 2#11#);

   ------------------------------
   --  Configuration Register  --
   ------------------------------

   package CFGR is

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
            MCO2    : CFGR .MCO2   . T;
            MCO2PRE : CFGR .MCO2PRE. T;
            MCO1PRE : CFGR .MCO1PRE. T;
            I2SSCR  : CFGR .I2SSCR . T;
            MCO1    : CFGR .MCO1   . T;
            RTCPRE  : CFGR .RTCPRE . T;
            PPRE2   : CFGR .PPRE2  . T;
            PPRE1   : CFGR .PPRE1  . T;
            HPRE    : CFGR .HPRE   . T;
            SWS     : CFGR .SWS    . T;
            SW      : CFGR .SW     . T;
         end record;

      for T use
         record
            MCO2    at 0 range MCO2   .R'First .. MCO2   .R'Last;
            MCO2PRE at 0 range MCO2PRE.R'First .. MCO2PRE.R'Last;
            MCO1PRE at 0 range MCO1PRE.R'First .. MCO1PRE.R'Last;
            I2SSCR  at 0 range I2SSCR .R'First .. I2SSCR .R'Last;
            MCO1    at 0 range MCO1   .R'First .. MCO1   .R'Last;
            RTCPRE  at 0 range RTCPRE .R'First .. RTCPRE .R'Last;
            PPRE2   at 0 range PPRE2  .R'First .. PPRE2  .R'Last;
            PPRE1   at 0 range PPRE1  .R'First .. PPRE1  .R'Last;
            HPRE    at 0 range HPRE   .R'First .. HPRE   .R'Last;
            SWS     at 0 range SWS    .R'First .. SWS    .R'Last;
            SW      at 0 range SW     .R'First .. SW     .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CFGR;

   subtype CFGR_T is CFGR.T;

   --  Field definitions

   function MCO2    is new CFGR.FN.B (CFGR.MCO2   );
   function MCO2PRE is new CFGR.FN.B (CFGR.MCO2PRE);
   function MCO1PRE is new CFGR.FN.B (CFGR.MCO1PRE);
   function I2SSCR  is new CFGR.FN.B (CFGR.I2SSCR );
   function MCO1    is new CFGR.FN.B (CFGR.MCO1   );
   function RTCPRE  is new CFGR.FN.B (CFGR.RTCPRE );
   function PPRE2   is new CFGR.FN.B (CFGR.PPRE2  );
   function PPRE1   is new CFGR.FN.B (CFGR.PPRE1  );
   function HPRE    is new CFGR.FN.B (CFGR.HPRE   );
   function SWS     is new CFGR.FN.B (CFGR.SWS    );
   function SW      is new CFGR.FN.B (CFGR.SW     );

   --  Functions

   function  "+"   is new CFGR.FN.Add;
   function  "+"   is new CFGR.FN.Add_RM;
   function  "-"   is new CFGR.FN.Clear;
   function  Init  is new CFGR.FN.Init;

   --  Constant definitions

   function System_Clock         is new CFGR.FN.C (CFGR.MCO2, 2#00#);
   function PLLI2S_Clock         is new CFGR.FN.C (CFGR.MCO2, 2#01#);
   function HSE_Oscillator_Clock is new CFGR.FN.C (CFGR.MCO2, 2#10#);
   function PLL_Clock            is new CFGR.FN.C (CFGR.MCO2, 2#11#);

   function No_Divided   is new CFGR.FN.C (CFGR.MCO2PRE, 2#000#);
   function Divided_By_2 is new CFGR.FN.C (CFGR.MCO2PRE, 2#100#);
   function Divided_By_3 is new CFGR.FN.C (CFGR.MCO2PRE, 2#101#);
   function Divided_By_4 is new CFGR.FN.C (CFGR.MCO2PRE, 2#110#);
   function Divided_By_5 is new CFGR.FN.C (CFGR.MCO2PRE, 2#111#);

   function No_Divided   is new CFGR.FN.C (CFGR.MCO1PRE, 2#000#);
   function Divided_By_2 is new CFGR.FN.C (CFGR.MCO1PRE, 2#100#);
   function Divided_By_3 is new CFGR.FN.C (CFGR.MCO1PRE, 2#101#);
   function Divided_By_4 is new CFGR.FN.C (CFGR.MCO1PRE, 2#110#);
   function Divided_By_5 is new CFGR.FN.C (CFGR.MCO1PRE, 2#111#);

   function PLLI2S_Clock                          is new CFGR.FN.C (CFGR.I2SSCR, 2#0#);
   function External_Clock_Mapped_On_The_I2S_CKIN is new CFGR.FN.C (CFGR.I2SSCR, 2#1#);

   function HSI_Clock_Selected            is new CFGR.FN.C (CFGR.MCO1, 2#00#);
   function LSE_Oscillator_Selected       is new CFGR.FN.C (CFGR.MCO1, 2#01#);
   function HSE_Oscillator_Clock_Selected is new CFGR.FN.C (CFGR.MCO1, 2#10#);
   function PLL_Clock_Selected            is new CFGR.FN.C (CFGR.MCO1, 2#11#);

   function No_Divided    is new CFGR.FN.C (CFGR.PPRE2, 2#000#);
   function Divided_By_2  is new CFGR.FN.C (CFGR.PPRE2, 2#100#);
   function Divided_By_4  is new CFGR.FN.C (CFGR.PPRE2, 2#101#);
   function Divided_By_8  is new CFGR.FN.C (CFGR.PPRE2, 2#110#);
   function Divided_By_16 is new CFGR.FN.C (CFGR.PPRE2, 2#111#);

   function No_Divided    is new CFGR.FN.C (CFGR.PPRE1, 2#000#);
   function Divided_By_2  is new CFGR.FN.C (CFGR.PPRE1, 2#100#);
   function Divided_By_4  is new CFGR.FN.C (CFGR.PPRE1, 2#101#);
   function Divided_By_8  is new CFGR.FN.C (CFGR.PPRE1, 2#110#);
   function Divided_By_16 is new CFGR.FN.C (CFGR.PPRE1, 2#111#);

   function System_Clock_No_Divided     is new CFGR.FN.C (CFGR.HPRE, 2#0000#);
   function System_Clock_Divided_By_2   is new CFGR.FN.C (CFGR.HPRE, 2#1000#);
   function System_Clock_Divided_By_4   is new CFGR.FN.C (CFGR.HPRE, 2#1001#);
   function System_Clock_Divided_By_8   is new CFGR.FN.C (CFGR.HPRE, 2#1010#);
   function System_Clock_Divided_By_16  is new CFGR.FN.C (CFGR.HPRE, 2#1011#);
   function System_Clock_Divided_By_64  is new CFGR.FN.C (CFGR.HPRE, 2#1100#);
   function System_Clock_Divided_By_128 is new CFGR.FN.C (CFGR.HPRE, 2#1101#);
   function System_Clock_Divided_By_256 is new CFGR.FN.C (CFGR.HPRE, 2#1110#);
   function System_Clock_Divided_By_512 is new CFGR.FN.C (CFGR.HPRE, 2#1111#);

   function HSI_Oscillator_Selected is new CFGR.FN.C (CFGR.SWS, 2#00#);
   function HSE_Oscillator_Selected is new CFGR.FN.C (CFGR.SWS, 2#01#);
   function PLL_Selected            is new CFGR.FN.C (CFGR.SWS, 2#10#);

   function HSI_Oscillator_Selected is new CFGR.FN.C (CFGR.SW, 2#00#);
   function HSE_Oscillator_Selected is new CFGR.FN.C (CFGR.SW, 2#01#);
   function PLL_Selected            is new CFGR.FN.C (CFGR.SW, 2#10#);

   --------------------------
   --  Interrupt Register  --
   --------------------------

   package CIR is

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
            CSSC        : CIR.CSSC       .T;
            PLLI2SRDYC  : CIR.PLLI2SRDYC .T;
            PLLRDYC     : CIR.PLLRDYC    .T;
            HSERDYC     : CIR.HSERDYC    .T;
            HSIRDYC     : CIR.HSIRDYC    .T;
            LSERDYC     : CIR.LSERDYC    .T;
            LSIRDYC     : CIR.LSIRDYC    .T;
            PLLI2SRDYIE : CIR.PLLI2SRDYIE.T;
            PLLRDYIE    : CIR.PLLRDYIE   .T;
            HSERDYIE    : CIR.HSERDYIE   .T;
            HSIRDYIE    : CIR.HSIRDYIE   .T;
            LSERDYIE    : CIR.LSERDYIE   .T;
            LSIRDYIE    : CIR.LSIRDYIE   .T;
            CSSF        : CIR.CSSF       .T;
            PLLI2SRDYF  : CIR.PLLI2SRDYF .T;
            PLLRDYF     : CIR.PLLRDYF    .T;
            HSERDYF     : CIR.HSERDYF    .T;
            HSIRDYF     : CIR.HSIRDYF    .T;
            LSERDYF     : CIR.LSERDYF    .T;
            LSIRDYF     : CIR.LSIRDYF    .T;
         end record;

      for T use
         record
            CSSC        at 0 range CSSC       .R'First .. CSSC       .R'Last;
            PLLI2SRDYC  at 0 range PLLI2SRDYC .R'First .. PLLI2SRDYC .R'Last;
            PLLRDYC     at 0 range PLLRDYC    .R'First .. PLLRDYC    .R'Last;
            HSERDYC     at 0 range HSERDYC    .R'First .. HSERDYC    .R'Last;
            HSIRDYC     at 0 range HSIRDYC    .R'First .. HSIRDYC    .R'Last;
            LSERDYC     at 0 range LSERDYC    .R'First .. LSERDYC    .R'Last;
            LSIRDYC     at 0 range LSIRDYC    .R'First .. LSIRDYC    .R'Last;
            PLLI2SRDYIE at 0 range PLLI2SRDYIE.R'First .. PLLI2SRDYIE.R'Last;
            PLLRDYIE    at 0 range PLLRDYIE   .R'First .. PLLRDYIE   .R'Last;
            HSERDYIE    at 0 range HSERDYIE   .R'First .. HSERDYIE   .R'Last;
            HSIRDYIE    at 0 range HSIRDYIE   .R'First .. HSIRDYIE   .R'Last;
            LSERDYIE    at 0 range LSERDYIE   .R'First .. LSERDYIE   .R'Last;
            LSIRDYIE    at 0 range LSIRDYIE   .R'First .. LSIRDYIE   .R'Last;
            CSSF        at 0 range CSSF       .R'First .. CSSF       .R'Last;
            PLLI2SRDYF  at 0 range PLLI2SRDYF .R'First .. PLLI2SRDYF .R'Last;
            PLLRDYF     at 0 range PLLRDYF    .R'First .. PLLRDYF    .R'Last;
            HSERDYF     at 0 range HSERDYF    .R'First .. HSERDYF    .R'Last;
            HSIRDYF     at 0 range HSIRDYF    .R'First .. HSIRDYF    .R'Last;
            LSERDYF     at 0 range LSERDYF    .R'First .. LSERDYF    .R'Last;
            LSIRDYF     at 0 range LSIRDYF    .R'First .. LSIRDYF    .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CIR;

   subtype CIR_T is CIR.T;

   --  Field definitions

   function CSSC        is new CIR.FN.B (CIR.CSSC       );
   function PLLI2SRDYC  is new CIR.FN.B (CIR.PLLI2SRDYC );
   function PLLRDYC     is new CIR.FN.B (CIR.PLLRDYC    );
   function HSERDYC     is new CIR.FN.B (CIR.HSERDYC    );
   function HSIRDYC     is new CIR.FN.B (CIR.HSIRDYC    );
   function LSERDYC     is new CIR.FN.B (CIR.LSERDYC    );
   function LSIRDYC     is new CIR.FN.B (CIR.LSIRDYC    );
   function PLLI2SRDYIE is new CIR.FN.B (CIR.PLLI2SRDYIE);
   function PLLRDYIE    is new CIR.FN.B (CIR.PLLRDYIE   );
   function HSERDYIE    is new CIR.FN.B (CIR.HSERDYIE   );
   function HSIRDYIE    is new CIR.FN.B (CIR.HSIRDYIE   );
   function LSERDYIE    is new CIR.FN.B (CIR.LSERDYIE   );
   function LSIRDYIE    is new CIR.FN.B (CIR.LSIRDYIE   );
   function CSSF        is new CIR.FN.B (CIR.CSSF       );
   function PLLI2SRDYF  is new CIR.FN.B (CIR.PLLI2SRDYF );
   function PLLRDYF     is new CIR.FN.B (CIR.PLLRDYF    );
   function HSERDYF     is new CIR.FN.B (CIR.HSERDYF    );
   function HSIRDYF     is new CIR.FN.B (CIR.HSIRDYF    );
   function LSERDYF     is new CIR.FN.B (CIR.LSERDYF    );
   function LSIRDYF     is new CIR.FN.B (CIR.LSIRDYF    );

   --  Functions

   function  "+"   is new CIR.FN.Add;
   function  "+"   is new CIR.FN.Add_RM;
   function  "-"   is new CIR.FN.Clear;
   function  Init  is new CIR.FN.Init;

   --  Constant definitions

   function Clear_Flag is new CIR.FN.C (CIR.CSSC, 2#1#);
   function Clear_Flag is new CIR.FN.C (CIR.PLLI2SRDYC, 2#1#);
   function Clear_Flag is new CIR.FN.C (CIR.PLLRDYC, 2#1#);
   function Clear_Flag is new CIR.FN.C (CIR.HSERDYC, 2#1#);
   function Clear_Flag is new CIR.FN.C (CIR.HSIRDYC, 2#1#);
   function Clear_Flag is new CIR.FN.C (CIR.LSERDYC, 2#1#);
   function Clear_Flag is new CIR.FN.C (CIR.LSIRDYC, 2#1#);

   function Lock_Interrupt_Disabled is new CIR.FN.C (CIR.PLLI2SRDYIE, 2#0#);
   function Lock_Interrupt_Enabled  is new CIR.FN.C (CIR.PLLI2SRDYIE, 2#1#);

   function Lock_Interrupt_Disabled is new CIR.FN.C (CIR.PLLRDYIE, 2#0#);
   function Lock_Interrupt_Enabled  is new CIR.FN.C (CIR.PLLRDYIE, 2#1#);

   function Lock_Interrupt_Disabled is new CIR.FN.C (CIR.HSERDYIE, 2#0#);
   function Lock_Interrupt_Enabled  is new CIR.FN.C (CIR.HSERDYIE, 2#1#);

   function Lock_Interrupt_Disabled is new CIR.FN.C (CIR.HSIRDYIE, 2#0#);
   function Lock_Interrupt_Enabled  is new CIR.FN.C (CIR.HSIRDYIE, 2#1#);

   function Lock_Interrupt_Disabled is new CIR.FN.C (CIR.LSERDYIE, 2#0#);
   function Lock_Interrupt_Enabled  is new CIR.FN.C (CIR.LSERDYIE, 2#1#);

   function Lock_Interrupt_Disabled is new CIR.FN.C (CIR.LSIRDYIE, 2#0#);
   function Lock_Interrupt_Enabled  is new CIR.FN.C (CIR.LSIRDYIE, 2#1#);


   function No_Clock_Security is new CIR.FN.C (CIR.CSSF, 2#0#);
   function Clock_Security    is new CIR.FN.C (CIR.CSSF, 2#1#);

   function No_Clock_Security is new CIR.FN.C (CIR.PLLI2SRDYF, 2#0#);
   function Clock_Security    is new CIR.FN.C (CIR.PLLI2SRDYF, 2#1#);

   function No_Clock_Security is new CIR.FN.C (CIR.PLLRDYF, 2#0#);
   function Clock_Security    is new CIR.FN.C (CIR.PLLRDYF, 2#1#);

   function No_Clock_Security is new CIR.FN.C (CIR.HSERDYF, 2#0#);
   function Clock_Security    is new CIR.FN.C (CIR.HSERDYF, 2#1#);

   function No_Clock_Security is new CIR.FN.C (CIR.HSIRDYF, 2#0#);
   function Clock_Security    is new CIR.FN.C (CIR.HSIRDYF, 2#1#);

   function No_Clock_Security is new CIR.FN.C (CIR.LSERDYF, 2#0#);
   function Clock_Security    is new CIR.FN.C (CIR.LSERDYF, 2#1#);

   function No_Clock_Security is new CIR.FN.C (CIR.LSIRDYF, 2#0#);
   function Clock_Security    is new CIR.FN.C (CIR.LSIRDYF, 2#1#);

   --------------------------------------
   --  AHB1 Peripheral Reset Register  --
   --------------------------------------

   package AHB1RSTR is

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
            OTGHSRST  : AHB1RSTR .OTGHSRST . T;
            ETHMACRST : AHB1RSTR .ETHMACRST. T;
            DMA2RST   : AHB1RSTR .DMA2RST  . T;
            DMA1RST   : AHB1RSTR .DMA1RST  . T;
            CRCRST    : AHB1RSTR .CRCRST   . T;
            GPIOIRST  : AHB1RSTR .GPIOIRST . T;
            GPIOHRST  : AHB1RSTR .GPIOHRST . T;
            GPIOGRST  : AHB1RSTR .GPIOGRST . T;
            GPIOFRST  : AHB1RSTR .GPIOFRST . T;
            GPIOERST  : AHB1RSTR .GPIOERST . T;
            GPIODRST  : AHB1RSTR .GPIODRST . T;
            GPIOCRST  : AHB1RSTR .GPIOCRST . T;
            GPIOBRST  : AHB1RSTR .GPIOBRST . T;
            GPIOARST  : AHB1RSTR .GPIOARST . T;
         end record;

      for T use
         record
            OTGHSRST  at 0 range OTGHSRST .R'First .. OTGHSRST .R'Last;
            ETHMACRST at 0 range ETHMACRST.R'First .. ETHMACRST.R'Last;
            DMA2RST   at 0 range DMA2RST  .R'First .. DMA2RST  .R'Last;
            DMA1RST   at 0 range DMA1RST  .R'First .. DMA1RST  .R'Last;
            CRCRST    at 0 range CRCRST   .R'First .. CRCRST   .R'Last;
            GPIOIRST  at 0 range GPIOIRST .R'First .. GPIOIRST .R'Last;
            GPIOHRST  at 0 range GPIOHRST .R'First .. GPIOHRST .R'Last;
            GPIOGRST  at 0 range GPIOGRST .R'First .. GPIOGRST .R'Last;
            GPIOFRST  at 0 range GPIOFRST .R'First .. GPIOFRST .R'Last;
            GPIOERST  at 0 range GPIOERST .R'First .. GPIOERST .R'Last;
            GPIODRST  at 0 range GPIODRST .R'First .. GPIODRST .R'Last;
            GPIOCRST  at 0 range GPIOCRST .R'First .. GPIOCRST .R'Last;
            GPIOBRST  at 0 range GPIOBRST .R'First .. GPIOBRST .R'Last;
            GPIOARST  at 0 range GPIOARST .R'First .. GPIOARST .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHB1RSTR;

   subtype AHB1RSTR_T is AHB1RSTR.T;

   --  Field definitions

   function OTGHSRST  is new AHB1RSTR.FN.B (AHB1RSTR.OTGHSRST );
   function ETHMACRST is new AHB1RSTR.FN.B (AHB1RSTR.ETHMACRST);
   function DMA2RST   is new AHB1RSTR.FN.B (AHB1RSTR.DMA2RST  );
   function DMA1RST   is new AHB1RSTR.FN.B (AHB1RSTR.DMA1RST  );
   function CRCRST    is new AHB1RSTR.FN.B (AHB1RSTR.CRCRST   );
   function GPIOIRST  is new AHB1RSTR.FN.B (AHB1RSTR.GPIOIRST);
   function GPIOHRST  is new AHB1RSTR.FN.B (AHB1RSTR.GPIOHRST);
   function GPIOGRST  is new AHB1RSTR.FN.B (AHB1RSTR.GPIOGRST);
   function GPIOFRST  is new AHB1RSTR.FN.B (AHB1RSTR.GPIOFRST);
   function GPIOERST  is new AHB1RSTR.FN.B (AHB1RSTR.GPIOERST);
   function GPIODRST  is new AHB1RSTR.FN.B (AHB1RSTR.GPIODRST);
   function GPIOCRST  is new AHB1RSTR.FN.B (AHB1RSTR.GPIOCRST);
   function GPIOBRST  is new AHB1RSTR.FN.B (AHB1RSTR.GPIOBRST);
   function GPIOARST  is new AHB1RSTR.FN.B (AHB1RSTR.GPIOARST);

   --  Functions

   function  "+"   is new AHB1RSTR.FN.Add;
   function  "+"   is new AHB1RSTR.FN.Add_RM;
   function  "-"   is new AHB1RSTR.FN.Clear;
   function  Init  is new AHB1RSTR.FN.Init;

   --  Constant definitions

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.OTGHSRST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.OTGHSRST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.ETHMACRST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.ETHMACRST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.DMA2RST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.DMA2RST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.DMA1RST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.DMA1RST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.CRCRST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.CRCRST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.GPIOIRST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.GPIOIRST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.GPIOHRST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.GPIOHRST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.GPIOGRST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.GPIOGRST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.GPIOFRST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.GPIOFRST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.GPIOERST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.GPIOERST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.GPIODRST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.GPIODRST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.GPIOCRST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.GPIOCRST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.GPIOBRST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.GPIOBRST, 2#1#);

   function Does_Not_Reset is new AHB1RSTR.FN.C (AHB1RSTR.GPIOARST, 2#0#);
   function Resets         is new AHB1RSTR.FN.C (AHB1RSTR.GPIOARST, 2#1#);

   --------------------------------------
   --  AHB2 Peripheral Reset Register  --
   --------------------------------------

   package AHB2RSTR is

      package Tp is new Types (R32);

      package OTGFSRST is new Bitfield (Tp, 7);
      package RNGRST   is new Bitfield (Tp, 6);
      package HASHRST  is new Bitfield (Tp, 5);
      package CRYPRST  is new Bitfield (Tp, 4);
      package DCMIRST  is new Bitfield (Tp, 0);

      type T is
         record
            OTGFSRST : AHB2RSTR .OTGFSRST. T;
            RNGRST   : AHB2RSTR .RNGRST  . T;
            HASHRST  : AHB2RSTR .HASHRST . T;
            CRYPRST  : AHB2RSTR .CRYPRST . T;
            DCMIRST  : AHB2RSTR .DCMIRST . T;
         end record;

      for T use
         record
            OTGFSRST at 0 range OTGFSRST.R'First .. OTGFSRST.R'Last;
            RNGRST   at 0 range RNGRST  .R'First .. RNGRST  .R'Last;
            HASHRST  at 0 range HASHRST .R'First .. HASHRST .R'Last;
            CRYPRST  at 0 range CRYPRST .R'First .. CRYPRST .R'Last;
            DCMIRST  at 0 range DCMIRST .R'First .. DCMIRST .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHB2RSTR;

   subtype AHB2RSTR_T is AHB2RSTR.T;

   --  Field definitions

   function OTGFSRST is new AHB2RSTR.FN.B (AHB2RSTR.OTGFSRST);
   function RNGRST   is new AHB2RSTR.FN.B (AHB2RSTR.RNGRST  );
   function HASHRST  is new AHB2RSTR.FN.B (AHB2RSTR.HASHRST );
   function CRYPRST  is new AHB2RSTR.FN.B (AHB2RSTR.CRYPRST );
   function DCMIRST  is new AHB2RSTR.FN.B (AHB2RSTR.DCMIRST );

   --  Functions

   function  "+"   is new AHB2RSTR.FN.Add;
   function  "+"   is new AHB2RSTR.FN.Add_RM;
   function  "-"   is new AHB2RSTR.FN.Clear;
   function  Init  is new AHB2RSTR.FN.Init;

   --  Constant definitions

   function Does_Not_Reset is new AHB2RSTR.FN.C (AHB2RSTR.OTGFSRST, 2#0#);
   function Resets         is new AHB2RSTR.FN.C (AHB2RSTR.OTGFSRST, 2#1#);

   function Does_Not_Reset is new AHB2RSTR.FN.C (AHB2RSTR.RNGRST, 2#0#);
   function Resets         is new AHB2RSTR.FN.C (AHB2RSTR.RNGRST, 2#1#);

   function Does_Not_Reset is new AHB2RSTR.FN.C (AHB2RSTR.HASHRST, 2#0#);
   function Resets         is new AHB2RSTR.FN.C (AHB2RSTR.HASHRST, 2#1#);

   function Does_Not_Reset is new AHB2RSTR.FN.C (AHB2RSTR.CRYPRST, 2#0#);
   function Resets         is new AHB2RSTR.FN.C (AHB2RSTR.CRYPRST, 2#1#);

   function Does_Not_Reset is new AHB2RSTR.FN.C (AHB2RSTR.DCMIRST, 2#0#);
   function Resets         is new AHB2RSTR.FN.C (AHB2RSTR.DCMIRST, 2#1#);

   --------------------------------------
   --  AHB3 Peripheral Reset Register  --
   --------------------------------------

   package AHB3RSTR is

      package Tp is new Types (R32);

      package FSMCRST is new Bitfield (Tp, 0);

      type T is
         record
            FSMCRST : AHB3RSTR.FSMCRST.T;
         end record;

      for T use
         record
            FSMCRST at 0 range FSMCRST.R'First .. FSMCRST.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHB3RSTR;

   subtype AHB3RSTR_T is AHB3RSTR.T;

   --  Field definitions

   function FSMCRST is new AHB3RSTR.FN.B (AHB3RSTR.FSMCRST);

   --  Functions

   function  "+"   is new AHB3RSTR.FN.Add;
   function  "+"   is new AHB3RSTR.FN.Add_RM;
   function  "-"   is new AHB3RSTR.FN.Clear;
   function  Init  is new AHB3RSTR.FN.Init;

   --  Constant definitions

   function Does_Not_Reset is new AHB3RSTR.FN.C (AHB3RSTR.FSMCRST, 2#0#);
   function Resets         is new AHB3RSTR.FN.C (AHB3RSTR.FSMCRST, 2#1#);

   --------------------------------------
   --  APB1 Peripheral Reset Register  --
   --------------------------------------

   package APB1RSTR is

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
            DACRST   : APB1RSTR.DACRST  .T;
            PWRRST   : APB1RSTR.PWRRST  .T;
            CAN2RST  : APB1RSTR.CAN2RST .T;
            CAN1RST  : APB1RSTR.CAN1RST .T;
            I2C3RST  : APB1RSTR.I2C3RST .T;
            I2C2RST  : APB1RSTR.I2C2RST .T;
            I2C1RST  : APB1RSTR.I2C1RST .T;
            UART5RST : APB1RSTR.UART5RST.T;
            UART4RST : APB1RSTR.UART4RST.T;
            UART3RST : APB1RSTR.UART3RST.T;
            UART2RST : APB1RSTR.UART2RST.T;
            SPI3RST  : APB1RSTR.SPI3RST .T;
            SPI2RST  : APB1RSTR.SPI2RST .T;
            WWDGRST  : APB1RSTR.WWDGRST .T;
            TIM14RST : APB1RSTR.TIM14RST.T;
            TIM13RST : APB1RSTR.TIM13RST.T;
            TIM12RST : APB1RSTR.TIM12RST.T;
            TIM7RST  : APB1RSTR.TIM7RST .T;
            TIM6RST  : APB1RSTR.TIM6RST .T;
            TIM5RST  : APB1RSTR.TIM5RST .T;
            TIM4RST  : APB1RSTR.TIM4RST .T;
            TIM3RST  : APB1RSTR.TIM3RST .T;
            TIM2RST  : APB1RSTR.TIM2RST .T;
         end record;

      for T use
         record
            DACRST   at 0 range DACRST  .R'First .. DACRST  .R'Last;
            PWRRST   at 0 range PWRRST  .R'First .. PWRRST  .R'Last;
            CAN2RST  at 0 range CAN2RST .R'First .. CAN2RST .R'Last;
            CAN1RST  at 0 range CAN1RST .R'First .. CAN1RST .R'Last;
            I2C3RST  at 0 range I2C3RST .R'First .. I2C3RST .R'Last;
            I2C2RST  at 0 range I2C2RST .R'First .. I2C2RST .R'Last;
            I2C1RST  at 0 range I2C1RST .R'First .. I2C1RST .R'Last;
            UART5RST at 0 range UART5RST.R'First .. UART5RST.R'Last;
            UART4RST at 0 range UART4RST.R'First .. UART4RST.R'Last;
            UART3RST at 0 range UART3RST.R'First .. UART3RST.R'Last;
            UART2RST at 0 range UART2RST.R'First .. UART2RST.R'Last;
            SPI3RST  at 0 range SPI3RST .R'First .. SPI3RST .R'Last;
            SPI2RST  at 0 range SPI2RST .R'First .. SPI2RST .R'Last;
            WWDGRST  at 0 range WWDGRST .R'First .. WWDGRST .R'Last;
            TIM14RST at 0 range TIM14RST.R'First .. TIM14RST.R'Last;
            TIM13RST at 0 range TIM13RST.R'First .. TIM13RST.R'Last;
            TIM12RST at 0 range TIM12RST.R'First .. TIM12RST.R'Last;
            TIM7RST  at 0 range TIM7RST .R'First .. TIM7RST .R'Last;
            TIM6RST  at 0 range TIM6RST .R'First .. TIM6RST .R'Last;
            TIM5RST  at 0 range TIM5RST .R'First .. TIM5RST .R'Last;
            TIM4RST  at 0 range TIM4RST .R'First .. TIM4RST .R'Last;
            TIM3RST  at 0 range TIM3RST .R'First .. TIM3RST .R'Last;
            TIM2RST  at 0 range TIM2RST .R'First .. TIM2RST .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB1RSTR;

   subtype APB1RSTR_T is APB1RSTR.T;

   --  Field definitions

   function DACRST   is new APB1RSTR.FN.B (APB1RSTR.DACRST  );
   function PWRRST   is new APB1RSTR.FN.B (APB1RSTR.PWRRST  );
   function CAN2RST  is new APB1RSTR.FN.B (APB1RSTR.CAN2RST );
   function CAN1RST  is new APB1RSTR.FN.B (APB1RSTR.CAN1RST );
   function I2C3RST  is new APB1RSTR.FN.B (APB1RSTR.I2C3RST );
   function I2C2RST  is new APB1RSTR.FN.B (APB1RSTR.I2C2RST );
   function I2C1RST  is new APB1RSTR.FN.B (APB1RSTR.I2C1RST );
   function UART5RST is new APB1RSTR.FN.B (APB1RSTR.UART5RST);
   function UART4RST is new APB1RSTR.FN.B (APB1RSTR.UART4RST);
   function UART3RST is new APB1RSTR.FN.B (APB1RSTR.UART3RST);
   function UART2RST is new APB1RSTR.FN.B (APB1RSTR.UART2RST);
   function SPI3RST  is new APB1RSTR.FN.B (APB1RSTR.SPI3RST );
   function SPI2RST  is new APB1RSTR.FN.B (APB1RSTR.SPI2RST );
   function WWDGRST  is new APB1RSTR.FN.B (APB1RSTR.WWDGRST );
   function TIM14RST is new APB1RSTR.FN.B (APB1RSTR.TIM14RST);
   function TIM13RST is new APB1RSTR.FN.B (APB1RSTR.TIM13RST);
   function TIM12RST is new APB1RSTR.FN.B (APB1RSTR.TIM12RST);
   function TIM7RST  is new APB1RSTR.FN.B (APB1RSTR.TIM7RST );
   function TIM6RST  is new APB1RSTR.FN.B (APB1RSTR.TIM6RST );
   function TIM5RST  is new APB1RSTR.FN.B (APB1RSTR.TIM5RST );
   function TIM4RST  is new APB1RSTR.FN.B (APB1RSTR.TIM4RST );
   function TIM3RST  is new APB1RSTR.FN.B (APB1RSTR.TIM3RST );
   function TIM2RST  is new APB1RSTR.FN.B (APB1RSTR.TIM2RST );

   --  Functions

   function  "+"   is new APB1RSTR.FN.Add;
   function  "+"   is new APB1RSTR.FN.Add_RM;
   function  "-"   is new APB1RSTR.FN.Clear;
   function  Init  is new APB1RSTR.FN.Init;

   --  Constant definitions

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.DACRST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.DACRST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.PWRRST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.PWRRST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.CAN2RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.CAN2RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.CAN1RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.CAN1RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.I2C3RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.I2C3RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.I2C2RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.I2C2RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.I2C1RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.I2C1RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.UART5RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.UART5RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.UART4RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.UART4RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.UART3RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.UART3RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.UART2RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.UART2RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.SPI3RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.SPI3RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.SPI2RST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.SPI2RST, 2#1#);

   function Does_Not_Reset is new APB1RSTR.FN.C (APB1RSTR.WWDGRST, 2#0#);
   function Resets         is new APB1RSTR.FN.C (APB1RSTR.WWDGRST, 2#1#);

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

   --------------------------------------
   --  APB2 Peripheral Reset Register  --
   --------------------------------------

   package APB2RSTR is

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
            TIM11RST  : APB2RSTR.TIM11RST .T;
            TIM10RST  : APB2RSTR.TIM10RST .T;
            TIM9RST   : APB2RSTR.TIM9RST  .T;
            SYSCFGRST : APB2RSTR.SYSCFGRST.T;
            SPI1RST   : APB2RSTR.SPI1RST  .T;
            SDIORST   : APB2RSTR.SDIORST  .T;
            ADCRST    : APB2RSTR.ADCRST   .T;
            USART6RST : APB2RSTR.USART6RST.T;
            USART1RST : APB2RSTR.USART1RST.T;
            TIM8RST   : APB2RSTR.TIM8RST  .T;
            TIM1RST   : APB2RSTR.TIM1RST  .T;
         end record;

      for T use
         record
            TIM11RST  at 0 range TIM11RST .R'First .. TIM11RST .R'Last;
            TIM10RST  at 0 range TIM10RST .R'First .. TIM10RST .R'Last;
            TIM9RST   at 0 range TIM9RST  .R'First .. TIM9RST  .R'Last;
            SYSCFGRST at 0 range SYSCFGRST.R'First .. SYSCFGRST.R'Last;
            SPI1RST   at 0 range SPI1RST  .R'First .. SPI1RST  .R'Last;
            SDIORST   at 0 range SDIORST  .R'First .. SDIORST  .R'Last;
            ADCRST    at 0 range ADCRST   .R'First .. ADCRST   .R'Last;
            USART6RST at 0 range USART6RST.R'First .. USART6RST.R'Last;
            USART1RST at 0 range USART1RST.R'First .. USART1RST.R'Last;
            TIM8RST   at 0 range TIM8RST  .R'First .. TIM8RST  .R'Last;
            TIM1RST   at 0 range TIM1RST  .R'First .. TIM1RST  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB2RSTR;

   subtype APB2RSTR_T is APB2RSTR.T;

   --  Field definitions

   function TIM11RST  is new APB2RSTR.FN.B (APB2RSTR.TIM11RST );
   function TIM10RST  is new APB2RSTR.FN.B (APB2RSTR.TIM10RST );
   function TIM9RST   is new APB2RSTR.FN.B (APB2RSTR.TIM9RST  );
   function SYSCFGRST is new APB2RSTR.FN.B (APB2RSTR.SYSCFGRST);
   function SPI1RST   is new APB2RSTR.FN.B (APB2RSTR.SPI1RST  );
   function SDIORST   is new APB2RSTR.FN.B (APB2RSTR.SDIORST  );
   function ADCRST    is new APB2RSTR.FN.B (APB2RSTR.ADCRST   );
   function USART6RST is new APB2RSTR.FN.B (APB2RSTR.USART6RST);
   function USART1RST is new APB2RSTR.FN.B (APB2RSTR.USART1RST);
   function TIM8RST   is new APB2RSTR.FN.B (APB2RSTR.TIM8RST  );
   function TIM1RST   is new APB2RSTR.FN.B (APB2RSTR.TIM1RST  );

   --  Functions

   function  "+"   is new APB2RSTR.FN.Add;
   function  "+"   is new APB2RSTR.FN.Add_RM;
   function  "-"   is new APB2RSTR.FN.Clear;
   function  Init  is new APB2RSTR.FN.Init;

   --  Constant definitions

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.TIM11RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.TIM11RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.TIM10RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.TIM10RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.TIM9RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.TIM9RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.SYSCFGRST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.SYSCFGRST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.SPI1RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.SPI1RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.SDIORST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.SDIORST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.ADCRST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.ADCRST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.USART6RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.USART6RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.USART1RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.USART1RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.TIM8RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.TIM8RST, 2#1#);

   function Does_Not_Reset is new APB2RSTR.FN.C (APB2RSTR.TIM1RST, 2#0#);
   function Resets         is new APB2RSTR.FN.C (APB2RSTR.TIM1RST, 2#1#);

   --------------------------------------------
   --  AHB1 Peipheral Clock Enable Register  --
   --------------------------------------------

   package AHB1ENR is

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
            OTGHSULPIEN  : AHB1ENR .OTGHSULPIEN . T;
            OTGHSEN      : AHB1ENR .OTGHSEN     . T;
            ETHMACPTPEN  : AHB1ENR .ETHMACPTPEN . T;
            ETHMACRXEN   : AHB1ENR .ETHMACRXEN  . T;
            ETHMACTXEN   : AHB1ENR .ETHMACTXEN  . T;
            ETHMACEN     : AHB1ENR .ETHMACEN    . T;
            DMA2EN       : AHB1ENR .DMA2EN      . T;
            DMA1EN       : AHB1ENR .DMA1EN      . T;
            CCMDATARAMEN : AHB1ENR .CCMDATARAMEN. T;
            BKPSRAMEN    : AHB1ENR .BKPSRAMEN   . T;
            CRCEN        : AHB1ENR .CRCEN       . T;
            GPIOIEN      : AHB1ENR .GPIOIEN     . T;
            GPIOHEN      : AHB1ENR .GPIOHEN     . T;
            GPIOGEN      : AHB1ENR .GPIOGEN     . T;
            GPIOFEN      : AHB1ENR .GPIOFEN     . T;
            GPIOEEN      : AHB1ENR .GPIOEEN     . T;
            GPIODEN      : AHB1ENR .GPIODEN     . T;
            GPIOCEN      : AHB1ENR .GPIOCEN     . T;
            GPIOBEN      : AHB1ENR .GPIOBEN     . T;
            GPIOAEN      : AHB1ENR .GPIOAEN     . T;
         end record;

      for T use
         record
            OTGHSULPIEN  at 0 range OTGHSULPIEN .R'First .. OTGHSULPIEN .R'Last;
            OTGHSEN      at 0 range OTGHSEN     .R'First .. OTGHSEN     .R'Last;
            ETHMACPTPEN  at 0 range ETHMACPTPEN .R'First .. ETHMACPTPEN .R'Last;
            ETHMACRXEN   at 0 range ETHMACRXEN  .R'First .. ETHMACRXEN  .R'Last;
            ETHMACTXEN   at 0 range ETHMACTXEN  .R'First .. ETHMACTXEN  .R'Last;
            ETHMACEN     at 0 range ETHMACEN    .R'First .. ETHMACEN    .R'Last;
            DMA2EN       at 0 range DMA2EN      .R'First .. DMA2EN      .R'Last;
            DMA1EN       at 0 range DMA1EN      .R'First .. DMA1EN      .R'Last;
            CCMDATARAMEN at 0 range CCMDATARAMEN.R'First .. CCMDATARAMEN.R'Last;
            BKPSRAMEN    at 0 range BKPSRAMEN   .R'First .. BKPSRAMEN   .R'Last;
            CRCEN        at 0 range CRCEN       .R'First .. CRCEN       .R'Last;
            GPIOIEN      at 0 range GPIOIEN     .R'First .. GPIOIEN     .R'Last;
            GPIOHEN      at 0 range GPIOHEN     .R'First .. GPIOHEN     .R'Last;
            GPIOGEN      at 0 range GPIOGEN     .R'First .. GPIOGEN     .R'Last;
            GPIOFEN      at 0 range GPIOFEN     .R'First .. GPIOFEN     .R'Last;
            GPIOEEN      at 0 range GPIOEEN     .R'First .. GPIOEEN     .R'Last;
            GPIODEN      at 0 range GPIODEN     .R'First .. GPIODEN     .R'Last;
            GPIOCEN      at 0 range GPIOCEN     .R'First .. GPIOCEN     .R'Last;
            GPIOBEN      at 0 range GPIOBEN     .R'First .. GPIOBEN     .R'Last;
            GPIOAEN      at 0 range GPIOAEN     .R'First .. GPIOAEN     .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHB1ENR;

   subtype AHB1ENR_T is AHB1ENR.T;

   --  Field definitions

   function OTGHSULPIEN  is new AHB1ENR.FN.B (AHB1ENR.OTGHSULPIEN );
   function OTGHSEN      is new AHB1ENR.FN.B (AHB1ENR.OTGHSEN     );
   function ETHMACPTPEN  is new AHB1ENR.FN.B (AHB1ENR.ETHMACPTPEN );
   function ETHMACRXEN   is new AHB1ENR.FN.B (AHB1ENR.ETHMACRXEN  );
   function ETHMACTXEN   is new AHB1ENR.FN.B (AHB1ENR.ETHMACTXEN  );
   function ETHMACEN     is new AHB1ENR.FN.B (AHB1ENR.ETHMACEN    );
   function DMA2EN       is new AHB1ENR.FN.B (AHB1ENR.DMA2EN      );
   function DMA1EN       is new AHB1ENR.FN.B (AHB1ENR.DMA1EN      );
   function CCMDATARAMEN is new AHB1ENR.FN.B (AHB1ENR.CCMDATARAMEN);
   function BKPSRAMEN    is new AHB1ENR.FN.B (AHB1ENR.BKPSRAMEN   );
   function CRCEN        is new AHB1ENR.FN.B (AHB1ENR.CRCEN       );
   function GPIOIEN      is new AHB1ENR.FN.B (AHB1ENR.GPIOIEN      );
   function GPIOHEN      is new AHB1ENR.FN.B (AHB1ENR.GPIOHEN      );
   function GPIOGEN      is new AHB1ENR.FN.B (AHB1ENR.GPIOGEN      );
   function GPIOFEN      is new AHB1ENR.FN.B (AHB1ENR.GPIOFEN      );
   function GPIOEEN      is new AHB1ENR.FN.B (AHB1ENR.GPIOEEN      );
   function GPIODEN      is new AHB1ENR.FN.B (AHB1ENR.GPIODEN      );
   function GPIOCEN      is new AHB1ENR.FN.B (AHB1ENR.GPIOCEN      );
   function GPIOBEN      is new AHB1ENR.FN.B (AHB1ENR.GPIOBEN      );
   function GPIOAEN      is new AHB1ENR.FN.B (AHB1ENR.GPIOAEN      );

   --  Functions

   function  "+"   is new AHB1ENR.FN.Add;
   function  "+"   is new AHB1ENR.FN.Add_RM;
   function  "-"   is new AHB1ENR.FN.Clear;
   function  Init  is new AHB1ENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.OTGHSULPIEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.OTGHSULPIEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.OTGHSEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.OTGHSEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.ETHMACPTPEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.ETHMACPTPEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.ETHMACRXEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.ETHMACRXEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.ETHMACTXEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.ETHMACTXEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.ETHMACEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.ETHMACEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.DMA2EN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.DMA2EN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.DMA1EN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.DMA1EN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.CCMDATARAMEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.CCMDATARAMEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.BKPSRAMEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.BKPSRAMEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.CRCEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.CRCEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.GPIOIEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.GPIOIEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.GPIOHEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.GPIOHEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.GPIOGEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.GPIOGEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.GPIOFEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.GPIOFEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.GPIOEEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.GPIOEEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.GPIODEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.GPIODEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.GPIOCEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.GPIOCEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.GPIOBEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.GPIOBEN, 2#1#);

   function Clock_Disabled is new AHB1ENR.FN.C (AHB1ENR.GPIOAEN, 2#0#);
   function Clock_Enabled  is new AHB1ENR.FN.C (AHB1ENR.GPIOAEN, 2#1#);

   ---------------------------------------------
   --  AHB2 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package AHB2ENR is

      package Tp is new Types (R32);

      package OTGFSEN is new Bitfield (Tp, 7);
      package RNGEN   is new Bitfield (Tp, 6);
      package HASHEN  is new Bitfield (Tp, 5);
      package CRYPEN  is new Bitfield (Tp, 4);
      package DCMIEN  is new Bitfield (Tp, 0);

      type T is
         record
            OTGFSEN : AHB2ENR .OTGFSEN. T;
            RNGEN   : AHB2ENR .RNGEN  . T;
            HASHEN  : AHB2ENR .HASHEN . T;
            CRYPEN  : AHB2ENR .CRYPEN . T;
            DCMIEN  : AHB2ENR .DCMIEN . T;
         end record;

      for T use
         record
            OTGFSEN at 0 range OTGFSEN.R'First .. OTGFSEN.R'Last;
            RNGEN   at 0 range RNGEN  .R'First .. RNGEN  .R'Last;
            HASHEN  at 0 range HASHEN .R'First .. HASHEN .R'Last;
            CRYPEN  at 0 range CRYPEN .R'First .. CRYPEN .R'Last;
            DCMIEN  at 0 range DCMIEN .R'First .. DCMIEN .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHB2ENR;

   subtype AHB2ENR_T is AHB2ENR.T;

   --  Field definitions

   function OTGFSEN is new AHB2ENR.FN.B (AHB2ENR.OTGFSEN);
   function RNGEN   is new AHB2ENR.FN.B (AHB2ENR.RNGEN  );
   function HASHEN  is new AHB2ENR.FN.B (AHB2ENR.HASHEN );
   function CRYPEN  is new AHB2ENR.FN.B (AHB2ENR.CRYPEN );
   function DCMIEN  is new AHB2ENR.FN.B (AHB2ENR.DCMIEN );

   --  Functions

   function  "+"   is new AHB2ENR.FN.Add;
   function  "+"   is new AHB2ENR.FN.Add_RM;
   function  "-"   is new AHB2ENR.FN.Clear;
   function  Init  is new AHB2ENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new AHB2ENR.FN.C (AHB2ENR.OTGFSEN, 2#0#);
   function Clock_Enabled  is new AHB2ENR.FN.C (AHB2ENR.OTGFSEN, 2#1#);

   function Clock_Disabled is new AHB2ENR.FN.C (AHB2ENR.RNGEN, 2#0#);
   function Clock_Enabled  is new AHB2ENR.FN.C (AHB2ENR.RNGEN, 2#1#);

   function Clock_Disabled is new AHB2ENR.FN.C (AHB2ENR.HASHEN, 2#0#);
   function Clock_Enabled  is new AHB2ENR.FN.C (AHB2ENR.HASHEN, 2#1#);

   function Clock_Disabled is new AHB2ENR.FN.C (AHB2ENR.CRYPEN, 2#0#);
   function Clock_Enabled  is new AHB2ENR.FN.C (AHB2ENR.CRYPEN, 2#1#);

   function Clock_Disabled is new AHB2ENR.FN.C (AHB2ENR.DCMIEN, 2#0#);
   function Clock_Enabled  is new AHB2ENR.FN.C (AHB2ENR.DCMIEN, 2#1#);

   ---------------------------------------------
   --  AHB3 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package AHB3ENR is

      package Tp is new Types (R32);

      package FSMCEN is new Bitfield (Tp, 0);

      type T is
         record
            FSMCEN : AHB3ENR.FSMCEN.T;
         end record;

      for T use
         record
            FSMCEN at 0 range FSMCEN.R'First .. FSMCEN.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHB3ENR;

   subtype AHB3ENR_T is AHB3ENR.T;

   --  Field definitions

   function FSMCEN is new AHB3ENR.FN.B (AHB3ENR.FSMCEN);

   --  Functions

   function  "+"   is new AHB3ENR.FN.Add;
   function  "+"   is new AHB3ENR.FN.Add_RM;
   function  "-"   is new AHB3ENR.FN.Clear;
   function  Init  is new AHB3ENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new AHB3ENR.FN.C (AHB3ENR.FSMCEN, 2#0#);
   function Clock_Enabled  is new AHB3ENR.FN.C (AHB3ENR.FSMCEN, 2#1#);

   ---------------------------------------------
   --  APB1 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package APB1ENR is

      package Tp is new Types (R32);

      package DACEN   is new Bitfield (Tp, 29);
      package PWREN   is new Bitfield (Tp, 28);
      package CAN2EN  is new Bitfield (Tp, 26);
      package CAN1EN  is new Bitfield (Tp, 25);
      package I2C3EN  is new Bitfield (Tp, 23);
      package I2C2EN  is new Bitfield (Tp, 22);
      package I2C1EN  is new Bitfield (Tp, 21);
      package UART5EN is new Bitfield (Tp, 20);
      package UART4EN is new Bitfield (Tp, 19);
      package USART3EN is new Bitfield (Tp, 18);
      package USART2EN is new Bitfield (Tp, 17);
      package SPI3EN  is new Bitfield (Tp, 15);
      package SPI2EN  is new Bitfield (Tp, 14);
      package WWDGEN  is new Bitfield (Tp, 11);
      package TIM14EN is new Bitfield (Tp, 8);
      package TIM13EN is new Bitfield (Tp, 7);
      package TIM12EN is new Bitfield (Tp, 6);
      package TIM7EN  is new Bitfield (Tp, 5);
      package TIM6EN  is new Bitfield (Tp, 4);
      package TIM5EN  is new Bitfield (Tp, 3);
      package TIM4EN  is new Bitfield (Tp, 2);
      package TIM3EN  is new Bitfield (Tp, 1);
      package TIM2EN  is new Bitfield (Tp, 0);

      type T is
         record
            DACEN    : APB1ENR.DACEN  .T;
            PWREN    : APB1ENR.PWREN  .T;
            CAN2EN   : APB1ENR.CAN2EN .T;
            CAN1EN   : APB1ENR.CAN1EN .T;
            I2C3EN   : APB1ENR.I2C3EN .T;
            I2C2EN   : APB1ENR.I2C2EN .T;
            I2C1EN   : APB1ENR.I2C1EN .T;
            UART5EN  : APB1ENR.UART5EN.T;
            UART4EN  : APB1ENR.UART4EN.T;
            USART3EN : APB1ENR.USART3EN.T;
            USART2EN : APB1ENR.USART2EN.T;
            SPI3EN   : APB1ENR.SPI3EN .T;
            SPI2EN   : APB1ENR.SPI2EN .T;
            WWDGEN   : APB1ENR.WWDGEN .T;
            TIM14EN  : APB1ENR.TIM14EN.T;
            TIM13EN  : APB1ENR.TIM13EN.T;
            TIM12EN  : APB1ENR.TIM12EN.T;
            TIM7EN   : APB1ENR.TIM7EN .T;
            TIM6EN   : APB1ENR.TIM6EN .T;
            TIM5EN   : APB1ENR.TIM5EN .T;
            TIM4EN   : APB1ENR.TIM4EN .T;
            TIM3EN   : APB1ENR.TIM3EN .T;
            TIM2EN   : APB1ENR.TIM2EN .T;
         end record;

      for T use
         record
            DACEN    at 0 range DACEN  .R'First .. DACEN  .R'Last;
            PWREN    at 0 range PWREN  .R'First .. PWREN  .R'Last;
            CAN2EN   at 0 range CAN2EN .R'First .. CAN2EN .R'Last;
            CAN1EN   at 0 range CAN1EN .R'First .. CAN1EN .R'Last;
            I2C3EN   at 0 range I2C3EN .R'First .. I2C3EN .R'Last;
            I2C2EN   at 0 range I2C2EN .R'First .. I2C2EN .R'Last;
            I2C1EN   at 0 range I2C1EN .R'First .. I2C1EN .R'Last;
            UART5EN  at 0 range UART5EN.R'First .. UART5EN.R'Last;
            UART4EN  at 0 range UART4EN.R'First .. UART4EN.R'Last;
            USART3EN at 0 range USART3EN.R'First .. USART3EN.R'Last;
            USART2EN at 0 range USART2EN.R'First .. USART2EN.R'Last;
            SPI3EN   at 0 range SPI3EN .R'First .. SPI3EN .R'Last;
            SPI2EN   at 0 range SPI2EN .R'First .. SPI2EN .R'Last;
            WWDGEN   at 0 range WWDGEN .R'First .. WWDGEN .R'Last;
            TIM14EN  at 0 range TIM14EN.R'First .. TIM14EN.R'Last;
            TIM13EN  at 0 range TIM13EN.R'First .. TIM13EN.R'Last;
            TIM12EN  at 0 range TIM12EN.R'First .. TIM12EN.R'Last;
            TIM7EN   at 0 range TIM7EN .R'First .. TIM7EN .R'Last;
            TIM6EN   at 0 range TIM6EN .R'First .. TIM6EN .R'Last;
            TIM5EN   at 0 range TIM5EN .R'First .. TIM5EN .R'Last;
            TIM4EN   at 0 range TIM4EN .R'First .. TIM4EN .R'Last;
            TIM3EN   at 0 range TIM3EN .R'First .. TIM3EN .R'Last;
            TIM2EN   at 0 range TIM2EN .R'First .. TIM2EN .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB1ENR;

   subtype APB1ENR_T is APB1ENR.T;

   --  Field definitions

   function DACEN   is new APB1ENR.FN.B (APB1ENR.DACEN  );
   function PWREN   is new APB1ENR.FN.B (APB1ENR.PWREN  );
   function CAN2EN  is new APB1ENR.FN.B (APB1ENR.CAN2EN );
   function CAN1EN  is new APB1ENR.FN.B (APB1ENR.CAN1EN );
   function I2C3EN  is new APB1ENR.FN.B (APB1ENR.I2C3EN );
   function I2C2EN  is new APB1ENR.FN.B (APB1ENR.I2C2EN );
   function I2C1EN  is new APB1ENR.FN.B (APB1ENR.I2C1EN );
   function UART5EN is new APB1ENR.FN.B (APB1ENR.UART5EN);
   function UART4EN is new APB1ENR.FN.B (APB1ENR.UART4EN);
   function USART3EN is new APB1ENR.FN.B (APB1ENR.USART3EN);
   function USART2EN is new APB1ENR.FN.B (APB1ENR.USART2EN);
   function SPI3EN  is new APB1ENR.FN.B (APB1ENR.SPI3EN );
   function SPI2EN  is new APB1ENR.FN.B (APB1ENR.SPI2EN );
   function WWDGEN  is new APB1ENR.FN.B (APB1ENR.WWDGEN );
   function TIM14EN is new APB1ENR.FN.B (APB1ENR.TIM14EN);
   function TIM13EN is new APB1ENR.FN.B (APB1ENR.TIM13EN);
   function TIM12EN is new APB1ENR.FN.B (APB1ENR.TIM12EN);
   function TIM7EN  is new APB1ENR.FN.B (APB1ENR.TIM7EN );
   function TIM6EN  is new APB1ENR.FN.B (APB1ENR.TIM6EN );
   function TIM5EN  is new APB1ENR.FN.B (APB1ENR.TIM5EN );
   function TIM4EN  is new APB1ENR.FN.B (APB1ENR.TIM4EN );
   function TIM3EN  is new APB1ENR.FN.B (APB1ENR.TIM3EN );
   function TIM2EN  is new APB1ENR.FN.B (APB1ENR.TIM2EN );

   --  Functions

   function  "+"   is new APB1ENR.FN.Add;
   function  "+"   is new APB1ENR.FN.Add_RM;
   function  "-"   is new APB1ENR.FN.Clear;
   function  Init  is new APB1ENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.DACEN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.DACEN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.PWREN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.PWREN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.CAN2EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.CAN2EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.CAN1EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.CAN1EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.I2C3EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.I2C3EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.I2C2EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.I2C2EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.I2C1EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.I2C1EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.UART5EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.UART5EN, 2#1#);

   function Clock_Disabled is new APB1ENR.FN.C (APB1ENR.UART4EN, 2#0#);
   function Clock_Enabled  is new APB1ENR.FN.C (APB1ENR.UART4EN, 2#1#);

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

   ---------------------------------------------
   --  APB2 Peripheral Clock Enable Register  --
   ---------------------------------------------

   package APB2ENR is

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
            TIM11EN  : APB2ENR.TIM11EN .T;
            TIM10EN  : APB2ENR.TIM10EN .T;
            TIM9EN   : APB2ENR.TIM9EN  .T;
            SYSCFGEN : APB2ENR.SYSCFGEN.T;
            SPI1EN   : APB2ENR.SPI1EN  .T;
            SDIOEN   : APB2ENR.SDIOEN  .T;
            ADCEN    : APB2ENR.ADCEN   .T;
            USART6EN : APB2ENR.USART6EN.T;
            USART1EN : APB2ENR.USART1EN.T;
            TIM8EN   : APB2ENR.TIM8EN  .T;
            TIM1EN   : APB2ENR.TIM1EN  .T;
         end record;

      for T use
         record
            TIM11EN  at 0 range TIM11EN .R'First .. TIM11EN .R'Last;
            TIM10EN  at 0 range TIM10EN .R'First .. TIM10EN .R'Last;
            TIM9EN   at 0 range TIM9EN  .R'First .. TIM9EN  .R'Last;
            SYSCFGEN at 0 range SYSCFGEN.R'First .. SYSCFGEN.R'Last;
            SPI1EN   at 0 range SPI1EN  .R'First .. SPI1EN  .R'Last;
            SDIOEN   at 0 range SDIOEN  .R'First .. SDIOEN  .R'Last;
            ADCEN    at 0 range ADCEN   .R'First .. ADCEN   .R'Last;
            USART6EN at 0 range USART6EN.R'First .. USART6EN.R'Last;
            USART1EN at 0 range USART1EN.R'First .. USART1EN.R'Last;
            TIM8EN   at 0 range TIM8EN  .R'First .. TIM8EN  .R'Last;
            TIM1EN   at 0 range TIM1EN  .R'First .. TIM1EN  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB2ENR;

   subtype APB2ENR_T is APB2ENR.T;

   --  Field definitions

   function TIM11EN  is new APB2ENR.FN.B (APB2ENR.TIM11EN );
   function TIM10EN  is new APB2ENR.FN.B (APB2ENR.TIM10EN );
   function TIM9EN   is new APB2ENR.FN.B (APB2ENR.TIM9EN  );
   function SYSCFGEN is new APB2ENR.FN.B (APB2ENR.SYSCFGEN);
   function SPI1EN   is new APB2ENR.FN.B (APB2ENR.SPI1EN  );
   function SDIOEN   is new APB2ENR.FN.B (APB2ENR.SDIOEN  );
   function ADCEN    is new APB2ENR.FN.B (APB2ENR.ADCEN   );
   function USART6EN is new APB2ENR.FN.B (APB2ENR.USART6EN);
   function USART1EN is new APB2ENR.FN.B (APB2ENR.USART1EN);
   function TIM8EN   is new APB2ENR.FN.B (APB2ENR.TIM8EN  );
   function TIM1EN   is new APB2ENR.FN.B (APB2ENR.TIM1EN  );

   --  Functions

   function  "+"   is new APB2ENR.FN.Add;
   function  "+"   is new APB2ENR.FN.Add_RM;
   function  "-"   is new APB2ENR.FN.Clear;
   function  Init  is new APB2ENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.TIM11EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.TIM11EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.TIM10EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.TIM10EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.TIM9EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.TIM9EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.SYSCFGEN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.SYSCFGEN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.SPI1EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.SPI1EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.SDIOEN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.SDIOEN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.ADCEN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.ADCEN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.USART6EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.USART6EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.USART1EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.USART1EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.TIM8EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.TIM8EN, 2#1#);

   function Clock_Disabled is new APB2ENR.FN.C (APB2ENR.TIM1EN, 2#0#);
   function Clock_Enabled  is new APB2ENR.FN.C (APB2ENR.TIM1EN, 2#1#);

   ---------------------------------------------------------------
   --  AHB1 Peipheral Clock Enabled In Low Power Mode Register  --
   ---------------------------------------------------------------

   package AHB1LPENR is

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
            OTGHSULPILPEN  : AHB1LPENR .OTGHSULPILPEN . T;
            OTGHSLPEN      : AHB1LPENR .OTGHSLPEN     . T;
            ETHMACPTPLPEN  : AHB1LPENR .ETHMACPTPLPEN . T;
            ETHMACRXLPEN   : AHB1LPENR .ETHMACRXLPEN  . T;
            ETHMACTXLPEN   : AHB1LPENR .ETHMACTXLPEN  . T;
            ETHMACLPEN     : AHB1LPENR .ETHMACLPEN    . T;
            DMA2LPEN       : AHB1LPENR .DMA2LPEN      . T;
            DMA1LPEN       : AHB1LPENR .DMA1LPEN      . T;
            CCMDATARAMLPEN : AHB1LPENR .CCMDATARAMLPEN. T;
            BKPSRAMLPEN    : AHB1LPENR .BKPSRAMLPEN   . T;
            CRCLPEN        : AHB1LPENR .CRCLPEN       . T;
            GPIOILPEN      : AHB1LPENR .GPIOILPEN     . T;
            GPIOHLPEN      : AHB1LPENR .GPIOHLPEN     . T;
            GPIOGLPEN      : AHB1LPENR .GPIOGLPEN     . T;
            GPIOFLPEN      : AHB1LPENR .GPIOFLPEN     . T;
            GPIOELPEN      : AHB1LPENR .GPIOELPEN     . T;
            GPIODLPEN      : AHB1LPENR .GPIODLPEN     . T;
            GPIOCLPEN      : AHB1LPENR .GPIOCLPEN     . T;
            GPIOBLPEN      : AHB1LPENR .GPIOBLPEN     . T;
            GPIOALPEN      : AHB1LPENR .GPIOALPEN     . T;
         end record;

      for T use
         record
            OTGHSULPILPEN  at 0 range OTGHSULPILPEN .R'First .. OTGHSULPILPEN .R'Last;
            OTGHSLPEN      at 0 range OTGHSLPEN     .R'First .. OTGHSLPEN     .R'Last;
            ETHMACPTPLPEN  at 0 range ETHMACPTPLPEN .R'First .. ETHMACPTPLPEN .R'Last;
            ETHMACRXLPEN   at 0 range ETHMACRXLPEN  .R'First .. ETHMACRXLPEN  .R'Last;
            ETHMACTXLPEN   at 0 range ETHMACTXLPEN  .R'First .. ETHMACTXLPEN  .R'Last;
            ETHMACLPEN     at 0 range ETHMACLPEN    .R'First .. ETHMACLPEN    .R'Last;
            DMA2LPEN       at 0 range DMA2LPEN      .R'First .. DMA2LPEN      .R'Last;
            DMA1LPEN       at 0 range DMA1LPEN      .R'First .. DMA1LPEN      .R'Last;
            CCMDATARAMLPEN at 0 range CCMDATARAMLPEN.R'First .. CCMDATARAMLPEN.R'Last;
            BKPSRAMLPEN    at 0 range BKPSRAMLPEN   .R'First .. BKPSRAMLPEN   .R'Last;
            CRCLPEN        at 0 range CRCLPEN       .R'First .. CRCLPEN       .R'Last;
            GPIOILPEN      at 0 range GPIOILPEN     .R'First .. GPIOILPEN     .R'Last;
            GPIOHLPEN      at 0 range GPIOHLPEN     .R'First .. GPIOHLPEN     .R'Last;
            GPIOGLPEN      at 0 range GPIOGLPEN     .R'First .. GPIOGLPEN     .R'Last;
            GPIOFLPEN      at 0 range GPIOFLPEN     .R'First .. GPIOFLPEN     .R'Last;
            GPIOELPEN      at 0 range GPIOELPEN     .R'First .. GPIOELPEN     .R'Last;
            GPIODLPEN      at 0 range GPIODLPEN     .R'First .. GPIODLPEN     .R'Last;
            GPIOCLPEN      at 0 range GPIOCLPEN     .R'First .. GPIOCLPEN     .R'Last;
            GPIOBLPEN      at 0 range GPIOBLPEN     .R'First .. GPIOBLPEN     .R'Last;
            GPIOALPEN      at 0 range GPIOALPEN     .R'First .. GPIOALPEN     .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHB1LPENR;

   subtype AHB1LPENR_T is AHB1LPENR.T;

   --  Field definitions

   function OTGHSULPILPEN  is new AHB1LPENR.FN.B (AHB1LPENR.OTGHSULPILPEN );
   function OTGHSLPEN      is new AHB1LPENR.FN.B (AHB1LPENR.OTGHSLPEN     );
   function ETHMACPTPLPEN  is new AHB1LPENR.FN.B (AHB1LPENR.ETHMACPTPLPEN );
   function ETHMACRXLPEN   is new AHB1LPENR.FN.B (AHB1LPENR.ETHMACRXLPEN  );
   function ETHMACTXLPEN   is new AHB1LPENR.FN.B (AHB1LPENR.ETHMACTXLPEN  );
   function ETHMACLPEN     is new AHB1LPENR.FN.B (AHB1LPENR.ETHMACLPEN    );
   function DMA2LPEN       is new AHB1LPENR.FN.B (AHB1LPENR.DMA2LPEN      );
   function DMA1LPEN       is new AHB1LPENR.FN.B (AHB1LPENR.DMA1LPEN      );
   function CCMDATARAMLPEN is new AHB1LPENR.FN.B (AHB1LPENR.CCMDATARAMLPEN);
   function BKPSRAMLPEN    is new AHB1LPENR.FN.B (AHB1LPENR.BKPSRAMLPEN   );
   function CRCLPEN        is new AHB1LPENR.FN.B (AHB1LPENR.CRCLPEN       );
   function GPIOILPEN      is new AHB1LPENR.FN.B (AHB1LPENR.GPIOILPEN      );
   function GPIOHLPEN      is new AHB1LPENR.FN.B (AHB1LPENR.GPIOHLPEN      );
   function GPIOGLPEN      is new AHB1LPENR.FN.B (AHB1LPENR.GPIOGLPEN      );
   function GPIOFLPEN      is new AHB1LPENR.FN.B (AHB1LPENR.GPIOFLPEN      );
   function GPIOELPEN      is new AHB1LPENR.FN.B (AHB1LPENR.GPIOELPEN      );
   function GPIODLPEN      is new AHB1LPENR.FN.B (AHB1LPENR.GPIODLPEN      );
   function GPIOCLPEN      is new AHB1LPENR.FN.B (AHB1LPENR.GPIOCLPEN      );
   function GPIOBLPEN      is new AHB1LPENR.FN.B (AHB1LPENR.GPIOBLPEN      );
   function GPIOALPEN      is new AHB1LPENR.FN.B (AHB1LPENR.GPIOALPEN      );

   --  Functions

   function  "+"   is new AHB1LPENR.FN.Add;
   function  "+"   is new AHB1LPENR.FN.Add_RM;
   function  "-"   is new AHB1LPENR.FN.Clear;
   function  Init  is new AHB1LPENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.OTGHSULPILPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.OTGHSULPILPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.OTGHSLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.OTGHSLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.ETHMACPTPLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.ETHMACPTPLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.ETHMACRXLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.ETHMACRXLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.ETHMACTXLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.ETHMACTXLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.ETHMACLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.ETHMACLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.DMA2LPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.DMA2LPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.DMA1LPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.DMA1LPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.CCMDATARAMLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.CCMDATARAMLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.BKPSRAMLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.BKPSRAMLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.CRCLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.CRCLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.GPIOILPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.GPIOILPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.GPIOHLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.GPIOHLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.GPIOGLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.GPIOGLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.GPIOFLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.GPIOFLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.GPIOELPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.GPIOELPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.GPIODLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.GPIODLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.GPIOCLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.GPIOCLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.GPIOBLPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.GPIOBLPEN, 2#1#);

   function Clock_Disabled is new AHB1LPENR.FN.C (AHB1LPENR.GPIOALPEN, 2#0#);
   function Clock_Enabled  is new AHB1LPENR.FN.C (AHB1LPENR.GPIOALPEN, 2#1#);


   ----------------------------------------------------------------
   --  AHB2 Peripheral Clock Enabled In Low Power Mode Register  --
   ----------------------------------------------------------------

   package AHB2LPENR is

      package Tp is new Types (R32);

      package OTGFSLPEN is new Bitfield (Tp, 7);
      package RNGLPEN   is new Bitfield (Tp, 6);
      package HASHLPEN  is new Bitfield (Tp, 5);
      package CRYPLPEN  is new Bitfield (Tp, 4);
      package DCMILPEN  is new Bitfield (Tp, 0);

      type T is
         record
            OTGFSLPEN : AHB2LPENR .OTGFSLPEN. T;
            RNGLPEN   : AHB2LPENR .RNGLPEN  . T;
            HASHLPEN  : AHB2LPENR .HASHLPEN . T;
            CRYPLPEN  : AHB2LPENR .CRYPLPEN . T;
            DCMILPEN  : AHB2LPENR .DCMILPEN . T;
         end record;

      for T use
         record
            OTGFSLPEN at 0 range OTGFSLPEN.R'First .. OTGFSLPEN.R'Last;
            RNGLPEN   at 0 range RNGLPEN  .R'First .. RNGLPEN  .R'Last;
            HASHLPEN  at 0 range HASHLPEN .R'First .. HASHLPEN .R'Last;
            CRYPLPEN  at 0 range CRYPLPEN .R'First .. CRYPLPEN .R'Last;
            DCMILPEN  at 0 range DCMILPEN .R'First .. DCMILPEN .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHB2LPENR;

   subtype AHB2LPENR_T is AHB2LPENR.T;

   --  Field definitions

   function OTGFSLPEN is new AHB2LPENR.FN.B (AHB2LPENR.OTGFSLPEN);
   function RNGLPEN   is new AHB2LPENR.FN.B (AHB2LPENR.RNGLPEN  );
   function HASHLPEN  is new AHB2LPENR.FN.B (AHB2LPENR.HASHLPEN );
   function CRYPLPEN  is new AHB2LPENR.FN.B (AHB2LPENR.CRYPLPEN );
   function DCMILPEN  is new AHB2LPENR.FN.B (AHB2LPENR.DCMILPEN );

   --  Functions

   function  "+"   is new AHB2LPENR.FN.Add;
   function  "+"   is new AHB2LPENR.FN.Add_RM;
   function  "-"   is new AHB2LPENR.FN.Clear;
   function  Init  is new AHB2LPENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new AHB2LPENR.FN.C (AHB2LPENR.OTGFSLPEN, 2#0#);
   function Clock_Enabled  is new AHB2LPENR.FN.C (AHB2LPENR.OTGFSLPEN, 2#1#);

   function Clock_Disabled is new AHB2LPENR.FN.C (AHB2LPENR.RNGLPEN, 2#0#);
   function Clock_Enabled  is new AHB2LPENR.FN.C (AHB2LPENR.RNGLPEN, 2#1#);

   function Clock_Disabled is new AHB2LPENR.FN.C (AHB2LPENR.HASHLPEN, 2#0#);
   function Clock_Enabled  is new AHB2LPENR.FN.C (AHB2LPENR.HASHLPEN, 2#1#);

   function Clock_Disabled is new AHB2LPENR.FN.C (AHB2LPENR.CRYPLPEN, 2#0#);
   function Clock_Enabled  is new AHB2LPENR.FN.C (AHB2LPENR.CRYPLPEN, 2#1#);

   function Clock_Disabled is new AHB2LPENR.FN.C (AHB2LPENR.DCMILPEN, 2#0#);
   function Clock_Enabled  is new AHB2LPENR.FN.C (AHB2LPENR.DCMILPEN, 2#1#);

   ----------------------------------------------------------------
   --  AHB3 Peripheral Clock Enabled In Low Power Mode Register  --
   ----------------------------------------------------------------

   package AHB3LPENR is

      package Tp is new Types (R32);

      package FSMCLPEN is new Bitfield (Tp, 0);

      type T is
         record
            FSMCLPEN : AHB3LPENR.FSMCLPEN.T;
         end record;

      for T use
         record
            FSMCLPEN at 0 range FSMCLPEN.R'First .. FSMCLPEN.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end AHB3LPENR;

   subtype AHB3LPENR_T is AHB3LPENR.T;

   --  Field definitions

   function FSMCLPEN is new AHB3LPENR.FN.B (AHB3LPENR.FSMCLPEN);

   --  Functions

   function  "+"   is new AHB3LPENR.FN.Add;
   function  "+"   is new AHB3LPENR.FN.Add_RM;
   function  "-"   is new AHB3LPENR.FN.Clear;
   function  Init  is new AHB3LPENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new AHB3LPENR.FN.C (AHB3LPENR.FSMCLPEN, 2#0#);
   function Clock_Enabled  is new AHB3LPENR.FN.C (AHB3LPENR.FSMCLPEN, 2#1#);

   ----------------------------------------------------------------
   --  APB1 Peripheral Clock Enabled In Low Power Mode Register  --
   ----------------------------------------------------------------

   package APB1LPENR is

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
            DACLPEN    : APB1LPENR.DACLPEN  .T;
            PWRLPEN    : APB1LPENR.PWRLPEN  .T;
            CAN2LPEN   : APB1LPENR.CAN2LPEN .T;
            CAN1LPEN   : APB1LPENR.CAN1LPEN .T;
            I2C3LPEN   : APB1LPENR.I2C3LPEN .T;
            I2C2LPEN   : APB1LPENR.I2C2LPEN .T;
            I2C1LPEN   : APB1LPENR.I2C1LPEN .T;
            UART5LPEN  : APB1LPENR.UART5LPEN.T;
            UART4LPEN  : APB1LPENR.UART4LPEN.T;
            USART3LPEN : APB1LPENR.USART3LPEN.T;
            USART2LPEN : APB1LPENR.USART2LPEN.T;
            SPI3LPEN   : APB1LPENR.SPI3LPEN .T;
            SPI2LPEN   : APB1LPENR.SPI2LPEN .T;
            WWDGLPEN   : APB1LPENR.WWDGLPEN .T;
            TIM14LPEN  : APB1LPENR.TIM14LPEN.T;
            TIM13LPEN  : APB1LPENR.TIM13LPEN.T;
            TIM12LPEN  : APB1LPENR.TIM12LPEN.T;
            TIM7LPEN   : APB1LPENR.TIM7LPEN .T;
            TIM6LPEN   : APB1LPENR.TIM6LPEN .T;
            TIM5LPEN   : APB1LPENR.TIM5LPEN .T;
            TIM4LPEN   : APB1LPENR.TIM4LPEN .T;
            TIM3LPEN   : APB1LPENR.TIM3LPEN .T;
            TIM2LPEN   : APB1LPENR.TIM2LPEN .T;
         end record;

      for T use
         record
            DACLPEN    at 0 range DACLPEN  .R'First .. DACLPEN  .R'Last;
            PWRLPEN    at 0 range PWRLPEN  .R'First .. PWRLPEN  .R'Last;
            CAN2LPEN   at 0 range CAN2LPEN .R'First .. CAN2LPEN .R'Last;
            CAN1LPEN   at 0 range CAN1LPEN .R'First .. CAN1LPEN .R'Last;
            I2C3LPEN   at 0 range I2C3LPEN .R'First .. I2C3LPEN .R'Last;
            I2C2LPEN   at 0 range I2C2LPEN .R'First .. I2C2LPEN .R'Last;
            I2C1LPEN   at 0 range I2C1LPEN .R'First .. I2C1LPEN .R'Last;
            UART5LPEN  at 0 range UART5LPEN.R'First .. UART5LPEN.R'Last;
            UART4LPEN  at 0 range UART4LPEN.R'First .. UART4LPEN.R'Last;
            USART3LPEN at 0 range USART3LPEN.R'First .. USART3LPEN.R'Last;
            USART2LPEN at 0 range USART2LPEN.R'First .. USART2LPEN.R'Last;
            SPI3LPEN   at 0 range SPI3LPEN .R'First .. SPI3LPEN .R'Last;
            SPI2LPEN   at 0 range SPI2LPEN .R'First .. SPI2LPEN .R'Last;
            WWDGLPEN   at 0 range WWDGLPEN .R'First .. WWDGLPEN .R'Last;
            TIM14LPEN  at 0 range TIM14LPEN.R'First .. TIM14LPEN.R'Last;
            TIM13LPEN  at 0 range TIM13LPEN.R'First .. TIM13LPEN.R'Last;
            TIM12LPEN  at 0 range TIM12LPEN.R'First .. TIM12LPEN.R'Last;
            TIM7LPEN   at 0 range TIM7LPEN .R'First .. TIM7LPEN .R'Last;
            TIM6LPEN   at 0 range TIM6LPEN .R'First .. TIM6LPEN .R'Last;
            TIM5LPEN   at 0 range TIM5LPEN .R'First .. TIM5LPEN .R'Last;
            TIM4LPEN   at 0 range TIM4LPEN .R'First .. TIM4LPEN .R'Last;
            TIM3LPEN   at 0 range TIM3LPEN .R'First .. TIM3LPEN .R'Last;
            TIM2LPEN   at 0 range TIM2LPEN .R'First .. TIM2LPEN .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB1LPENR;

   subtype APB1LPENR_T is APB1LPENR.T;

   --  Field definitions

   function DACLPEN    is new APB1LPENR.FN.B (APB1LPENR.DACLPEN  );
   function PWRLPEN    is new APB1LPENR.FN.B (APB1LPENR.PWRLPEN  );
   function CAN2LPEN   is new APB1LPENR.FN.B (APB1LPENR.CAN2LPEN );
   function CAN1LPEN   is new APB1LPENR.FN.B (APB1LPENR.CAN1LPEN );
   function I2C3LPEN   is new APB1LPENR.FN.B (APB1LPENR.I2C3LPEN );
   function I2C2LPEN   is new APB1LPENR.FN.B (APB1LPENR.I2C2LPEN );
   function I2C1LPEN   is new APB1LPENR.FN.B (APB1LPENR.I2C1LPEN );
   function UART5LPEN  is new APB1LPENR.FN.B (APB1LPENR.UART5LPEN);
   function UART4LPEN  is new APB1LPENR.FN.B (APB1LPENR.UART4LPEN);
   function USART3LPEN is new APB1LPENR.FN.B (APB1LPENR.USART3LPEN);
   function USART2LPEN is new APB1LPENR.FN.B (APB1LPENR.USART2LPEN);
   function SPI3LPEN   is new APB1LPENR.FN.B (APB1LPENR.SPI3LPEN );
   function SPI2LPEN   is new APB1LPENR.FN.B (APB1LPENR.SPI2LPEN );
   function WWDGLPEN   is new APB1LPENR.FN.B (APB1LPENR.WWDGLPEN );
   function TIM14LPEN  is new APB1LPENR.FN.B (APB1LPENR.TIM14LPEN);
   function TIM13LPEN  is new APB1LPENR.FN.B (APB1LPENR.TIM13LPEN);
   function TIM12LPEN  is new APB1LPENR.FN.B (APB1LPENR.TIM12LPEN);
   function TIM7LPEN   is new APB1LPENR.FN.B (APB1LPENR.TIM7LPEN );
   function TIM6LPEN   is new APB1LPENR.FN.B (APB1LPENR.TIM6LPEN );
   function TIM5LPEN   is new APB1LPENR.FN.B (APB1LPENR.TIM5LPEN );
   function TIM4LPEN   is new APB1LPENR.FN.B (APB1LPENR.TIM4LPEN );
   function TIM3LPEN   is new APB1LPENR.FN.B (APB1LPENR.TIM3LPEN );
   function TIM2LPEN   is new APB1LPENR.FN.B (APB1LPENR.TIM2LPEN );

   --  Functions

   function  "+"   is new APB1LPENR.FN.Add;
   function  "+"   is new APB1LPENR.FN.Add_RM;
   function  "-"   is new APB1LPENR.FN.Clear;
   function  Init  is new APB1LPENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.DACLPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.DACLPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.PWRLPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.PWRLPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.CAN2LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.CAN2LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.CAN1LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.CAN1LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.I2C3LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.I2C3LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.I2C2LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.I2C2LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.I2C1LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.I2C1LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.UART5LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.UART5LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.UART4LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.UART4LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.USART3LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.USART3LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.USART2LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.USART2LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.SPI3LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.SPI3LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.SPI2LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.SPI2LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.WWDGLPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.WWDGLPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.TIM14LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.TIM14LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.TIM13LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.TIM13LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.TIM12LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.TIM12LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.TIM7LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.TIM7LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.TIM6LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.TIM6LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.TIM5LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.TIM5LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.TIM4LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.TIM4LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.TIM3LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.TIM3LPEN, 2#1#);

   function Clock_Disabled is new APB1LPENR.FN.C (APB1LPENR.TIM2LPEN, 2#0#);
   function Clock_Enabled  is new APB1LPENR.FN.C (APB1LPENR.TIM2LPEN, 2#1#);

   ----------------------------------------------------------------
   --  APB2 Peripheral Clock Enabled In Low Power Mode Register  --
   ----------------------------------------------------------------

   package APB2LPENR is

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
            TIM11LPEN  : APB2LPENR.TIM11LPEN .T;
            TIM10LPEN  : APB2LPENR.TIM10LPEN .T;
            TIM9LPEN   : APB2LPENR.TIM9LPEN  .T;
            SYSCFGLPEN : APB2LPENR.SYSCFGLPEN.T;
            SPI1LPEN   : APB2LPENR.SPI1LPEN  .T;
            SDIOLPEN   : APB2LPENR.SDIOLPEN  .T;
            ADCLPEN    : APB2LPENR.ADCLPEN   .T;
            USART6LPEN : APB2LPENR.USART6LPEN.T;
            USART1LPEN : APB2LPENR.USART1LPEN.T;
            TIM8LPEN   : APB2LPENR.TIM8LPEN  .T;
            TIM1LPEN   : APB2LPENR.TIM1LPEN  .T;
         end record;

      for T use
         record
            TIM11LPEN  at 0 range TIM11LPEN .R'First .. TIM11LPEN .R'Last;
            TIM10LPEN  at 0 range TIM10LPEN .R'First .. TIM10LPEN .R'Last;
            TIM9LPEN   at 0 range TIM9LPEN  .R'First .. TIM9LPEN  .R'Last;
            SYSCFGLPEN at 0 range SYSCFGLPEN.R'First .. SYSCFGLPEN.R'Last;
            SPI1LPEN   at 0 range SPI1LPEN  .R'First .. SPI1LPEN  .R'Last;
            SDIOLPEN   at 0 range SDIOLPEN  .R'First .. SDIOLPEN  .R'Last;
            ADCLPEN    at 0 range ADCLPEN   .R'First .. ADCLPEN   .R'Last;
            USART6LPEN at 0 range USART6LPEN.R'First .. USART6LPEN.R'Last;
            USART1LPEN at 0 range USART1LPEN.R'First .. USART1LPEN.R'Last;
            TIM8LPEN   at 0 range TIM8LPEN  .R'First .. TIM8LPEN  .R'Last;
            TIM1LPEN   at 0 range TIM1LPEN  .R'First .. TIM1LPEN  .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end APB2LPENR;

   subtype APB2LPENR_T is APB2LPENR.T;

   --  Field definitions

   function TIM11LPEN  is new APB2LPENR.FN.B (APB2LPENR.TIM11LPEN );
   function TIM10LPEN  is new APB2LPENR.FN.B (APB2LPENR.TIM10LPEN );
   function TIM9LPEN   is new APB2LPENR.FN.B (APB2LPENR.TIM9LPEN  );
   function SYSCFGLPEN is new APB2LPENR.FN.B (APB2LPENR.SYSCFGLPEN);
   function SPI1LPEN   is new APB2LPENR.FN.B (APB2LPENR.SPI1LPEN  );
   function SDIOLPEN   is new APB2LPENR.FN.B (APB2LPENR.SDIOLPEN  );
   function ADCLPEN    is new APB2LPENR.FN.B (APB2LPENR.ADCLPEN   );
   function USART6LPEN is new APB2LPENR.FN.B (APB2LPENR.USART6LPEN);
   function USART1LPEN is new APB2LPENR.FN.B (APB2LPENR.USART1LPEN);
   function TIM8LPEN   is new APB2LPENR.FN.B (APB2LPENR.TIM8LPEN  );
   function TIM1LPEN   is new APB2LPENR.FN.B (APB2LPENR.TIM1LPEN  );

   --  Functions

   function  "+"   is new APB2LPENR.FN.Add;
   function  "+"   is new APB2LPENR.FN.Add_RM;
   function  "-"   is new APB2LPENR.FN.Clear;
   function  Init  is new APB2LPENR.FN.Init;

   --  Constant definitions

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.TIM11LPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.TIM11LPEN, 2#1#);

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.TIM10LPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.TIM10LPEN, 2#1#);

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.TIM9LPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.TIM9LPEN, 2#1#);

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.SYSCFGLPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.SYSCFGLPEN, 2#1#);

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.SPI1LPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.SPI1LPEN, 2#1#);

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.SDIOLPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.SDIOLPEN, 2#1#);

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.ADCLPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.ADCLPEN, 2#1#);

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.USART6LPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.USART6LPEN, 2#1#);

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.USART1LPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.USART1LPEN, 2#1#);

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.TIM8LPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.TIM8LPEN, 2#1#);

   function Clock_Disabled is new APB2LPENR.FN.C (APB2LPENR.TIM1LPEN, 2#0#);
   function Clock_Enabled  is new APB2LPENR.FN.C (APB2LPENR.TIM1LPEN, 2#1#);

   --------------------------------------
   --  Backup Domain Control Register  --
   --------------------------------------

   package BDCR is

      package Tp is new Types (R32);

      package BDRST  is new Bitfield (Tp, 16);
      package RTCEN  is new Bitfield (Tp, 15);
      package RTCSEL is new Bitfield (Tp, 8, 2);
      package LSEBYP is new Bitfield (Tp, 2);
      package LSERDY is new Bitfield (Tp, 1);
      package LSEON  is new Bitfield (Tp, 0);

      type T is
         record
            BDRST  : BDCR.BDRST .T;
            RTCEN  : BDCR.RTCEN .T;
            RTCSEL : BDCR.RTCSEL.T;
            LSEBYP : BDCR.LSEBYP.T;
            LSERDY : BDCR.LSERDY.T;
            LSEON  : BDCR.LSEON .T;
         end record;

      for T use
         record
            BDRST  at 0 range BDRST .R'First .. BDRST .R'Last;
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

   function BDRST  is new BDCR.FN.B (BDCR.BDRST );
   function RTCEN  is new BDCR.FN.B (BDCR.RTCEN );
   function RTCSEL is new BDCR.FN.B (BDCR.RTCSEL);
   function LSEBYP is new BDCR.FN.B (BDCR.LSEBYP);
   function LSERDY is new BDCR.FN.B (BDCR.LSERDY);
   function LSEON  is new BDCR.FN.B (BDCR.LSEON );

   --  Functions

   function  "+"   is new BDCR.FN.Add;
   function  "+"   is new BDCR.FN.Add_RM;
   function  "-"   is new BDCR.FN.Clear;
   function  Init  is new BDCR.FN.Init;

   --  Constant definitions

   function Reset_Not_Activated         is new BDCR.FN.C (BDCR.BDRST, 2#0#);
   function Resets_Entire_Backup_Domain is new BDCR.FN.C (BDCR.BDRST, 2#1#);

   function Clock_Disabled is new BDCR.FN.C (BDCR.RTCEN, 2#0#);
   function Clock_Enabled  is new BDCR.FN.C (BDCR.RTCEN, 2#1#);

   function No_Clock       is new BDCR.FN.C (BDCR.RTCSEL, 2#00#);
   function LSE_Oscillator is new BDCR.FN.C (BDCR.RTCSEL, 2#01#);
   function LSI_Oscillator is new BDCR.FN.C (BDCR.RTCSEL, 2#10#);
   function HSE_Oscillator is new BDCR.FN.C (BDCR.RTCSEL, 2#11#);

   function Oscillator_Not_Bypassed is new BDCR.FN.C (BDCR.LSEBYP, 2#0#);
   function Oscillator_Bypassed     is new BDCR.FN.C (BDCR.LSEBYP, 2#1#);

   function Clock_Not_Ready is new BDCR.FN.C (BDCR.LSERDY, 2#0#);
   function Clock_Ready     is new BDCR.FN.C (BDCR.LSERDY, 2#1#);

   function Clock_Off is new BDCR.FN.C (BDCR.LSEON, 2#0#);
   function Clock_On  is new BDCR.FN.C (BDCR.LSEON, 2#1#);

   ---------------------------------------
   --  Clock Control & Status Register  --
   ---------------------------------------

   package CSR is

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
            LPWRRSTF : CSR.LPWRRSTF.T;
            WWDGRSTF : CSR.WWDGRSTF.T;
            IWDGRSTF : CSR.IWDGRSTF.T;
            SFTRSTF  : CSR.SFTRSTF .T;
            PORRSTF  : CSR.PORRSTF .T;
            PINRSTF  : CSR.PINRSTF .T;
            BORRSTF  : CSR.BORRSTF .T;
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
            BORRSTF  at 0 range BORRSTF .R'First .. BORRSTF .R'Last;
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
   function SFTRSTF  is new CSR.FN.B (CSR.SFTRSTF );
   function PORRSTF  is new CSR.FN.B (CSR.PORRSTF );
   function PINRSTF  is new CSR.FN.B (CSR.PINRSTF );
   function BORRSTF  is new CSR.FN.B (CSR.BORRSTF );
   function RMVF     is new CSR.FN.B (CSR.RMVF    );
   function LSIRDY   is new CSR.FN.B (CSR.LSIRDY  );
   function LSION    is new CSR.FN.B (CSR.LSION   );

   --  Functions

   function  "+"   is new CSR.FN.Add;
   function  "+"   is new CSR.FN.Add_RM;
   function  "-"   is new CSR.FN.Clear;
   function  Init  is new CSR.FN.Init;

   --  Constant definitions

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

   function No_POR_PDR_Or_BOR_Reset_Ocurred is new CSR.FN.C (CSR.BORRSTF, 2#0#);
   function POR_PDR_Or_BOR_Reset_Ocurred    is new CSR.FN.C (CSR.BORRSTF, 2#1#);

   function Clear_Flag is new CSR.FN.C (CSR.RMVF, 2#1#);

   function Oscillator_Not_Ready is new CSR.FN.C (CSR.LSIRDY, 2#0#);
   function Oscillator_Ready     is new CSR.FN.C (CSR.LSIRDY, 2#1#);

   function Oscillator_Off is new CSR.FN.C (CSR.LSION, 2#0#);
   function Oscillator_On  is new CSR.FN.C (CSR.LSION, 2#1#);

   -------------------------------------------------
   --  Spread Spectrum Clock Generation Register  --
   -------------------------------------------------

   package SSCGR is

      package Tp is new Types (R32);

      package SSCGEN    is new Bitfield (Tp, 31);
      package SPREADSEL is new Bitfield (Tp, 30);
      package INCSTEP   is new Bitfield (Tp, 13, 15);
      package MODPER    is new Bitfield (Tp, 0, 13);

      type T is
         record
            SSCGEN    : SSCGR.SSCGEN   .T;
            SPREADSEL : SSCGR.SPREADSEL.T;
            INCSTEP   : SSCGR.INCSTEP  .T;
            MODPER    : SSCGR.MODPER   .T;
         end record;

      for T use
         record
            SSCGEN    at 0 range SSCGEN   .R'First .. SSCGEN   .R'Last;
            SPREADSEL at 0 range SPREADSEL.R'First .. SPREADSEL.R'Last;
            INCSTEP   at 0 range INCSTEP  .R'First .. INCSTEP  .R'Last;
            MODPER    at 0 range MODPER   .R'First .. MODPER   .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end SSCGR;

   subtype SSCGR_T is SSCGR.T;

   --  Field definitions

   function SSCGEN    is new SSCGR.FN.B (SSCGR.SSCGEN   );
   function SPREADSEL is new SSCGR.FN.B (SSCGR.SPREADSEL);
   function INCSTEP   is new SSCGR.FN.B (SSCGR.INCSTEP  );
   function MODPER    is new SSCGR.FN.B (SSCGR.MODPER   );

   --  Functions

   function  "+"   is new SSCGR.FN.Add;
   function  "+"   is new SSCGR.FN.Add_RM;
   function  "-"   is new SSCGR.FN.Clear;
   function  Init  is new SSCGR.FN.Init;

   --  Constant definitions

   function SSM_Disable is new SSCGR.FN.C (SSCGR.SSCGEN, 2#0#);
   function SSM_Enable  is new SSCGR.FN.C (SSCGR.SSCGEN, 2#1#);

   function Center_Spread is new SSCGR.FN.C (SSCGR.SPREADSEL, 2#0#);
   function Down_Spread   is new SSCGR.FN.C (SSCGR.SPREADSEL, 2#1#);

   --------------------------------------
   --  PLL I2S Configuration Register  --
   --------------------------------------

   package PLLI2SCFGR is

      package Tp is new Types (R32);

      package PLLI2SR is new Bitfield (Tp, 28, 3);
      package PLLI2SN is new Bitfield (Tp, 6, 9);

      type T is
         record
            PLLI2SR : PLLI2SCFGR.PLLI2SR.T;
            PLLI2SN : PLLI2SCFGR.PLLI2SN.T;
         end record;

      for T use
         record
            PLLI2SR at 0 range PLLI2SR.R'First .. PLLI2SR.R'Last;
            PLLI2SN at 0 range PLLI2SN.R'First .. PLLI2SN.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PLLI2SCFGR;

   subtype PLLI2SCFGR_T is PLLI2SCFGR.T;

   --  Field definitions

   function PLLI2SR is new PLLI2SCFGR.FN.B (PLLI2SCFGR.PLLI2SR);
   function PLLI2SN is new PLLI2SCFGR.FN.B (PLLI2SCFGR.PLLI2SN);

   --  Functions

   function  "+"   is new PLLI2SCFGR.FN.Add;
   function  "+"   is new PLLI2SCFGR.FN.Add_RM;
   function  "-"   is new PLLI2SCFGR.FN.Clear;
   function  Init  is new PLLI2SCFGR.FN.Init;

   --------------------------------------------------------------------------
   --                         Register definition                          --
   --------------------------------------------------------------------------

   type RCC_T is
      record
         CR         : CR_T;
         PLLCFGR    : PLLCFGR_T;
         CFGR       : CFGR_T;
         CIR        : CIR_T;
         AHB1RSTR   : AHB1RSTR_T;
         AHB2RSTR   : AHB2RSTR_T;
         AHB3RSTR   : AHB3RSTR_T;
         APB1RSTR   : APB1RSTR_T;
         APB2RSTR   : APB2RSTR_T;
         AHB1ENR    : AHB1ENR_T;
         AHB2ENR    : AHB2ENR_T;
         AHB3ENR    : AHB3ENR_T;
         APB1ENR    : APB1ENR_T;
         APB2ENR    : APB2ENR_T;
         AHB1LPENR  : AHB1LPENR_T;
         AHB2LPENR  : AHB2LPENR_T;
         AHB3LPENR  : AHB3LPENR_T;
         APB1LPENR  : APB1LPENR_T;
         APB2LPENR  : APB2LPENR_T;
         BDCR       : BDCR_T;
         CSR        : CSR_T;
         SSCGR      : SSCGR_T;
         PLLI2SCFGR : PLLI2SCFGR_T;
         pragma Volatile (CR);
         pragma Volatile (PLLCFGR);
         pragma Volatile (CFGR);
         pragma Volatile (CIR);
         pragma Volatile (AHB1RSTR);
         pragma Volatile (AHB2RSTR);
         pragma Volatile (AHB3RSTR);
         pragma Volatile (APB1RSTR);
         pragma Volatile (APB2RSTR);
         pragma Volatile (AHB1ENR);
         pragma Volatile (AHB2ENR);
         pragma Volatile (AHB3ENR);
         pragma Volatile (APB1ENR);
         pragma Volatile (APB2ENR);
         pragma Volatile (AHB1LPENR);
         pragma Volatile (AHB2LPENR);
         pragma Volatile (AHB3LPENR);
         pragma Volatile (APB1LPENR);
         pragma Volatile (APB2LPENR);
         pragma Volatile (BDCR);
         pragma Volatile (CSR);
         pragma Volatile (SSCGR);
         pragma Volatile (PLLI2SCFGR);
      end record;

   for RCC_T use
      record
         CR         at 16#00# range 0 .. 31;
         PLLCFGR    at 16#04# range 0 .. 31;
         CFGR       at 16#08# range 0 .. 31;
         CIR        at 16#0C# range 0 .. 31;
         AHB1RSTR   at 16#10# range 0 .. 31;
         AHB2RSTR   at 16#14# range 0 .. 31;
         AHB3RSTR   at 16#18# range 0 .. 31;
         APB1RSTR   at 16#20# range 0 .. 31;
         APB2RSTR   at 16#24# range 0 .. 31;
         AHB1ENR    at 16#30# range 0 .. 31;
         AHB2ENR    at 16#34# range 0 .. 31;
         AHB3ENR    at 16#38# range 0 .. 31;
         APB1ENR    at 16#40# range 0 .. 31;
         APB2ENR    at 16#44# range 0 .. 31;
         AHB1LPENR  at 16#50# range 0 .. 31;
         AHB2LPENR  at 16#54# range 0 .. 31;
         AHB3LPENR  at 16#58# range 0 .. 31;
         APB1LPENR  at 16#60# range 0 .. 31;
         APB2LPENR  at 16#64# range 0 .. 31;
         BDCR       at 16#70# range 0 .. 31;
         CSR        at 16#74# range 0 .. 31;
         SSCGR      at 16#80# range 0 .. 31;
         PLLI2SCFGR at 16#84# range 0 .. 31;
      end record;

   RCC : RCC_T;

   for RCC'Address use System'To_Address (16#4002_3800#);

end ARM.Registers.RCC_F41XXX;
