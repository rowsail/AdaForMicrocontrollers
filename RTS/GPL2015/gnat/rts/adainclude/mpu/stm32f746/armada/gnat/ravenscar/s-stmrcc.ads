------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S T M 3 2 F 4 . R C C                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2015, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the definitions of STM32's Reset and Clock Control
--  (RCC) registers.

pragma Restrictions (No_Elaboration_Code);

package System.STM32F4.RCC is
   pragma Preelaborate (System.STM32F4.RCC);

   subtype Frequency is Word;

   type RCC_System_Clocks is record
      SYSCLK  : Frequency;
      HCLK    : Frequency;
      PCLK1   : Frequency;
      PCLK2   : Frequency;
      TIMCLK1 : Frequency;
      TIMCLK2 : Frequency;
   end record;

   function System_Clocks return RCC_System_Clocks;

   ---------------------------------
   -- RCC Reset and Clock Control --
   ---------------------------------

   type RCC_Registers is record
      CR          : Word;  --  RCC clock control register at 16#00#
      PLLCFGR     : Word;  --  RCC PLL configuration register at 16#04#
      CFGR        : Word;  --  RCC clock configuration register at 16#08#
      CIR         : Word;  --  RCC clock interrupt register at 16#0C#
      AHB1RSTR    : Word;  --  RCC AHB1 peripheral reset register at 16#10#
      AHB2RSTR    : Word;  --  RCC AHB2 peripheral reset register at 16#14#
      AHB3RSTR    : Word;  --  RCC AHB3 peripheral reset register at 16#18#
      Reserved_0  : Word;  --  Reserved at 16#1C#
      APB1RSTR    : Word;  --  RCC APB1 peripheral reset register at 16#20#
      APB2RSTR    : Word;  --  RCC APB2 peripheral reset register at 16#24#
      Reserved_1  : Word;  --  Reserved at 16#28#
      Reserved_2  : Word;  --  Reserved at 16#2c#
      AHB1ENR     : Word;  --  RCC AHB1 peripheral clock register at 16#30#
      AHB2ENR     : Word;  --  RCC AHB2 peripheral clock register at 16#34#
      AHB3ENR     : Word;  --  RCC AHB3 peripheral clock register at 16#38#
      Reserved_3  : Word;  --  Reserved at 16#0C#
      APB1ENR     : Word;  --  RCC APB1 peripheral clock enable at 16#40#
      APB2ENR     : Word;  --  RCC APB2 peripheral clock enable at 16#44#
      Reserved_4  : Word;  --  Reserved at 16#48#
      Reserved_5  : Word;  --  Reserved at 16#4c#
      AHB1LPENR   : Word;  --  RCC AHB1 periph. low power clk en. at 16#50#
      AHB2LPENR   : Word;  --  RCC AHB2 periph. low power clk en. at 16#54#
      AHB3LPENR   : Word;  --  RCC AHB3 periph. low power clk en. at 16#58#
      Reserved_6  : Word;  --  Reserved, 16#5C#
      APB1LPENR   : Word;  --  RCC APB1 periph. low power clk en. at 16#60#
      APB2LPENR   : Word;  --  RCC APB2 periph. low power clk en. at 16#64#
      Reserved_7  : Word;  --  Reserved at 16#68#
      Reserved_8  : Word;  --  Reserved at 16#6C#
      BDCR        : Word;  --  RCC Backup domain control register at 16#70#
      CSR         : Word;  --  RCC clock control/status register at 16#74#
      Reserved_9  : Word;  --  Reserved at 16#78#
      Reserved_10 : Word;  --  Reserved at 16#7C#
      SSCGR       : Word;  --  RCC spread spectrum clk gen. reg. at 16#80#
      PLLI2SCFGR  : Word;  --  RCC PLLI2S configuration register at 16#84#
      PLLSAICFGR  : Word;  --  PLLSAI configuration register at 16#88#
      DCKCFGR     : Word;  --  DCK configuration register at 16#8C#
   end record;

   Registers : RCC_Registers
     with Volatile, Address => System'To_Address (RCC_Base);

   --  Constants for RCC CR register

   package CR is
      HSION      : constant Word := 2**0;  -- Internal high-speed clock enable
      HSIRDY     : constant Word := 2**1;  -- Internal high-speed clock ready
      HSEON      : constant Word := 2**16; -- External high-speed clock enable
      HSERDY     : constant Word := 2**17; -- External high-speed clock ready
      HSEBYP     : constant Word := 2**18; -- External HS clk. resonator bypass
      CSSON      : constant Word := 2**19; -- Clock security system enable
      PLLON      : constant Word := 2**24; -- Main PLL enable
      PLLRDY     : constant Word := 2**25; -- Main PLL ready
      PLLI2SON   : constant Word := 2**26; -- Main PLL enable
      PLLI2SRDY  : constant Word := 2**27; -- Main PLL ready
   end CR;

   subtype PLLM_Range is Integer range 2 .. 63;
   subtype PLLN_Range is Integer range 192 .. 432;
   subtype PLLP_Range is Integer range 2 .. 8
     with Static_Predicate => (case PLLP_Range is
                                 when 2 | 4 | 6 | 8 => True,
                                 when others => False);
   subtype PLLQ_Range is Integer range 2 .. 15;

   subtype HSECLK_Range is Integer range   1_000_000 ..  26_000_000;
   subtype PLLIN_Range  is Integer range     950_000 ..   2_000_000;
   subtype PLLVC0_Range is Integer range 192_000_000 .. 432_000_000;
   subtype PLLOUT_Range is Integer range  24_000_000 .. 200_000_000;
   subtype SYSCLK_Range is Integer range           1 .. 200_000_000;
   subtype HCLK_Range   is Integer range           1 .. 200_000_000;
   subtype PCLK1_Range  is Integer range           1 ..  50_000_000;
   subtype PCLK2_Range  is Integer range           1 .. 100_000_000;
   subtype SPII2S_Range is Integer range           1 ..  37_500_000;
   pragma Unreferenced (SPII2S_Range);

   --  These internal low and high speed clocks are fixed (do not modify)

   HSICLK : constant := 16_000_000;
   LSICLK : constant :=     32_000;

   PLLSRC_HSE      : constant := 2**22; -- PLL source clock is HSE

   package CFGR is

      --  Constants for RCC CFGR register

      --  AHB prescaler

      type AHB_PRE_Value is (AHBPRE_DIV1, AHBPRE_DIV2, AHBPRE_DIV4,
                             AHBPRE_DIV8, AHBPRE_DIV16, AHBPRE_DIV64,
                             AHBPRE_DIV128, AHBPRE_DIV256, AHBPRE_DIV512);
      for AHB_PRE_Value use
        (AHBPRE_DIV1   => 16#00#,
         AHBPRE_DIV2   => 16#80#,
         AHBPRE_DIV4   => 16#90#,
         AHBPRE_DIV8   => 16#A0#,
         AHBPRE_DIV16  => 16#B0#,
         AHBPRE_DIV64  => 16#C0#,
         AHBPRE_DIV128 => 16#D0#,
         AHBPRE_DIV256 => 16#E0#,
         AHBPRE_DIV512 => 16#F0#);

      --  APB1 prescaler

      type APB1_PRE_Value is (APB1PRE_DIV1, APB1PRE_DIV2, APB1PRE_DIV4,
                              APB1PRE_DIV8, APB1PRE_DIV16);
      for APB1_PRE_Value use
        (APB1PRE_DIV1  => 16#0000#,
         APB1PRE_DIV2  => 16#1000#,
         APB1PRE_DIV4  => 16#1400#,
         APB1PRE_DIV8  => 16#1800#,
         APB1PRE_DIV16 => 16#1C00#);

      --  APB2 prescaler

      type APB2_PRE_Value is (APB2PRE_DIV1, APB2PRE_DIV2, APB2PRE_DIV4,
                              APB2PRE_DIV8, APB2PRE_DIV16);
      for APB2_PRE_Value use
        (APB2PRE_DIV1  => 16#0000#,
         APB2PRE_DIV2  => 16#8000#,
         APB2PRE_DIV4  => 16#A000#,
         APB2PRE_DIV8  => 16#C000#,
         APB2PRE_DIV16 => 16#E000#);

      --  MCO1 clock selector

      MCO1SEL_HSI    : constant Word := 0 * 2**21; -- HSI clock on MC01 pin
      MCO1SEL_LSE    : constant Word := 1 * 2**21; -- LSE clock on MC01 pin
      MCO1SEL_HSE    : constant Word := 2 * 2**21; -- HSE clock on MC01 pin
      MCO1SEL_PLL    : constant Word := 3 * 2**21; -- PLL clock on MC01 pin

      --  MCO1 prescaler

      MCO1PRE_DIV1   : constant Word := 0 * 2**24; -- MC01 divides by 1
      MCO1PRE_DIV2   : constant Word := 4 * 2**24; -- MC01 divides by 2
      MCO1PRE_DIV3   : constant Word := 5 * 2**24; -- MC01 divides by 3
      MCO1PRE_DIV4   : constant Word := 6 * 2**24; -- MC01 divides by 4
      MCO1PRE_DIV5   : constant Word := 7 * 2**24; -- MC01 divides by 5

      --  MCO2 clock selector

      MCO2SEL_SYSCLK : constant Word := 0 * 2**30; -- SYSCLK clock on MCO2 pin
      MCO2SEL_PLLI2S : constant Word := 1 * 2**30; -- SYSCLK clock on MCO2 pin
      MCO2SEL_HSE    : constant Word := 2 * 2**30; -- SYSCLK clock on MCO2 pin
      MCO2SEL_PLL    : constant Word := 3 * 2**30; -- SYSCLK clock on MCO2 pin

      --  MCO2 prescaler

      MCO2PRE_DIV1   : constant Word := 0 * 2**27; -- MCO2 divides by 1
      MCO2PRE_DIV2   : constant Word := 4 * 2**27; -- MCO2 divides by 4
      MCO2PRE_DIV3   : constant Word := 5 * 2**27; -- MCO2 divides by 5
      MCO2PRE_DIV4   : constant Word := 6 * 2**27; -- MCO2 divides by 6
      MCO2PRE_DIV5   : constant Word := 7 * 2**27; -- MCO2 divides by 7

      --  I2S clock source

      I2SSRC_PLLI2S : constant Word := 0 * 2**23; -- I2SSRC is PLLI2S
      I2SSRC_PCKIN  : constant Word := 1 * 2**23; -- I2SSRC is I2S_CKIN

      --  System clock switch

      SW_HSI        : constant Word := 16#0#; -- HSI selected as system clock
      SW_HSE        : constant Word := 16#1#; -- HSI selected as system clock
      SW_PLL        : constant Word := 16#2#; -- PLL selected as system clock

      --  System clock switch status

      SWS_HSI       : constant Word := 16#0#; -- HSI used as system clock
      SWS_HSE       : constant Word := 16#4#; -- HSI used as system clock
      SWS_PLL       : constant Word := 16#8#; -- PLL used as system clock

   end CFGR;

   --  Constants for RCC CR register

   package CSR is
      LSION     : constant Word := 2**0; -- Int. low-speed clock enable
      LSIRDY    : constant Word := 2**1; -- Int. low-speed clock enable
   end CSR;

   RCC_APB1ENR_PWR    : constant Word := 16#1000_0000#;
   --  Bit definition for RCC APB1ENR register

   RCC_APB2ENR_USART1 : constant Word := 16#10#;
   --  Bit definition for RCC APB2ENR register

   RCC_AHB1ENR_GPIOB  : constant Word := 16#02#;
   --  Bit definition for RCC AHB1ENR register

end System.STM32F4.RCC;
