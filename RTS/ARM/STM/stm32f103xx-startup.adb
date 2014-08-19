--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                  S T M 3 2 F 1 0 3 X X . S t a r t u p                     --
--                                  B o d y                                   --
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

------------------------
--  Imported Packages --
------------------------

with STM32.Registers;
use  STM32.Registers;

with STM32.Registers.RCC_F103VC;
use  STM32.Registers.RCC_F103VC;
--  RCC subsystem

with STM32.Registers.FLASH_F103VC;
use  STM32.Registers.FLASH_F103VC;
--  FLASH subsystem

package body STM32F103XX.Startup is

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize is

      use type CR.HSERDY.T;
      use type CR.PLLRDY.T;
      use type CFGR.SWS.T;

   begin
      RCC.CR.HSION := Set;
      --  Turn ON internal oscillator

      RCC.CFGR := RCC.CFGR - HPRE - PPRE1 - PPRE2 - ADCPRE - MCO;
      --  SYSCLK not divided, HCLK for APB1 & APB2 not divided
      --  ADC clock divided by 2, no clock output

      RCC.CR := RCC.CR - HSEON - CSSON - PLLON;
      --  Turn OFF external oscillator, clock protection, PLL

      RCC.CR.HSEBYP := Oscillator_Not_Bypassed;
      --  External oscillator bypassed by with external clock

      RCC.CIR := LSIRDYC + LSERDYC + HSIRDYC + HSERDYC + PLLRDYC + CSSC;
      --  Disable & clear any pending interrupts

      RCC.CFGR2 := Init (0);
      --  Reset CFGR2 register

      RCC.CR.HSEON := Set;
      --  Turn on an external oscillator

      while RCC.CR.HSERDY = Oscillator_Not_Ready loop null; end loop;
      --  Wait for external oscillator to become ready

      FLASH.ACR.PRFTBE := Set;
      --  Enable flash pre-fetch buffer

      case Desired_Frequency is
         when 0 .. 23999999        => FLASH.ACR.LATENCY := Latency_2;
         when 24000000 .. 47999999 => FLASH.ACR.LATENCY := Latency_2;
         when others               => FLASH.ACR.LATENCY := Latency_2;
      end case;
      --  Set flash latency to expected clock speed

      RCC.CFGR := RCC.CFGR - PLLSRC - PLLXTPRE - PLLMUL;
      --  Clear PLL configuration bits
      --
      RCC.CFGR := RCC.CFGR + MCO (HSE) + PLLMUL (PLLx9);
      --  Set to external oscillator & mul x 9
      --
      RCC.CFGR.HPRE := Div1;
      --  Set divider for main clock
      --
      RCC.CFGR.PPRE2 := Div1;
      --  Set divider for APB2 BUS
      --
      RCC.CFGR.PPRE1 := Div2;
      --  Set divider for APB1 BUS
      --
      RCC.CR.PLLON := Set;
      --  Turn on PLL
      --
      while RCC.CR.PLLRDY = Clear loop null; end loop;
      --  Wait for PLL to become ready
      --
      RCC.CFGR.SW := PLL;
      --  Make PLL system clock
      --
      while RCC.CFGR.SWS /= PLL loop null; end loop;
      --  Wait for PLL become active

      --        Init_PLL (Clock_Frequency);
      --  Initialize main PLL

      --        System_Clock_Temp := System_Clock;
      --  Save before memory initialization

   end Initialize;

end STM32F103XX.Startup;
