--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--               A R M . S t a r t u p . C l o c k _ F 4 0 X X X              --
--                                B o d y                                     --
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

with ARM.Driver.Clock_F40XXX;
with ARM.Driver.Power_F40XXX;
with ARM.Driver.Flash_F40XXX;

--------------------------------------------------------------------------------
--                           ARM.Startup.Clock_F40XXX                         --
--------------------------------------------------------------------------------

package body ARM.Startup.Clock_F40XXX is

   pragma Warnings (Off);  --  Warning about Constraint may call Last_Chance_Handler

   --  Use drivers & correspondent register files
   package CLOCK renames ARM.Driver.Clock_F40XXX; use CLOCK; use CLOCK.RCC;
   package POWER renames ARM.Driver.Power_F40XXX; use POWER; use POWER.PWR;
   package FLASH renames ARM.Driver.Flash_F40XXX; use FLASH; use FLASH.FLASH;

   procedure Reset is
   begin
      CLOCK.RCC.CR.Reset_Register;
      CLOCK.RCC.CFGR.Reset_Register;
      CLOCK.RCC.PLLCFGR.Reset_Register;
      CLOCK.RCC.CIR.Reset_Register;
   end Reset;

   procedure Init_Low_Speed_Clock is
   begin
      Turn_LSI_On;
      Wait_For_LSI;
   end Init_Low_Speed_Clock;

   --  Procedure to initializing Clock
   --  If HSE_Clock = No_Clock then we must use HSI internal oscillator
   --  If AHB_Clock is higher then HSE_Clock or HSI then we must use PLL
   --  If AHB_Clock or HSI is lower then AHB then we must use prescaler

   use type RCC_CFGR.SWS.T;

   procedure Init_High_Speed_Clock is

      use type RCC_CFGR.SW.T;

      Active_Clock : RCC_CFGR.SW.T;
      Used_Clock   : Frequency;
      PLL_Clock    : Frequency;

   begin
      --  Reset Power interface
      Set_APB1ENR (CLOCK.RCC.PWREN);

      --  Set proper voltage for speed
      if AHB_Clock > 144.000 then
         Higher_Voltage;
      else
         Lower_Voltage;
      end if;

      Wait_For_VOS;

      --  check for HSI because we are maybe in restart and HSI could be turned off?
      --  so always turn HSI on, but sometimes need not wait for it
      if not Is_HSI_On then

         Turn_HSI_On;
         Wait_For_HSI;

      end if;

      --  Check if we want HSE clock
      if HSE_Clock /= No_Clock then

         --  We want HSE clock, so we must turn on HSE
         Turn_HSE_On;

         --  If we want to use external source -> enable it
         if Ext_Clock then
            Bypass_HSE;
         end if;

         Wait_For_HSE;

         --  Store HSE clock value
         HSE_Clk := HSE_Clock;

         --  HSE is working now, so use its frequency
         Used_Clock := HSE_Clk;

         --  And possible our system clock will be HSE clock
         Active_Clock := CLOCK.RCC.HSE_Oscillator_Selected;

      else
         --  We don't want HSE clock, so use already running HSI clock
         Used_Clock := Frequency (HSI_Clk);

         --  And possible our system clock will be HSI clock
         Active_Clock := CLOCK.RCC.HSI_Oscillator_Selected;

      end if;

      --  Check if wanted clock isn't exactly used clock
      --  If it is then use choosen before HSE or HSI as our system clock
      if Frequency (AHB_Clock) /= Used_Clock then

         --  If not exactly the same try to decrease or increase choosen clock
         --  Check if wanted clock is lower than HSI
         if Frequency (AHB_Clock) < Used_Clock then

            --  Must divide & change system clock
            --  We needn't to change active clock set before
            --  But setuped clock may not exactly be wanted one
            AHB_Clk := AHB_Frequency (Set_Optimal_HPRE_Divider (Used_Clock,
                                      Frequency (AHB_Clock)));

         else
            --  Try to setup best PLL coefficients
            PLL_Clock := Set_PLL_Best (Used_Clock, Frequency (AHB_Clock), PLL_Error);

            --  AHB_Clock is higher than used clock so we must setup PLL
            if PLL_Clock /= No_Clock then

               if Active_Clock = CLOCK.RCC.HSI_Oscillator_Selected then
                  PLLCFGR.Register.PLLSRC := CLOCK.RCC.HSI_Clock;
               else
                  PLLCFGR.Register.PLLSRC := CLOCK.RCC.HSE_Clock;
               end if;

               --  Turn on PLL & wait for lock
               Turn_PLL_On;
               Wait_For_PLL;

               --  Set system clock
               AHB_Clk := AHB_Frequency (PLL_Clock);

               --  Change system clock to be a PLL
               Active_Clock := CLOCK.RCC.PLL_Selected;

            end if;

            --  Sorry back to slow clock
            --  Set system clock as pure HSI or HSE
            AHB_Clk := AHB_Frequency (Used_Clock);

         end if;

      else

         --  Set system clock as pure HSI or HSE
         AHB_Clk := AHB_Frequency (Used_Clock);

      end if;

      Set_ACR (FLASH.FLASH.DCEN + FLASH.FLASH.ICEN);

      --  Setup wait states before turn on higher clock
      Set_Optimal_Latency (VDD, Frequency (AHB_Clock));

      --------------------------------------------------------------
      --                Configure APB1, APB2 dividers             --
      --                Configure MCO1, MCO outputs               --
      --                   Need to change here ???                --
      --------------------------------------------------------------

      Set_CFGR (PPRE1 (CLOCK.RCC.Divided_By_4) +
                  PPRE2 (CLOCK.RCC.Divided_By_2) +
                  MCO1 (CLOCK.RCC.HSI_Clock_Selected) +
                  MCO1PRE (CLOCK.RCC.Not_Divided) +
                  MCO2 (CLOCK.RCC.System_Clock) +
                  MCO2PRE (CLOCK.RCC.Divided_By_5) +
                --  Activate clock at the end
                  CLOCK.RCC.SW (Active_Clock));

      loop
         exit when CFGR.Register.SWS = RCC_CFGR.SWS.T (Active_Clock);
      end loop;

   end Init_High_Speed_Clock;

   procedure Init_Clock is
   begin
      Reset;
      Init_High_Speed_Clock;
      Init_Low_Speed_Clock;
   end Init_Clock;

   pragma Warnings (On);  --  Warning about Constraint may call Last_Chance_Handler


end ARM.Startup.Clock_F40XXX;
