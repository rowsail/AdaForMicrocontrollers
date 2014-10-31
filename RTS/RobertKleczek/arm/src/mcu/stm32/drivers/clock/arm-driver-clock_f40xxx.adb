--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 A R M . D r i v e r . C l o c k _ F 4 0 X X X              --
--                                 B o d y                                    --
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

package body ARM.Driver.Clock_F40XXX is

   pragma Warnings (Off);  --  Warning about Constraint may call Last_Chance_Handler

   --------------------------------
   --  Interface implementation  --
   --------------------------------

   --  HSI implementation  --

   function Turn_HSI_Off return Driver_Error is
   begin
      --  Check if HSI Clock is used direct or with PLL
      if Is_HSI_Active then
         --  HSI is used
         return Error;
      end if;
      --  HSI oscillator can be turned off
      RCC.CR.Register.HSION := Oscillator_Off;
      return No_Error;
   end Turn_HSI_Off;

   function Is_HSI_Active return Boolean is
   begin
      --  Used directly
      return Is_HSI_Selected
      --  Used with PLL
        or else (Is_PLL_Selected and then Is_PLL_Source_HSI);
   end Is_HSI_Active;

   function Is_PLL_HSI_Based return Boolean is
   begin
      --  Used with PLL
      return Is_PLL_Selected and then Is_PLL_Source_HSI;
   end Is_PLL_HSI_Based;

   --  HSI implementation  --

   function Turn_HSE_Off return Driver_Error is
   begin
      --  Check if HSE Clock is used direct or with PLL
      if Is_HSE_Active then
         --  HSE is used
         return Error;
      end if;
      --  HSE oscillator can be turned off
      RCC.CR.Register.HSEON := Oscillator_Off;
      return No_Error;
   end Turn_HSE_Off;

   function Is_HSE_Active return Boolean is
   begin
      --  Used directly
      return Is_HSE_Selected
      --  Used with PLL
        or else (Is_PLL_Selected and then Is_PLL_Source_HSE);
   end Is_HSE_Active;

   function Is_PLL_HSE_Based return Boolean is
   begin
      --  Used with PLL
      return Is_PLL_Selected and then Is_PLL_Source_HSE;
   end Is_PLL_HSE_Based;

   --  Complex functions  --

   function Set_Optimal_HPRE_Divider (Source_Clock : Frequency;
                                      Wanted_Clock : Frequency) return Frequency is
      Divider : Frequency;
      Result  : Frequency;
   begin
      Divider := Source_Clock / Wanted_Clock;

      if Divider in 0.0 .. 1.4 then
         CFGR.Register.HPRE := System_Clock_Not_Divided;
         Result := 1.0;
      elsif Divider in 1.4 .. 2.4 then
         CFGR.Register.HPRE := System_Clock_Divided_By_2;
         Result := 2.0;
      elsif Divider in 2.5 .. 5.9 then
         CFGR.Register.HPRE := System_Clock_Divided_By_4;
         Result := 4.0;
      elsif Divider in 6.0 .. 11.9 then
         CFGR.Register.HPRE := System_Clock_Divided_By_8;
         Result := 8.0;
      elsif Divider in 12.0 .. 31.9 then
         CFGR.Register.HPRE := System_Clock_Divided_By_16;
         Result := 16.0;
      elsif Divider in 32.0 .. 95.9 then
         CFGR.Register.HPRE := System_Clock_Divided_By_64;
         Result := 64.0;
      elsif Divider in 96.0 .. 191.9 then
         CFGR.Register.HPRE := System_Clock_Divided_By_128;
         Result := 128.0;
      elsif Divider in 192.0 .. 383.9 then
         CFGR.Register.HPRE := System_Clock_Divided_By_256;
         Result := 256.0;
      elsif Divider in 384.0 .. Frequency'Last then
         CFGR.Register.HPRE := System_Clock_Divided_By_512;
         Result := 512.0;
      end if;

      --  Return real computed clock
      return Source_Clock / Result;

   end Set_Optimal_HPRE_Divider;

   function Set_PLL_Best (Source_Clock : Frequency;
                          Wanted_Clock : Frequency;
                          Max_Error    : Frequency
                         ) return Frequency is
      Error  : Frequency := Max_Error + 0.001;  --  Initial value decreases No iterations
      Result : Frequency;
      PLL_M  : RCC_PLLCFGR.PLLM.T; use type RCC_PLLCFGR.PLLM.T;
      PLL_N  : RCC_PLLCFGR.PLLN.T; use type RCC_PLLCFGR.PLLN.T;
      PLL_P  : RCC_PLLCFGR.PLLP.T; use type RCC_PLLCFGR.PLLP.T;
      PLL_Q  : RCC_PLLCFGR.PLLQ.T;
   begin
      --  Check for all source possible PLL input clocks from lowest to highest
      --  storing the last one => the best
      for M in reverse 2 .. 63 loop
         if Frequency (Source_Clock / Frequency (M)) in 1.000 .. 2.000 then
            --  Check this frequency
            for N in 192 .. 432 loop
               if Frequency (Frequency (Source_Clock / Frequency (M)) * Frequency (N)) in 192.000 .. 432.000 then
                  --  Valid PLL range, so check output frequences
                  for P in 1 .. 4 loop  --  real range 2, 4, 6, 8 => x2
                     Result := Frequency (Frequency (Frequency (Source_Clock / Frequency (M))) * Frequency (N)) / Frequency (2 * P);
                     if abs (Wanted_Clock - Result) <= Error then
                        --  Posibble good result
                        Error := abs (Wanted_Clock - Result);  --  set new better error
                        PLL_M := RCC_PLLCFGR.PLLM.T (M);
                        PLL_N := RCC_PLLCFGR.PLLN.T (N);
                        PLL_P := RCC_PLLCFGR.PLLP.T (P - 1);
                     end if;
                  end loop;
               end if;
            end loop;
         end if;
      end loop;

      --  Here we check all coeff possibilities, so result is the best
      --  or we get nothing
      if Error <= Max_Error then

         --  Calculate Q, if PLL clock is in the range, then all Qs should be valid
         --  but more or less accurate
         PLL_Q := RCC_PLLCFGR.PLLQ.T (Frequency (Frequency (Source_Clock / Frequency (PLL_M)) * Frequency (PLL_N)) / 48.000);
         --  Set PLL parameters
         Set_PLLCFGR (PLLQ (PLL_Q) + PLLM (PLL_M) + PLLN (PLL_N) + PLLP (PLL_P));

         return Frequency (Frequency (Source_Clock / Frequency (PLL_M)) * Frequency (PLL_N)) / (2 * Frequency (PLL_P + 1));

      else

         --  No valid coeff found, so no PLL
         return No_Clock;

      end if;

   end Set_PLL_Best;

   pragma Warnings (On);  --  Warning about Constraint may call Last_Chance_Handler


end ARM.Driver.Clock_F40XXX;
