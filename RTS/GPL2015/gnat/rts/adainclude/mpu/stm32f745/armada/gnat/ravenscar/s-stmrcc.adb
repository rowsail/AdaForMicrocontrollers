------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S T M 3 2 F 4 . R C C                   --
--                                                                          --
--                                 B o d y                                  --
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

with System.BB.Parameters;

package body System.STM32F4.RCC is

   package Param renames System.BB.Parameters;

   HSI_VALUE : constant := 16_000_000;
   --  Internal oscillator in Hz

   HPRE_Presc_Table : constant array (Bits_4) of Word :=
     (1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 8, 16, 64, 128, 256, 512);

   PPRE_Presc_Table : constant array (Bits_3) of Word :=
     (1, 1, 1, 1, 2, 4, 8, 16);

   -------------------
   -- System_Clocks --
   -------------------

   function System_Clocks return RCC_System_Clocks is
      HSE_VALUE    : constant Word := Word (Param.HSE_Clock (MCU_ID.DEV_ID));
      RCC_CFGR_SWS : constant := 16#C#;
      Source       : constant Word := Registers.CFGR and RCC_CFGR_SWS;
      Result       : RCC_System_Clocks;

   begin
      case Source is
         --  HSI as source

         when 16#00# =>
            Result.SYSCLK := HSI_VALUE;

         --  HSE as source

         when 16#04# =>
            Result.SYSCLK := HSE_VALUE;

         --  PLL as source

         when 16#08# =>
            declare
               Pllsource : constant Word :=
                 (Registers.PLLCFGR and 16#00400000#) / (2**22);
               Pllm : constant Word :=
                 Registers.PLLCFGR and 16#0000003F#;
               Plln : constant Word :=
                 (Registers.PLLCFGR and 16#00007FC0#) / (2**6);
               Pllp : constant Word :=
                 (((Registers.PLLCFGR and 16#00030000#) / (2**16)) + 1) * 2;
               Pllvco : Word;

            begin
               if Pllsource /= 0 then
                  Pllvco := (HSE_VALUE / Pllm) * Plln;
               else
                  Pllvco := (HSI_VALUE / Pllm) * Plln;
               end if;

               Result.SYSCLK := Pllvco / Pllp;
            end;

         when others =>
            Result.SYSCLK := HSI_VALUE;
      end case;

      declare
         HPRE  : constant Bits_4 :=
                   Bits_4 ((Registers.CFGR and 16#00F0#) / (2**4));
         PPRE1 : constant Bits_3 :=
                   Bits_3 ((Registers.CFGR and 16#1C00#) / (2**10));
         PPRE2 : constant Bits_3 :=
                   Bits_3 ((Registers.CFGR and 16#E000#) / (2**13));
         TIMPR : constant Word   := (Registers.DCKCFGR / (2**24)) and 1;

      begin
         Result.HCLK  := Result.SYSCLK / HPRE_Presc_Table (HPRE);
         Result.PCLK1 := Result.HCLK / PPRE_Presc_Table (PPRE1);
         Result.PCLK2 := Result.HCLK / PPRE_Presc_Table (PPRE2);

         --  Timer clocks (see Dedicated clock cfg RCC documentation)

         if TIMPR = 0 then
            if PPRE_Presc_Table (PPRE1) = 1 then
               Result.TIMCLK1 := Result.PCLK1;
            else
               Result.TIMCLK1 := Result.PCLK1 * 2;
            end if;

            if PPRE_Presc_Table (PPRE2) = 1 then
               Result.TIMCLK2 := Result.PCLK2;
            else
               Result.TIMCLK2 := Result.PCLK2 * 2;
            end if;

         else
            if PPRE_Presc_Table (PPRE1) in 1 .. 4 then
               Result.TIMCLK1 := Result.HCLK;
            else
               Result.TIMCLK1 := Result.PCLK1 * 4;
            end if;

            if PPRE_Presc_Table (PPRE2) in 1 .. 4 then
               Result.TIMCLK2 := Result.HCLK;
            else
               Result.TIMCLK2 := Result.PCLK1 * 4;
            end if;
         end if;
      end;

      return Result;
   end System_Clocks;

end System.STM32F4.RCC;
