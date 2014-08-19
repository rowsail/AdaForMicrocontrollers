--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                       L P C 2 1 X X . S t a r t u p                        --
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

--  Body of startup for LPC21XX family

------------------------
--  Imported Packages --
------------------------

with Interfaces;
--  Used for Unsigned_32;

with System.Machine_Code;
--  Used for ASM sequences

with LPC.Registers.SCB_214X;
use  LPC.Registers.SCB_214X;
--  Used for System Control Block registers

package body LPC21XX.Startup is

   package SMC renames System.Machine_Code;

   LFHT : constant String := ASCII.LF & ASCII.HT;

   ------------------
   --  Enter_Idle  --
   ------------------

   procedure Enter_Idle is
   begin
      null;
   end Enter_Idle;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize is

      use type Interfaces.Unsigned_32;
      use type PLL0STAT.PLOCK.T;
      use type PLL0CFG.MSEL.T;
      use type PLL0CFG.PSEL.T;
      --  Make visible arithmetic

      type PSEL_Index is range 1 .. 16;
      M : PLL0CFG.MSEL.T;
      P : PLL0CFG.PSEL.T;
      PSEL_VAL : constant array (PSEL_Index) of PLL0CFG.PSEL.T :=
        (1 => 0, 2 .. 3 => 1, 4 .. 7 => 2, 8 .. 16 => 3);
      --  Some constants to evaluate PLL parameters

      procedure FEED_Confirm;
      --  Magic feed procedure

      procedure FEED_Confirm is
      begin
         SMC.Asm ("strb %1, %0"     & LFHT &
             "strb %2, %0",
           Inputs =>
             (PLL0FEED.PLLFEED.T'Asm_Input ("r", FEED_AA),
              PLL0FEED.PLLFEED.T'Asm_Input ("r", FEED_55)),
           Outputs =>
             PLL0FEED_T'Asm_Output ("=m", SCB.PLL0FEED),
           Volatile => True,
           Clobber => "memory");
      end FEED_Confirm;

   begin
      SCB.MEMMAP.MAP := User_Flash_Mode;
      --  Vectors in FLASH

      case Desired_Frequency is
         when 0 .. 19999999        => SCB.MAMTIM.MAMFCT := CCLK1;
         when 20000000 .. 39999999 => SCB.MAMTIM.MAMFCT := CCLK2;
         when others               => SCB.MAMTIM.MAMFCT := CCLK3;
      end case;
      --  Set appropriate FLASH read latency

      case Peripheral_Clock_Divider is
         when 1 => SCB.VPBDIV.VPB_DIV := Div1;
         when 2 => SCB.VPBDIV.VPB_DIV := Div2;
         when 4 => SCB.VPBDIV.VPB_DIV := Div4;
         when others => pragma Assert (False);
      end case;
      --  Set peripheral clock

      M := PLL0CFG.MSEL.T (Desired_Frequency / Crystal_Frequency);
      P := PSEL_VAL (PSEL_Index (320_000_000 / (Desired_Frequency * 2)));
      --  Compute MSEL & PSEL

      if M in 1 .. 32 and then P in 1 .. 16 then

         SCB.PLL0CFG := PSEL (P) + MSEL (M - 1);
         SCB.PLL0CON := PLLE + PLLC (0);
         FEED_Confirm;
         --  Start PLL

         while SCB.PLL0STAT.PLOCK = PLL_Not_Locked loop null; end loop;
         --  Wait for synchronization

         SCB.PLL0CON := PLLE + PLLC;
         FEED_Confirm;
         --  Set PLL as a clock

      else
         --  Wrong parameters
         SCB.PLL0CON := PLLE (0) + PLLC (0);
         FEED_Confirm;
         --  Shut down PLL

         case Crystal_Frequency is
            when 0 .. 19999999        => SCB.MAMTIM.MAMFCT := CCLK1;
            when 20000000 .. 39999999 => SCB.MAMTIM.MAMFCT := CCLK2;
            when others               => SCB.MAMTIM.MAMFCT := CCLK3;
         end case;
         --  Set back appropriate FLASH read latency

      end if;

   end Initialize;

end LPC21XX.Startup;
