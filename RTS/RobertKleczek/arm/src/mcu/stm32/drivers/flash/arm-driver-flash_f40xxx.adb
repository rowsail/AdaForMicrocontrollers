--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 A R M . D r i v e r . F l a s h _ F 4 0 X X X              --
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

package body ARM.Driver.Flash_F40XXX is

   pragma Warnings (Off);  --  Warning about Constraint may call Last_Chance_Handler

   --------------------------------
   --  Interface implementation  --
   --------------------------------

   --  HSI implementation  --

   procedure Set_Optimal_Latency (VDD : Voltage; Clock : Frequency) is

      Incr_Table : constant array (Integer range 18 .. 36) of Frequency
        := (18 .. 21 => 20.000,
            22 .. 24 => 22.000,
            25 .. 27 => 24.000,
            others   => 30.000);

      Incr : constant Frequency := Incr_Table (Integer (VDD * 10));
      Ltc  : constant FLASH.FLASH_ACR.LATENCY.T :=
               FLASH.FLASH_ACR.LATENCY.T ((Clock - 1.000) / Incr);
   begin
      --  Set Ltency
      ACR.Register.LATENCY := Ltc;

      --  For lower voltages disable prefetch
      if Incr = 20.000 then
         ACR.Register.PRFTEN := Prefetch_Is_Disabled;
      else
         ACR.Register.PRFTEN := Prefetch_Is_Enabled;
      end if;
   end Set_Optimal_Latency;

   pragma Warnings (On);  --  Warning about Constraint may call Last_Chance_Handler

end ARM.Driver.Flash_F40XXX;
