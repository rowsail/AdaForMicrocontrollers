--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                      T i m i n g . L P C 2 1 X X . T                       --
--                                 S p e c                                    --
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

--  Internal timer version of timing feature for LPC21XX

with MCU.Interrupts_Names;
use  MCU.Interrupts_Names;

--  One timer version
--  To be generic

pragma Warnings (Off);
--  Switch Off warning of using System (not portable) unit
with System.OS_Interface;
pragma Warnings (On);

package Timing.LPC21XX.T is
   --     pragma Preelaborate;

   Prescaler : constant := 1;

   Delay_Margin : constant := 100;
   --  Delay margin in ticks to process alarm settings

   package SOI renames System.OS_Interface;

   protected Dispatcher is
      private
      procedure Handler;
      pragma Attach_Handler (Handler, INT_TIMER0);
      --  This is only to satisfy language rules
      --  Handler will never be executed
      --  due to "earlier calling" optimization
   end Dispatcher;

   procedure Initialize_Timers;
   pragma Export (Ada, Initialize_Timers);

   function Read_Clock return SOI.Time;
   pragma Export (Ada, Read_Clock);

   procedure Cancel_Alarm;
   pragma Export (Ada, Cancel_Alarm);

   function Set_Alarm (Next_Time : SOI.Time) return Boolean;
   pragma Export (Ada, Set_Alarm);

   procedure Clear_Alarm_Interrupt;
   pragma Export (Ada, Clear_Alarm_Interrupt);

private

   procedure Tick (Handler : access procedure);
   pragma Export (Ada, Tick);
   --  Main dispatching procedure
   --  This routine is always protected because irq's are off already

end Timing.LPC21XX.T;
