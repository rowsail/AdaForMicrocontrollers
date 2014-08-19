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

--  Body of internal timer version of timing feature for LPC21XX

with LPC.Registers;
use  LPC.Registers;

with LPC.Registers.T0_21XX;
use  LPC.Registers.T0_21XX;

package body Timing.LPC21XX.T is

   type Representation_T is (Full_View, Split_View);

   type Splitted_Time is mod 2 ** 32;

   type Local_Time (Representation : Representation_T := Full_View) is
      record
         case Representation is
            when Full_View =>
               Time : SOI.Time;
            when Split_View =>
               MSP_Time : Splitted_Time;
               LSP_Time : Splitted_Time;
         end case;
      end record;

   pragma Unchecked_Union (Local_Time);
   for Local_Time'Size use 64;

   MSP_Time : Splitted_Time := 0;
   pragma Volatile (MSP_Time);
   --  Most significant (software) part of System time

   Alarm_Time : Local_Time;
   --  Actual set alarm time

   Alarm_In_Future : Boolean := False;

   protected body Dispatcher is
      procedure Handler is
      begin
         null;
      end Handler;
   end Dispatcher;

   procedure Initialize_Timers is
   begin
      Alarm_Time.Time := 0;
      T0.TCR          := (CEN => Disable, CRST => On);
      --  Stop & reset timer 0
      T0.CTCR         := (C_T => Timer_Mode, INPUT_SEL => Input_CAP0);
      --  Count internal rising PCLK edges
      T0.EMR          := (EM0 => Off, EM1 => Off, EM2 => Off, EM3 => Off,
                          EMC0 => Do_Nothing, EMC1 => Do_Nothing,
                          EMC2 => Do_Nothing, EMC3 => Do_Nothing);
      --  Disable External match feature
      T0.CCR          := (Off, Off, Off,
                          Off, Off, Off,
                          Off, Off, Off,
                          Off, Off, Off);
      --  Disable all captures
      T0.PR           := Prescaler;
      --  Set prescaler
      T0.MR0          := 16#FFFF_FFFF#;
      T0.MCR          := (Off, Off, Off,
                          Off, Off, Off,
                          Off, Off, Off,
                          Off, Off, Interrupt_On_Match);
      --  Set main clock timer overflow interrupt
      T0.TCR          := (CEN => Enable, CRST => Off);
      --  Start timer forever
   end Initialize_Timers;

   function Read_Clock return SOI.Time is
      Temp : Local_Time;
   begin
      Temp.MSP_Time := MSP_Time;
      Temp.LSP_Time := Splitted_Time (T0.TC);

      if MSP_Time > Temp.MSP_Time then
         Temp.MSP_Time := MSP_Time;
         Temp.LSP_Time := Splitted_Time (T0.TC);
      end if;

      return SOI.Time (Temp.Time);

   end Read_Clock;

   procedure Cancel_Alarm is

   begin
      T0.MCR := T0.MCR - MR1I;
      --  Switch off interrupts on match on MR1
   end Cancel_Alarm;

   function Set_Alarm (Next_Time : SOI.Time) return Boolean is

      use type SOI.Time;

      Temp  : Local_Time;
      Clock : Local_Time;
   begin
      Temp.Time  := Next_Time;
      Clock.Time := Read_Clock;
      if Temp.Time > Clock.Time + SOI.Time (Delay_Margin) then
         return False;  --  alarm is already outdated
      else
         if Temp.MSP_Time = Clock.MSP_Time then
            --  We are in the same clock period so set alarm
            T0.MR1 := R32 (Temp.LSP_Time);
            T0.MCR := T0.MCR + MR1I;
         else
            Alarm_Time.Time := Next_Time;
            Alarm_In_Future := True;
         end if;
         --  Alarm will be set in the Tick if both periods will be equal
         return True;  --  alarm valid
      end if;

   end Set_Alarm;

   procedure Clear_Alarm_Interrupt is
   begin
      T0.IR.MR1 := Reset;
      --  Clear pending match register 1 Timer 0 interrupt
   end Clear_Alarm_Interrupt;

   procedure Tick (Handler : access procedure) is

      use type SOI.Time;
      use type IR.MR0.T;
      use type IR.MR1.T;
   begin
      if T0.IR.MR0 = Is_Active then
         T0.IR.MR0 := Reset;
         --  Clear clock interrupt
         MSP_Time := MSP_Time + 1;

         if Alarm_In_Future then
            if MSP_Time = Alarm_Time.MSP_Time then
               --  Same period
               if R32 (Alarm_Time.LSP_Time) > T0.TC + Delay_Margin then
                  --  Alarm is in this period later
                  T0.MR1 := R32 (Alarm_Time.LSP_Time);
                  T0.MCR := T0.MCR + MR1I;
               end if;
               --  Alarm will be outdated soon, so call handler
               Handler.all;
               Alarm_In_Future := False;
               --  Reset Alarm_In_Future in same period
            end if;
            --  Alarm not in this period
         end if;
         --  Bye, bye,
         --  maybe there is an alarm interrupt pending so the same interrupt
         --  will be revoke, as alarm is less prioritized than clock
      else
         --  We are in alarm event
         T0.IR.MR1 := Reset;
         --  Clear alarm interrupt
         Handler.all;
         --  Invoke alarm Handler
      end if;
   end Tick;

end Timing.LPC21XX.T;
