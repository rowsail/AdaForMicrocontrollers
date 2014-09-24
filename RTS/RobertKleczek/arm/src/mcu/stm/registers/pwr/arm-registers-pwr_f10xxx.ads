--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--             A R M . R e g i s t e r s . P W R _ F 1 0 X X X            --
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

--  Package defines STM32 F10XXX family power related registers

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
--                           ARM.Registers.PWR_F10XXX                         --
--------------------------------------------------------------------------------

package ARM.Registers.PWR_F10XXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                                 PWR                                 --
   -------------------------------------------------------------------------

   ----------------------
   --  CR Register  --
   ----------------------

   package CR is

      package Tp is new Types (R32);

      package DPB   is new Bitfield (Tp, 8);
      package PLS   is new Bitfield (Tp, 5, 3);
      package PVDE  is new Bitfield (Tp, 4);
      package CSBF  is new Bitfield (Tp, 3);
      package CWUF  is new Bitfield (Tp, 2);
      package PDDS  is new Bitfield (Tp, 1);
      package LPDS  is new Bitfield (Tp, 0);

      type T is
         record
            DPB   : CR.DPB  .T;
            PLS   : CR.PLS  .T;
            PVDE  : CR.PVDE .T;
            CSBF  : CR.CSBF .T;
            CWUF  : CR.CWUF .T;
            PDDS  : CR.PDDS .T;
            LPDS  : CR.LPDS .T;
         end record;

      for T use
         record
            DPB   at 0 range DPB  .R'First .. DPB  .R'Last;
            PLS   at 0 range PLS  .R'First .. PLS  .R'Last;
            PVDE  at 0 range PVDE .R'First .. PVDE .R'Last;
            CSBF  at 0 range CSBF .R'First .. CSBF .R'Last;
            CWUF  at 0 range CWUF .R'First .. CWUF .R'Last;
            PDDS  at 0 range PDDS .R'First .. PDDS .R'Last;
            LPDS  at 0 range LPDS .R'First .. LPDS .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CR;

   subtype CR_T is CR.T;

   --  Field definitions

   function DPB   is new CR.FN.B (CR.DPB  );
   function PLS   is new CR.FN.B (CR.PLS  );
   function PVDE  is new CR.FN.B (CR.PVDE );
   function CSBF  is new CR.FN.B (CR.CSBF );
   function CWUF  is new CR.FN.B (CR.CWUF );
   function PDDS  is new CR.FN.B (CR.PDDS );
   function LPDS  is new CR.FN.B (CR.LPDS );

   --  Functions

   function  "+"   is new CR.FN.Add;
   function  "+"   is new CR.FN.Add_RM;
   function  "-"   is new CR.FN.Clear;
   function  Init  is new CR.FN.Init;

   --  Constant definitions

   function Access_To_RTC_And_Backup_Registers_Disabled is new CR.FN.C (CR.DPB, 2#0#);
   function Access_To_RTC_And_Backup_Registers_Enabled  is new CR.FN.C (CR.DPB, 2#1#);

   function PVD_2_2V is new CR.FN.C (CR.PLS, 2#000#);
   function PVD_2_3V is new CR.FN.C (CR.PLS, 2#001#);
   function PVD_2_4V is new CR.FN.C (CR.PLS, 2#010#);
   function PVD_2_5V is new CR.FN.C (CR.PLS, 2#011#);
   function PVD_2_6V is new CR.FN.C (CR.PLS, 2#100#);
   function PVD_2_7V is new CR.FN.C (CR.PLS, 2#101#);
   function PVD_2_8V is new CR.FN.C (CR.PLS, 2#110#);
   function PVD_2_9V is new CR.FN.C (CR.PLS, 2#111#);

   function PVD_Disabled is new CR.FN.C (CR.PVDE, 2#0#);
   function PVD_Enabled  is new CR.FN.C (CR.PVDE, 2#1#);

   function Clear_Flag is new CR.FN.C (CR.CSBF, 2#1#);

   function Clear_Flag is new CR.FN.C (CR.CWUF, 2#1#);

   function Enter_Stop_Mode    is new CR.FN.C (CR.PDDS, 2#0#);
   function Enter_Standby_Mode is new CR.FN.C (CR.PDDS, 2#1#);

   function Voltage_Regulator_On_During_Stop       is new CR.FN.C (CR.LPDS, 2#0#);
   function Voltage_Regulator_LowPower_During_Stop is new CR.FN.C (CR.LPDS, 2#1#);

   ----------------------
   --  CSR Register  --
   ----------------------

   package CSR is

      package Tp is new Types (R32);

      package EWUP is new Bitfield (Tp, 8);
      package PVDO is new Bitfield (Tp, 2);
      package SBF  is new Bitfield (Tp, 1);
      package WUF  is new Bitfield (Tp, 0);

      type T is
         record
            EWUP : CSR.EWUP .T;
            PVDO : CSR.PVDO .T;
            SBF  : CSR.SBF  .T;
            WUF  : CSR.WUF  .T;
         end record;

      for T use
         record
            EWUP at 0 range EWUP.R'First .. EWUP.R'Last;
            PVDO at 0 range PVDO.R'First .. PVDO.R'Last;
            SBF  at 0 range SBF .R'First .. SBF .R'Last;
            WUF  at 0 range WUF .R'First .. WUF .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end CSR;

   subtype CSR_T is CSR.T;

   --  Field definitions

   function EWUP is new CSR.FN.B (CSR.EWUP);
   function PVDO is new CSR.FN.B (CSR.PVDO);
   function SBF  is new CSR.FN.B (CSR.SBF );
   function WUF  is new CSR.FN.B (CSR.WUF );

   --  Functions

   function  "+"   is new CSR.FN.Add;
   function  "+"   is new CSR.FN.Add_RM;
   function  "-"   is new CSR.FN.Clear;
   function  Init  is new CSR.FN.Init;

   --  Constant definitions

   function WKUP_Pin_Used_For_IO     is new CSR.FN.C (CSR.EWUP, 2#0#);
   function WKUP_Pin_Used_For_Wakeup is new CSR.FN.C (CSR.EWUP, 2#1#);

   function VDD_Is_Higher_Than_PVD is new CSR.FN.C (CSR.PVDO, 2#0#);
   function VDD_Is_Lower_Than_PVD  is new CSR.FN.C (CSR.PVDO, 2#1#);

   function Device_Has_Not_Been_In_Standby is new CSR.FN.C (CSR.SBF, 2#0#);
   function Device_Has_Been_In_Standby     is new CSR.FN.C (CSR.SBF, 2#1#);

   function No_Wakeup_Event_Occurred      is new CSR.FN.C (CSR.WUF, 2#0#);
   function Wakeup_Event_From_WKUP_Or_RTC is new CSR.FN.C (CSR.WUF, 2#1#);

   --------------------------------------------------------------------------
   --                         Register definition                          --
   --------------------------------------------------------------------------

   type PWR_T is
      record
         CR      : CR_T;
         CSR     : CSR_T;
         pragma Volatile (CR);
         pragma Volatile (CSR);
      end record;

   for PWR_T use
      record
         CR       at 16#00# range 0 .. 31;
         CSR      at 16#04# range 0 .. 31;
      end record;

   PWR : PWR_T;

   for PWR'Address use System'To_Address (16#4000_7000#);

end ARM.Registers.PWR_F10XXX;
