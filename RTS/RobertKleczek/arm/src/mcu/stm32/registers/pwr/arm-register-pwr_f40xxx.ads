--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--               A R M . R e g i s t e r . P W R _ F 4 0 X X X                --
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

--  Package defines STM32 F40XXX family power related registers

-------------------------
--  Imported packages  --
-------------------------

with ARM.Register;
use  ARM.Register;

with ARM.Register.Types;
with ARM.Register.Bitfield;
with ARM.Register.Register;

--------------------------------------------------------------------------------
--                            ARM.Register.PWR_F40XXX                         --
--------------------------------------------------------------------------------

package ARM.Register.PWR_F40XXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                                 PWR                                 --
   -------------------------------------------------------------------------

   ----------------------
   --  CR Register  --
   ----------------------

   package PWR_CR is

      package Tp is new Types (R32);

      package VOS   is new Bitfield (Tp, 14);
      package FPDS  is new Bitfield (Tp, 9);
      package DPB   is new Bitfield (Tp, 8);
      package PLS   is new Bitfield (Tp, 5, 3);
      package PVDE  is new Bitfield (Tp, 4);
      package CSBF  is new Bitfield (Tp, 3);
      package CWUF  is new Bitfield (Tp, 2);
      package PDDS  is new Bitfield (Tp, 1);
      package LPDS  is new Bitfield (Tp, 0);

      type T is
         record
            VOS   : PWR_CR.VOS .T;
            FPDS  : PWR_CR.FPDS.T;
            DPB   : PWR_CR.DPB .T;
            PLS   : PWR_CR.PLS .T;
            PVDE  : PWR_CR.PVDE.T;
            CSBF  : PWR_CR.CSBF.T;
            CWUF  : PWR_CR.CWUF.T;
            PDDS  : PWR_CR.PDDS.T;
            LPDS  : PWR_CR.LPDS.T;
         end record;

      for T use
         record
            VOS   at 0 range VOS .F .. VOS .L;
            FPDS  at 0 range FPDS.F .. FPDS.L;
            DPB   at 0 range DPB .F .. DPB .L;
            PLS   at 0 range PLS .F .. PLS .L;
            PVDE  at 0 range PVDE.F .. PVDE.L;
            CSBF  at 0 range CSBF.F .. CSBF.L;
            CWUF  at 0 range CWUF.F .. CWUF.L;
            PDDS  at 0 range PDDS.F .. PDDS.L;
            LPDS  at 0 range LPDS.F .. LPDS.L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end PWR_CR;

   package CR    is new Register (PWR_CR.T, PWR_CR.Tp, 16#4000_7000#);
   subtype CR_T  is CR.T;
   subtype CR_F  is CR.F;
   

   --  Field definitions

   function VOS   is new CR.B (PWR_CR.VOS ) with Inline_Always;
   function FPDS  is new CR.B (PWR_CR.FPDS) with Inline_Always;
   function DPB   is new CR.B (PWR_CR.DPB ) with Inline_Always;
   function PLS   is new CR.B (PWR_CR.PLS ) with Inline_Always;
   function PVDE  is new CR.B (PWR_CR.PVDE) with Inline_Always;
   function CSBF  is new CR.B (PWR_CR.CSBF) with Inline_Always;
   function CWUF  is new CR.B (PWR_CR.CWUF) with Inline_Always;
   function PDDS  is new CR.B (PWR_CR.PDDS) with Inline_Always;
   function LPDS  is new CR.B (PWR_CR.LPDS) with Inline_Always;

   --  Functions

   function "+"  is new CR.Add      with Inline_Always;
   function "+"  is new CR.Add_F    with Inline_Always;
   function "+"  is new CR.Add_FF   with Inline_Always;
   function "-"  is new CR.Clear    with Inline_Always;
   function "-"  is new CR.Clear_FF with Inline_Always;
   function "="  is new CR.Equal    with Inline_Always;
   function Init is new CR.Init     with Inline_Always;

   --  Constant definitions

   function Scale_2_Mode is new CR.C (PWR_CR.VOS, 2#0#) with Inline_Always;
   function Scale_1_Mode is new CR.C (PWR_CR.VOS, 2#1#) with Inline_Always;

   function Flash_Not_In_Power_Down_During_Stop is new CR.C (PWR_CR.FPDS, 2#0#) with Inline_Always;
   function Flash_In_Power_Down_During_Stop     is new CR.C (PWR_CR.FPDS, 2#1#) with Inline_Always;

   function Access_To_RTC_And_Backup_Registers_Disabled is new CR.C (PWR_CR.DPB, 2#0#) with Inline_Always;
   function Access_To_RTC_And_Backup_Registers_Enabled  is new CR.C (PWR_CR.DPB, 2#1#) with Inline_Always;

   function PVD_2_0V is new CR.C (PWR_CR.PLS, 2#000#) with Inline_Always;
   function PVD_2_1V is new CR.C (PWR_CR.PLS, 2#001#) with Inline_Always;
   function PVD_2_3V is new CR.C (PWR_CR.PLS, 2#010#) with Inline_Always;
   function PVD_2_5V is new CR.C (PWR_CR.PLS, 2#011#) with Inline_Always;
   function PVD_2_6V is new CR.C (PWR_CR.PLS, 2#100#) with Inline_Always;
   function PVD_2_7V is new CR.C (PWR_CR.PLS, 2#101#) with Inline_Always;
   function PVD_2_8V is new CR.C (PWR_CR.PLS, 2#110#) with Inline_Always;
   function PVD_2_9V is new CR.C (PWR_CR.PLS, 2#111#) with Inline_Always;

   function PVD_Disabled is new CR.C (PWR_CR.PVDE, 2#0#) with Inline_Always;
   function PVD_Enabled  is new CR.C (PWR_CR.PVDE, 2#1#) with Inline_Always;

   function Clear_Flag is new CR.C (PWR_CR.CSBF, 2#1#) with Inline_Always;

   function Clear_Flag is new CR.C (PWR_CR.CWUF, 2#1#) with Inline_Always;

   function Enter_Stop_Mode    is new CR.C (PWR_CR.PDDS, 2#0#) with Inline_Always;
   function Enter_Standby_Mode is new CR.C (PWR_CR.PDDS, 2#1#) with Inline_Always;

   function Voltage_Regulator_On_During_Stop       is new CR.C (PWR_CR.LPDS, 2#0#) with Inline_Always;
   function Voltage_Regulator_LowPower_During_Stop is new CR.C (PWR_CR.LPDS, 2#1#) with Inline_Always;

   ----------------------
   --  CSR Register  --
   ----------------------

   package PWR_CSR is

      package Tp is new Types (R32);

      package VOSRDY is new Bitfield (Tp, 14);
      package BRE    is new Bitfield (Tp, 9);
      package EWUP   is new Bitfield (Tp, 8);
      package BRR    is new Bitfield (Tp, 3);
      package PVDO   is new Bitfield (Tp, 2);
      package SBF    is new Bitfield (Tp, 1);
      package WUF    is new Bitfield (Tp, 0);

      type T is
         record
            VOSRDY : PWR_CSR.VOSRDY.T;
            BRE    : PWR_CSR.BRE   .T;
            EWUP   : PWR_CSR.EWUP  .T;
            BRR    : PWR_CSR.BRR   .T;
            PVDO   : PWR_CSR.PVDO  .T;
            SBF    : PWR_CSR.SBF   .T;
            WUF    : PWR_CSR.WUF   .T;
         end record;

      for T use
         record
            VOSRDY at 0 range VOSRDY.F .. VOSRDY.L;
            BRE    at 0 range BRE   .F .. BRE   .L;
            EWUP   at 0 range EWUP  .F .. EWUP  .L;
            BRR    at 0 range BRR   .F .. BRR   .L;
            PVDO   at 0 range PVDO  .F .. PVDO  .L;
            SBF    at 0 range SBF   .F .. SBF   .L;
            WUF    at 0 range WUF   .F .. WUF   .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end PWR_CSR;

   package CSR    is new Register (PWR_CSR.T, PWR_CSR.Tp, 16#4000_7004#);
   subtype CSR_T  is CSR.T;
   subtype CSR_F  is CSR.F;
   

   --  Field definitions

   function VOSRDY is new CSR.B (PWR_CSR.VOSRDY) with Inline_Always;
   function BRE    is new CSR.B (PWR_CSR.BRE   ) with Inline_Always;
   function EWUP   is new CSR.B (PWR_CSR.EWUP  ) with Inline_Always;
   function BRR    is new CSR.B (PWR_CSR.BRR   ) with Inline_Always;
   function PVDO   is new CSR.B (PWR_CSR.PVDO  ) with Inline_Always;
   function SBF    is new CSR.B (PWR_CSR.SBF   ) with Inline_Always;
   function WUF    is new CSR.B (PWR_CSR.WUF   ) with Inline_Always;

   --  Functions

   function "+"  is new CSR.Add      with Inline_Always;
   function "+"  is new CSR.Add_F    with Inline_Always;
   function "+"  is new CSR.Add_FF   with Inline_Always;
   function "-"  is new CSR.Clear    with Inline_Always;
   function "-"  is new CSR.Clear_FF with Inline_Always;
   function "="  is new CSR.Equal    with Inline_Always;
   function Init is new CSR.Init     with Inline_Always;

   --  Constant definitions

   function Not_Ready is new CSR.C (PWR_CSR.VOSRDY, 2#0#) with Inline_Always;
   function Ready     is new CSR.C (PWR_CSR.VOSRDY, 2#1#) with Inline_Always;

   function Backup_Regulator_Disabled is new CSR.C (PWR_CSR.BRE, 2#0#) with Inline_Always;
   function Backup_Regulator_Enabled  is new CSR.C (PWR_CSR.BRE, 2#1#) with Inline_Always;

   function WKUP_Pin_Used_For_IO     is new CSR.C (PWR_CSR.EWUP, 2#0#) with Inline_Always;
   function WKUP_Pin_Used_For_Wakeup is new CSR.C (PWR_CSR.EWUP, 2#1#) with Inline_Always;

   function Backup_Regulator_Not_Ready is new CSR.C (PWR_CSR.BRR, 2#0#) with Inline_Always;
   function Backup_Regulator_Ready     is new CSR.C (PWR_CSR.BRR, 2#1#) with Inline_Always;

   function VDD_Is_Higher_Than_PVD is new CSR.C (PWR_CSR.PVDO, 2#0#) with Inline_Always;
   function VDD_Is_Lower_Than_PVD  is new CSR.C (PWR_CSR.PVDO, 2#1#) with Inline_Always;

   function Device_Has_Not_Been_In_Standby is new CSR.C (PWR_CSR.SBF, 2#0#) with Inline_Always;
   function Device_Has_Been_In_Standby     is new CSR.C (PWR_CSR.SBF, 2#1#) with Inline_Always;

   function No_Wakeup_Event_Occurred      is new CSR.C (PWR_CSR.WUF, 2#0#) with Inline_Always;
   function Wakeup_Event_From_WKUP_Or_RTC is new CSR.C (PWR_CSR.WUF, 2#1#) with Inline_Always;

end ARM.Register.PWR_F40XXX;
