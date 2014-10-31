--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                A R M . R e g i s t e r . P W R _ F 3 7 X X X               --
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

--  Package defines STM32 F37XXX family power related registers

-------------------------
--  Imported packages  --
-------------------------

with ARM.Register;
use  ARM.Register;

with ARM.Register.Types;
with ARM.Register.Bitfield;
with ARM.Register.Register;

--------------------------------------------------------------------------------
--                            ARM.Register.PWR_F37XXX                         --
--------------------------------------------------------------------------------

package ARM.Register.PWR_F37XXX is

   pragma Preelaborate;

   -------------------------------------------------------------------------
   --                                 PWR                                 --
   -------------------------------------------------------------------------

   ----------------------
   --  CR Register  --
   ----------------------

   package PWR_CR is

      package Tp is new Types (R32);

      package ENSD3 is new Bitfield (Tp, 11);
      package ENSD2 is new Bitfield (Tp, 10);
      package ENSD1 is new Bitfield (Tp, 9);
      package DPB   is new Bitfield (Tp, 8);
      package PLS   is new Bitfield (Tp, 5, 3);
      package PVDE  is new Bitfield (Tp, 4);
      package CSBF  is new Bitfield (Tp, 3);
      package CWUF  is new Bitfield (Tp, 2);
      package PDDS  is new Bitfield (Tp, 1);
      package LPDS  is new Bitfield (Tp, 0);

      type T is
         record
            ENSD3 : PWR_CR.ENSD3.T;
            ENSD2 : PWR_CR.ENSD2.T;
            ENSD1 : PWR_CR.ENSD1.T;
            DPB   : PWR_CR.DPB  .T;
            PLS   : PWR_CR.PLS  .T;
            PVDE  : PWR_CR.PVDE .T;
            CSBF  : PWR_CR.CSBF .T;
            CWUF  : PWR_CR.CWUF .T;
            PDDS  : PWR_CR.PDDS .T;
            LPDS  : PWR_CR.LPDS .T;
         end record;

      for T use
         record
            ENSD3 at 0 range ENSD3.F .. ENSD3.L;
            ENSD2 at 0 range ENSD2.F .. ENSD2.L;
            ENSD1 at 0 range ENSD1.F .. ENSD1.L;
            DPB   at 0 range DPB  .F .. DPB  .L;
            PLS   at 0 range PLS  .F .. PLS  .L;
            PVDE  at 0 range PVDE .F .. PVDE .L;
            CSBF  at 0 range CSBF .F .. CSBF .L;
            CWUF  at 0 range CWUF .F .. CWUF .L;
            PDDS  at 0 range PDDS .F .. PDDS .L;
            LPDS  at 0 range LPDS .F .. LPDS .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end PWR_CR;

   package CR    is new Register (PWR_CR.T, PWR_CR.Tp, 16#4000_7000#);
   subtype CR_T  is CR.T;
   subtype CR_F  is CR.F;
   

   --  Field definitions

   function ENSD3 is new CR.B (PWR_CR.ENSD3) with Inline_Always;
   function ENSD2 is new CR.B (PWR_CR.ENSD2) with Inline_Always;
   function ENSD1 is new CR.B (PWR_CR.ENSD1) with Inline_Always;
   function DPB   is new CR.B (PWR_CR.DPB  ) with Inline_Always;
   function PLS   is new CR.B (PWR_CR.PLS  ) with Inline_Always;
   function PVDE  is new CR.B (PWR_CR.PVDE ) with Inline_Always;
   function CSBF  is new CR.B (PWR_CR.CSBF ) with Inline_Always;
   function CWUF  is new CR.B (PWR_CR.CWUF ) with Inline_Always;
   function PDDS  is new CR.B (PWR_CR.PDDS ) with Inline_Always;
   function LPDS  is new CR.B (PWR_CR.LPDS ) with Inline_Always;

   --  Functions

   function "+"  is new CR.Add      with Inline_Always;
   function "+"  is new CR.Add_F    with Inline_Always;
   function "+"  is new CR.Add_FF   with Inline_Always;
   function "-"  is new CR.Clear    with Inline_Always;
   function "-"  is new CR.Clear_FF with Inline_Always;
   function "="  is new CR.Equal    with Inline_Always;
   function Init is new CR.Init     with Inline_Always;

   --  Constant definitions

   function SDADC3_Disabled is new CR.C (PWR_CR.ENSD3, 2#0#) with Inline_Always;
   function SD3_Enabled     is new CR.C (PWR_CR.ENSD3, 2#1#) with Inline_Always;

   function SDADC2_Disabled is new CR.C (PWR_CR.ENSD2, 2#0#) with Inline_Always;
   function SD2_Enabled     is new CR.C (PWR_CR.ENSD2, 2#1#) with Inline_Always;

   function SDADC1_Disabled is new CR.C (PWR_CR.ENSD1, 2#0#) with Inline_Always;
   function SD1_Enabled     is new CR.C (PWR_CR.ENSD1, 2#1#) with Inline_Always;

   function Access_To_RTC_And_Backup_Registers_Disabled is new CR.C (PWR_CR.DPB, 2#0#) with Inline_Always;
   function Access_To_RTC_And_Backup_Registers_Enabled  is new CR.C (PWR_CR.DPB, 2#1#) with Inline_Always;

   function PVD_2_2V is new CR.C (PWR_CR.PLS, 2#000#) with Inline_Always;
   function PVD_2_3V is new CR.C (PWR_CR.PLS, 2#001#) with Inline_Always;
   function PVD_2_4V is new CR.C (PWR_CR.PLS, 2#010#) with Inline_Always;
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

      package EWUP3       is new Bitfield (Tp, 10);
      package EWUP2       is new Bitfield (Tp, 9);
      package EWUP1       is new Bitfield (Tp, 8);
      package VREFINTRDYF is new Bitfield (Tp, 3);
      package PVDO        is new Bitfield (Tp, 2);
      package SBF         is new Bitfield (Tp, 1);
      package WUF         is new Bitfield (Tp, 0);

      type T is
         record
            EWUP3       : PWR_CSR.EWUP3      .T;
            EWUP2       : PWR_CSR.EWUP2      .T;
            EWUP1       : PWR_CSR.EWUP1      .T;
            VREFINTRDYF : PWR_CSR.VREFINTRDYF.T;
            PVDO        : PWR_CSR.PVDO       .T;
            SBF         : PWR_CSR.SBF        .T;
            WUF         : PWR_CSR.WUF        .T;
         end record;

      for T use
         record
            EWUP3       at 0 range EWUP3      .F .. EWUP3      .L;
            EWUP2       at 0 range EWUP2      .F .. EWUP2      .L;
            EWUP1       at 0 range EWUP1      .F .. EWUP1      .L;
            VREFINTRDYF at 0 range VREFINTRDYF.F .. VREFINTRDYF.L;
            PVDO        at 0 range PVDO       .F .. PVDO       .L;
            SBF         at 0 range SBF        .F .. SBF        .L;
            WUF         at 0 range WUF        .F .. WUF        .L;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

   end PWR_CSR;

   package CSR    is new Register (PWR_CSR.T, PWR_CSR.Tp, 16#4000_7004#);
   subtype CSR_T  is CSR.T;
   subtype CSR_F  is CSR.F;
   

   --  Field definitions

   function EWUP3       is new CSR.B (PWR_CSR.EWUP3      ) with Inline_Always;
   function EWUP2       is new CSR.B (PWR_CSR.EWUP2      ) with Inline_Always;
   function EWUP1       is new CSR.B (PWR_CSR.EWUP1      ) with Inline_Always;
   function VREFINTRDYF is new CSR.B (PWR_CSR.VREFINTRDYF) with Inline_Always;
   function PVDO        is new CSR.B (PWR_CSR.PVDO       ) with Inline_Always;
   function SBF         is new CSR.B (PWR_CSR.SBF        ) with Inline_Always;
   function WUF         is new CSR.B (PWR_CSR.WUF        ) with Inline_Always;

   --  Functions

   function "+"  is new CSR.Add      with Inline_Always;
   function "+"  is new CSR.Add_F    with Inline_Always;
   function "+"  is new CSR.Add_FF   with Inline_Always;
   function "-"  is new CSR.Clear    with Inline_Always;
   function "-"  is new CSR.Clear_FF with Inline_Always;
   function "="  is new CSR.Equal    with Inline_Always;
   function Init is new CSR.Init     with Inline_Always;

   --  Constant definitions

   function WKUP3_Pin_Used_For_IO     is new CSR.C (PWR_CSR.EWUP3, 2#0#) with Inline_Always;
   function WKUP3_Pin_Used_For_Wakeup is new CSR.C (PWR_CSR.EWUP3, 2#1#) with Inline_Always;

   function WKUP2_Pin_Used_For_IO     is new CSR.C (PWR_CSR.EWUP2, 2#0#) with Inline_Always;
   function WKUP2_Pin_Used_For_Wakeup is new CSR.C (PWR_CSR.EWUP2, 2#1#) with Inline_Always;

   function WKUP1_Pin_Used_For_IO     is new CSR.C (PWR_CSR.EWUP1, 2#0#) with Inline_Always;
   function WKUP1_Pin_Used_For_Wakeup is new CSR.C (PWR_CSR.EWUP1, 2#1#) with Inline_Always;

   function IS_VREFINT_Ready is new CSR.C (PWR_CSR.VREFINTRDYF, 2#1#) with Inline_Always;

   function VDD_Is_Higher_Than_PVD is new CSR.C (PWR_CSR.PVDO, 2#0#) with Inline_Always;
   function VDD_Is_Lower_Than_PVD  is new CSR.C (PWR_CSR.PVDO, 2#1#) with Inline_Always;

   function Device_Has_Not_Been_In_Standby is new CSR.C (PWR_CSR.SBF, 2#0#) with Inline_Always;
   function Device_Has_Been_In_Standby     is new CSR.C (PWR_CSR.SBF, 2#1#) with Inline_Always;

   function No_Wakeup_Event_Occurred      is new CSR.C (PWR_CSR.WUF, 2#0#) with Inline_Always;
   function Wakeup_Event_From_WKUP_Or_RTC is new CSR.C (PWR_CSR.WUF, 2#1#) with Inline_Always;

end ARM.Register.PWR_F37XXX;
