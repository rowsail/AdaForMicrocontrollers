--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                A R M . D r i v e r . C l o c k _ F 4 0 X X X               --
--                                  S p e c                                   --
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

-------------------------
--  Imported packages  --
-------------------------

with ARM.Register.RCC_F40XXX;

with ARM.Driver.Auxillary;
use  ARM.Driver.Auxillary;

--------------------------------------------------------------------------------
--                      ARM.Driver.Clock_F40XXX                           --
--------------------------------------------------------------------------------

package ARM.Driver.Clock_F40XXX is

   pragma Preelaborate;

   -------------
   --  Types  --
   -------------

   -----------------
   --  Constants  --
   -----------------

   -------------------------------
   --  Used packages renamings  --
   -------------------------------

   package RCC   renames ARM.Register.RCC_F40XXX;   use RCC;

   ----------------------
   --  Used bitfields  --
   ----------------------

   package HSION  is new CR.Bitfield (RCC_CR.HSION,  HSION);
   package HSIRDY is new CR.Bitfield (RCC_CR.HSIRDY, HSIRDY);

   package HSEON  is new CR.Bitfield (RCC_CR.HSEON,  HSEON);
   package HSERDY is new CR.Bitfield (RCC_CR.HSERDY, HSERDY);
   package HSEBYP is new CR.Bitfield (RCC_CR.HSEBYP, HSEBYP);

   package PLLON  is new CR.Bitfield (RCC_CR.PLLON,  PLLON);
   package PLLRDY is new CR.Bitfield (RCC_CR.PLLRDY, PLLRDY);

   package LSION  is new CSR.Bitfield (RCC_CSR.LSION,  LSION);
   package LSIRDY is new CSR.Bitfield (RCC_CSR.LSIRDY, LSIRDY);

   package SW     is new CFGR.Bitfield (RCC_CFGR.SW, SW);

   ------------------------
   --  Public interface  --
   ------------------------

   --  Primitive register manipulation routines  --

   procedure Set_CR        is new CR.Set_Field         with Inline_Always;

   procedure Set_PLLCFGR   is new PLLCFGR.Set_Field    with Inline_Always;

   procedure Set_CFGR      is new CFGR.Set_Field       with Inline_Always;

   procedure Set_APB1ENR   is new APB1ENR.Set_Field    with Inline_Always;


   --  Common part  --

   function Init is new Function_Do_Nothing
     with Inline_Always;

   function Reset is new Function_Do_Nothing
     with Inline_Always;

   --  HSI clock  --

   procedure Turn_HSI_On is new HSION.Set_With (Oscillator_On)
   with Inline_Always;

   function Turn_HSI_Off return Driver_Error
     with Inline_Always;

   procedure Activate_HSI is new SW.Set_With (HSI_Oscillator_Selected)
   with Inline_Always;

   procedure Wait_For_HSI is new HSIRDY.Wait_For (Oscillator_Ready)
   with Inline_Always;

   function Is_HSI_On is new HSION.Check_For (Oscillator_On)
   with Inline_Always;

   function Is_HSI_Ready is new HSIRDY.Check_For (Oscillator_Ready)
   with Inline_Always;

   function Is_HSI_Active return Boolean
     with Inline_Always;

   function Is_PLL_HSI_Based return Boolean
     with Inline_Always;

   --  HSE clock  --

   procedure Turn_HSE_On is new HSEON.Set_With (Oscillator_On)
   with Inline_Always;

   function Turn_HSE_Off return Driver_Error
     with Inline_Always;

   procedure Activate_HSE is new SW.Set_With (HSE_Oscillator_Selected)
   with Inline_Always;

   procedure Bypass_HSE is new HSEBYP.Set_With (Oscillator_Bypassed)
   with Inline_Always;

   procedure Wait_For_HSE is new HSERDY.Wait_For (Oscillator_Ready)
   with Inline_Always;

   function Is_HSE_On is new HSEON.Check_For (Oscillator_On)
   with Inline_Always;

   function Is_HSE_Ready is new HSERDY.Check_For (Oscillator_Ready)
   with Inline_Always;

   function Is_HSE_Active return Boolean
     with Inline_Always;

   function Is_PLL_HSE_Based return Boolean
     with Inline_Always;

   --  PLL  --

   procedure Turn_PLL_On is new PLLON.Set_With (PLL_On)
   with Inline_Always;

   procedure Wait_For_PLL is new PLLRDY.Wait_For (PLL_Locked)
   with Inline_Always;

   --  LSI clock  --

   procedure Turn_LSI_On is new LSION.Set_With (Oscillator_On)
   with Inline_Always;

   procedure Wait_For_LSI is new LSIRDY.Wait_For (Oscillator_Ready)
   with Inline_Always;

   --  Complex functions  --

   --  Sets the HPRE divider to get clock as close as possible to wanted clock
   --  then returns this clock
   function Set_Optimal_HPRE_Divider (Source_Clock : Frequency;
                                      Wanted_Clock : Frequency) return Frequency
     with
       Inline_Always,
       Pre =>
         Source_Clock in 4.000 .. 24.000 and
         Wanted_Clock in 1.000 .. 24.000;


   --  Set PLL parameters, then returns clock
   function Set_PLL_Best (Source_Clock : Frequency;
                          Wanted_Clock : Frequency;
                          Max_Error    : Frequency) return Frequency
     with
       Inline_Always,
       Pre =>
         Source_Clock in  4.000 ..  24.000 and
         Wanted_Clock in 24.000 .. 168.000;

private

   ----------------------
   --  Used bitfields  --
   ----------------------

   package SWS    is new CFGR.Bitfield  (RCC_CFGR.SWS, SWS);
   package PLLSRC is new PLLCFGR.Bitfield (RCC_PLLCFGR.PLLSRC, PLLSRC);

   -------------------------
   --  Private interface  --
   -------------------------

   function Is_HSI_Selected is new SWS.Check_For (HSI_Oscillator_Selected)
   with Inline_Always;

   function Is_HSE_Selected is new SWS.Check_For (HSE_Oscillator_Selected)
   with Inline_Always;

   function Is_PLL_Selected is new SWS.Check_For (PLL_Selected)
   with Inline_Always;

   function Is_PLL_Source_HSI is new PLLSRC.Check_For (HSI_Clock)
   with Inline_Always;

   function Is_PLL_Source_HSE is new PLLSRC.Check_For (HSE_Clock)
   with Inline_Always;

end ARM.Driver.Clock_F40XXX;
