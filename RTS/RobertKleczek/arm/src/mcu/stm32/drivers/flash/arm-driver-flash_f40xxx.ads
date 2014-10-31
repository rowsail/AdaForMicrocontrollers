--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                A R M . D r i v e r . F l a s h _ F 4 0 X X X               --
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

with ARM.Register.FLASH_F40XXX;

with ARM.Driver.Auxillary;
use  ARM.Driver.Auxillary;

--------------------------------------------------------------------------------
--                      ARM.Driver.Flash_F40XXX                           --
--------------------------------------------------------------------------------

package ARM.Driver.Flash_F40XXX is

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

   package FLASH renames ARM.Register.FLASH_F40XXX; use FLASH;

   ----------------------
   --  Used bitfields  --
   ----------------------

   package DCRST  is new FLASH.ACR.Bitfield  (FLASH_ACR.DCRST,  DCRST);
   package ICRST  is new FLASH.ACR.Bitfield  (FLASH_ACR.ICRST,  ICRST);
   package DCEN   is new FLASH.ACR.Bitfield  (FLASH_ACR.DCEN,   DCEN);
   package ICEN   is new FLASH.ACR.Bitfield  (FLASH_ACR.ICEN,   ICEN);
   package PRFTEN is new FLASH.ACR.Bitfield  (FLASH_ACR.PRFTEN, PRFTEN);

   ------------------------
   --  Public interface  --
   ------------------------

   --  Primitive register manipulation routines  --

   procedure Set_ACR    is new ACR.Set_Field    with Inline_Always;

   --  Common part  --

   function Init is new Function_Do_Nothing
     with Inline_Always;

   function Reset is new Function_Do_Nothing
     with Inline_Always;

   --  Access control  --

   procedure Reset_Data_Cache is new DCRST.Set_With (Data_Cache_Is_Reset)
   with Inline_Always;

   procedure Reset_Instruction_Cache is new ICRST.Set_With (Instruction_Cache_Is_Reset)
   with Inline_Always;

   procedure Enable_Data_Cache is new DCEN.Set_With (Data_Cache_Is_Enabled)
   with Inline_Always;

   procedure Enable_Instruction_Cache is new ICEN.Set_With (Instruction_Cache_Is_Enabled)
   with Inline_Always;

   procedure Enable_Prefetch is new PRFTEN.Set_With (Prefetch_Is_Enabled)
   with Inline_Always;

   --  Complex functions  --

   --  Procedure sets optimal flash wait states according to VDD & main clock
   --  Look at DocID022298 Rev 2 9/20 AN3988
   procedure Set_Optimal_Latency (VDD : Voltage; Clock : Frequency)
     with
       Inline_Always,
       Pre =>
         VDD   in 1.8 .. 3.6 and
         Clock in 1.000 .. 168.000;

private

   ----------------------
   --  Used bitfields  --
   ----------------------

   -------------------------
   --  Private interface  --
   --------------------

end ARM.Driver.Flash_F40XXX;
