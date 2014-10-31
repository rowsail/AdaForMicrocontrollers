--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                A R M . D r i v e r . P o w e r _ F 4 0 X X X               --
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

with ARM.Register.PWR_F40XXX;

with ARM.Driver.Auxillary;
use  ARM.Driver.Auxillary;

--------------------------------------------------------------------------------
--                      ARM.Driver.Power_F40XXX                               --
--------------------------------------------------------------------------------

package ARM.Driver.Power_F40XXX is

   pragma Preelaborate;

   -------------------------------
   --  Used packages renamings  --
   -------------------------------

   package PWR renames ARM.Register.PWR_F40XXX; use PWR;

   ----------------------
   --  Used bitfields  --
   ----------------------

   package VOS    is new CR.Bitfield  (PWR_CR.VOS, VOS);
   package VOSRDY is new CSR.Bitfield (PWR_CSR.VOSRDY, VOSRDY);

   ------------------------
   --  Public interface  --
   ------------------------

   --  Primitive register manipulation routines  --

   procedure Set_Field    is new CR.Set_Field    with Inline_Always;

   --  Common part  --

   function Init is new Function_Do_Nothing
     with Inline_Always;

   function Reset is new Function_Do_Nothing
     with Inline_Always;

   --  Power interface  --

   procedure Higher_Voltage is new VOS.Set_With (Scale_1_Mode)
   with Inline_Always;

   procedure Lower_Voltage is new VOS.Set_With (Scale_2_Mode)
   with Inline_Always;

   procedure Wait_For_VOS is new VOSRDY.Set_With (Ready)
   with Inline_Always;

end ARM.Driver.Power_F40XXX;
