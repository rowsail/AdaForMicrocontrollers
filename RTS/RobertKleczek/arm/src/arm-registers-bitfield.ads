--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 A R M . R e g i s t e r s . B i t f i e l d                --
--                               G e n e r i c                                --
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

--  Some generics for Registers usage

------------------------
--  Imported Packages --
------------------------

with ARM.Registers.Types;

--------------------------------------------------------------------------------
--                            ARM.Registers.Bitfield                          --
--------------------------------------------------------------------------------

generic
   with package Tp is new Types (<>);
   Pos  : Natural;       --  Position of first bit in register
   Len  : Natural := 1;  --  Length of bit field
package ARM.Registers.Bitfield is

   pragma Preelaborate;

   subtype RM  is Tp.RM;
   subtype Reg is Tp.Reg;

   use type Reg;

   type T is new Reg range 0 .. 2 ** Len - 1;  --  Bitfield type

   subtype R is Natural range Pos .. Pos + Len - 1;

   function B (Value : T) return RM;
   pragma Inline_Always (B);
   --  Function returning bit field variable value

end ARM.Registers.Bitfield;
