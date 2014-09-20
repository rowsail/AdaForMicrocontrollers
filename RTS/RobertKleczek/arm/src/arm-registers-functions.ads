--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                A R M . R e g i s t e r s . F u n c t i o n s               --
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

with ARM.Registers.Bitfield;
with ARM.Registers.Types;

--------------------------------------------------------------------------------
--                             ARM.Registers.Functions                        --
--------------------------------------------------------------------------------

generic
   type Register is private;
   with package Tp is new Types (<>);
package ARM.Registers.Functions is

   pragma Preelaborate;

   subtype Reg is Tp.Reg;
   subtype RM  is Tp.RM;

   use type Reg;

   generic
   function Add_RM (Left : RM; Right : RM) return Register;
   pragma Inline_Always (Add_RM);

   generic
   function Add (Left : Register; Right : RM) return Register;
   pragma Inline_Always (Add);

   generic
   function Clear (Left : Register; Right : RM) return Register;
   pragma Inline_Always (Clear);

   generic
   function Init (Val : Reg) return Register;
   pragma Inline_Always (Init);

   generic
      with package Z is new Bitfield (<>);
      Value : Z.T;
   function C return Z.T;
   pragma Inline_Always (C);

   generic
      with package Z is new Bitfield (<>);
   function B (V : Z.T := Z.T (1)) return Z.RM;
   pragma Inline_Always (B);

end ARM.Registers.Functions;
