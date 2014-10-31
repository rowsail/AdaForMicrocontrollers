--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                  A R M . R e g i s t e r . R e g i s t e r                 --
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

with System;

with ARM.Register.Types;
with ARM.Register.Bitfield;

--------------------------------------------------------------------------------
--                                ARM.Register.Register                       --
--------------------------------------------------------------------------------

generic
   type R is private;                  --  Register RECORD TYPE
   with package Tp is new Types (<>);  --  Base type include
   Addr    : System.Address;           --  Address of hardware
   Default : Tp.Reg := 0;
package ARM.Register.Register is

   pragma Preelaborate;

   ---------------------------
   --  Renaming used types  --
   ---------------------------

   subtype Reg is Tp.Reg;
   subtype T   is R;                 --  register T (main type) definition
   subtype F   is Tp.F;              --  register F (field type) definition

   use type Reg;

   --------------------------------
   --  MAIN REGISTER DEFINITION  --
   --------------------------------

   Register : T with Volatile;
   for Register'Address use Addr;

   --------------------------------------------
   --  Definitions for oveloading operators  --
   --------------------------------------------

   generic function Add_F    (Left : F; Right : F) return T with Inline_Always;
   generic function Add      (Left : T; Right : F) return T with Inline_Always;
   generic function Add_FF   (Left : F; Right : F) return F with Inline_Always;
   generic function Clear    (Left : T; Right : F) return T with Inline_Always;
   generic function Clear_FF (Left : F; Right : F) return F with Inline_Always;
   generic function Init     (Val : Reg)           return T with Inline_Always;
   generic function Equal    (Left : T; Right : F) return Boolean with Inline_Always;

   ------------------------------------
   --  Define new bitfield CONSTANT  --
   ------------------------------------

   generic
      with package Z is new Bitfield (<>);
      Value : Z.T;
   function C return Z.T with Inline_Always;

   ---------------------------------
   --  Define new BITFIELD value  --
   ---------------------------------

   generic
      with package Z is new Bitfield (<>);
   function B (V : Z.T := Z.T'Last) return Z.RM with Inline_Always;

   --------------------------------------
   --  Register manipulation routines  --
   --------------------------------------

   generic procedure Set_Field    (Arg : F)  with Inline_Always;

   procedure Reset_Register with Inline_Always;

   ---------------------------------------
   --  Bitfield manipulations routines  --
   ---------------------------------------

   generic
      with package B is new ARM.Register.Bitfield (<>);
      with function Arg (Val : B.T) return B.RM;
   package Bitfield is

      generic
         with function Cons return B.T;
      procedure Set_With with Inline_Always;

      generic
         with function Cons return B.T;
      function Check_For return Boolean with Inline_Always;

      generic
         with function Cons return B.T;
      procedure Wait_For with Inline_Always;

   end Bitfield;

end ARM.Register.Register;
