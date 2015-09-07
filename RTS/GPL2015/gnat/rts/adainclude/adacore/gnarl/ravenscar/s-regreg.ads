-------------------------------------------------------------------------------
--                                                                           --
--                       A R M   A D A   L I B R A R Y                       --
--                                                                           --
--               S Y S T E M . R e g i s t e r . R e g i s t e r             --
--                               G e n e r i c                               --
--                                                                           --
--    Copyright (C) 2015  Robert Kleczek                                     --
--                                                                           --
--    This program is free software: you can redistribute it and/or modify   --
--    it under the terms of the GNU General Public License as published by   --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    This program is distributed in the hope that it will be useful,        --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--    GNU General Public License for more details.                           --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                           --
-------------------------------------------------------------------------------

--  Some generics for Registers usage

------------------------
--  Imported Packages --
------------------------
with System.Register.Types;
with System.Register.Bitfield;

-------------------------------------------------------------------------------
--                              SYSTEM.Register.Register                     --
-------------------------------------------------------------------------------

generic
   type RR_T       is private;         --  register RECORD TYPE
   with package Tp is new Types (<>);  --  base types include
   Offset          : Tp.Bs.Adr_T;      --  relative address of register
   Default         : Tp.RC_T := 0;     --  default value for register

package System.Register.Register is

   pragma Preelaborate;

   -------------------------
   --  Package renamings  --
   -------------------------

   package Base renames Tp.Bs;

   ---------------------------
   --  Renaming used types  --
   ---------------------------

   subtype RC_T is Tp.RC_T;  --  register COMMON TYPE
   subtype R_T  is RR_T;     --  register RECORD TYPE must be visible
   subtype F_T  is Tp.F_T;   --  register F (field type) definition

   use type RC_T;

   -----------------
   --  Variables  --
   -----------------

   --  We must know any name of register to invoke procedures
   --  defined for RECORD TYPED registers,

   Register : R_T;
   for Register'Address use System'To_Address (Base.Addr + Offset);

   --------------------------------------------
   --  Definitions for oveloading operators  --
   --------------------------------------------

   generic function Add_F    (Left : F_T; Right : F_T) return R_T
     with Inline_Always;
   generic function Add      (Left : R_T; Right : F_T) return R_T
     with Inline_Always;
   generic function Add_FF   (Left : F_T; Right : F_T) return F_T
     with Inline_Always;
   generic function Clear    (Left : R_T; Right : F_T) return R_T
     with Inline_Always;
   generic function Clear_FF (Left : F_T; Right : F_T) return F_T
     with Inline_Always;
   generic function Init     (Val  : RC_T)             return R_T
     with Inline_Always;
   generic function Equal    (Left : R_T; Right : F_T) return Boolean
     with Inline_Always;

   ------------------------------------
   --  Define new bitfield CONSTANT  --
   ------------------------------------

   generic
      with package Z is new Bitfield (<>);
      Value : Z.B_T;
   function C return Z.B_T with Inline_Always;

   ---------------------------------
   --  Define new BITFIELD value  --
   ---------------------------------

   generic
      with package Z is new Bitfield (<>);
   function B (V : Z.B_T := Z.B_T'Last) return Z.RM_T with Inline_Always;

   --------------------------------------
   --  Register manipulation routines  --
   --------------------------------------

   procedure Set (Arg : F_T)  with Inline_Always;

   procedure Reset_Register   with Inline_Always;

   ---------------------------------------
   --  Bitfield manipulations routines  --
   ---------------------------------------

   generic
      with package B is new System.Register.Bitfield (<>);
      with function Arg (Val : B.B_T) return B.RM_T;
   package Bitfield is

      generic
         with function Cons return B.B_T;
      procedure Set with Inline_Always;

      generic
         with function Cons return B.B_T;
      function Check_For return Boolean with Inline_Always;

      generic
         with function Cons return B.B_T;
      procedure Wait_For with Inline_Always;

   end Bitfield;

end System.Register.Register;
