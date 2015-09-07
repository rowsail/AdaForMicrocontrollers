-------------------------------------------------------------------------------
--                                                                           --
--                       A R M   A D A   L I B R A R Y                       --
--                                                                           --
--                S Y S T E M . R e g i s t e r . R e g i s t e r            --
--                                 B o d y                                   --
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

------------------------
--  Imported Packages --
------------------------

with Ada.Unchecked_Conversion;

-------------------------------------------------------------------------------
--                            SYSTEM.Register.Register                       --
-------------------------------------------------------------------------------

package body System.Register.Register is

   -------------------------------
   --  Define some convertions  --
   -------------------------------

   function To_Register is new
     Ada.Unchecked_Conversion (Source => RC_T, Target => R_T)
     with Inline_Always;

   function From_Register is new
     Ada.Unchecked_Conversion (Source => R_T, Target => RC_T)
     with Inline_Always;

   function To_Reg is new
     Ada.Unchecked_Conversion (Source => Base.R_T, Target => R_T)
     with Inline_Always;

   function From_Reg is new
     Ada.Unchecked_Conversion (Source => R_T, Target => Base.R_T)
     with Inline_Always;

   -------------
   --  Add_F  --
   -------------

   function Add_F (Left : F_T; Right : F_T) return R_T is
   begin
      return To_Register (Left (Value) or Right (Value));
   end Add_F;

   --  Add  --

   function Add (Left : R_T; Right : F_T) return R_T is
      Temp : RC_T;
   begin
      Temp := From_Register (Left) and not Right (Mask);
      Temp := Temp or Right (Value);
      return To_Register (Temp);
   end Add;

   --  Add_FF  --

   function Add_FF (Left : F_T; Right : F_T) return F_T is
      Temp : F_T;
   begin
      Temp (Value) := Left (Value) or Right (Value);
      Temp (Mask)  := Left (Mask) or Right (Mask);
      return Temp;
   end Add_FF;

   --  Clear  --

   function Clear (Left : R_T; Right : F_T) return R_T is
   begin
      return To_Register (From_Register (Left) and not Right (Mask));
   end Clear;

   --  Clear_FF  --

   function Clear_FF (Left : F_T; Right : F_T) return F_T is
      Temp : F_T;
   begin
      Temp (Value) := Left (Value);
      Temp (Mask)  := Left (Mask) or Right (Mask);
      return Temp;
   end Clear_FF;

   --  Init  --

   function Init (Val : RC_T) return R_T is
   begin
      return To_Register (Val);
   end Init;

   --  Equal  --

   function Equal (Left : R_T; Right : F_T) return Boolean is
      Temp : RC_T;
   begin
      Temp := From_Register (Left) and Right (Mask);
      return Temp = Right (Value);
   end Equal;

   --  C  --

   function C return Z.B_T is
   begin
      return Value;
   end C;

   --  B  --

   function B (V : Z.B_T := Z.B_T'Last) return Z.RM_T is
   begin
      return Z.B (V);
   end B;

   --  +  --

   function "+"  is new Add with Inline_Always;

   --  Set  --

   procedure Set (Arg : F_T) is
      use type Base.R_T;
   begin
      Base.Set (Offset, From_Reg (To_Reg (Base.Get (Offset)) + Arg));
   end Set;

   --  Reset_Register  --

   procedure Reset_Register is
   begin
      Base.Set (Offset, From_Reg (To_Register (Default)));
   end Reset_Register;

   ------------------------
   --  Bitfield package  --
   ------------------------

   package body Bitfield is

      --  Helping functions  --

      function "=" is new Equal with Inline_Always;

      function To_F is new
        Ada.Unchecked_Conversion (Source => B.RM_T, Target => F_T)
        with Inline_Always;

      --  Set  --

      procedure Set is
      begin
         Base.Set (Offset, From_Reg (To_Reg (Base.Get (Offset)) +
                     To_F (Arg (Cons))));

      end Set;

      --  Check_For  --

      function Check_For return Boolean is
      begin
         return To_Reg (Base.Get (Offset)) = To_F (Arg (Cons));
      end Check_For;

      --  Wait_For  --

      procedure Wait_For is
      begin
         loop
            exit when To_Reg (Base.Get (Offset)) = To_F (Arg (Cons));
         end loop;
      end Wait_For;

   end Bitfield;

end System.Register.Register;
