--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                   A R M . R e g i s t e r . R e g i s t e r                --
--                                 B o d y                                    --
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

------------------------
--  Imported Packages --
------------------------

with Ada.Unchecked_Conversion;

--------------------------------------------------------------------------------
--                              ARM.Register.Register                         --
--------------------------------------------------------------------------------

package body ARM.Register.Register is

   function To_Register is new
     Ada.Unchecked_Conversion (Source => Reg, Target => T) with Inline_Always;

   function From_Register is new
     Ada.Unchecked_Conversion (Source => T, Target => Reg) with Inline_Always;

   function Add_F (Left : F; Right : F) return T is
   begin
      return To_Register (Left (Value) or Right (Value));
   end Add_F;

   function Add (Left : T; Right : F) return T is
      Temp : Reg;
   begin
       Temp := From_Register (Left) and not Right (Mask);
      Temp := Temp or Right (Value);
      return To_Register (Temp);
   end Add;

   function Add_FF (Left : F; Right : F) return F is
      Temp : F;
   begin
      Temp (Value) := Left (Value) or Right (Value);
      Temp (Mask)  := Left (Mask) or Right (Mask);
      return Temp;
   end Add_FF;

   function Clear (Left : T; Right : F) return T is
   begin
      return To_Register (From_Register (Left) and not Right (Mask));
   end Clear;

   function Clear_FF (Left : F; Right : F) return F is
      Temp : F;
   begin
      Temp (Value) := Left (Value);
      Temp (Mask)  := Left (Mask) or Right (Mask);
      return Temp;
   end Clear_FF;

   function Init (Val : Reg) return T is
   begin
      return To_Register (Val);
   end Init;

   function Equal (Left : T; Right : F) return Boolean is
      Temp : Reg;
   begin
      Temp := From_Register (Left) and Right (Mask);
      return Temp = Right (Value);
   end Equal;

   function C return Z.T is
   begin
      return Value;
   end C;

   function B (V : Z.T := Z.T'Last) return Z.RM is
   begin
      return Z.B (V);
   end B;

   function "+"  is new Add with Inline_Always;

   procedure Set_Field (Arg : F) is
   begin
      Register := Register + Arg;
   end Set_Field;

   procedure Reset_Register is
   begin
      Register := To_Register (Default);
   end Reset_Register;

   package body Bitfield is

      function "=" is new Equal with Inline_Always;

      function To_F is new
        Ada.Unchecked_Conversion (Source => B.RM, Target => F) with Inline_Always;

      procedure Set_With is
      begin
         Register := Register + To_F (Arg (Cons));
      end Set_With;

      function Check_For return Boolean is
      begin
         return Register = To_F (Arg (Cons));
      end Check_For;

      procedure Wait_For is
      begin
         loop
            exit when Register = To_F (Arg (Cons));
         end loop;
      end Wait_For;

   end Bitfield;

end ARM.Register.Register;
