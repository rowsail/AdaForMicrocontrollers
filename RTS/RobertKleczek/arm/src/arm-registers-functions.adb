--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 A R M . R e g i s t e r s . F u n c t i o n s              --
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
--                            ARM.Registers.Functions                         --
--------------------------------------------------------------------------------

package body ARM.Registers.Functions is

   function Add_RM (Left : RM; Right : RM) return Register is

      function To_Register is new
        Ada.Unchecked_Conversion (Source => Reg, Target => Register);

   begin
      return To_Register (Left (Value) or Right (Value));
   end Add_RM;

   function Add (Left : Register; Right : RM) return Register is

      function To_Register is new
        Ada.Unchecked_Conversion (Source => Reg, Target => Register);

      function From_Register is new
        Ada.Unchecked_Conversion (Source => Register, Target => Reg);

      Temp : Reg;

   begin
      Temp := From_Register (Left) and not Right (Mask);
      Temp := Temp or  Right (Value);
      return To_Register (Temp);
   end Add;

   function Clear (Left : Register; Right : RM) return Register is

      function To_Register is new
        Ada.Unchecked_Conversion (Source => Reg, Target => Register);

      function From_Register is new
        Ada.Unchecked_Conversion (Source => Register, Target => Reg);

   begin
      return To_Register (From_Register (Left) and not Right (Mask));
   end Clear;

   function Init (Val : Reg) return Register is

      function To_Register is new
        Ada.Unchecked_Conversion (Source => Reg, Target => Register);

   begin
      return To_Register (Val);
   end Init;

   function C return Z.T is
   begin
      return Value;
   end C;

   function B (V : Z.T := Z.T (1)) return Z.RM is
   begin
      return Z.B (V);
   end B;

end ARM.Registers.Functions;
