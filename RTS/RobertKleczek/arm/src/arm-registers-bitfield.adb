--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 A R M . R e g i s t e r s . B i t f i e l d                --
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

--------------------------------------------------------------------------------
--                            ARM.Registers.Bitfield                          --
--------------------------------------------------------------------------------

package body ARM.Registers.Bitfield is

   function B (Value : T) return RM is
      Temp : Reg := 0;
   begin
      for I in R loop
         Temp := Temp + 2 ** I;
      end loop;
      return RM'(Reg (Value * 2 ** Pos), Temp);
   end B;

end ARM.Registers.Bitfield;
