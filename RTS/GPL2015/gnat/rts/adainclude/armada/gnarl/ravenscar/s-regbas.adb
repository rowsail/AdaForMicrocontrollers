-------------------------------------------------------------------------------
--                                                                           --
--                       A R M   A D A   L I B R A R Y                       --
--                                                                           --
--                 S Y S T E M . R e g i s t e r . B a s e                   --
--                                   B o d y                                 --
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

-------------------------------------------------------------------------------
--                            SYSTEM.Register.Base                           --
-------------------------------------------------------------------------------

package body System.Register.Base is

   ------------------------------
   --  Functions & procedures  --
   ------------------------------

   --  Set  --
   --  Set register with given value

   procedure Set (Adr : Adr_T; Val : R_T) is
   begin
      pragma Warnings (Off);   --  prevent "Constraint_Error" warning
      --  That warning will never happend because divider should always be > 0
      Registers (Adr / (R_T'Size / 8)) := RV_T (Val);
      pragma Warnings (On);   --  switch warnings again
   end Set;

   --  Get  --
   --  Get Non-Volatile value of register

   function Get (Adr : Adr_T) return R_T is
   begin
      pragma Warnings (Off);   --  prevent "Constraint_Error" warning
      --  That warning will never happend because divider should always be > 0
      return R_T (Registers (Adr / (R_T'Size / 8)));
      pragma Warnings (On);   --  switch warnings again
   end Get;

end System.Register.Base;
