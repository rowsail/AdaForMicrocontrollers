-------------------------------------------------------------------------------
--                                                                           --
--                       A R M   A D A   L I B R A R Y                       --
--                                                                           --
--                       S Y S T E M . R e g i s t e r                       --
--                                 S p e c                                   --
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

--  Some registers names definitions

-------------------------------------------------------------------------------
--                                SYSTEM.Register                            --
-------------------------------------------------------------------------------

package System.Register is

   pragma Preelaborate;

   -------------
   --  Types  --
   -------------

   type Address is mod System.Memory_Size;

   type Register_And_Mask is (Value, Mask);

   type R32      is mod 2 ** 32;
   for  R32'Size use 32;

   type RV32     is new R32 with Volatile;

   type R16      is mod 2 ** 16;
   for  R16'Size use 16;

   type RV16     is new R16 with Volatile;

   type R8       is mod 2 ** 8;
   for  R8'Size  use 8;

   type RV8      is new R8 with Volatile;

end System.Register;
