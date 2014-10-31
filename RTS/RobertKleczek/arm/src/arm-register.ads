--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                          A R M . R e g i s t e r                           --
--                                 S p e c                                    --
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

--  Some registers names definitions

--------------------------------------------------------------------------------
--                               ARM.Register                                 --
--------------------------------------------------------------------------------

package ARM.Register is

   pragma Pure;

   -------------
   --  Types  --
   -------------

   type Register_And_Mask is (Value, Mask);

   type R32      is mod 2 ** 32;
   for  R32'Size use 32;

   type R16      is mod 2 ** 16;
   for  R16'Size use 16;

   type R8      is mod 2 ** 8;
   for  R8'Size use 8;

   --     Set   : constant := 1;
   --     Clear : constant := 0;

end ARM.Register;
