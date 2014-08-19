--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                                 A R M                                      --
--                                S p e c                                     --
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

package ARM is

   pragma Preelaborate;

   type Bit      is  (Clear, Set);
   for  Bit      use (Clear => 0, Set => 1);
   for  Bit'Size use 1;

   --  Clock type for Crystal Frequency

   subtype Frequency    is Natural;
   subtype Microseconds is Natural;
   subtype Miliseconds  is Natural;
   subtype Seconds      is Natural;

   Crystal_Frequency : Frequency := 0;
   System_Clock      : Frequency := 0;
   Peripheral_Clock  : Frequency := 0;

end ARM;
