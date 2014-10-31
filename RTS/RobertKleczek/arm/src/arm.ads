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

--------------------------------------------------------------------------------
--                                     ARM                                    --
--------------------------------------------------------------------------------

package ARM is

   pragma Pure;

   -------------
   --  Types  --
   -------------

   --  Frequency definitions  --

   type Frequency is delta 0.001 digits 7 range 0.000 .. 9999.999;

   --  Voltage definitions  --

   type Voltage is delta 0.1 digits 4;


   type Bit      is  (Clear, Set);
   for  Bit      use (Clear => 0, Set => 1);
   for  Bit'Size use 1;

   -----------------
   --  Constants  --
   -----------------

   --  Special Frequency value meaning => no clock;
   No_Clock : constant Frequency := 0.000;


end ARM;
