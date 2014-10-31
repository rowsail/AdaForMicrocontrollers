--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                   A R M . R e g i s t e r . T y p e s                      --
--                               G e n e r i c                                --
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

--  Some generics for Registers usage

--------------------------------------------------------------------------------
--                              ARM.Register.Types                            --
--------------------------------------------------------------------------------

generic
   type R is mod <>;
package ARM.Register.Types is

   pragma Preelaborate;

   type Reg is new R;
   type F   is array (Register_And_Mask) of Reg;

end ARM.Register.Types;
