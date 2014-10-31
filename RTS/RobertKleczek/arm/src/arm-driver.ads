--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                            A R M . D r i v e r                             --
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

--  Some driver names definitions

--------------------------------------------------------------------------------
--                                 ARM.Driver                                 --
--------------------------------------------------------------------------------

package ARM.Driver is

   pragma Preelaborate;

   -------------
   --  Types  --
   -------------

   --  Driver error must be declared as record with fatal error,
   --  cause etc. pools with manipulation procedures


   subtype Driver_Error is Natural;
   subtype Answer       is Boolean;

   --  Common type declaration for all drivers  --

   -----------------
   --  Constants  --
   -----------------

   No_Error : Driver_Error := 0;
   Error    : Driver_Error := 1;

   Yes      : Answer := True;
   No       : Answer := False;

end ARM.Driver;
