------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with TMS570.LEDs;   use TMS570.LEDs;
with Ada.Real_Time; use Ada.Real_Time;

package body Driver is

   type Index is mod 8;

   Pattern : constant array (Index) of User_LED :=
      (Right_Top, Top, Left_Top,
       Left,
       Left_Bottom, Bottom, Right_Bottom,
       Right);
   --  The LEDs are not physically laid out "consecutively" in such a way that
   --  we can simply drive them in enumeral order to get circular rotation.
   --  Thus we define this mapping, using a consecutive index to get the
   --  physical LED blinking order desired.

   task body Controller is
      Period       : constant Time_Span := Milliseconds (50);  -- arbitrary
      Next_Release : Time := Clock;
      Next_LED     : Index := 0;
   begin
      loop
         Off (Pattern (Next_LED));
         Next_LED := Next_LED + 1;
         On (Pattern (Next_LED));

         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Controller;

end Driver;
