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

with Ada.Unchecked_Conversion;

package body TMS570.LEDs is

   function As_Word is new Ada.Unchecked_Conversion
     (Source => User_LED, Target => Word);

   --------
   -- On --
   --------

   procedure On (This : User_LED) is
   begin
      Set_Pin (HET_Port, As_Word (This));
   end On;

   ---------
   -- Off --
   ---------

   procedure Off (This : User_LED) is
   begin
      Clear_Pin (HET_Port, As_Word (This));
   end Off;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  set all pins as outputs
      HET_Port.DIR := 16#FFFF_FFFF#;
   end Initialize;

begin
   Initialize;
end TMS570.LEDs;