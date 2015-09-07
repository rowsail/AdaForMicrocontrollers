------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M .  M E M O R Y _ S E T                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006-2014, Free Software Foundation, Inc.       --
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
with System; use System;
with Interfaces.C; use Interfaces.C;

package body System.Memory_Set is

   subtype Mem_Array is char_array (size_t);
   type Mem_Ptr is access Mem_Array;

   function To_Memptr is new Ada.Unchecked_Conversion (Address, Mem_Ptr);

   ------------
   -- memset --
   ------------

   function Memset (M : Address; C : int; Size : size_t) return Address is
      Dest : constant Mem_Ptr := To_Memptr (M);

   begin
      if Size > 0 then
         for J in 0 .. Size - 1 loop
            Dest (J) := char'Val (C);
         end loop;
      end if;

      return M;
   end Memset;

end System.Memory_Set;
