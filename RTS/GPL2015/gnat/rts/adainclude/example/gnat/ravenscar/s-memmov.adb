------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M .  M E M O R Y _ M O V E                 --
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
with Interfaces.C; use Interfaces.C;

package body System.Memory_Move is

   type IA is mod System.Memory_Size;
   --  The type used to provide the actual desired operations

   function To_IA is new Ada.Unchecked_Conversion (Address, IA);
   --  The operations are implemented by unchecked conversion to type IA,
   --  followed by doing the intrinsic operation on the IA values, followed
   --  by converting the result back to type Address.

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;
   --  Byte is the storage unit

   type Byte_Ptr is access Byte;
   --  Access to a byte

   function To_Byte_Ptr is new Ada.Unchecked_Conversion (IA, Byte_Ptr);
   --  Conversion between an integer address and access to byte

   Byte_Size : constant := 1;
   --  Number of storage unit in a byte

   type Word is mod 2 ** System.Word_Size;
   for Word'Size use System.Word_Size;
   --  Word is efficiently loaded and stored by the processor, but has
   --  alignment constraints.

   type Word_Ptr is access Word;
   --  Access to a word.

   function To_Word_Ptr is new Ada.Unchecked_Conversion (IA, Word_Ptr);
   --  Conversion from an integer adddress to word access

   Word_Size : constant := Word'Size / Storage_Unit;
   --  Number of storage unit per word

   -------------
   -- memmove --
   -------------

   function memmove
     (Dest : Address; Src : Address; N : size_t) return Address is
      D : IA := To_IA (Dest);
      S : IA := To_IA (Src);
      C : IA := IA (N);
   begin
      --  Return immediately if no bytes to copy.

      if N = 0 then
         return Dest;
      end if;

      --  This function must handle overlapping memory regions
      --  for the source and destination. If the Dest buffer is
      --  located past the Src buffer then we use backward copying,
      --  and forward copying otherwise.

      if D > S and then D < S + C then
         D := D + C;
         S := S + C;
         while C /= 0 loop
            D := D - Byte_Size;
            S := S - Byte_Size;
            To_Byte_Ptr (D).all := To_Byte_Ptr (S).all;

            C := C - Byte_Size;
         end loop;
      else
         --  Try to copy per word, if alignment constraints are respected
         if ((D or S) and (Word'Alignment - 1)) = 0 then
            while C >= Word_Size loop
               To_Word_Ptr (D).all := To_Word_Ptr (S).all;
               D := D + Word_Size;
               S := S + Word_Size;
               C := C - Word_Size;
            end loop;
         end if;

         --  Copy the remaining byte per byte
         while C > 0 loop
            To_Byte_Ptr (D).all := To_Byte_Ptr (S).all;
            D := D + Byte_Size;
            S := S + Byte_Size;
            C := C - Byte_Size;
         end loop;
      end if;

      return Dest;
   end memmove;

end System.Memory_Move;
