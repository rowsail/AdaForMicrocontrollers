--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                      A R M . A r c h i t e c t u r e                       --
--                                 B o d y                                    --
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

------------------------
--  Imported Packages --
------------------------

with System.Machine_Code;
use  System.Machine_Code;

with Ada.Unchecked_Conversion;

--------------------------------------------------------------------------------
--                                ARM.Architecture                            --
--------------------------------------------------------------------------------

package body ARM.Architecture is

   ------------
   --  Hang  --
   ------------

   procedure Hang is
   begin
      Asm (
           "0: b 0b",
           Volatile => True);
   end Hang;

   -----------------
   --  Copy_Data  --
   -----------------

   procedure Copy_Data is

      --  Suppress checking pointers from being zero (trust us)
      --  results in much better code
      pragma Suppress (Access_Check);

      type Int_Ptr is access all Integer_Address;

      function To_Access is
        new Ada.Unchecked_Conversion (Integer_Address, Int_Ptr);

      Data_Load   : Integer_Address := To_Integer (LD_Data_Load'Address);
      Data_Start  : Integer_Address := To_Integer (LD_Data_Start'Address);
      Data_End    : constant Integer_Address := To_Integer (LD_Data_End'Address);
      Src , Dst   : Int_Ptr;

   begin
      while Data_Start < Data_End
      loop
         Dst := To_Access (Data_Start);
         Src := To_Access (Data_Load);
         Dst.all := Src.all;                --  copy one word
         Data_Start := Data_Start + 4;      --  next word
         Data_Load := Data_Load + 4;
      end loop;
   end Copy_Data;

   -----------------
   --  Clear_BSS  --
   -------

   procedure Clear_BSS is

      --  Suppress checking pointers from being zero (trust us)
      --  results in much better code
      pragma Suppress (Access_Check);

      type Int_Ptr is access all Integer_Address;

      function To_Access is
        new Ada.Unchecked_Conversion (Integer_Address, Int_Ptr);

      BSS_Start : Integer_Address := To_Integer (LD_BSS_Start'Address);
      BSS_End   : constant Integer_Address := To_Integer (LD_BSS_End'Address);
      Src       : Int_Ptr;

   begin
      while BSS_Start < BSS_End
      loop
         Src := To_Access (BSS_Start);
         Src.all := 0;                      --  clear
         BSS_Start := BSS_Start + 4;
      end loop;
   end Clear_BSS;

end ARM.Architecture;
