-------------------------------------------------------------------------------
--                                                                           --
--                       A R M   A D A   L I B R A R Y                       --
--                                                                           --
--               S Y S T E M . R e g i s t e r . B i t f i e l d             --
--                               G e n e r i c                               --
--                                                                           --
--    Copyright (C) 2015  Robert Kleczek                                     --
--                                                                           --
--    This program is free software: you can redistribute it and/or modify   --
--    it under the terms of the GNU General Public License as published by   --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    This program is distributed in the hope that it will be useful,        --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--    GNU General Public License for more details.                           --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                           --
-------------------------------------------------------------------------------

--  Some generics for Registers usage

------------------------
--  Imported Packages --
------------------------

with System.Register.Types;

-------------------------------------------------------------------------------
--                           SYSTEM.Register.Bitfield                        --
-------------------------------------------------------------------------------

generic
   with package Tp is new Types (<>);
   Pos  : Natural;       --  Position of first bit in register
   Len  : Natural := 1;  --  Length of bit field
package System.Register.Bitfield is

   pragma Preelaborate;

   ----------------------
   --  Renaming types  --
   ----------------------
   subtype RM_T is Tp.F_T;
   subtype RC_T is Tp.RC_T;  --  common register type

   use type RC_T;

   -------------
   --  Types  --
   -------------

   type B_T     is new RC_T range 0 .. 2 ** Len - 1;  --  Bitfield type

   subtype BP_T is Natural range Pos .. Pos + Len - 1;  --  Bitfield position

   -----------------
   --  Constants  --
   -----------------

   F   : constant Natural := BP_T'First;  --  lowest bound of field
   L   : constant Natural := BP_T'Last;   --  highest bound of field

   Mlt : constant RC_T   := 2 ** Pos;     --  bitfield position multiplier

   --------------------------------
   --  Bitfield access routines  --
   --------------------------------

   function Msk return RC_T
     with
       Inline_Always;

   function B (Value : B_T) return RM_T
     with
       Inline_Always;
   --  Function returning bit field variable value

end System.Register.Bitfield;
