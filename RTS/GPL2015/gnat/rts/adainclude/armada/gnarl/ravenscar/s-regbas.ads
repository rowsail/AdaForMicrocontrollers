-------------------------------------------------------------------------------
--                                                                           --
--                       A R M   A D A   L I B R A R Y                       --
--                                                                           --
--                  S Y S T E M . R e g i s t e r . B a s e                  --
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
with Ada.Unchecked_Conversion;

-------------------------------------------------------------------------------
--                            SYSTEM.Register.Base                           --
-------------------------------------------------------------------------------

generic
   type R_T  is mod <>;  --  base register type
   type RV_T is mod <>;  --  base volatile register type
   Addr    : Address;    --  address of registers subsystem
   Size    : Address;    --  size in bytes of registers subsystem
package System.Register.Base is

   pragma Preelaborate;

   -------------
   --  Types  --
   -------------

   --  Type enclosing permitted range of addresses
   subtype Adr_T is Address range 0 .. Size - 1;

   --  Type enclosing permitted index of registers region
   pragma Warnings (Off);   --  prevent "Constraint_Error" warning
   --  That warning will never happend because divider should always be > 0
   subtype Idx_T is Address range 0 .. (Size - 1) / (R_T'Size / 8);
   pragma Warnings (On);   --  switch warnings again

   --  Type defining registers region of volatile registers
   type Registers_T is array (Idx_T) of aliased RV_T;
   pragma Suppress_Initialization (Registers_T);
   pragma Pack (Registers_T);

   -----------------
   --  Variables  --
   -----------------

   --  Instiantiate registers region
   Registers : Registers_T;
   for Registers'Address use System'To_Address (Addr);

   --------------------------------
   --  Register access routines  --
   --------------------------------

   procedure Set (Adr : Adr_T; Val : R_T) with Inline_Always;

   function Get (Adr : Adr_T) return R_T with Inline_Always;

end System.Register.Base;
