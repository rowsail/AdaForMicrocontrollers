--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 A R M . A r c h i t e c t u r e . A S M                    --
--                                  S p e c                                   --
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

--  Architecture assembler needed routines

--------------------------------------------------------------------------------
--                              ARM.Architecture.ASM                          --
--------------------------------------------------------------------------------

package ARM.Architecture.ASM is

   pragma Preelaborate;

   pragma Warnings (Off);
   --  Data Synchronization Barrier
   procedure DSB
     with Inline_Always;
   pragma Machine_Attribute (DSB, "naked");
   pragma Warnings (On);

   pragma Warnings (Off);
   --  Instruction Synchronization Barrier
   procedure ISB
     with Inline_Always;
   pragma Machine_Attribute (ISB, "naked");
   pragma Warnings (On);

   pragma Warnings (Off);
   --  Instruction Synchronization Barrier
   procedure Set_Stack (Stack : System.Address)
     with Inline_Always;
   pragma Machine_Attribute (Set_Stack, "naked");
   pragma Warnings (On);

end ARM.Architecture.ASM;
