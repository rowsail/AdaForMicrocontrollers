--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                       A R M . S T M 3 2 F 1 0 1 V C                        --
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

with ARM.Link;
with ARM.Architecture.Startup;
with ARM.Registers.FLASH_F1XXXX;

package ARM.STM32F103VC is

   pragma Preelaborate;

   Number_Of_Implemented_Interrupts : constant Natural := 60;
   Number_Of_Implemented_Traps      : constant Natural := 16;
   Has_FPU                          : constant Boolean := False;

   package Startup is new ARM.Architecture.Startup
     (ARM.Link.Link_To_ROM,
      Has_FPU,
      Number_Of_Implemented_Interrupts,
      Number_Of_Implemented_Traps);

   package FLASH renames ARM.Registers.FLASH_F1XXXX;

end ARM.STM32F103VC;
