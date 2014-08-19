--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                   B o a r d . L P C 2 1 4 8 _ B o a r d                    --
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

with STM32F103VC.Startup;
--  Used for include startup routines for STM32F103VC

package Board.STM32F103VC_Board is new STM32F103VC.Startup
  (Crystal_Frequency        =>  8_000_000,
   Desired_Frequency        => 60_000_000,
   Peripheral_Clock_Divider => 1);

pragma Preelaborate (Board.STM32F103VC_Board);
--  NECESSARY !!!  NECESSARY !!!  NECESSARY !!!  NECESSARY !!!
--  Preelaborate is necessary to include startup routines by compiler