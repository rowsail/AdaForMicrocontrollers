------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . P A R A M E T E R S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2015, AdaCore                     --
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
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines basic parameters used by the low level tasking system

--  This is the TMS570 (ARMv7) version of this package

pragma Restrictions (No_Elaboration_Code);

with System.STM32F4;
with System.STM32F4.RCC;

package System.BB.Parameters is
   pragma Preelaborate (System.BB.Parameters);

   Clock_Frequency : constant := 168_000_000;
   pragma Assert (Clock_Frequency in STM32F4.RCC.SYSCLK_Range);

   --  Set the requested SYSCLK frequency. Setup_Pll will try to set configure
   --  PLL to match this value when possible or reset the board.

   ----------------
   -- Prescalers --
   ----------------

   AHB_PRE  : constant STM32F4.RCC.CFGR.AHB_PRE_Value  :=
                STM32F4.RCC.CFGR.AHBPRE_DIV1;
   APB1_PRE : constant STM32F4.RCC.CFGR.APB1_PRE_Value :=
                STM32F4.RCC.CFGR.APB1PRE_DIV4;
   APB2_PRE : constant STM32F4.RCC.CFGR.APB2_PRE_Value :=
                STM32F4.RCC.CFGR.APB2PRE_DIV2;

   --------------------
   -- External Clock --
   --------------------

   --  The external clock can be specific for each board. We provide here
   --  values for the most common STM32 boards that we identify with the
   --  MCU used. Change the return value based on the external clock used on
   --  your specific hardware.

   function HSE_Clock
     (Device_ID : STM32F4.Bits_12) return STM32F4.RCC.HSECLK_Range
   is (case Device_ID is
          when STM32F4.DEV_ID_STM32F42xxx => 8_000_000,
          --  STM32F429 Disco board
          when STM32F4.DEV_ID_STM32F7xxxx => 25_000_000,
          --  STM32F7 Evaluation board
          when others => 8_000_000)
          --  STM32F407 Disco board and Unknown device
   with Inline_Always;

   Has_FPU : constant Boolean := True;
   --  Set to true if core has a FPU

   ----------------
   -- Interrupts --
   ----------------

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   Number_Of_Interrupt_ID : constant := 85;
   --  Number of interrupts (for both the interrupt controller and the
   --  Sys_Tick_Trap). This static constant is used to declare a type, and
   --  the handler table.

   Trap_Vectors : constant := 17;
   --  While on this target there is little difference between interrupts
   --  and traps, we consider the following traps:
   --
   --    Name                        Nr
   --
   --    Reset_Vector                 1
   --    NMI_Vector                   2
   --    Hard_Fault_Vector            3
   --    Mem_Manage_Vector            4
   --    Bus_Fault_Vector             5
   --    Usage_Fault_Vector           6
   --    SVC_Vector                  11
   --    Debug_Mon_Vector            12
   --    Pend_SV_Vector              14
   --    Sys_Tick_Vector             15
   --    Interrupt_Request_Vector    16
   --
   --  These trap vectors correspond to different low-level trap handlers in
   --  the run time. Note that as all interrupt requests (IRQs) will use the
   --  same interrupt wrapper, there is no benefit to consider using separate
   --  vectors for each.

   Context_Buffer_Capacity : constant := 10;
   --  The context buffer contains registers r4 .. r11 and the SP_process
   --  (PSP). The size is rounded up to an even number for alignment

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Size : constant := 2 * 1024;
   --  Size of each of the interrupt stacks in bytes. While there nominally is
   --  an interrupt stack per interrupt priority, the entire space is used as a
   --  single stack.

   Interrupt_Sec_Stack_Size : constant := 128;
   --  Size of the secondary stack for interrupt handlers

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 1;
   --  Maximum number of CPUs

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board?

end System.BB.Parameters;
