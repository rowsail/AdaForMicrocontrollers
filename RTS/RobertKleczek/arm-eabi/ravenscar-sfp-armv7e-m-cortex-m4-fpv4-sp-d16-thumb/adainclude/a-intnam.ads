------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1991-2013, Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the version for Cortex M4F STM32F4 targets

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   --  The STM32F4X reference manual defines the interrupt in Table 45
   --  (page 250 of Doc ID 018909 Rev 4). The meaningful number, the position
   --  starts at 0. Unfortunately, Interrupt_ID 0 is reserved and the SysTick
   --  interrupt (a core interrupt) is handled by the runtime like other
   --  interrupts. So the first interrupt (window watchdog) is numbered 2 while
   --  it is at position 0 in the manual. The offset of 2 is reflected in
   --  s-bbbosu-stm32f4.adb by the First_IRQ constant.

   INT_1                  : constant Interrupt_ID := 1;
   INT_2                  : constant Interrupt_ID := 2;
   INT_3                  : constant Interrupt_ID := 3;
   INT_4                  : constant Interrupt_ID := 4;
   INT_5                  : constant Interrupt_ID := 5;
   INT_6                  : constant Interrupt_ID := 6;
   INT_7                  : constant Interrupt_ID := 7;
   INT_8                  : constant Interrupt_ID := 8;
   INT_9                  : constant Interrupt_ID := 9;
   INT_10                 : constant Interrupt_ID := 10;
   INT_11                 : constant Interrupt_ID := 11;
   INT_12                 : constant Interrupt_ID := 12;
   INT_13                 : constant Interrupt_ID := 13;
   INT_14                 : constant Interrupt_ID := 14;
   INT_15                 : constant Interrupt_ID := 15;
   INT_16                 : constant Interrupt_ID := 16;
   INT_17                 : constant Interrupt_ID := 17;
   INT_18                 : constant Interrupt_ID := 18;
   INT_19                 : constant Interrupt_ID := 19;
   INT_20                 : constant Interrupt_ID := 20;
   INT_21                 : constant Interrupt_ID := 21;
   INT_22                 : constant Interrupt_ID := 22;
   INT_23                 : constant Interrupt_ID := 23;
   INT_24                 : constant Interrupt_ID := 24;
   INT_25                 : constant Interrupt_ID := 25;
   INT_26                 : constant Interrupt_ID := 26;
   INT_27                 : constant Interrupt_ID := 27;
   INT_28                 : constant Interrupt_ID := 28;
   INT_29                 : constant Interrupt_ID := 29;
   INT_30                 : constant Interrupt_ID := 30;
   INT_31                 : constant Interrupt_ID := 31;
   INT_32                 : constant Interrupt_ID := 32;
   INT_33                 : constant Interrupt_ID := 33;
   INT_34                 : constant Interrupt_ID := 34;
   INT_35                 : constant Interrupt_ID := 35;
   INT_36                 : constant Interrupt_ID := 36;
   INT_37                 : constant Interrupt_ID := 37;
   INT_38                 : constant Interrupt_ID := 38;
   INT_39                 : constant Interrupt_ID := 39;
   INT_40                 : constant Interrupt_ID := 40;
   INT_41                 : constant Interrupt_ID := 41;
   INT_42                 : constant Interrupt_ID := 42;
   INT_43                 : constant Interrupt_ID := 43;
   INT_44                 : constant Interrupt_ID := 44;
   INT_45                 : constant Interrupt_ID := 45;
   INT_46                 : constant Interrupt_ID := 46;
   INT_47                 : constant Interrupt_ID := 47;
   INT_48                 : constant Interrupt_ID := 48;
   INT_49                 : constant Interrupt_ID := 49;
   INT_50                 : constant Interrupt_ID := 50;
   INT_51                 : constant Interrupt_ID := 51;
   INT_52                 : constant Interrupt_ID := 52;
   INT_53                 : constant Interrupt_ID := 53;
   INT_54                 : constant Interrupt_ID := 54;
   INT_55                 : constant Interrupt_ID := 55;
   INT_56                 : constant Interrupt_ID := 56;
   INT_57                 : constant Interrupt_ID := 57;
   INT_58                 : constant Interrupt_ID := 58;
   INT_59                 : constant Interrupt_ID := 59;
   INT_60                 : constant Interrupt_ID := 60;
   INT_61                 : constant Interrupt_ID := 61;
   INT_62                 : constant Interrupt_ID := 62;
   INT_63                 : constant Interrupt_ID := 63;
   INT_64                 : constant Interrupt_ID := 64;
   INT_65                 : constant Interrupt_ID := 65;
   INT_66                 : constant Interrupt_ID := 66;
   INT_67                 : constant Interrupt_ID := 67;
   INT_68                 : constant Interrupt_ID := 68;
   INT_69                 : constant Interrupt_ID := 69;
   INT_70                 : constant Interrupt_ID := 70;
   INT_71                 : constant Interrupt_ID := 71;
   INT_72                 : constant Interrupt_ID := 72;
   INT_73                 : constant Interrupt_ID := 73;
   INT_74                 : constant Interrupt_ID := 74;
   INT_75                 : constant Interrupt_ID := 75;
   INT_76                 : constant Interrupt_ID := 76;
   INT_77                 : constant Interrupt_ID := 77;
   INT_78                 : constant Interrupt_ID := 78;
   INT_79                 : constant Interrupt_ID := 79;
   INT_80                 : constant Interrupt_ID := 80;
   INT_81                 : constant Interrupt_ID := 81;
   INT_82                 : constant Interrupt_ID := 82;
   INT_83                 : constant Interrupt_ID := 83;

end Ada.Interrupts.Names;
