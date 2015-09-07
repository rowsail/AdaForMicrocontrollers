------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . B B . T H R E A D S . S T A C K _ C H E C K I N G      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--               Copyright (C) 2004 The European Space Agency               --
--                     Copyright (C) 2004-2013, AdaCore                     --
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
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);
--  We want to guarantee the absence of elaboration code because the binder
--  does not handle references to this package.

with System.Parameters;
with System.BB.Interrupts;

package body System.BB.Threads.Stack_Checking is

   procedure Stack_Check (Stack_Address : System.Address) is
      Self : constant Thread_Id := Thread_Self;

   begin
      --  No stack checking can be performed if the run time has not yet been
      --  initialized.

      --  When Self = Null_Thread_Id it means that there is no task ready to
      --  execute, so only external hardware interrupts can be handled.
      --  Interrupts can also appear when there are tasks ready to execute.

      --  Check whether the stack pointer is outside the stack area, so that
      --  possible wrap-around address 0 is considered.

      if Initialized
        and then
          (Self = Null_Thread_Id
            or else Stack_Address < Self.Bottom_Of_Stack
            or else Stack_Address > Self.Top_Of_Stack)
        and then not Interrupts.Within_Interrupt_Stack (Stack_Address)
      then
         raise Storage_Error;
      end if;
   end Stack_Check;

end System.BB.Threads.Stack_Checking;
