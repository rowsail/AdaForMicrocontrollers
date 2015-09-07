------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . S Y N C H R O N O U S _ T A S K _ C O N T R O L          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
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

--  This is the generic bare board version of this package

with System.Task_Primitives.Operations;

package body Ada.Synchronous_Task_Control is

   -------------------
   -- Current_State --
   -------------------

   function Current_State (S : Suspension_Object) return Boolean is
   begin
      --  Read atomically without locking

      return S.Open;
   end Current_State;

   ---------------
   -- Set_False --
   ---------------

   procedure Set_False (S : in out Suspension_Object) is
   begin
      --  Write atomically without locking

      S.Open := False;
   end Set_False;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
      Self_Id   : constant System.Tasking.Task_Id := System.Tasking.Self;
      Self_Prio : constant System.Any_Priority :=
                    System.Task_Primitives.Operations.Get_Priority (Self_Id);

      T_Id      : System.Tasking.Task_Id;

      use type System.Tasking.Task_Id;

   begin
      --  Raise the priority to prevent race conditions when waking up a task

      System.Task_Primitives.Operations.Set_Priority
        (Self_Id, System.Any_Priority'Last);

      --  Wake up the suspended task

      if S.Id /= System.Tasking.Null_Task then
         T_Id := S.Id;
         S.Open := False; --  Should be false anyway
         S.Id := System.Tasking.Null_Task;

         System.Task_Primitives.Operations.Wakeup
           (T_Id, System.Tasking.Entry_Caller_Sleep);

      --  Set the object to open

      else
         S.Open := True;
      end if;

      --  Restore the original priority

      System.Task_Primitives.Operations.Set_Priority (Self_Id, Self_Prio);
   end Set_True;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
      Self_Id   : constant System.Tasking.Task_Id := System.Tasking.Self;
      Self_Prio : constant System.Any_Priority :=
                    System.Task_Primitives.Operations.Get_Priority (Self_Id);

      use type System.Tasking.Task_Id;

   begin
      --  For this run time, pragma Detect_Blocking is always active, so we
      --  must raise Program_Error if this potentially blocking operation is
      --  called from a protected action (RM H.5 (5/2)).

      if Self_Id.Common.Protected_Action_Nesting > 0 then
         raise Program_Error;
      end if;

      --  Raise the priority to prevent race conditions when waking up a task

      System.Task_Primitives.Operations.Set_Priority
        (Self_Id, System.Any_Priority'Last);

      if S.Id = System.Tasking.Null_Task then
         if not S.Open then

            --  Register the suspended task

            S.Id := Self_Id;

            --  There is a potential race condition between the Wakeup and the
            --  Sleep below (the Wakeup may be called before the Sleep). This
            --  case is explicitly handled in the Sleep and Wakeup procedures:
            --  Sleep won't block if Wakeup has been called before.

            --  The reason we change the priority before going to sleep is that
            --  if we do it the other way around, it affects the placement in
            --  the ready queue of the task once it is awakened.

            System.Task_Primitives.Operations.Set_Priority
              (Self_Id, Self_Prio);

            --  Suspend the task

            Self_Id.Common.State := System.Tasking.Entry_Caller_Sleep;
            System.Task_Primitives.Operations.Sleep
              (Self_Id, System.Tasking.Entry_Caller_Sleep);
            Self_Id.Common.State := System.Tasking.Runnable;

         --  The task becomes ready and the state of the object becomes False

         else
            S.Open := False;
            System.Task_Primitives.Operations.Set_Priority
              (Self_Id, Self_Prio);

         end if;

      --  Program_Error is raised upon calling Suspend_Until_True if another
      --  task is already waiting on this suspension object (RM D.10 (10)).

      else
         System.Task_Primitives.Operations.Set_Priority (Self_Id, Self_Prio);
         raise Program_Error;
      end if;
   end Suspend_Until_True;

end Ada.Synchronous_Task_Control;
