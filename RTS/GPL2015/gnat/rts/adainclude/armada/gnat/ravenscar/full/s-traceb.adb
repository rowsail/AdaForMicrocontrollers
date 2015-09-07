------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2014, Free Software Foundation, Inc.         --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This is the bare board version of this package for ARM EABI targets, using
--  unwind tables.

with Ada.Unchecked_Conversion;

package body System.Traceback is

   use System.Traceback_Entries;

   type Unwind_Reason_Code is
     (URC_OK,
      URC_FOREIGN_EXCEPTION_CAUGHT,
      URC_END_OF_STACK,
      URC_HANDLER_FOUND,
      URC_INSTALL_CONTEXT,
      URC_CONTINUE_UNWIND,
      URC_FAILURE);
   pragma Convention (C, Unwind_Reason_Code);
   --  The _Unwind_Reason_Code enum defined by ARM EHABI document

   pragma Unreferenced (URC_FOREIGN_EXCEPTION_CAUGHT,
                        URC_END_OF_STACK,
                        URC_HANDLER_FOUND,
                        URC_INSTALL_CONTEXT,
                        URC_CONTINUE_UNWIND);

   type Unwind_Context_Type is null record;
   type Unwind_Context_Acc is access Unwind_Context_Type;
   pragma Convention (C, Unwind_Context_Acc);
   --  Access to the opaque _Unwind_Context type

   type Unwind_Trace_Fn is access
     function (UC : Unwind_Context_Acc; Data : System.Address)
              return Unwind_Reason_Code;
   pragma Convention (C, Unwind_Trace_Fn);
   --  The _Unwind_Trace_Fn function (used for the callback)

   function Unwind_Backtrace
     (Func : Unwind_Trace_Fn;
      Data : System.Address) return Unwind_Reason_Code;
   pragma Import (C, Unwind_Backtrace, "_Unwind_Backtrace");
   --  The _Unwind_Backtrace function that calls Func with Data for each frame

   function Unwind_VRS_Get
     (UC        : Unwind_Context_Acc;
      Reg_Class : Integer;
      Reg_Num   : Integer;
      Data_Rep  : Integer;
      Addr      : System.Address) return Integer;
   pragma Import (C, Unwind_VRS_Get, "_Unwind_VRS_Get");
   --  The _Unwind_VRS_Get function to extract a register from the unwind
   --  context UC.

   UVRSR_OK : constant Integer := 0;
   --  Success return status for Unwind_VRS_Get

   UVRSC_CORE : constant Integer := 0;
   --  Core register class for Unwind_VRS_Get

   UVRSD_UINT32 : constant Integer := 0;
   --  Unsigned int 32 data representation for Unwind_VRS_Get

   type Tracebacks_Array_Ptr is access Tracebacks_Array (Positive);

   type Callback_Params_Type is record
      Tracebacks  : Tracebacks_Array_Ptr;
      Max_Len     : Natural;
      Len         : Natural;
      Exclude_Min : System.Address;
      Exclude_Max : System.Address;
      Skip_Frames : Natural;
   end record;
   --  This record contains the parameters for Call_Chain to be passed to
   --  the callback. We could have used a nested subprogram, but as we are
   --  interfacing with C (in bare board context), we prefer to use an
   --  explicit mechanism.

   type Callback_Params_Acc is access all Callback_Params_Type;

   function Backtrace_Callback
     (UC   : Unwind_Context_Acc;
      Data : System.Address) return Unwind_Reason_Code;
   pragma Convention (C, Backtrace_Callback);
   --  The callback for _Unwind_Backtrace, which is called for each frame

   ------------------------
   -- Backtrace_Callback --
   ------------------------

   function Backtrace_Callback
     (UC   : Unwind_Context_Acc;
      Data : System.Address) return Unwind_Reason_Code
   is
      function To_Callback_Params is new Ada.Unchecked_Conversion
        (System.Address, Callback_Params_Acc);
      Params : constant Callback_Params_Acc := To_Callback_Params (Data);
      --  The parameters of Call_Chain

      PC : System.Address;

   begin
      --  Exclude Skip_Frames frames from the traceback.

      if Params.Skip_Frames > 0 then
         Params.Skip_Frames := Params.Skip_Frames - 1;
         return URC_OK;
      end if;

      --  If the backtrace is full, simply discard new entries

      if Params.Len >= Params.Max_Len then
         return URC_OK;
      end if;

      --  Extract the PC (register 15)

      if Unwind_VRS_Get (UC, UVRSC_CORE, 15, UVRSD_UINT32, PC'Address) /=
                                                                    UVRSR_OK
      then
         return URC_FAILURE;
      end if;

      --  Discard exluded values

      if PC in Params.Exclude_Min .. Params.Exclude_Max then
         return URC_OK;
      end if;

      --  Append an entry

      Params.Len := Params.Len + 1;
      Params.Tracebacks (Params.Len) := PC;

      return URC_OK;
   end Backtrace_Callback;

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain
     (Traceback   : in out System.Traceback_Entries.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural        := 1)
   is
      function To_Tracebacks_Array_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Tracebacks_Array_Ptr);

      Params : aliased Callback_Params_Type;

      Res : Unwind_Reason_Code;
      pragma Unreferenced (Res);

   begin
      --  Copy parameters; add 1 to Skip_Frames to ignore the caller of
      --  Call_Chain.

      Params := (Tracebacks  => To_Tracebacks_Array_Ptr (Traceback'Address),
                 Len         => 0,
                 Max_Len     => Max_Len,
                 Exclude_Min => Exclude_Min,
                 Exclude_Max => Exclude_Max,
                 Skip_Frames => Skip_Frames + 1);

      --  Call the unwinder

      Res := Unwind_Backtrace (Backtrace_Callback'Access, Params'Address);

      --  Copy the result

      Len := Params.Len;
   end Call_Chain;

end System.Traceback;
