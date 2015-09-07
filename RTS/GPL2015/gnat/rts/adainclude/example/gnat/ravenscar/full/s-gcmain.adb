------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--       S Y S T E M . G E N E R I C _ C _ M A T H _ I N T E R F A C E      --
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

--  This is the Ada Cert Math specific version of s-gcmain.adb.

--  The separate version is necessary, because this system does not provide
--  an implementation of tanh, among other hyperbolic functions. The run time
--  currently has no code to implement this function, so the only short term
--  option was to remove the hyperbolic functions.

with Ada.Numerics; use Ada.Numerics;

package body System.Generic_C_Math_Interface is

   subtype T is Float_Type'Base;

   --  The implementations of these functions start with a summary
   --  of the Ada requirements for the following:
   --    * Principal branch of multivalued functions
   --    * Conditions for raising exceptions
   --    * Prescribed function results
   --    * Tightly approximated function results (strict mode only)

   --  Implementation choices are explained after the summary for each
   --  elementary function. Exceptions are raised either by checking the
   --  arguments or the C function result. Prescribed results are satisfied by
   --  referring to corresponding requirements in C, standard implementation
   --  practice or by explicit special-casing in the code below.

   --  If one of the arguments of a function is a NaN, the function will return
   --  a NaN value or raise Argument_Error. Generally, for functions that
   --  require Argument_Error to be raised for some arguments will also
   --  raise Argument_Error for NaN arguments.

   --  Many comparisons for special cases are inverted using "not" in order
   --  to make sure the condition is false for NaN values, using the principle
   --  that any comparison involving a NaN argument evaluates to false.

   --  Principal branch:
   --    Describes function result for cases where the mathematical
   --    function is multivalued.

   --  Exceptions:
   --    Describes in what situations exceptions such as
   --    Argument_Error and Constraint_Error must be raised.
   --    In addition to these required exceptions, Constraint_Error
   --    may also be raised instead of yielding an infinity value
   --    for types T where T'Machine_Overflows is True.

   --  Prescribed results:
   --    Describes identities that must be satisfied.

   --  Tightly approximated results:
   --    Describes arguments for which the function result must
   --    be in the model interval of the mathematical result.
   --    This is required for strict mode.

   --  Special values:
   --    These are implementation-defined results arguments with
   --    special values such as infinities (represented by +Inf and -Inf)
   --    not-a-number values (written as NaN). Where consistent with the
   --    Ada standard, the implementation satisfies the identities given
   --    in Chapter F.9 of the C standard.

   ----------
   -- "**" --
   ----------

   --  Principle branch:
   --    The result is nonnegative.

   --  Required exceptions:
   --    Argument_Error is raised when Left < 0.0, Left is a NaN
   --      or when Left = 0.0 and Right = 0.0.
   --    Constraint_Error is raised when Left = 0.0 and Right < 0.0.

   --  Prescribed results:
   --    (1)  Left ** 0.0   = 1.0
   --    (2)  Left ** 1.0   = Left
   --    (3)  0.0  ** Right = 0.0
   --    (4)  1.0  ** Right = 1.0

   --  The prescribed result (1) is satisfied by C_Pow.
   --  Result (2) is not, and therefore is special-cased.
   --  For case (3) this implementation always returns +0.0,
   --  while C_Pow would return -0.0 when Left = -0.0 and Right a positive
   --  odd integer. This would seem inconsistent with the required principle
   --  branch, although it is debatable whether -0.0 is negative.
   --  For case (4), C_Pow would return NaN, so a special case is required.

   function "**" (Left, Right : Float_Type'Base) return Float_Type'Base is
   begin
      if Left <= 0.0 then
         if not (Left = 0.0) or else not (Right /= 0.0) then
            raise Argument_Error;

         elsif not (Right >= 0.0) then
            raise Constraint_Error;

         else
            --  Left = 0.0 and Right > 0.0

            return 0.0;
         end if;

      elsif Right = 1.0 then
         return Left;

      elsif Left = 1.0 then
         return 1.0;
      end if;

      return C_Pow (Left, Right);
   end "**";

   ------------
   -- Arccos --
   ------------

   --  (Natural cycle)

   --  Principal branch:
   --    The result is in the quadrant containing the point (X, 1.0).
   --    This quadrant is I or II; thus, the Arccos function ranges
   --    from 0.0 to approximately Pi.

   --  Exceptions:
   --    Argument_Error is raised when abs (X) > 1.0

   --  Tightly approximated results:
   --    Arccos (0.0) = Pi / 2.0;
   --    Arccos (1.0) = 0.0;

   --  Since C mandates a NaN result for abs (X) > 1.0 and testing
   --  for a NaN only requires a single test without calling the "abs"
   --  function, the result is checked rather than the argument.

   function Arccos (X : Float_Type'Base) return Float_Type'Base is
      R : T;

   begin
      R := C_Acos (X);

      if R /= R then
         raise Argument_Error;
      else
         return R;
      end if;
   end Arccos;

   --  (Arbitrary cycle)

   --  Principal branch:
   --    The result is in the quadrant containing the point (X, 1.0).
   --    This quadrant is I or II; thus, the Arccos function ranges
   --    from 0.0 to approximately Cycle / 2.0.

   --  Exceptions:
   --    Argument_Error is raised when abs (X) > 1.0 or when Cycle <= 0.0
   --      or when either parameter is a NaN

   --  Prescribed results:
   --    Arccos (1.0) = 0.0

   --  Tightly approximated results:
   --    Arccos (0.0) = Cycle / 4.0

   --  Since C mandates a NaN result for abs (X) > 1.0 and testing for a NaN
   --  only requires a single test without calling the "abs" function, the
   --  result is checked rather than the argument. The tightly approximated
   --  result may not be obtained by dividing the C_Acos result by Pi, since
   --  these are transcedental numbers.

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if not (Cycle > 0.0) then
         raise Argument_Error;

      elsif not (abs X < 1.0) then
         if X = 1.0 then
            return 0.0;

         elsif X = -1.0 then
            return Cycle / 2.0;
         end if;

         raise Argument_Error;
      end if;

      if X = 0.0 then
         return Cycle / 4.0;
      end if;

      return C_Acos (X) / (Pi / 2.0) * (Cycle / 4.0);
   end Arccos;

   -------------
   -- Arccosh --
   -------------

   --  Principal branch:
   --    The result is positive

   --  Exceptions:
   --    Argument_Error is raised when X < 1.0

   --  Prescribed results:
   --    Arccosh (1.0) = 0.0;

   --  General description
   --    TODO

   function Arccosh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if X < 1.0 then
         raise Argument_Error;
      else
         return C_Acosh (X);
      end if;
   end Arccosh;

   ------------
   -- Arccot --
   ------------

   --  Natural cycle

   --  Principal branch:
   --    The results are in the quadrant containing the point (X, Y).
   --    This may be any quadrant (I through IV) when the parameter Y is
   --    specified, but it is restricted to quadrants I and II when that
   --    parameter is omitted. Thus the range when that parameter is
   --    specified is approximately -Pi to Pi; when omitted the range is
   --    0.0 to Pi.

   --  Exceptions:
   --    Argument_Error is raised when parameters X and Y both have the
   --    value zero

   --  Prescribed results:
   --    Arccot (X, 0.0) = 0.0 when X > 0.0

   function Arccot
     (X : Float_Type'Base;
      Y : Float_Type'Base := 1.0) return Float_Type'Base
   is
   begin
      if X = 0.0 and then Y = 0.0 then
         raise Argument_Error;
      else

         --  Just reverse arguments

         return Arctan (Y, X);
      end if;
   end Arccot;

   --  Arbitrary cycle

   function Arccot
     (X     : Float_Type'Base;
      Y     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base) return Float_Type'Base
   is
   begin
      if X = 0.0 and then Y = 0.0 then
         raise Argument_Error;

      else
         --  Just reverse arguments

         return Arctan (Y, X, Cycle);
      end if;
   end Arccot;

   -------------
   -- Arccoth --
   -------------

   --  Exceptions:
   --    Argument_Error is raised if abs (X) < 1.0
   --    Constraint_Error is raised if X = +-1.0

   function Arccoth (X : Float_Type'Base) return Float_Type'Base is
   begin
      if abs X <= 1.0 then
         if abs X = 1.0 then
            raise Constraint_Error;
         else
            raise Argument_Error;
         end if;

      elsif abs X > 2.0 then
         return C_Atanh (1.0 / X);

      else
         --  1.0 < abs X <= 2.0. One of X + 1.0 and X - 1.0 is exact, the
         --  other has error 0 or Epsilon.

         return 0.5 * (C_Log (abs (X + 1.0)) - C_Log (abs (X - 1.0)));
      end if;
   end Arccoth;

   ------------
   -- Arcsin --
   ------------

   --  (Natural cycle)

   --  Principal branch:
   --    The result of the Arcsin function is in the quadrant containing the
   --    the point (1.0, X). This quadrant is I or IV; thus, the range of the
   --    function is approximately -Pi/2.0 to Pi/2.0.

   --  Exceptions:
   --    Argument_Error is raised when abs X > 1.0 or X is a NaN

   --  Prescribed results:
   --    Arcsin (0.0) = 0.0

   --  Tightly approximated results:
   --    Arcsin (1.0) = Pi / 2.0
   --    Arcsin (-1.0) = -Pi / 2.0

   --  The prescribed result is guaranteed by C, but the tightly approximated
   --  results are not.

   function Arcsin (X : Float_Type'Base) return Float_Type'Base is
      Y : constant T := abs X;

   begin
      if not (Y < 1.0) then
         if X = 1.0 then
            return Pi / 2.0;

         elsif X = -1.0 then
            return -Pi / 2.0;

         else
            raise Argument_Error;
         end if;
      end if;

      return C_Asin (X);
   end Arcsin;

   --  (Arbitrary cycle)

   --  Principal branch:
   --    The result of the Arcsin function is in the quadrant containing the
   --    the point (1.0, X). This quadrant is I or IV; thus, the range of the
   --    function is approximately -Cycle/4.0 to Cycle/4.0.

   --  Exceptions:
   --    Argument_Error is raised when abs X > 1.0 or X is a NaN
   --      or when Cycle <= 0.0 or Cycle is a NaN

   --  Prescribed results:
   --    Arcsin (0.0) = 0.0

   --  Tightly approximated results:
   --    Arcsin (1.0) = Cycle / 4.0
   --    Arcsin (-1.0) = -Cycle / 4.0

   --  The prescribed result is guaranteed by C, but the tightly approximated
   --  results are not.

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base is
      Y : constant T := abs X;

   begin
      if not (Cycle > 0.0) then
         raise Argument_Error;

      elsif not (Y < 1.0) then
         if X = 1.0 then
            return Cycle / 4.0;

         elsif X = -1.0 then
            return -Cycle / 4.0;

         else
            raise Argument_Error;
         end if;
      end if;

      return C_Asin (X) / (Pi / 2.0) * (Cycle / 4.0);
   end Arcsin;

   -------------
   -- Arcsinh --
   -------------

   --  Prescribed results:
   --    Arcsinh (0.0) = 0.0

   --  TODO - general description

   function Arcsinh (X : Float_Type'Base) return Float_Type'Base is
     (C_Asinh (X));

   ------------
   -- Arctan --
   ------------

   --  (Natural cycle)

   --  Principal branch:
   --    The results are in the quadrant containing the point (X, Y).
   --    This may be any quadrant (I through IV) when the parameter X is
   --    specified, but it is restricted to quadrants I and IV when that
   --    parameter is omitted. Thus the range when that parameter is
   --    specified is approximately -Pi to Pi; when omitted the range is
   --    -Pi/2.0 to Pi/2.0.

   --  Exceptions:
   --    Argument_Error is raised when both X and Y have the value zero.

   --  Prescribed results:
   --    Arctan (  X,  0.0) =  0.0, when X > 0.0

   --  Tightly approximated results:
   --    Arctan (0.0,    Y) =  Pi/2.0, when Y > 0.0
   --    Arctan (0.0,    Y) = -Pi/2.0, when Y < 0.0
   --    Arctan (  X, +0.0) = +Pi, when X < 0.0
   --    Arctan (  X, -0.0) = -Pi, when X < 0.0

   --  The prescribed result and tightly approximated results are all
   --  guaranteed by C.

   function Arctan
     (Y : Float_Type'Base;
      X : Float_Type'Base := 1.0) return Float_Type'Base
   is
   begin
      if not (X /= 0.0) and then not (Y /= 0.0) then
         raise Argument_Error;
      end if;

      return C_Atan2 (Y, X);
   end Arctan;

   --  (Arbitrary cycle)

   --  Principal branch:
   --    The results are in the quadrant containing the point (X, Y).
   --    This may be any quadrant (I through IV) when the parameter X is
   --    specified, but it is restricted to quadrants I and IV when that
   --    parameter is omitted. Thus the range when that parameter is
   --    specified is approximately -Cycle/2.0 to Cycle/2.0; when omitted
   --    the range is -Cycle/4.0 to Cycle/4.0.

   --  Exceptions:
   --    Argument_Error is raised when both X and Y have the value zero,
   --    or when Cycle <= 0.0 or Cycle is a NaN.

   --  Prescribed results:
   --    Arctan (  X,  0.0, Cycle) =  0.0, when X > 0.0

   --  Tightly approximated results:
   --    Arctan (0.0,    Y, Cycle) =  Cycle/4.0, when Y > 0.0
   --    Arctan (0.0,    Y, Cycle) = -Cycle/4.0, when Y < 0.0
   --    Arctan (  X, +0.0, Cycle) =  Cycle/2.0, when X < 0.0
   --    Arctan (  X, -0.0, Cycle) = -Cycle/2.0, when X < 0.0

   --  The prescribed result and tightly approximated results are all
   --  guaranteed by C.

   function Arctan
     (Y     : Float_Type'Base;
      X     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base) return Float_Type'Base
   is
   begin
      if not (Cycle > 0.0) then
         raise Argument_Error;
      end if;

      if X = 0.0 then
         if Y = 0.0 then
            raise Argument_Error;

         elsif Y > 0.0 then
            return Cycle / 4.0;

         elsif Y < 0.0 then
            return -Cycle / 4.0;
         end if;

         --  Y is a NaN

      elsif Y = 0.0 then
         --  X /= 0

         if X > 0.0 then
            return 0.0;

         elsif X < 0.0 then
            return T'Copy_Sign (Cycle / 2.0, Y);
         end if;

         --  X is a NaN
      end if;

      return C_Atan2 (Y, X) * Cycle / (2.0 * Pi);
   end Arctan;

   -------------
   -- Arctanh --
   -------------

   --  Exceptions:
   --    Argument_Error is raised when abs (X) > 1.0
   --    Constraint_Error is raised when X = +-1.0

   --  Prescribed results:
   --    Arctanh (0.0) = 0.0

   --  TODO - general description

   function Arctanh (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not (abs (X) < 1.0) then
         if abs (X) = 1.0 then
            raise Constraint_Error;
         else
            raise Argument_Error;
         end if;
      else
         return C_Atanh (X);
      end if;
   end Arctanh;

   ---------
   -- Cos --
   ---------

   --  (Natural cycle)

   --  Prescribed results:
   --    Cos (0.0) = 1.0

   --  Special values:
   --    Cos (X), where X is positive or negative infinity returns NaN value

   --  The C_Cos function satisfies all requirements

   function Cos (X : Float_Type'Base) return Float_Type'Base is
   begin
      return C_Cos (X);
   end Cos;

   --  (Arbitrary cycle)

   --  Exceptions:
   --    Argument_Error is raised when Cycle <= 0

   --  Prescribed results:
   --    Cos (X) = 0.0, when X is K * Cycle / 4.0 with odd integer K
   --    Cos (X) = 1.0, when X is K * Cycle, with integer K
   --    Cos (X) = -1.0, with X is K * Cycle / 2.0, with odd integer K

   --  Special values:
   --    Cos (X), where X is positive or negative infinity returns a
   --    NaN value.

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      --  Just reuse the code for Sin. The potential small
      --  loss of speed is negligible with proper (front-end) inlining.

      return -Sin (abs X - Cycle * 0.25, Cycle);
   end Cos;

   ----------
   -- Cosh --
   ----------

   --  Prescribed results:
   --    Cosh (0.0) = 1.0

   --  Tightly approximated results:
   --    TODO

   --  TODO - general description

   function Cosh (X : Float_Type'Base) return Float_Type'Base is
     (C_Cosh (X));

   ---------
   -- Cot --
   ---------

   --  (natural cycle)

   --  Exceptions:
   --    Constraint_Error is raised when X = 0.0

   --  As there is no cotangent function defined for C99, it is implemented
   --  here in terms of the regular tangent function.

   function Cot (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not (X /= 0.0) then
         raise Constraint_Error;
      else
         return 1.0 / C_Tan (X);
      end if;
   end Cot;

   --  (arbitrary cycle)

   --  Exceptions:
   --    Argument_Error is raised when Cycle <= 0
   --    Constraint_Error is raised when X = K * Cycle / 2.0, with integer K

   --  Prescribed results:
   --    Cot (X) = 0.0, when X is K * Cycle / 4.0 with odd integer K

   --  Special values:
   --    Cot (X), where X is positive or negative infinity returns NaN value

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base is
      T, TA : Float_Type'Base;

   begin
      if not (Cycle > 0.0) then
         raise Argument_Error;
      end if;

      T := Float_Type'Base'Remainder (X, Cycle) / Cycle;
      TA := abs T;

      if not (T /= 0.0 and then TA /= 0.5) then
         raise Constraint_Error;
      end if;

      if TA = 0.25 then
         return 0.0;
      end if;

      return 1.0 / C_Tan (T * 2.0 * Pi);
   end Cot;

   ----------
   -- Coth --
   ----------

   --  Exceptions:
   --    Argument_Error is raised when X = 0.

   --  Tightly approximated results:
   --    TODO

   --  TODO - general description

   function Coth (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not (X /= 0.0) then
         raise Argument_Error;
      else
         return 1.0 / C_Tanh (X);
      end if;
   end Coth;

   ---------
   -- Exp --
   ---------

   --  Prescribed results:
   --    Exp (0.0) = 1.0

   --  Special values:
   --    Exp (X) = +0.0, for X is negative infinity
   --    Exp (X) = X, for X is positive infinity
   --      and Float_Type'Machine_Overflows = False

   --  The C_Exp function satisfies all Ada requirements

   function Exp (X : Float_Type'Base) return Float_Type'Base is
   begin
      return C_Exp (X);
   end Exp;

   ---------
   -- Log --
   ---------

   --  (natural base)

   --  Exceptions:
   --    Argument is raised when X < 0.0
   --    Constraint_Error is raised when X = 0.0

   --  Prescribed results:
   --    Log (1.0) = 0.0;

   --  Special values:
   --    Log (X) = X, for X is positive infinity

   --  Apart from exceptions, the C_Log function satisfies all constraints

   function Log (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not (X > 0.0) then
         if X < 0.0 then
            raise Argument_Error;
         end if;

         raise Constraint_Error;
      end if;

      return C_Log (X);
   end Log;

   --  (arbitrary base)

   --  Exceptions:
   --    Argument is raised when X < 0.0, Base <= 0.0 or Base = 1.0
   --    Constraint_Error is raised when X = 0.0

   --  Prescribed results:
   --    Log (1.0, Base) = 0.0

   --  Special values:
   --    Log (X, Base) = X, for X is positive infinity

   --  Apart from exceptions, the C_Log function satisfies all constraints

   function Log (X, Base : Float_Type'Base) return Float_Type'Base is
   begin
      --  Try to execute the common case of X > 0.0 and Base > 1.0 with
      --  minimal checks.

      if X <= 0.0 or else Base <= 1.0 then
         if X < 0.0 or else Base <= 0.0 or else Base = 1.0 then
            raise Argument_Error;
         end if;

         if X = 0.0 then
            raise Constraint_Error;
         end if;
      end if;

      return C_Log (X) / C_Log (Base);
   end Log;

   ---------
   -- Sin --
   ---------

   --  (Natural cycle)

   --  Prescribed results:
   --    Sin (+0.0) = +0.0
   --    Sin (-0.0) = -0.0

   --  Special values:
   --    Sin (X), where X is positive or negative infinity returns a
   --    NaN value.

   --  The C_Sin function satisfies all requirements

   function Sin (X : Float_Type'Base) return Float_Type'Base is
   begin
      return C_Sin (X);
   end Sin;

   --  (Arbitrary cycle)

   --  Exceptions:
   --    Argument_Error is raised when Cycle <= 0

   --  Prescribed results:
   --    Sin (-0.0) = -0.0
   --    Sin (+0.0) = +0.0
   --    Sin (X) = 1.0, when X is K * Cycle + Cycle / 4.0, with integer K
   --    Sin (X) = -1.0, with X is K * Cycle - Cycle / 4.0, with integer K

   --  Special values:
   --    Sin (X), where X is positive or negative infinity returns NaN value

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base is
      T : Float_Type'Base;

   begin
      if not (Cycle > 0.0) then
         raise Argument_Error;
      end if;

      T := Float_Type'Base'Remainder (X, Cycle);

      --  The following reduction reduces the argument to the interval
      --  [-0.5 Cycle, 0.5 * Cycle]. The entire reduction is exact.

      if T > 0.25 * Cycle then
         T := 0.5 * Cycle - T;

      elsif T < -0.25 * Cycle then
         T := -T - 0.5 * Cycle;
      end if;

      return C_Sin (T / Cycle * 2.0 * Pi);
   end Sin;

   ----------
   -- Sinh --
   ----------

   --  Prescribed results:
   --    Sinh (0.0) = 0.0

   --  TODO - general description

   function Sinh (X : Float_Type'Base) return Float_Type'Base is
     (C_Sinh (X));

   ----------
   -- Sqrt --
   ----------

   --  Principle branch:
   --    The result is nonnegative.

   --  Exceptions:
   --    Argument_Error is raised when X < 0.0

   --  Prescribed results:
   --    Sqrt (-0.0) = -0.0
   --    Sqrt (+0.0) = +0.0
   --    Sqrt (1.0) = 1.0

   --  Special values:
   --    Sqrt (X) = X, for X is positive infinity

   --  C_Sqrt satisfies all requirements

   function Sqrt (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not (X >= 0.0) then
         raise Argument_Error;
      end if;

      return C_Sqrt (X);
   end Sqrt;

   ---------
   -- Tan --
   ---------

   --  (natural cycle)

   --  Prescribed results:
   --    Tan (-0.0) = -0.0
   --    Tan (+0.0) = +0.0

   --  Special values:
   --    Tan (X) returns a NaN value, when X is positive or negative infinity

   --  The C_Tan function satisfies all requirements

   function Tan (X : Float_Type'Base) return Float_Type'Base is
   begin
      return C_Tan (X);
   end Tan;

   --  (arbitrary cycle)

   --  Exceptions:
   --    Argument_Error is raised for Cycle <= 0.0

   --  Prescribed results:
   --    Tan (-0.0, Cycle) = -0.0
   --    Tan (+0.0, Cycle) = +0.0
   --    Tan (X, Cycle) = 0, for X a multiple of Cycle / 2.0

   --  Special values:
   --    Tan (X, Cycle) returns a NaN value, when X is positive or
   --    negative infinity

   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base is
      T  : Float_Type'Base;
      TA : Float_Type'Base;

   begin
      if not (Cycle > 0.0) then
         raise Argument_Error;
      end if;

      T := Float_Type'Base'Remainder (X, Cycle) / Cycle;
      TA := abs T;

      --  The TA = 0.75 case is not needed because the remainder function
      --  is defined so that it never returns a value greater than Cycle/2,
      --  the value of TA will always be less than or equal to 0.5. Therefore,
      --  the condition TA = 0.75 can never be true.

      if TA = 0.25 then
         raise Constraint_Error;
      end if;

      if TA = 0.5 then
         return 0.0;
      end if;

      return C_Tan (T * 2.0 * Pi);
   end Tan;

   ----------
   -- Tanh --
   ----------

   --  Principal branch:
   --    The absolute value of the result is smaller than 1.0

   --  Prescribed results:
   --    Tanh (0.0) = 0.0

   --  TODO - general description

   function Tanh (X : Float_Type'Base) return Float_Type'Base is
     (C_Tanh (X));

end System.Generic_C_Math_Interface;
