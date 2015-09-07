------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                ADA.NUMERICS.GENERIC_ELEMENTARY_FUNCTIONS                 --
--                                                                          --
--                                 S p e c                                  --
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

--  @llrset a-ngelfu.ads
--  Generic_Elementary_Functions
--  ============================

--  This is the Ada Cert Math version of a-ngelfu.ads

generic
   type Float_Type is digits <>;

package Ada.Numerics.Generic_Elementary_Functions is
pragma Pure (Generic_Elementary_Functions);

   function Sqrt (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Sqrt (Float_Type)
   --  This function shall return the square root of <X>.
     Post => Sqrt'Result >= 0.0
       and then (if X = 0.0 then Sqrt'Result = 0.0)
       and then (if X = 1.0 then Sqrt'Result = 1.0)

       --  Finally if X is positive, the result of Sqrt is positive (because
       --  the sqrt of numbers greater than 1 is greater than or equal to 1,
       --  and the sqrt of numbers less than 1 is greater than the argument).

       --  This property is useful in particular for static analysis. The
       --  property that X is positive is not expressed as (X > 0.0), as
       --  the value X may be held in registers that have larger range and
       --  precision on some architecture (for example, on x86 using x387
       --  FPU, as opposed to SSE2). So, it might be possible for X to be
       --  2.0**(-5000) or so, which could cause the number to compare as
       --  greater than 0, but Sqrt would still return a zero result.

       --  Note: we use the comparison with Succ (0.0) here because this is
       --  more amenable to CodePeer analysis than the use of 'Machine.

       and then (if X >= Float_Type'Succ (0.0) then Sqrt'Result > 0.0);

   function Log (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Log (Float_Type)
   --  This function shall return the logarithm of <X>.
     Post => (if X = 1.0 then Log'Result = 0.0);

   function Log (X, Base : Float_Type'Base) return Float_Type'Base with
   --  @llr Log (Float_Type; Float_Type)
   --  This function shall compute the logarithm of <X> with the specified
   --  base.
     Post => (if X = 1.0 then Log'Result = 0.0);

   function Exp (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Exp (Float_Type)
   --  This function shall compute the exponent of <X>.
     Post => (if X = 0.0 then Exp'Result = 1.0);

   function "**" (Left, Right : Float_Type'Base) return Float_Type'Base with
   --  @llr "**" (Float_Type; Float_Type)
   --  This function shall compute <Left> to the power of <Right>.
     Post => "**"'Result >= 0.0
       and then (if Right = 0.0 then "**"'Result = 1.0)
       and then (if Right = 1.0 then "**"'Result = Left)
       and then (if Left  = 1.0 then "**"'Result = 1.0)
       and then (if Left  = 0.0 then "**"'Result = 0.0);

   function Sin (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Sin (Float_Type)
   --  This function shall return the sine of <X>.
     Post => Sin'Result in -1.0 .. 1.0
       and then (if X = 0.0 then Sin'Result = 0.0);

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base with
   --  @llr Sin (Float_Type; Float_Type)
   --  This function shall return the sine of <X> with the specified base.
     Post => Sin'Result in -1.0 .. 1.0
       and then (if X = 0.0 then Sin'Result = 0.0);

   function Cos (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Cos (Float_Type)
   --  This function shall return the cosine of <X>.
     Post => Cos'Result in -1.0 .. 1.0
       and then (if X = 0.0 then Cos'Result = 1.0);

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base with
   --  @llr Cos (Float_Type; Float_Type)
   --  This funtion shall return the cosine of <X> with the sepcified base.
     Post => Cos'Result in -1.0 .. 1.0
       and then (if X = 0.0 then Cos'Result = 1.0);

   function Tan (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Tan (Float_Type)
   --  This function shall return the tangent of <X>.
     Post => (if X = 0.0 then Tan'Result = 0.0);

   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base with
   --  @llr Tan (Float_Type; Float_Type)
   --  This funtion shall return the tangent of <X> with the sepcified base.
     Post => (if X = 0.0 then Tan'Result = 0.0);

   function Cot (X : Float_Type'Base) return Float_Type'Base;
   --  @llr Cot (Float_Type)
   --  This function shall return the cotangent of <X>.

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base;
   --  @llr Cot (Float_Type; Float_Type)
   --  This funtion shall return the cotangent of <X> with the sepcified base.

   function Arcsin (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Arcsin (Float_Type)
   --  This function shall return the inverse sine of <X>.
     Post => (if X = 0.0 then Arcsin'Result = 0.0);

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base with
   --  @llr Arcsin (Float_Type; Float_Type)
   --  This funtion shall return the inverse sine of <X> with the specified
   --  base.
     Post => (if X = 0.0 then Arcsin'Result = 0.0);

   function Arccos (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Arccos (Float_Type)
   --  This function shall return the inverse cosine of <X>.
     Post => (if X = 1.0 then Arccos'Result = 0.0);

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base with
   --  @llr Arccos (Float_Type; Float_Type)
   --  This funtion shall return the inverse cosine of <X> with the specified
   --  base.
     Post => (if X = 1.0 then Arccos'Result = 0.0);

   function Arctan
     (Y : Float_Type'Base;
      X : Float_Type'Base := 1.0) return Float_Type'Base with
   --  @llr Arctan (Float_Type; Float_Type)
   --  This function shall compute the principal value of the inverse tangent
   --  of <Y> / <X>.
     Post => (if X > 0.0 and then Y = 0.0 then Arctan'Result = 0.0);

   function Arctan
     (Y : Float_Type'Base;
      X : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base) return Float_Type'Base with
   --  @llr Arctan (Float_Type; Float_Type; FLoat_Type)
   --  This function shall compute the principal value of the inverse tangent
   --  of <Y> / <X> with the specified base.
     Post => (if X > 0.0 and then Y = 0.0 then Arctan'Result = 0.0);

   function Arccot
     (X : Float_Type'Base;
      Y : Float_Type'Base := 1.0) return Float_Type'Base with
   --  @llr Arccot (Float_Type; Float_Type)
   --  This function shall compute the principal value of the inverse cotangent
   --  of <Y> / <X>.
     Post => (if X > 0.0 and then Y = 0.0 then Arccot'Result = 0.0);

   function Arccot
     (X : Float_Type'Base;
      Y : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base) return  Float_Type'Base with
   --  @llr Arccot (Float_Type; Float_Type; FLoat_Type)
   --  This function shall compute the principal value of the inverse cotangent
   --  of <Y> / <X> with the specified base.
     Post => (if X > 0.0 and then Y = 0.0 then Arccot'Result = 0.0);

   function Sinh (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Sinh (Float_Type)
   --  This function shall return the hyperbolic sine of <X>.
     Post => (if X = 0.0 then Sinh'Result = 0.0);

   function Cosh (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Cosh (Float_Type)
   --  This function shall return the hyperbolic cosine of <X>.
     Post => Cosh'Result >= 1.0
       and then (if X = 0.0 then Cosh'Result = 1.0);

   function Tanh (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Tanh (Float_Type)
   --  This function shall return the hyperbolic tangent of <X>.
     Post => Tanh'Result in -1.0 .. 1.0
       and then (if X = 0.0 then Tanh'Result = 0.0);

   function Coth (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Coth (Float_Type)
   --  This function shall return the hyperbolic cotangent of <X>.
     Post => abs Coth'Result >= 1.0;

   function Arcsinh (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Arcsinh (Float_Type)
   --  This function shall return the inverse hyperbolic sine of <X>.
     Post => (if X = 0.0 then Arcsinh'Result = 0.0);

   function Arccosh (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Arccosh (Float_Type)
   --  This function shall return the inverse hyperbolic cosine of <X>.
     Post => Arccosh'Result >= 0.0
       and then (if X = 1.0 then Arccosh'Result = 0.0);

   function Arctanh (X : Float_Type'Base) return Float_Type'Base with
   --  @llr Arctanh (Float_Type)
   --  This function shall return the inverse hyperbolic tangent of <X>.
     Post => (if X = 0.0 then Arctanh'Result = 0.0);

   function Arccoth (X : Float_Type'Base) return Float_Type'Base;
   --  @llr Arccoth (Float_Type)
   --  This function shall return the inverse hyperbolic cotangent of <X>.

private
   pragma Assert
     (Float_Type'Machine_Radix = 2,
      "only binary floating-point types supported");
   --  Why not Compile_Time_Error??? here
end Ada.Numerics.Generic_Elementary_Functions;
