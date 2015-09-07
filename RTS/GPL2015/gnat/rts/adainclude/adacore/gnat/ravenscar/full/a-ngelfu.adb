------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                ADA.NUMERICS.GENERIC_ELEMENTARY_FUNCTIONS                 --
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

--  This is the Ada Cert Math specific version of a-ngelfu.adb

--  This body does not implement Ada.Numerics.Generic_Elementary_Functions as
--  defined by the standard. See the package specification for more details.

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Numerics.Long_Long_Elementary_Functions;

use Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Long_Elementary_Functions;

package body Ada.Numerics.Generic_Elementary_Functions is

   subtype T is Float_Type'Base;

   subtype F is Float;
   subtype LF is Long_Float;
   subtype LLF is Long_Long_Float;

   Is_Float : constant Boolean :=
     T'Machine_Mantissa = Float'Machine_Mantissa
     and then Float (T'First) = Float'First
     and then Float (T'Last) = Float'Last;

   Is_Long_Float : constant Boolean :=
     T'Machine_Mantissa = Long_Float'Machine_Mantissa
     and then Long_Float (T'First) = Long_Float'First
     and then Long_Float (T'Last) = Long_Float'Last;

   Is_Long_Long_Float : constant Boolean :=
     not (T'Machine_Mantissa = Long_Float'Machine_Mantissa)
     and then T'Machine_Mantissa = Long_Long_Float'Machine_Mantissa
     and then Long_Long_Float (T'First) = Long_Long_Float'First
     and then Long_Long_Float (T'Last) = Long_Long_Float'Last;

   ----------
   -- "**" --
   ----------

   function "**" (Left, Right : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (F (Left) ** F (Right))
      elsif Is_Long_Float then T (LF (Left) ** LF (Right))
      elsif Is_Long_Long_Float then T (LLF (Left) ** LLF (Right))
      else raise Program_Error);

   ------------
   -- Arccos --
   ------------

   --  Natural cycle

   function Arccos (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Arccos (F (X)))
      elsif Is_Long_Float then T (Arccos (LF (X)))
      elsif Is_Long_Long_Float then T (Arccos (LLF (X)))
      else raise Program_Error);

   --  Arbitrary cycle

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Arccos (F (X), F (Cycle)))
      elsif Is_Long_Float then T (Arccos (LF (X), LF (Cycle)))
      elsif Is_Long_Long_Float then T (Arccos (LLF (X), LLF (Cycle)))
      else raise Program_Error);

   -------------
   -- Arccosh --
   -------------

   function Arccosh (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Arccosh (F (X)))
      elsif Is_Long_Float then T (Arccosh (LF (X)))
      elsif Is_Long_Long_Float then T (Arccosh (LLF (X)))
      else raise Program_Error);

   ------------
   -- Arccot --
   ------------

   --  Natural cycle

   function Arccot
     (X    : Float_Type'Base;
      Y    : Float_Type'Base := 1.0)
      return Float_Type'Base
   is
     (if Is_Float then T (Arccot (F (X), F (Y)))
      elsif Is_Long_Float then T (Arccot (LF (X), LF (Y)))
      elsif Is_Long_Long_Float then T (Arccot (LLF (X), LLF (Y)))
      else raise Program_Error);

   --  Arbitrary cycle

   function Arccot
     (X     : Float_Type'Base;
      Y     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return  Float_Type'Base
   is
     (if Is_Float then T (Arccot (F (X), F (Y), F (Cycle)))
      elsif Is_Long_Float then T (Arccot (LF (X), LF (Y), LF (Cycle)))
      elsif Is_Long_Long_Float then T (Arccot (LLF (X), LLF (Y), LLF (Cycle)))
      else raise Program_Error);

   -------------
   -- Arccoth --
   -------------

   function Arccoth (X : Float_Type'Base) return Float_Type'Base
   is
     (if Is_Float then T (Arccoth (F (X)))
      elsif Is_Long_Float then T (Arccoth (LF (X)))
      elsif Is_Long_Long_Float then T (Arccoth (LLF (X)))
      else raise Program_Error);

   ------------
   -- Arcsin --
   ------------

   --  Natural cycle

   function Arcsin (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Arcsin (F (X)))
      elsif Is_Long_Float then T (Arcsin (LF (X)))
      elsif Is_Long_Long_Float then T (Arcsin (LLF (X)))
      else raise Program_Error);

   --  Arbitrary cycle

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Arcsin (F (X), F (Cycle)))
      elsif Is_Long_Float then T (Arcsin (LF (X), LF (Cycle)))
      elsif Is_Long_Long_Float then T (Arcsin (LLF (X), LLF (Cycle)))
      else raise Program_Error);

   -------------
   -- Arcsinh --
   -------------

   function Arcsinh (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Arcsinh (F (X)))
      elsif Is_Long_Float then T (Arcsinh (LF (X)))
      elsif Is_Long_Long_Float then T (Arcsinh (LLF (X)))
      else raise Program_Error);

   ------------
   -- Arctan --
   ------------

   --  Natural cycle

   function Arctan
     (Y    : Float_Type'Base;
      X    : Float_Type'Base := 1.0)
      return Float_Type'Base
   is
     (if Is_Float then T (Arctan (F (Y), F (X)))
      elsif Is_Long_Float then T (Arctan (LF (Y), LF (X)))
      elsif Is_Long_Long_Float then T (Arctan (LLF (Y), LLF (X)))
      else raise Program_Error);

   --  Arbitrary cycle

   function Arctan
     (Y     : Float_Type'Base;
      X     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return  Float_Type'Base
   is
     (if Is_Float then T (Arctan (F (Y), F (X), F (Cycle)))
      elsif Is_Long_Float then T (Arctan (LF (Y), LF (X), LF (Cycle)))
      elsif Is_Long_Long_Float then T (Arctan (LLF (Y), LLF (X), LLF (Cycle)))
      else raise Program_Error);

   -------------
   -- Arctanh --
   -------------

   function Arctanh (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Arctanh (F (X)))
      elsif Is_Long_Float then T (Arctanh (LF (X)))
      elsif Is_Long_Long_Float then T (Arctanh (LLF (X)))
      else raise Program_Error);

   ---------
   -- Cos --
   ---------

   --  Natural cycle

   function Cos (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Cos (F (X)))
      elsif Is_Long_Float then T (Cos (LF (X)))
      elsif Is_Long_Long_Float then T (Cos (LLF (X)))
      else raise Program_Error);

   --  Arbitrary cycle

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Cos (F (X), F (Cycle)))
      elsif Is_Long_Float then T (Cos (LF (X), LF (Cycle)))
      elsif Is_Long_Long_Float then T (Cos (LLF (X), LLF (Cycle)))
      else raise Program_Error);

   ----------
   -- Cosh --
   ----------

   function Cosh (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Cosh (F (X)))
      elsif Is_Long_Float then T (Cosh (LF (X)))
      elsif Is_Long_Long_Float then T (Cosh (LLF (X)))
      else raise Program_Error);

   ---------
   -- Cot --
   ---------

   --  Natural cycle

   function Cot (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Cot (F (X)))
      elsif Is_Long_Float then T (Cot (LF (X)))
      elsif Is_Long_Long_Float then T (Cot (LLF (X)))
      else raise Program_Error);

   --  Arbitrary cycle

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Cot (F (X), F (Cycle)))
      elsif Is_Long_Float then T (Cot (LF (X), LF (Cycle)))
      elsif Is_Long_Long_Float then T (Cot (LLF (X), LLF (Cycle)))
      else raise Program_Error);

   ----------
   -- Coth --
   ----------

   function Coth (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Coth (F (X)))
      elsif Is_Long_Float then T (Coth (LF (X)))
      elsif Is_Long_Long_Float then T (Coth (LLF (X)))
      else raise Program_Error);

   ---------
   -- Exp --
   ---------

   function Exp (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Exp (F (X)))
      elsif Is_Long_Float then T (Exp (LF (X)))
      elsif Is_Long_Long_Float then T (Exp (LLF (X)))
      else raise Program_Error);

   ---------
   -- Log --
   ---------

   --  Natural base

   function Log (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Log (F (X)))
      elsif Is_Long_Float then T (Log (LF (X)))
      elsif Is_Long_Long_Float then T (Log (LLF (X)))
      else raise Program_Error);

   --  Arbitrary base

   function Log (X, Base : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Log (F (X), F (Base)))
      elsif Is_Long_Float then T (Log (LF (X), LF (Base)))
      elsif Is_Long_Long_Float then T (Log (LLF (X), LLF (Base)))
      else raise Program_Error);

   ---------
   -- Sin --
   ---------

   --  Natural cycle

   function Sin (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Sin (F (X)))
      elsif Is_Long_Float then T (Sin (LF (X)))
      elsif Is_Long_Long_Float then T (Sin (LLF (X)))
      else raise Program_Error);

   --  Arbitrary cycle

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Sin (F (X), F (Cycle)))
      elsif Is_Long_Float then T (Sin (LF (X), LF (Cycle)))
      elsif Is_Long_Long_Float then T (Sin (LLF (X), LLF (Cycle)))
      else raise Program_Error);

   ----------
   -- Sinh --
   ----------

   function Sinh (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Sinh (F (X)))
      elsif Is_Long_Float then T (Sinh (LF (X)))
      elsif Is_Long_Long_Float then T (Sinh (LLF (X)))
      else raise Program_Error);

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Sqrt (F (X)))
      elsif Is_Long_Float then T (Sqrt (LF (X)))
      elsif Is_Long_Long_Float then T (Sqrt (LLF (X)))
      else raise Program_Error);

   ---------
   -- Tan --
   ---------

   --  Natural cycle

   function Tan (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Tan (F (X)))
      elsif Is_Long_Float then T (Tan (LF (X)))
      elsif Is_Long_Long_Float then T (Tan (LLF (X)))
      else raise Program_Error);

   --  Arbitrary cycle

   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Tan (F (X), F (Cycle)))
      elsif Is_Long_Float then T (Tan (LF (X), LF (Cycle)))
      elsif Is_Long_Long_Float then T (Tan (LLF (X), LLF (Cycle)))
      else raise Program_Error);

   ----------
   -- Tanh --
   ----------

   function Tanh (X : Float_Type'Base) return Float_Type'Base is
     (if Is_Float then T (Tanh (F (X)))
      elsif Is_Long_Float then T (Tanh (LF (X)))
      elsif Is_Long_Long_Float then T (Tanh (LLF (X)))
      else raise Program_Error);

end Ada.Numerics.Generic_Elementary_Functions;
