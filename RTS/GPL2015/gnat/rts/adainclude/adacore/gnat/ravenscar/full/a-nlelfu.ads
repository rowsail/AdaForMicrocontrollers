------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                  ADA.NUMERICS.LONG_ELEMENTARY_FUNCTIONS                  --
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

--  @llrset a-nlelfu.ads
--  Long_Elementary_Functions
--  =========================
--
--  This is the Ada Cert Math specific version of a-nlelfu.ads.

with System.Generic_C_Math_Interface;
with System.Libm_Double;

package Ada.Numerics.Long_Elementary_Functions is
   new System.Generic_C_Math_Interface
     (Float_Type => Long_Float,
      C_Sqrt  => System.Libm_Double.Sqrt,
      C_Log   => System.Libm_Double.Log,
      C_Exp   => System.Libm_Double.Exp,
      C_Pow   => System.Libm_Double.Pow,

      C_Sin   => System.Libm_Double.Sin,
      C_Cos   => System.Libm_Double.Cos,
      C_Tan   => System.Libm_Double.Tan,

      C_Asin  => System.Libm_Double.Asin,
      C_Acos  => System.Libm_Double.Acos,
      C_Atan2 => System.Libm_Double.Atan2,

      C_Sinh  => System.Libm_Double.Sinh,
      C_Cosh  => System.Libm_Double.Cosh,
      C_Tanh  => System.Libm_Double.Tanh,

      C_Asinh => System.Libm_Double.Asinh,
      C_Acosh => System.Libm_Double.Acosh,
      C_Atanh => System.Libm_Double.Atanh);
pragma Pure (Long_Elementary_Functions);
