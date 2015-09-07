------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--     A D A . N U M E R I C S . E L E M E N T A R Y _ F U N C T I O N S    --
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

--  @llrset a-nuelfu.ads
--  Generic_Elementary_Functions
--  ============================

--  This is the Ada Cert Math specific version of a-nuelfu.ads

with System.Generic_C_Math_Interface;
with System.Libm_Single;

package Ada.Numerics.Elementary_Functions is
   new System.Generic_C_Math_Interface
     (Float_Type => Float,
      C_Sqrt  => System.Libm_Single.Sqrt,
      C_Log   => System.Libm_Single.Log,
      C_Exp   => System.Libm_Single.Exp,
      C_Pow   => System.Libm_Single.Pow,

      C_Sin   => System.Libm_Single.Sin,
      C_Cos   => System.Libm_Single.Cos,
      C_Tan   => System.Libm_Single.Tan,

      C_Asin  => System.Libm_Single.Asin,
      C_Acos  => System.Libm_Single.Acos,
      C_Atan2 => System.Libm_Single.Atan2,

      C_Sinh  => System.Libm_Single.Sinh,
      C_Cosh  => System.Libm_Single.Cosh,
      C_Tanh  => System.Libm_Single.Tanh,

      C_Asinh => System.Libm_Single.Asinh,
      C_Acosh => System.Libm_Single.Acosh,
      C_Atanh => System.Libm_Single.Atanh);
pragma Pure (Elementary_Functions);
