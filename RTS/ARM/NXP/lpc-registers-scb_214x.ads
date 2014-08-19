--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--                 L P C . R e g i s t e r s . S C B _ 2 1 4 X                --
--                                   S p e c                                  --
--                                                                            --
--    Copyright (C) 2014  Robert Kleczek                                      --
--                                                                            --
--    This program is free software: you can redistribute it and/or modify    --
--    it under the terms of the GNU General Public License as published by    --
--    the Free Software Foundation, either version 3 of the License, or       --
--    (at your option) any later version.                                     --
--                                                                            --
--    This program is distributed in the hope that it will be useful,         --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of          --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           --
--    GNU General Public License for more details.                            --
--                                                                            --
--    You should have received a copy of the GNU General Public License       --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.   --
--                                                                            --
--------------------------------------------------------------------------------


--  Package defines LPC214X family System Control Block related registers

-------------------------
--  Imported packages  --
-------------------------

with System;
use  System;

with Bitfields;
use  Bitfields;

with Bitfields.Types;
with Bitfields.Bitfield;
with Bitfields.Functions;

-----------------------------------------------------------------------------
--                          LPC.Registers.SCB_214X                         --
-----------------------------------------------------------------------------

package LPC.Registers.SCB_214X is

   pragma Preelaborate;

   ---------------------
   --  MAMCR Register --
   ---------------------

   package MAMCR is

      package Tp is new Types (R8);

      package MAMMC is new Bitfield (Tp, 0, 2);

      type T is
         record
            MAMMC : MAMCR.MAMMC.T;
         end record;

      for T use
         record
            MAMMC at 0 range MAMMC.R'First .. MAMMC.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end MAMCR;

   subtype MAMCR_T is MAMCR.T;

   --  Field definitions

   function MAMMC is new MAMCR.FN.B (MAMCR.MAMMC);

   --  Functions

   function  "+"   is new MAMCR.FN.Add;
   function  "+"   is new MAMCR.FN.Add_RM;
   function  "-"   is new MAMCR.FN.Clear;
   function  Init  is new MAMCR.FN.Init;

   --  Constant definitions

   function Disabled          is new MAMCR.FN.C (MAMCR.MAMMC, 2#00#);
   function Partially_Enabled is new MAMCR.FN.C (MAMCR.MAMMC, 2#01#);
   function Fully_Enabled     is new MAMCR.FN.C (MAMCR.MAMMC, 2#10#);

   ---------------------
   --  MAMTIM Register --
   ---------------------

   package MAMTIM is

      package Tp is new Types (R8);

      package MAMFCT is new Bitfield (Tp, 0, 3);

      type T is
         record
            MAMFCT : MAMTIM.MAMFCT.T;
         end record;

      for T use
         record
            MAMFCT at 0 range MAMFCT.R'First .. MAMFCT.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end MAMTIM;

   subtype MAMTIM_T is MAMTIM.T;

   --  Field definitions

   function MAMFCT is new MAMTIM.FN.B (MAMTIM.MAMFCT);

   --  Functions

   function  "+"   is new MAMTIM.FN.Add;
   function  "+"   is new MAMTIM.FN.Add_RM;
   function  "-"   is new MAMTIM.FN.Clear;
   function  Init  is new MAMTIM.FN.Init;

   --  Constant definitions

   function CCLK1 is new MAMTIM.FN.C (MAMTIM.MAMFCT, 2#001#);
   function CCLK2 is new MAMTIM.FN.C (MAMTIM.MAMFCT, 2#010#);
   function CCLK3 is new MAMTIM.FN.C (MAMTIM.MAMFCT, 2#011#);
   function CCLK4 is new MAMTIM.FN.C (MAMTIM.MAMFCT, 2#100#);
   function CCLK5 is new MAMTIM.FN.C (MAMTIM.MAMFCT, 2#101#);
   function CCLK6 is new MAMTIM.FN.C (MAMTIM.MAMFCT, 2#110#);
   function CCLK7 is new MAMTIM.FN.C (MAMTIM.MAMFCT, 2#111#);

   ----------------------
   --  MEMMAP Register --
   ----------------------

   package MEMMAP is

      package Tp is new Types (R8);

      package MAP is new Bitfield (Tp, 0, 2);

      type T is
         record
            MAP : MEMMAP.MAP.T;
         end record;

      for T use
         record
            MAP at 0 range MAP.R'First .. MAP.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end MEMMAP;

   subtype MEMMAP_T is MEMMAP.T;

   --  Field definitions

   function MAP is new MEMMAP.FN.B (MEMMAP.MAP);

   --  Functions

   function  "+"   is new MEMMAP.FN.Add;
   function  "+"   is new MEMMAP.FN.Add_RM;
   function  "-"   is new MEMMAP.FN.Clear;
   function  Init  is new MEMMAP.FN.Init;

   --  Constant definitions

   function Boot_Loader_Mode is new MEMMAP.FN.C (MEMMAP.MAP, 2#00#);
   function User_Flash_Mode  is new MEMMAP.FN.C (MEMMAP.MAP, 2#01#);
   function User_RAM_Mode    is new MEMMAP.FN.C (MEMMAP.MAP, 2#10#);

   -----------------------
   --  PLL0CON Register --
   -----------------------

   package PLL0CON is

      package Tp is new Types (R8);

      package PLLC is new Bitfield (Tp, 1);
      package PLLE is new Bitfield (Tp, 0);

      type T is
         record
            PLLC : PLL0CON.PLLC.T;
            PLLE : PLL0CON.PLLE.T;
         end record;

      for T use
         record
            PLLC at 0 range PLLC.R'First .. PLLC.R'Last;
            PLLE at 0 range PLLE.R'First .. PLLE.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PLL0CON;

   subtype PLL0CON_T is PLL0CON.T;

   --  Field definitions

   function PLLC is new PLL0CON.FN.B (PLL0CON.PLLC);
   function PLLE is new PLL0CON.FN.B (PLL0CON.PLLE);

   --  Functions

   function  "+"   is new PLL0CON.FN.Add;
   function  "+"   is new PLL0CON.FN.Add_RM;
   function  "-"   is new PLL0CON.FN.Clear;
   function  Init  is new PLL0CON.FN.Init;

   --  Constant definitions

   function PLL_Disconnect is new PLL0CON.FN.C (PLL0CON.PLLC, 2#0#);
   function PLL_Connect    is new PLL0CON.FN.C (PLL0CON.PLLC, 2#1#);
   function PLL_Disable    is new PLL0CON.FN.C (PLL0CON.PLLE, 2#0#);
   function PLL_Enable     is new PLL0CON.FN.C (PLL0CON.PLLE, 2#1#);

   -----------------------
   --  PLL0CFG Register --
   -----------------------

   package PLL0CFG is

      package Tp is new Types (R8);

      package PSEL is new Bitfield (Tp, 5, 2);
      package MSEL is new Bitfield (Tp, 0, 5);

      type T is
         record
            PSEL : PLL0CFG.PSEL.T;
            MSEL : PLL0CFG.MSEL.T;
         end record;

      for T use
         record
            PSEL at 0 range PSEL.R'First .. PSEL.R'Last;
            MSEL at 0 range MSEL.R'First .. MSEL.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PLL0CFG;

   subtype PLL0CFG_T is PLL0CFG.T;

   --  Field definitions

   function PSEL is new PLL0CFG.FN.B (PLL0CFG.PSEL);
   function MSEL is new PLL0CFG.FN.B (PLL0CFG.MSEL);

   --  Functions

   function  "+"   is new PLL0CFG.FN.Add;
   function  "+"   is new PLL0CFG.FN.Add_RM;
   function  "-"   is new PLL0CFG.FN.Clear;
   function  Init  is new PLL0CFG.FN.Init;

   ------------------------
   --  PLL0STAT Register --
   ------------------------

   package PLL0STAT is

      package Tp is new Types (R16);

      package PLOCK is new Bitfield (Tp, 10);
      package PLLC  is new Bitfield (Tp, 9);
      package PLLE  is new Bitfield (Tp, 8);
      package PSEL  is new Bitfield (Tp, 5, 2);
      package MSEL  is new Bitfield (Tp, 0, 4);

      type T is
         record
            PLOCK : PLL0STAT.PLOCK.T;
            PLLC  : PLL0STAT.PLLC.T;
            PLLE  : PLL0STAT.PLLE.T;
            PSEL  : PLL0STAT.PSEL.T;
            MSEL  : PLL0STAT.MSEL.T;
         end record;

      for T use
         record
            PLOCK at 0 range PLOCK.R'First .. PLOCK.R'Last;
            PLLC  at 0 range PLLC .R'First .. PLLC .R'Last;
            PLLE  at 0 range PLLE .R'First .. PLLE .R'Last;
            PSEL  at 0 range PSEL .R'First .. PSEL .R'Last;
            MSEL  at 0 range MSEL .R'First .. MSEL .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PLL0STAT;

   subtype PLL0STAT_T is PLL0STAT.T;

   --  Field definitions

   function PLOCK is new PLL0STAT.FN.B (PLL0STAT.PLOCK);
   function PLLC  is new PLL0STAT.FN.B (PLL0STAT.PLLC);
   function PLLE  is new PLL0STAT.FN.B (PLL0STAT.PLLE);
   function PSEL  is new PLL0STAT.FN.B (PLL0STAT.PSEL);
   function MSEL  is new PLL0STAT.FN.B (PLL0STAT.MSEL);

   --  Functions

   function  "+"   is new PLL0STAT.FN.Add;
   function  "+"   is new PLL0STAT.FN.Add_RM;
   function  "-"   is new PLL0STAT.FN.Clear;
   function  Init  is new PLL0STAT.FN.Init;

   --  Constant definitions

   function PLL_Not_Locked   is new PLL0STAT.FN.C (PLL0STAT.PLOCK, 2#0#);
   function PLL_Locked       is new PLL0STAT.FN.C (PLL0STAT.PLOCK, 2#1#);
   function PLL_Disconnected is new PLL0STAT.FN.C (PLL0STAT.PLLC, 2#0#);
   function PLL_Connected    is new PLL0STAT.FN.C (PLL0STAT.PLLC, 2#1#);
   function PLL_Disabled     is new PLL0STAT.FN.C (PLL0STAT.PLLE, 2#0#);
   function PLL_Enabled      is new PLL0STAT.FN.C (PLL0STAT.PLLE, 2#1#);

   ------------------------
   --  PLL0FEED Register --
   ------------------------

   package PLL0FEED is

      package Tp is new Types (R8);

      package PLLFEED is new Bitfield (Tp, 0, 8);

      type T is
         record
            PLLFEED : PLL0FEED.PLLFEED.T;
         end record;

      for T use
         record
            PLLFEED at 0 range PLLFEED.R'First .. PLLFEED.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PLL0FEED;

   subtype PLL0FEED_T is PLL0FEED.T;

   --  Field definitions

   function PLLFEED is new PLL0FEED.FN.B (PLL0FEED.PLLFEED);

   --  Functions

   function  "+"   is new PLL0FEED.FN.Add;
   function  "+"   is new PLL0FEED.FN.Add_RM;
   function  "-"   is new PLL0FEED.FN.Clear;
   function  Init  is new PLL0FEED.FN.Init;

   --  Constant definitions

   function FEED_55 is new PLL0FEED.FN.C (PLL0FEED.PLLFEED, 16#55#);
   function FEED_AA is new PLL0FEED.FN.C (PLL0FEED.PLLFEED, 16#AA#);

   -----------------------
   --  PLL1CON Register --
   -----------------------

   package PLL1CON is

      package Tp is new Types (R8);

      package PLLC is new Bitfield (Tp, 1);
      package PLLE is new Bitfield (Tp, 0);

      type T is
         record
            PLLC : PLL1CON.PLLC.T;
            PLLE : PLL1CON.PLLE.T;
         end record;

      for T use
         record
            PLLC at 0 range PLLC.R'First .. PLLC.R'Last;
            PLLE at 0 range PLLE.R'First .. PLLE.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PLL1CON;

   subtype PLL1CON_T is PLL1CON.T;

   --  Field definitions

   function PLLC is new PLL1CON.FN.B (PLL1CON.PLLC);
   function PLLE is new PLL1CON.FN.B (PLL1CON.PLLE);

   --  Functions

   function  "+"   is new PLL1CON.FN.Add;
   function  "+"   is new PLL1CON.FN.Add_RM;
   function  "-"   is new PLL1CON.FN.Clear;
   function  Init  is new PLL1CON.FN.Init;

   --  Constant definitions

   function PLL_Disconnect is new PLL1CON.FN.C (PLL1CON.PLLC, 2#0#);
   function PLL_Connect    is new PLL1CON.FN.C (PLL1CON.PLLC, 2#1#);
   function PLL_Disable    is new PLL1CON.FN.C (PLL1CON.PLLE, 2#0#);
   function PLL_Enable     is new PLL1CON.FN.C (PLL1CON.PLLE, 2#1#);

   -----------------------
   --  PLL1CFG Register --
   -----------------------

   package PLL1CFG is

      package Tp is new Types (R8);

      package PSEL is new Bitfield (Tp, 5, 2);
      package MSEL is new Bitfield (Tp, 0, 5);

      type T is
         record
            PSEL : PLL1CFG.PSEL.T;
            MSEL : PLL1CFG.MSEL.T;
         end record;

      for T use
         record
            PSEL at 0 range PSEL.R'First .. PSEL.R'Last;
            MSEL at 0 range MSEL.R'First .. MSEL.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PLL1CFG;

   subtype PLL1CFG_T is PLL1CFG.T;

   --  Field definitions

   function PSEL is new PLL1CFG.FN.B (PLL1CFG.PSEL);
   function MSEL is new PLL1CFG.FN.B (PLL1CFG.MSEL);

   --  Functions

   function  "+"   is new PLL1CFG.FN.Add;
   function  "+"   is new PLL1CFG.FN.Add_RM;
   function  "-"   is new PLL1CFG.FN.Clear;
   function  Init  is new PLL1CFG.FN.Init;

   ------------------------
   --  PLL1STAT Register --
   ------------------------

   package PLL1STAT is

      package Tp is new Types (R16);

      package PLOCK is new Bitfield (Tp, 10);
      package PLLC  is new Bitfield (Tp, 9);
      package PLLE  is new Bitfield (Tp, 8);
      package PSEL  is new Bitfield (Tp, 5, 2);
      package MSEL  is new Bitfield (Tp, 0, 4);

      type T is
         record
            PLOCK : PLL1STAT.PLOCK.T;
            PLLC  : PLL1STAT.PLLC.T;
            PLLE  : PLL1STAT.PLLE.T;
            PSEL  : PLL1STAT.PSEL.T;
            MSEL  : PLL1STAT.MSEL.T;
         end record;

      for T use
         record
            PLOCK at 0 range PLOCK.R'First .. PLOCK.R'Last;
            PLLC  at 0 range PLLC .R'First .. PLLC .R'Last;
            PLLE  at 0 range PLLE .R'First .. PLLE .R'Last;
            PSEL  at 0 range PSEL .R'First .. PSEL .R'Last;
            MSEL  at 0 range MSEL .R'First .. MSEL .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PLL1STAT;

   subtype PLL1STAT_T is PLL1STAT.T;

   --  Field definitions

   function PLOCK is new PLL1STAT.FN.B (PLL1STAT.PLOCK);
   function PLLC  is new PLL1STAT.FN.B (PLL1STAT.PLLC);
   function PLLE  is new PLL1STAT.FN.B (PLL1STAT.PLLE);
   function PSEL  is new PLL1STAT.FN.B (PLL1STAT.PSEL);
   function MSEL  is new PLL1STAT.FN.B (PLL1STAT.MSEL);

   --  Functions

   function  "+"   is new PLL1STAT.FN.Add;
   function  "+"   is new PLL1STAT.FN.Add_RM;
   function  "-"   is new PLL1STAT.FN.Clear;
   function  Init  is new PLL1STAT.FN.Init;

   --  Constant definitions

   function PLL_Not_Locked   is new PLL1STAT.FN.C (PLL1STAT.PLOCK, 2#0#);
   function PLL_Locked       is new PLL1STAT.FN.C (PLL1STAT.PLOCK, 2#1#);
   function PLL_Disconnected is new PLL1STAT.FN.C (PLL1STAT.PLLC, 2#0#);
   function PLL_Connected    is new PLL1STAT.FN.C (PLL1STAT.PLLC, 2#1#);
   function PLL_Disabled     is new PLL1STAT.FN.C (PLL1STAT.PLLE, 2#0#);
   function PLL_Enabled      is new PLL1STAT.FN.C (PLL1STAT.PLLE, 2#1#);

   ------------------------
   --  PLL1FEED Register --
   ------------------------

   package PLL1FEED is

      package Tp is new Types (R8);

      package PLLFEED is new Bitfield (Tp, 0, 8);

      type T is
         record
            PLLFEED : PLL1FEED.PLLFEED.T;
         end record;

      for T use
         record
            PLLFEED at 0 range PLLFEED.R'First .. PLLFEED.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PLL1FEED;

   subtype PLL1FEED_T is PLL1FEED.T;

   --  Field definitions

   function PLLFEED is new PLL1FEED.FN.B (PLL1FEED.PLLFEED);

   --  Functions

   function  "+"   is new PLL1FEED.FN.Add;
   function  "+"   is new PLL1FEED.FN.Add_RM;
   function  "-"   is new PLL1FEED.FN.Clear;
   function  Init  is new PLL1FEED.FN.Init;

   --  Constant definitions

   function FEED_55 is new PLL1FEED.FN.C (PLL1FEED.PLLFEED, 16#55#);
   function FEED_AA is new PLL1FEED.FN.C (PLL1FEED.PLLFEED, 16#AA#);

   ---------------------
   --  PCON Register --
   ---------------------

   package PCON is

      package Tp is new Types (R8);

      package BORD   is new Bitfield (Tp, 5);
      package BOGD   is new Bitfield (Tp, 4);
      package BODPDM is new Bitfield (Tp, 3);
      package PDBOD  is new Bitfield (Tp, 2);
      package PD     is new Bitfield (Tp, 1);
      package IDL    is new Bitfield (Tp, 0);

      type T is
         record
            BORD   : PCON.BORD  .T;
            BOGD   : PCON.BOGD  .T;
            BODPDM : PCON.BODPDM.T;
            PDBOD  : PCON.PDBOD .T;
            PD     : PCON.PD    .T;
            IDL    : PCON.IDL   .T;
         end record;

      for T use
         record
            BORD   at 0 range BORD  .R'First .. BORD  .R'Last;
            BOGD   at 0 range BOGD  .R'First .. BOGD  .R'Last;
            BODPDM at 0 range BODPDM.R'First .. BODPDM.R'Last;
            PDBOD  at 0 range PDBOD .R'First .. PDBOD .R'Last;
            PD     at 0 range PD    .R'First .. PD    .R'Last;
            IDL    at 0 range IDL   .R'First .. IDL   .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PCON;

   subtype PCON_T is PCON.T;

   --  Field definitions

   function BORD   is new PCON.FN.B (PCON.BORD);
   function BOGD   is new PCON.FN.B (PCON.BOGD);
   function BODPDM is new PCON.FN.B (PCON.BODPDM);
   function PDBOD  is new PCON.FN.B (PCON.PDBOD);
   function PD     is new PCON.FN.B (PCON.PD);
   function IDL    is new PCON.FN.B (PCON.IDL);

   --  Functions

   function  "+"   is new PCON.FN.Add;
   function  "+"   is new PCON.FN.Add_RM;
   function  "-"   is new PCON.FN.Clear;
   function  Init  is new PCON.FN.Init;

   ---------------------
   --  PCONP Register --
   ---------------------

   package PCONP is

      package Tp is new Types (R32);

      package PUSB    is new Bitfield (Tp, 31);
      package PCAD1   is new Bitfield (Tp, 20);
      package PCI2C1  is new Bitfield (Tp, 19);
      package PCAD0   is new Bitfield (Tp, 12);
      package PCSPI1  is new Bitfield (Tp, 10);
      package PCRTC   is new Bitfield (Tp, 9);
      package PCSPI0  is new Bitfield (Tp, 8);
      package PCI2C0  is new Bitfield (Tp, 7);
      package PCPWM0  is new Bitfield (Tp, 5);
      package PCUART1 is new Bitfield (Tp, 4);
      package PCUART0 is new Bitfield (Tp, 3);
      package PCTIM1  is new Bitfield (Tp, 2);
      package PCTIM0  is new Bitfield (Tp, 1);

      type T is
         record
            PUSB    : PCONP.PUSB   .T;
            PCAD1   : PCONP.PCAD1  .T;
            PCI2C1  : PCONP.PCI2C1 .T;
            PCAD0   : PCONP.PCAD0  .T;
            PCSPI1  : PCONP.PCSPI1 .T;
            PCRTC   : PCONP.PCRTC  .T;
            PCSPI0  : PCONP.PCSPI0 .T;
            PCI2C0  : PCONP.PCI2C0 .T;
            PCPWM0  : PCONP.PCPWM0 .T;
            PCUART1 : PCONP.PCUART1.T;
            PCUART0 : PCONP.PCUART0.T;
            PCTIM1  : PCONP.PCTIM1 .T;
            PCTIM0  : PCONP.PCTIM0 .T;
         end record;

      for T use
         record
            PUSB    at 0 range PUSB   .R'First .. PUSB   .R'Last;
            PCAD1   at 0 range PCAD1  .R'First .. PCAD1  .R'Last;
            PCI2C1  at 0 range PCI2C1 .R'First .. PCI2C1 .R'Last;
            PCAD0   at 0 range PCAD0  .R'First .. PCAD0  .R'Last;
            PCSPI1  at 0 range PCSPI1 .R'First .. PCSPI1 .R'Last;
            PCRTC   at 0 range PCRTC  .R'First .. PCRTC  .R'Last;
            PCSPI0  at 0 range PCSPI0 .R'First .. PCSPI0 .R'Last;
            PCI2C0  at 0 range PCI2C0 .R'First .. PCI2C0 .R'Last;
            PCPWM0  at 0 range PCPWM0 .R'First .. PCPWM0 .R'Last;
            PCUART1 at 0 range PCUART1.R'First .. PCUART1.R'Last;
            PCUART0 at 0 range PCUART0.R'First .. PCUART0.R'Last;
            PCTIM1  at 0 range PCTIM1 .R'First .. PCTIM1 .R'Last;
            PCTIM0  at 0 range PCTIM0 .R'First .. PCTIM0 .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end PCONP;

   subtype PCONP_T is PCONP.T;

   --  Field definitions

   function PUSB    is new PCONP.FN.B (PCONP.PUSB);
   function PCAD1   is new PCONP.FN.B (PCONP.PCAD1);
   function PCI2C1  is new PCONP.FN.B (PCONP.PCI2C1);
   function PCAD0   is new PCONP.FN.B (PCONP.PCAD0);
   function PCSPI1  is new PCONP.FN.B (PCONP.PCSPI1);
   function PCRTC   is new PCONP.FN.B (PCONP.PCRTC);
   function PCSPI0  is new PCONP.FN.B (PCONP.PCSPI0);
   function PCI2C0  is new PCONP.FN.B (PCONP.PCI2C0);
   function PCPWM0  is new PCONP.FN.B (PCONP.PCPWM0);
   function PCUART1 is new PCONP.FN.B (PCONP.PCUART1);
   function PCUART0 is new PCONP.FN.B (PCONP.PCUART0);
   function PCTIM1  is new PCONP.FN.B (PCONP.PCTIM1);
   function PCTIM0  is new PCONP.FN.B (PCONP.PCTIM0);

   --  Functions

   function  "+"   is new PCONP.FN.Add;
   function  "+"   is new PCONP.FN.Add_RM;
   function  "-"   is new PCONP.FN.Clear;
   function  Init  is new PCONP.FN.Init;

   ----------------------
   --  VPBDIV Register --
   ----------------------

   package VPBDIV is

      package Tp is new Types (R8);

      package VPB_DIV is new Bitfield (Tp, 0, 2);

      type T is
         record
            VPB_DIV : VPBDIV.VPB_DIV.T;
         end record;

      for T use
         record
            VPB_DIV at 0 range VPB_DIV.R'First .. VPB_DIV.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end VPBDIV;

   subtype VPBDIV_T is VPBDIV.T;

   --  Field definitions

   function VPB_DIV is new VPBDIV.FN.B (VPBDIV.VPB_DIV);

   --  Functions

   function  "+"   is new VPBDIV.FN.Add;
   function  "+"   is new VPBDIV.FN.Add_RM;
   function  "-"   is new VPBDIV.FN.Clear;
   function  Init  is new VPBDIV.FN.Init;

   --  Constant definitions

   function Div4 is new VPBDIV.FN.C (VPBDIV.VPB_DIV, 2#00#);
   function Div1 is new VPBDIV.FN.C (VPBDIV.VPB_DIV, 2#01#);
   function Div2 is new VPBDIV.FN.C (VPBDIV.VPB_DIV, 2#10#);

   ----------------------
   --  EXTINT Register --
   ----------------------

   package EXTINT is

      package Tp is new Types (R8);

      package EINT3 is new Bitfield (Tp, 3);
      package EINT2 is new Bitfield (Tp, 2);
      package EINT1 is new Bitfield (Tp, 1);
      package EINT0 is new Bitfield (Tp, 0);

      type T is
         record
            EINT3 : EXTINT.EINT3.T;
            EINT2 : EXTINT.EINT2.T;
            EINT1 : EXTINT.EINT1.T;
            EINT0 : EXTINT.EINT0.T;
         end record;

      for T use
         record
            EINT3 at 0 range EINT3.R'First .. EINT3.R'Last;
            EINT2 at 0 range EINT2.R'First .. EINT2.R'Last;
            EINT1 at 0 range EINT1.R'First .. EINT1.R'Last;
            EINT0 at 0 range EINT0.R'First .. EINT0.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end EXTINT;

   subtype EXTINT_T is EXTINT.T;

   --  Field definitions

   function EINT3 is new EXTINT.FN.B (EXTINT.EINT3);
   function EINT2 is new EXTINT.FN.B (EXTINT.EINT2);
   function EINT1 is new EXTINT.FN.B (EXTINT.EINT1);
   function EINT0 is new EXTINT.FN.B (EXTINT.EINT0);

   --  Functions

   function  "+"   is new EXTINT.FN.Add;
   function  "+"   is new EXTINT.FN.Add_RM;
   function  "-"   is new EXTINT.FN.Clear;
   function  Init  is new EXTINT.FN.Init;

   ---------------------
   --  INTWAKE Register --
   ---------------------

   package INTWAKE is

      package Tp is new Types (R16);

      package RTCWAKE  is new Bitfield (Tp, 15);
      package BODWAKE  is new Bitfield (Tp, 14);
      package USBWAKE  is new Bitfield (Tp, 5);
      package EXTWAKE3 is new Bitfield (Tp, 3);
      package EXTWAKE2 is new Bitfield (Tp, 2);
      package EXTWAKE1 is new Bitfield (Tp, 1);
      package EXTWAKE0 is new Bitfield (Tp, 0);

      type T is
         record
            RTCWAKE  : INTWAKE.RTCWAKE .T;
            BODWAKE  : INTWAKE.BODWAKE .T;
            USBWAKE  : INTWAKE.USBWAKE .T;
            EXTWAKE3 : INTWAKE.EXTWAKE3.T;
            EXTWAKE2 : INTWAKE.EXTWAKE2.T;
            EXTWAKE1 : INTWAKE.EXTWAKE1.T;
            EXTWAKE0 : INTWAKE.EXTWAKE0.T;
         end record;

      for T use
         record
            RTCWAKE  at 0 range RTCWAKE .R'First .. RTCWAKE .R'Last;
            BODWAKE  at 0 range BODWAKE .R'First .. BODWAKE .R'Last;
            USBWAKE  at 0 range USBWAKE .R'First .. USBWAKE .R'Last;
            EXTWAKE3 at 0 range EXTWAKE3.R'First .. EXTWAKE3.R'Last;
            EXTWAKE2 at 0 range EXTWAKE2.R'First .. EXTWAKE2.R'Last;
            EXTWAKE1 at 0 range EXTWAKE1.R'First .. EXTWAKE1.R'Last;
            EXTWAKE0 at 0 range EXTWAKE0.R'First .. EXTWAKE0.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end INTWAKE;

   subtype INTWAKE_T is INTWAKE.T;

   --  Field definitions

   function RTCWAKE  is new INTWAKE.FN.B (INTWAKE.RTCWAKE);
   function BODWAKE  is new INTWAKE.FN.B (INTWAKE.BODWAKE);
   function USBWAKE  is new INTWAKE.FN.B (INTWAKE.USBWAKE);
   function EXTWAKE3 is new INTWAKE.FN.B (INTWAKE.EXTWAKE3);
   function EXTWAKE2 is new INTWAKE.FN.B (INTWAKE.EXTWAKE2);
   function EXTWAKE1 is new INTWAKE.FN.B (INTWAKE.EXTWAKE1);
   function EXTWAKE0 is new INTWAKE.FN.B (INTWAKE.EXTWAKE0);

   --  Functions

   function  "+"   is new INTWAKE.FN.Add;
   function  "+"   is new INTWAKE.FN.Add_RM;
   function  "-"   is new INTWAKE.FN.Clear;
   function  Init  is new INTWAKE.FN.Init;

   ---------------------
   --  EXTMODE Register --
   ---------------------

   package EXTMODE is

      package Tp is new Types (R8);

      package EXTMODE3 is new Bitfield (Tp, 3);
      package EXTMODE2 is new Bitfield (Tp, 2);
      package EXTMODE1 is new Bitfield (Tp, 1);
      package EXTMODE0 is new Bitfield (Tp, 0);

      type T is
         record
            EXTMODE3 : EXTMODE.EXTMODE3.T;
            EXTMODE2 : EXTMODE.EXTMODE2.T;
            EXTMODE1 : EXTMODE.EXTMODE1.T;
            EXTMODE0 : EXTMODE.EXTMODE0.T;
         end record;

      for T use
         record
            EXTMODE3 at 0 range EXTMODE3.R'First .. EXTMODE3.R'Last;
            EXTMODE2 at 0 range EXTMODE2.R'First .. EXTMODE2.R'Last;
            EXTMODE1 at 0 range EXTMODE1.R'First .. EXTMODE1.R'Last;
            EXTMODE0 at 0 range EXTMODE0.R'First .. EXTMODE0.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end EXTMODE;

   subtype EXTMODE_T is EXTMODE.T;

   --  Field definitions

   function EXTMODE3 is new EXTMODE.FN.B (EXTMODE.EXTMODE3);
   function EXTMODE2 is new EXTMODE.FN.B (EXTMODE.EXTMODE2);
   function EXTMODE1 is new EXTMODE.FN.B (EXTMODE.EXTMODE1);
   function EXTMODE0 is new EXTMODE.FN.B (EXTMODE.EXTMODE0);

   --  Functions

   function  "+"   is new EXTMODE.FN.Add;
   function  "+"   is new EXTMODE.FN.Add_RM;
   function  "-"   is new EXTMODE.FN.Clear;
   function  Init  is new EXTMODE.FN.Init;

   --  Constant definitions

   function Level_Sensitive is new EXTMODE.FN.C (EXTMODE.EXTMODE3, 2#0#);
   function Edge_Sensitive  is new EXTMODE.FN.C (EXTMODE.EXTMODE3, 2#1#);
   function Level_Sensitive is new EXTMODE.FN.C (EXTMODE.EXTMODE2, 2#0#);
   function Edge_Sensitive  is new EXTMODE.FN.C (EXTMODE.EXTMODE2, 2#1#);
   function Level_Sensitive is new EXTMODE.FN.C (EXTMODE.EXTMODE1, 2#0#);
   function Edge_Sensitive  is new EXTMODE.FN.C (EXTMODE.EXTMODE1, 2#1#);
   function Level_Sensitive is new EXTMODE.FN.C (EXTMODE.EXTMODE0, 2#0#);
   function Edge_Sensitive  is new EXTMODE.FN.C (EXTMODE.EXTMODE0, 2#1#);

   ---------------------
   --  EXTPOLAR Register --
   ---------------------

   package EXTPOLAR is

      package Tp is new Types (R8);

      package EXTPOLAR3 is new Bitfield (Tp, 3);
      package EXTPOLAR2 is new Bitfield (Tp, 2);
      package EXTPOLAR1 is new Bitfield (Tp, 1);
      package EXTPOLAR0 is new Bitfield (Tp, 0);

      type T is
         record
            EXTPOLAR3 : EXTPOLAR.EXTPOLAR3.T;
            EXTPOLAR2 : EXTPOLAR.EXTPOLAR2.T;
            EXTPOLAR1 : EXTPOLAR.EXTPOLAR1.T;
            EXTPOLAR0 : EXTPOLAR.EXTPOLAR0.T;
         end record;

      for T use
         record
            EXTPOLAR3 at 0 range EXTPOLAR3.R'First .. EXTPOLAR3.R'Last;
            EXTPOLAR2 at 0 range EXTPOLAR2.R'First .. EXTPOLAR2.R'Last;
            EXTPOLAR1 at 0 range EXTPOLAR1.R'First .. EXTPOLAR1.R'Last;
            EXTPOLAR0 at 0 range EXTPOLAR0.R'First .. EXTPOLAR0.R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end EXTPOLAR;

   subtype EXTPOLAR_T is EXTPOLAR.T;

   --  Field definitions

   function EXTPOLAR3 is new EXTPOLAR.FN.B (EXTPOLAR.EXTPOLAR3);
   function EXTPOLAR2 is new EXTPOLAR.FN.B (EXTPOLAR.EXTPOLAR2);
   function EXTPOLAR1 is new EXTPOLAR.FN.B (EXTPOLAR.EXTPOLAR1);
   function EXTPOLAR0 is new EXTPOLAR.FN.B (EXTPOLAR.EXTPOLAR0);

   --  Functions

   function  "+"   is new EXTPOLAR.FN.Add;
   function  "+"   is new EXTPOLAR.FN.Add_RM;
   function  "-"   is new EXTPOLAR.FN.Clear;
   function  Init  is new EXTPOLAR.FN.Init;

   --  Constant definitions

   function Low_Or_Falling is new EXTPOLAR.FN.C (EXTPOLAR.EXTPOLAR3, 2#0#);
   function High_Or_Rising is new EXTPOLAR.FN.C (EXTPOLAR.EXTPOLAR3, 2#1#);
   function Low_Or_Falling is new EXTPOLAR.FN.C (EXTPOLAR.EXTPOLAR2, 2#0#);
   function High_Or_Rising is new EXTPOLAR.FN.C (EXTPOLAR.EXTPOLAR2, 2#1#);
   function Low_Or_Falling is new EXTPOLAR.FN.C (EXTPOLAR.EXTPOLAR1, 2#0#);
   function High_Or_Rising is new EXTPOLAR.FN.C (EXTPOLAR.EXTPOLAR1, 2#1#);
   function Low_Or_Falling is new EXTPOLAR.FN.C (EXTPOLAR.EXTPOLAR0, 2#0#);
   function High_Or_Rising is new EXTPOLAR.FN.C (EXTPOLAR.EXTPOLAR0, 2#1#);

   ---------------------
   --  RSID Register --
   ---------------------

   package RSID is

      package Tp is new Types (R8);

      package BODR is new Bitfield (Tp, 3);
      package WDTR is new Bitfield (Tp, 2);
      package EXTR is new Bitfield (Tp, 1);
      package POR  is new Bitfield (Tp, 0);

      type T is
         record
            BODR : RSID.BODR.T;
            WDTR : RSID.WDTR.T;
            EXTR : RSID.EXTR.T;
            POR  : RSID.POR .T;
         end record;

      for T use
         record
            BODR at 0 range BODR.R'First .. BODR.R'Last;
            WDTR at 0 range WDTR.R'First .. WDTR.R'Last;
            EXTR at 0 range EXTR.R'First .. EXTR.R'Last;
            POR  at 0 range POR .R'First .. POR .R'Last;
         end record;

      for T'Size use Tp.Reg'Size;
      pragma Suppress_Initialization (T);

      package FN is new Functions (T, Tp);

   end RSID;

   subtype RSID_T is RSID.T;

   --  Field definitions

   function BODR is new RSID.FN.B (RSID.BODR);
   function WDTR is new RSID.FN.B (RSID.WDTR);
   function EXTR is new RSID.FN.B (RSID.EXTR);
   function POR  is new RSID.FN.B (RSID.POR);

   --  Functions

   function  "+"   is new RSID.FN.Add;
   function  "+"   is new RSID.FN.Add_RM;
   function  "-"   is new RSID.FN.Clear;
   function  Init  is new RSID.FN.Init;

   --     SCB Registers Collection  --

   type SCB_T is
      record
         MAMCR     : MAMCR_T;
         MAMTIM    : MAMTIM_T;
         MEMMAP    : MEMMAP_T;
         PLL0CON   : PLL0CON_T;
         PLL0CFG   : PLL0CFG_T;
         PLL0STAT  : PLL0STAT_T;
         PLL0FEED  : PLL0FEED_T;
         PLL1CON   : PLL1CON_T;
         PLL1CFG   : PLL1CFG_T;
         PLL1STAT  : PLL1STAT_T;
         PLL1FEED  : PLL1FEED_T;
         PCON      : PCON_T;
         PCONP     : PCONP_T;
         VPBDIV    : VPBDIV_T;
         EXTINT    : EXTINT_T;
         INTWAKE   : INTWAKE_T;
         EXTMODE   : EXTMODE_T;
         EXTPOLAR  : EXTPOLAR_T;
         RSID      : RSID_T;
         pragma Volatile (MAMCR);
         pragma Volatile (MAMTIM);
         pragma Volatile (MEMMAP);
         pragma Volatile (PLL0CON);
         pragma Volatile (PLL0CFG);
         pragma Volatile (PLL0STAT);
         pragma Volatile (PLL0FEED);
         pragma Volatile (PLL1CON);
         pragma Volatile (PLL1CFG);
         pragma Volatile (PLL1STAT);
         pragma Volatile (PLL1FEED);
         pragma Volatile (PCON);
         pragma Volatile (PCONP);
         pragma Volatile (VPBDIV);
         pragma Volatile (EXTINT);
         pragma Volatile (INTWAKE);
         pragma Volatile (EXTMODE);
         pragma Volatile (EXTPOLAR);
         pragma Volatile (RSID);
      end record;

   for SCB_T use
      record
         MAMCR    at 16#000# range 0 .. 7;
         MAMTIM   at 16#004# range 0 .. 7;
         MEMMAP   at 16#040# range 0 .. 7;
         PLL0CON  at 16#080# range 0 .. 7;
         PLL0CFG  at 16#084# range 0 .. 7;
         PLL0STAT at 16#088# range 0 .. 15;
         PLL0FEED at 16#08C# range 0 .. 7;
         PLL1CON  at 16#0A0# range 0 .. 7;
         PLL1CFG  at 16#0A4# range 0 .. 7;
         PLL1STAT at 16#0A8# range 0 .. 15;
         PLL1FEED at 16#0AC# range 0 .. 7;
         PCON     at 16#0C0# range 0 .. 7;
         PCONP    at 16#0C4# range 0 .. 31;
         VPBDIV   at 16#100# range 0 .. 7;
         EXTINT   at 16#140# range 0 .. 7;
         INTWAKE  at 16#144# range 0 .. 15;
         EXTMODE  at 16#148# range 0 .. 7;
         EXTPOLAR at 16#14C# range 0 .. 7;
         RSID     at 16#180# range 0 .. 7;
      end record;

   SCB : SCB_T;

   for SCB'Address use System'To_Address (16#E01F_C000#);

end LPC.Registers.SCB_214X;
