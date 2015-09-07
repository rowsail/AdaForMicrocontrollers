------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          Copyright (C) 2012-2015, Free Software Foundation, Inc.         --
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

--  This file provides register definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.

pragma Restrictions (No_Elaboration_Code);

with System.Storage_Elements;

package System.STM32F4 is
   pragma Preelaborate (System.STM32F4);

   subtype Address is System.Address;
   type Word is mod 2**32;

   type Bits_1  is mod 2**1  with Size => 1;
   type Bits_2  is mod 2**2  with Size => 2;
   type Bits_3  is mod 2**3  with Size => 3;
   type Bits_4  is mod 2**4  with Size => 4;
   type Bits_12 is mod 2**12 with Size => 12;
   type Bits_16 is mod 2**16 with Size => 16;

   type Bits_32x1 is array (0 .. 31) of Bits_1 with Pack, Size => 32;
   type Bits_16x2 is array (0 .. 15) of Bits_2 with Pack, Size => 32;
   type Bits_8x4  is array (0 ..  7) of Bits_4 with Pack, Size => 32;

   --  Define address bases for the various system components

   Peripheral_Base : constant := 16#4000_0000#;

   APB1_Peripheral_Base : constant := Peripheral_Base;
   APB2_Peripheral_Base : constant := Peripheral_Base + 16#0001_0000#;
   AHB1_Peripheral_Base : constant := Peripheral_Base + 16#0002_0000#;
   AHB2_Peripheral_Base : constant := Peripheral_Base + 16#1000_0000#;

   GPIOB_Base  : constant := AHB1_Peripheral_Base + 16#0400#;
   FLASH_Base  : constant := AHB1_Peripheral_Base + 16#3C00#;
   USART1_Base : constant := APB2_Peripheral_Base + 16#1000#;
   RCC_Base    : constant := AHB1_Peripheral_Base + 16#3800#;
   PWR_Base    : constant := APB1_Peripheral_Base + 16#7000#;

   ---------
   -- PWR --
   ---------

   type PWR_Registers is record
      CR    : Word; --  PWR power control register at 16#00#
      CSR   : Word; --  PWR power control/status register at 16#04#
   end record;

   PWR : PWR_Registers with Volatile, Import,
                            Address => System'To_Address (PWR_Base);

   PWR_CR_VOS_HIGH_407 : constant Word := 16#4000#; -- Core voltage set to high
   PWR_CR_VOS_HIGH_429 : constant Word := 16#C000#; -- Core voltage set to high
   PWR_CSR_VOSRDY      : constant Word := 2**14;    -- Regulator output ready

   ---------------
   -- FLASH_ACR --
   ---------------

   package FLASH_ACR is

      --  Constants for FLASH ACR register

      --  Wait states

      LATENCY_0WS : constant Word := 16#0#;
      LATENCY_1WS : constant Word := 16#1#;
      LATENCY_2WS : constant Word := 16#2#;
      LATENCY_3WS : constant Word := 16#3#;
      LATENCY_4WS : constant Word := 16#4#;
      LATENCY_5WS : constant Word := 16#5#;
      LATENCY_6WS : constant Word := 16#6#;
      LATENCY_7WS : constant Word := 16#7#;

      PRFTEN      : constant Word := 16#01_00#; -- Preftech enable
      ICEN        : constant Word := 16#02_00#; -- Instruction cache enable
      DCEN        : constant Word := 16#04_00#; -- Data cache enable
      ICRST       : constant Word := 16#08_00#; -- Instruction cache reset
      DCRST       : constant Word := 16#10_00#; -- Data cache reset
   end FLASH_ACR;

   type FLASH_Registers is record
      ACR     : Word;
      KEYR    : Word;
      OPTKEYR : Word;
      SR      : Word;
      CR      : Word;
      OPTCR   : Word;
   end record;

   FLASH : FLASH_Registers with Volatile,
                                Address => System'To_Address (FLASH_Base);
   pragma Import (Ada, FLASH);

   ----------
   -- GPIO --
   ----------

   package GPIO is

      --  MODER constants

      Mode_IN        : constant Bits_2 := 0;
      Mode_OUT       : constant Bits_2 := 1;
      Mode_AF        : constant Bits_2 := 2;
      Mode_AN        : constant Bits_2 := 3;

      --  OTYPER constants

      Type_PP       : constant Bits_1 := 0; -- Push/pull
      Type_OD       : constant Bits_1 := 1; -- Open drain

      --  OSPEEDR constants

      Speed_2MHz    : constant Bits_2 := 0; -- Low speed
      Speed_25MHz   : constant Bits_2 := 1; -- Medium speed
      Speed_50MHz   : constant Bits_2 := 2; -- Fast speed
      Speed_100MHz  : constant Bits_2 := 3; -- High speed on 30pF, 80MHz on 15

      --  PUPDR constants

      No_Pull       : constant Bits_2 := 0;
      Pull_Up       : constant Bits_2 := 1;
      Pull_Down     : constant Bits_2 := 2;

      --  AFL constants

      AF_USART1    : constant Bits_4 := 7;
   end GPIO;

   type GPIO_Registers is record
      MODER   : Bits_16x2;
      OTYPER  : Bits_32x1;
      OSPEEDR : Bits_16x2;
      PUPDR   : Bits_16x2;

      IDR     : Word;
      ODR     : Word;
      BSRR    : Word;
      LCKR    : Word;

      AFRL    : Bits_8x4;
      AFRH    : Bits_8x4;
   end record;

   GPIOB : GPIO_Registers with Volatile,
                               Address => System'To_Address (GPIOB_Base);
   pragma Import (Ada, GPIOB);

   -----------
   -- USART --
   -----------

   package USART is

      --  Bit definitions for USART CR1 register

      CR1_SBK     : constant Bits_16 := 16#0001#;   -- Send Break
      CR1_RWU     : constant Bits_16 := 16#0002#;   -- Receiver Wakeup
      CR1_RE      : constant Bits_16 := 16#0004#;   -- Receiver Enable
      CR1_TE      : constant Bits_16 := 16#0008#;   -- Transmitter Enable
      CR1_IDLEIE  : constant Bits_16 := 16#0010#;   -- IDLE Interrupt Enable
      CR1_RXNEIE  : constant Bits_16 := 16#0020#;   -- RXNE Interrupt Enable
      CR1_TCIE    : constant Bits_16 := 16#0040#;   -- Xfer Complete Int. Ena.
      CR1_TXEIE   : constant Bits_16 := 16#0080#;   -- PE Interrupt Enable
      CR1_PEIE    : constant Bits_16 := 16#0100#;   -- PE Interrupt Enable
      CR1_PS      : constant Bits_16 := 16#0200#;   -- Parity Selection
      CR1_PCE     : constant Bits_16 := 16#0400#;   -- Parity Control Enable
      CR1_WAKE    : constant Bits_16 := 16#0800#;   -- Wakeup Method
      CR1_M       : constant Bits_16 := 16#1000#;   -- Word Length
      CR1_UE      : constant Bits_16 := 16#2000#;   -- USART Enable
      CR1_OVER8   : constant Bits_16 := 16#8000#;   -- Oversampling by 8 Enable

      --  Bit definitions for USART CR2 register

      CR2_ADD     : constant Bits_16 := 16#000F#;   -- Address of USART Node
      CR2_LBDL    : constant Bits_16 := 16#0020#;   -- LIN Brk Detection Length
      CR2_LBDIE   : constant Bits_16 := 16#0040#;   -- LIN Brk Det. Int. Enable
      CR2_LBCL    : constant Bits_16 := 16#0100#;   -- Last Bit Clock pulse
      CR2_CPHA    : constant Bits_16 := 16#0200#;   -- Clock Phase
      CR2_CPOL    : constant Bits_16 := 16#0400#;   -- Clock Polarity
      CR2_CLKEN   : constant Bits_16 := 16#0800#;   -- Clock Enable

      CR2_STOP    : constant Bits_16 := 16#3000#;   -- STOP bits
      CR2_STOP_0  : constant Bits_16 := 16#1000#;   -- Bit 0
      CR2_STOP_1  : constant Bits_16 := 16#2000#;   -- Bit 1

      CR2_LINEN   : constant Bits_16 := 16#4000#;   -- LIN mode enable

      --  Bit definitions for USART CR3 register

      CR3_EIE     : constant Bits_16 := 16#0001#;   -- Error Interrupt Enable
      CR3_IREN    : constant Bits_16 := 16#0002#;   -- IrDA mode Enable
      CR3_IRLP    : constant Bits_16 := 16#0004#;   -- IrDA Low-Power
      CR3_HDSEL   : constant Bits_16 := 16#0008#;   -- Half-Duplex Selection
      CR3_NACK    : constant Bits_16 := 16#0010#;   -- Smartcard NACK enable
      CR3_SCEN    : constant Bits_16 := 16#0020#;   -- Smartcard mode enable
      CR3_DMAR    : constant Bits_16 := 16#0040#;   -- DMA Enable Receiver
      CR3_DMAT    : constant Bits_16 := 16#0080#;   -- DMA Enable Transmitter
      CR3_RTSE    : constant Bits_16 := 16#0100#;   -- RTS Enable
      CR3_CTSE    : constant Bits_16 := 16#0200#;   -- CTS Enable
      CR3_CTSIE   : constant Bits_16 := 16#0400#;   -- CTS Interrupt Enable
      CR3_ONEBIT  : constant Bits_16 := 16#0800#;   -- One bit method enable
   end USART;

   type USART_Registers is record
      SR         : Bits_16; -- USART Status register
      Reserved_0 : Bits_16;
      DR         : Bits_16; -- USART Data register
      Reserved_1 : Bits_16;
      BRR        : Bits_16; -- USART Baud rate register
      Reserved_2 : Bits_16;
      CR1        : Bits_16; -- USART Control register 1
      Reserved_3 : Bits_16;
      CR2        : Bits_16; -- USART Control register 2
      Reserved_4 : Bits_16;
      CR3        : Bits_16; -- USART Control register 3
      Reserved_5 : Bits_16;
      GTPR       : Bits_16; -- USART Guard time and prescaler register
      Reserved_6 : Bits_16;
   end record;

   USART1 : USART_Registers with Volatile,
                                 Address => System'To_Address (USART1_Base);

   type MCU_ID_Register is record
      DEV_ID   : Bits_12;
      Reserved : Bits_4;
      REV_ID   : Bits_16;
   end record with Pack, Size => 32;

   MCU_ID : MCU_ID_Register with Volatile,
                                 Address => System'To_Address (16#E004_2000#);
   --  Only 32-bits access supported (read-only)

   DEV_ID_STM32F40xxx : constant := 16#413#;
   DEV_ID_STM32F42xxx : constant := 16#419#;
   DEV_ID_STM32F7xxxx : constant := 16#449#;

end System.STM32F4;
