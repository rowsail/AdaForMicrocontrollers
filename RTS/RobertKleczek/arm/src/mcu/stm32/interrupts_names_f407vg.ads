--------------------------------------------------------------------------------
--                                                                            --
--                       A R M   A D A   L I B R A R Y                        --
--                                                                            --
--               I n t e r r u p t s _ N a m e s _ F 4 0 7 V G                --
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

--  Specific Interrupt names fo STM32F407VG family
--                    TO BE CONTINUED

------------------------
--  Imported Packages --
------------------------

with Ada.Interrupts;
use  Ada.Interrupts;
--   Interrupt_ID

with Ada.Interrupts.Names;
use  Ada.Interrupts.Names;

--------------------------------------------------------------------------------
--                         Interrupt_Names_F407VG                             --
--------------------------------------------------------------------------------

package Interrupts_Names_F407VG is

   ----------------------------
   --  Vector names aliases  --
   ----------------------------
   --       STM32F407VG      --
   ----------------------------

   --   INT_WWDG            : Interrupt_ID renames INT_0;
   INT_PVD             : Interrupt_ID renames INT_1;
   INT_TAMPER          : Interrupt_ID renames INT_2;
   INT_RTC             : Interrupt_ID renames INT_3;
   INT_FLASH           : Interrupt_ID renames INT_4;
   INT_RCC             : Interrupt_ID renames INT_5;
   INT_EXTI0           : Interrupt_ID renames INT_6;
   INT_EXTI1           : Interrupt_ID renames INT_7;
   INT_EXTI2           : Interrupt_ID renames INT_8;
   INT_EXTI3           : Interrupt_ID renames INT_9;
   INT_EXTI4           : Interrupt_ID renames INT_10;
   INT_DMA1_Channel1   : Interrupt_ID renames INT_11;
   INT_DMA1_Channel2   : Interrupt_ID renames INT_12;
   INT_DMA1_Channel3   : Interrupt_ID renames INT_13;
   INT_DMA1_Channel4   : Interrupt_ID renames INT_14;
   INT_DMA1_Channel5   : Interrupt_ID renames INT_15;
   INT_DMA1_Channel6   : Interrupt_ID renames INT_16;
   INT_DMA1_Channel7   : Interrupt_ID renames INT_17;
   INT_ADC1_2          : Interrupt_ID renames INT_18;
   INT_USB_HP_CAN1_TX  : Interrupt_ID renames INT_19;
   INT_USB_LP_CAN1_RX0 : Interrupt_ID renames INT_20;
   INT_CAN1_RX1        : Interrupt_ID renames INT_21;
   INT_CAN1_SCE        : Interrupt_ID renames INT_22;
   INT_EXTI9_5         : Interrupt_ID renames INT_23;
   INT_TIM1_BRK        : Interrupt_ID renames INT_24;
   INT_TIM1_UP         : Interrupt_ID renames INT_25;
   INT_TIM1_TRG_COM    : Interrupt_ID renames INT_26;
   INT_TIM1_CC         : Interrupt_ID renames INT_27;
   INT_TIM2            : Interrupt_ID renames INT_28;
   INT_TIM3            : Interrupt_ID renames INT_29;
   INT_TIM4            : Interrupt_ID renames INT_30;
   INT_I2C1_EV         : Interrupt_ID renames INT_31;
   INT_I2C1_ER         : Interrupt_ID renames INT_32;
   INT_I2C2_EV         : Interrupt_ID renames INT_33;
   INT_I2C2_ER         : Interrupt_ID renames INT_34;
   INT_SPI1            : Interrupt_ID renames INT_35;
   INT_SPI2            : Interrupt_ID renames INT_36;
   INT_USART1          : Interrupt_ID renames INT_37;
   INT_USART2          : Interrupt_ID renames INT_38;
   INT_USART3          : Interrupt_ID renames INT_39;
   INT_EXTI15_10       : Interrupt_ID renames INT_40;
   INT_RTCAlarm        : Interrupt_ID renames INT_41;
   INT_USBWakeUp       : Interrupt_ID renames INT_42;
   INT_TIM8_BRK        : Interrupt_ID renames INT_43;
   INT_TIM8_UP         : Interrupt_ID renames INT_44;
   INT_TIM8_TRG_COM    : Interrupt_ID renames INT_45;
   INT_TIM8_CC         : Interrupt_ID renames INT_46;
   INT_ADC3            : Interrupt_ID renames INT_47;
   INT_FSMC            : Interrupt_ID renames INT_48;
   INT_SDIO            : Interrupt_ID renames INT_49;
   INT_TIM5            : Interrupt_ID renames INT_50;
   INT_SPI3            : Interrupt_ID renames INT_51;
   INT_UART4           : Interrupt_ID renames INT_52;
   INT_UART5           : Interrupt_ID renames INT_53;
   INT_TIM6            : Interrupt_ID renames INT_54;
   INT_TIM7            : Interrupt_ID renames INT_55;
   INT_DMA2_Channel1   : Interrupt_ID renames INT_56;
   INT_DMA2_Channel2   : Interrupt_ID renames INT_57;
   INT_DMA2_Channel3   : Interrupt_ID renames INT_58;
   INT_DMA2_Channel4_5 : Interrupt_ID renames INT_59;

end Interrupts_Names_F407VG;
