with MCU.Interrupts_Names;
use  MCU.Interrupts_Names;

package PP is

   protected IO is
      entry Put (X : Boolean);
   private
      procedure Handler;
      pragma Attach_Handler (Handler, INT_RTC);
      Rx : Boolean := False;
   end IO;

end PP;
