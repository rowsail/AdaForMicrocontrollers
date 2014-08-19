package body PP is

   protected body IO is
      procedure Handler is
      begin
         Rx := not Rx;
      end Handler;

      entry Put (X : Boolean) when Rx is
      begin
         Rx := X;
      end Put;
   end IO;

end PP;
