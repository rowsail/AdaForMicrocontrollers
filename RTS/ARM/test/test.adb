with Board.LPC2148_Board;
pragma Unreferenced (Board.LPC2148_Board);

with Timing.LPC21XX.T;
pragma Unreferenced (Timing.LPC21XX.T);

with PP;

procedure Test is

begin
   PP.IO.Put (True);

   while True
   loop
      null;
   end loop;

end Test;
