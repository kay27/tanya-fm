uses mb2;
begin
   repeat
      scan;
      if kp then writeln (keyv,#9,ek);
   until kp and not ek and (keyv=27);
end.
