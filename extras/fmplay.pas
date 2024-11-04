uses mb2;

const fport = $388;
      freq : array [0..11] of word = ($157,$16B,$181,$198,$1B0,$1CA,$1E5,$202,$220,$241,$263,$287);

var n, t, pa, po, k, b, ps : byte;
   f : file of byte;
   fe, err : boolean;
   tune : array [0..19, 1..9, 0..63, 0..1] of byte;
   order : array [0..255] of byte;
   smp : array [1..15, 1..11] of byte;
   orn : array [1..15, 0..31] of byte;
   a1, a2, a : word;
   fdata : array [0..255] of byte;

procedure fout (a, b : byte); assembler;
asm
   mov   dx,fport
   mov   al,a
   out   dx,al
   mov   cx,6
@1:in    al,dx
   loop  @1
   inc   dl
   mov   al,b
   out   dx,al
   dec   dl
   mov   cx,35
@2:in    al,dx
   loop  @2
   mov   bl,a
   xor   bh,bh
   add   bx,offset fdata
   mov   al,b
   mov   [bx],al
end;

function fin (a : byte) : byte;
begin asm
   mov   bl,a
   xor   bh,bh
   add   bx,offset fdata
   mov   al,[bx]
   mov   @result,al
end end;

procedure fstop;
var f : byte;
begin
   for f := 0 to 8 do
      begin
         fout ($a0 + f, 0);
         fout ($b0 + f, 0)
      end
end;

procedure lplay;
var k, o1, o2, t, o, s, ok, frq : byte;
    fr : word;
begin
   for k := 1 to 9 do
      begin
         if k < 4 then o1 := k - 1
         else
            if k > 6 then o1 := k + 9
            else o1 := k + 4;
         o2 := o1 + 3;
         t := tune [pa, k, po, 0];
         s := tune [pa, k, po, 1];
         if t = 253 then fout ($bd, s);
         o := s and 15;
         s := s div 16;
         if (o <> 0) or (s <> 0) or (t = 254) then
            fout ($b0 + k - 1, fin ($b0 + k - 1) and $DF);
         ok := (t div 12);
         if t < 96 then
            begin
               t := t mod 12;
               fr := freq [t];
               ok := ok * 4 + fr div 256;
               frq := fr and 255;
               if s > 0 then
                  begin
                     fout ($e0+o1,smp [s, 1]);
                     fout ($e0+o2,smp [s, 2]);
                     fout ($c0+k-1,smp [s, 3]);
                     fout ($20+o1, smp [s, 4]);
                     fout ($40+o1, smp [s, 5]);
                     fout ($60+o1, smp [s, 6]);
                     fout ($80+o1, smp [s, 7])
                  end;
               fout ($a0+k-1, frq);
               fout ($b0+k-1, $20 or ok);
               if s > 0 then
                  begin
                     fout ($20+o2, smp [s, 8]);
                     fout ($40+o2, smp [s, 9]);
                     fout ($60+o2, smp [s, 10]);
                     fout ($80+o2, smp [s, 11])
                  end
            end
      end
end;

begin
   fstop;
   writeln ('FM-Tracker Module Retrace Player Version 1.00 Beta.');
   writeln ('NUCLEAR Group, (C) 1998, 1999.');
   if paramcount = 0 then
      writeln ('Technout: to listen FMT-Module type "FMPLAY FILENAME.FMM" in DOS command line.')
    else
       begin
          for n := 1 to paramcount do
             begin
                writeln ('Loading ', paramstr (n), '...');
                assign (f, paramstr (n));
                reset (f);
                read (f, b);
                err := b <> ord ('F');
                read (f, b);
                err := err or (b <> ord ('M'));
                if err then
                   writeln ('   this is not FMT-Module!')
                 else
                    begin
                       read (f, t);
                       read (f, b);
                       a := b;
                       read (f, b);
                       a := 256 * b + a;
                       seek (f, a);
                       a := 2;
                       repeat
                          read (f, b);
                          order [a] := b;
                          inc (a)
                       until b = 255;
                       read (f, b);
                       order [1] := b;
                       for a1 := 1 to 15 do
                          begin
                             seek (f, 3+2*a1);
                             read (f, b);
                             a := b;
                             read (f, b);
                             a := 256 * b + a;
                             if a <> 0 then
                                begin
                                   seek (f, a);
                                   for a2 := 1 to 11 do
                                      begin
                                         read (f, b);
                                         smp [a1, a2] := b
                                      end
                                end;
                             seek (f, 33+a1*2);
                             read (f, b);
                             a := b;
                             read (f, b);
                             a := 256 * b + a;
                             if a <> 0 then
                                begin
                                   seek (f, a);
                                   for a2 := 0 to 31 do
                                      begin
                                         read (f, b);
                                         orn [a1, a2] := b
                                      end
                                end
                          end;
                       for a1 := 0 to 19 do
                          begin
                             seek (f, 65 + a1 * 2);
                             read (f, b);
                             a := b;
                             read (f, b);
                             a := 256 * b + a;
                             if a <> 0 then
                                begin
                                   seek (f, a);
                                   for k := 1 to 9 do
                                      for po := 0 to 63 do
                                         for a2 := 0 to 1 do
                                            begin
                                               read (f, b);
                                               tune [a1, k, po, a2] := b
                                            end
                                end
                          end;
                       ps := 0;
                       fout ($BD,0);
                       writeln ('Playing...');
                       repeat
                          pa := order [ps + 2];
                          fe := key;
                          if pa = 255 then ps := order [1]
                           else
                              begin
                                 po := 0;
                                 repeat
                                    lplay;
                                    for a1 := 1 to t do waiting;
                                    inc (po);
                                    fe := key
                                 until (po>63) or fe;
                                 inc (ps)
                              end
                       until fe;
                       fstop
                    end
             end
       end
end.