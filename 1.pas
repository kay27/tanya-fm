uses mb2,browser,dos;
const tn=128; {max of notes in pattern, 1..256}
      cn=9; {max of channels, 9 for FM Synthesis}
      pn=48; {max of patterns, 1..254, so 255 is the finish marker in list}
      sn=64; {max of samples}
      ml=256; {max of positions in list, now up to 256}
      vn=64; {max of volume gradatuons}
      ec: array[0..9] of string=(
       'OK ;)','Can not write file','Disk space overflow','Can not open file',
       'File format is wrong','Out of range!','Bad timer constant! Press F9 to change...',
       'Position list is empty! Press F9 and fill...',
       'Can not fully complete! Press again to confirm.',
       'List (F9) does not contain this pattern.');
      tune_text: array[0..11] of string[2]=(
       'C-','C#','D-','D#','E-','F-','F#','G-','G#','A-','A#','B-');
      rl=6; {lenght of single row}
      tune_keys: string='ZSXDCVGBHNJM'+'Q2W3ER5T6Y7U'+'I9O0P[=]\';
      stn=28; {max of sample text lenght}
      hlppos: word=51;
type tune=record n: byte; s: byte; e: word; v: byte; end; {n=note; s=sample; e: effect}
     tune_old=record n: byte; s: byte end; {n=note; s=sample}
     row=array [0..cn-1] of tune; {one horizontal line with tunes in editor}
     row_old=array [0..cn-1] of tune_old; {one horizontal line with tunes in editor}
     pattern=record l: byte; t: array [0..tn-1] of row; end; {l=length}
     pattern_old=record l: byte; t: array [0..tn-1] of row_old; end; {l=length}
     sample=record t: string[stn]; dn:string[12];f0:shortint;a20,a21, a40,a41, a60,a61, a80,a81, ac, ae0, ae1: byte end;
     sample_old=record t: string[20]; a20,a21, a40,a41, a60,a61, a80,a81, ac, ae0, ae1: byte end;
     sysinfo=record
        n: array [0..31] of char; {title}
        t: word; {tempo}
        s: byte; {samples}
        p: byte; {patterns}
        m: word; {active channel bit mask}
    end;
    selectpart=record x,y: byte end;
     hlpitems=array[0..9] of string[6];
var tp: pattern; {temporary}
    m: array [0..pn-1] of ^pattern; {whole song}
    list: array [0..ml-1] of byte; {position list}
    s: array [0..sn] of sample; {sn: temporary}
    i,i0: sysinfo;
    table: array [0..10*12-1] of word;
    e: byte; {error code}
    start,finish,start1,finish1: selectpart;

    cp, cs, co, cv, {current: pattern, sample, octave, volume, }
    cpos, cc, coffs,     {    position, channel, page offset,}
    cwhat, cax, cbx,     {    tune/octabe/sample, play registers }
    vsc, csh, cel, ceh   {    volume scaling, show: samples/volumes/efects, effect: low/high}
           :byte;
    c_autostep
           :shortint;
    c_unsaved, c_cursorstep
           : boolean;

    xs: byte; {where the bars begin...}
    ov: pointer;
    ticker: byte;
    csp,ctp,cvp: array [0..cn-1] of byte; {current samples,tunes,volumes for playback}
    ppl, ppn, ppp: byte; {play positions: in list, pattern number, in pattern}
    ppj, playback, lockpat: boolean; {jump: yes/no; playback now: yes/no; 1 pattern: yes/no}
    ppjl, ppjp: byte; {jump to position in list, in pattern}
    eff: array [0..cn-1] of record c: char; p: byte end; {effects}
const already:byte=$a2;
procedure prog (x, y: byte); var i,j: byte; begin port[$388]:=x; for i:=0 to 5 do j:=port[$388];
port[$389]:=y; for i:=0 to 34 do j:=port[$389] end;
procedure timer_reset;
begin if already<>$79 then exit;
   asm cli end; port [$43]:=$36; port [$40]:=0; port [$40]:=0;
   asm sti end; setintvec ($8, ov); already:=$a2;
end;
function sambyte (n,i: byte): byte; var b: byte; begin case i of
  0: b:=s[n].a20; 1:b:=s[n].a21; 2: b:=s[n].a40; 3: b:=s[n].a41;
  4: b:=s[n].a60; 5:b:=s[n].a61; 6: b:=s[n].a80; 7: b:=s[n].a81; 8: b:=cax; 9: b:=cbx;
  10: b:=s[n].ac; 11: b:=s[n].ae0; 12: b:=s[n].ae1; end;
  sambyte:=b
end;
procedure samstore (n,i,b: byte); begin case i of
  0: s[n].a20:=b; 1:s[n].a21:=b; 2: s[n].a40:=b; 3: s[n].a41:=b;
  4: s[n].a60:=b; 5:s[n].a61:=b; 6: s[n].a80:=b; 7: s[n].a81:=b;
  10: s[n].ac:=b; 11: s[n].ae0:=b; 12: s[n].ae1:=b end;
end;
function samtrans (c, i: byte): byte; const trt:array[0..12]of byte=(
$20,$23,$40,$43,$60,$63,$80,$83,$a0,$b0,$c0,$e0,$e3); cht:array[0..8] of byte=(
0,1,2,8,9,10,16,17,18); var a: byte; begin
   a:=trt[i]; if (i<8)or(i>10) then inc (a,cht[c]) else inc (a,c);
samtrans:=a; end;
procedure splay (c, ss, t, v: byte); var i,q: byte; begin if ss<>255 then begin prog (samtrans(c,9),0); for i:=0 to 12 do
 if (i<8)or(i>9) then if ((i=3)or(i=3-s[ss].ac and 1))and(v<255) then begin q:=sambyte(ss,i); prog(samtrans(c,i),
   q and $c0+$3f-byte(word($3f-q and $3f)*(v and $3f) div 63)) end else prog(samtrans(c,i),sambyte(ss,i)) end;
 inc (t,s[ss].f0); if t>12*10-1 then t:=12*10-1;
 cax:=byte(table[t] and $ff); cbx:=byte(table[t] shr 8) or $20;
 prog (samtrans(c,8),cax); prog (samtrans(c,9),cbx); if ss<>255 then csp[c]:=ss;
end;
procedure change_pitch (c, t, v: byte); begin inc (t,s[csp[c]].f0); if t>12*10-1 then t:=12*10-1;
cax:=byte(table[t] and $ff); cbx:=byte(table[t] shr 8) or $20; prog (samtrans(c,8),cax); prog (samtrans(c,9),cbx); end;
procedure mute_channel (c: byte); var j: byte;
begin
   prog (samtrans(c,9),0);
   prog (samtrans(c,8),0);
{   for j:=0 to 12 do prog (samtrans(c,j),0)}
end;
procedure change_vol(c,v: byte); var q: byte; begin if v>63 then exit;
 q:=sambyte(csp[c],3); prog(samtrans(c,3), q and $c0+$3f-byte(word($3f-q and $3f)*(v and $3f) div 63));
 if s[csp[c]].ac and 1>0 then
  begin q:=sambyte(csp[c],2); prog(samtrans(c,2), q and $c0+$3f-byte(word($3f-q and $3f)*(v and $3f) div 63)) end;
end;
procedure effect (c: byte; e: word); forward;
procedure playbar_realtime; var j: byte;
begin
 for j:=0 to cn-1 do
  begin
    effect (j, m[ppn]^.t[ppp][j].e);
    if i0.m and (1 shl j)>0 then
     begin
       if m[ppn]^.t[ppp][j].n<10*12 then
        if m[ppn]^.t[ppp][j].s<sn then
         splay (j, m[ppn]^.t[ppp][j].s, m[ppn]^.t[ppp][j].n, m[ppn]^.t[ppp][j].v)
        else begin change_pitch (j, m[ppn]^.t[ppp][j].n, m[ppn]^.t[ppp][j].v); change_vol(j, m[ppn]^.t[ppp][j].v) end
       else
       if m[ppn]^.t[ppp][j].n=254 then
        mute_channel (j)
       else change_vol(j, m[ppn]^.t[ppp][j].v)
     end
    else begin cbx:=0; prog(samtrans(j,9),cbx) end;
    if m[ppn]^.t[ppp][j].n < 120 then
     begin ctp[j]:=m[ppn]^.t[ppp][j].n; cvp[j]:=m[ppn]^.t[ppp][j].v; if cvp[j]>63 then cvp[j]:=63 end
      else if m[ppn]^.t[ppp][j].n = 254 then ctp[j]:=255
      else begin cvp[j]:=m[ppn]^.t[ppp][j].v; if cvp[j]>63 then cvp[j]:=63 end;
  end
end;
procedure player2;
begin
 if ppj then
  begin
     if not lockpat then
      begin
         ppl:=ppjl;
         ppn:=list[ppl];
         if ppn>pn-1 then begin ppn:=list[0]; ppl:=0 end;
      end;
     ppp:=ppjp; if ppp>=m[ppn]^.l then ppp:=0;
     ppj:=FALSE
  end else
  begin
     inc (ppp);
     if ppp>=m[ppn]^.l then
      begin
         ppp:=0; if not lockpat then
          begin inc (ppl);
             if list[ppl]=255 then ppl:=0;
             ppn:=list[ppl]
          end;
      end;
  end;
 playbar_realtime;
end;
procedure e2; var j: byte;
begin for j:=0 to cn-1 do if ctp[j]<120 then
   case eff[j].c of
     'J': if ticker mod 3=0 then change_pitch (j, ctp[j], cvp[j]) else
          if ticker mod 3=1 then begin if ctp[j]+eff[j].p shr 4<120 then
            change_pitch(j, ctp[j]+eff[j].p shr 4,cvp[j]) end else
          if ctp[j]+eff[j].p and 15<120 then change_pitch (j, ctp[j]+eff[j].p and 15, cvp[j]);
   end;
end;
procedure player;interrupt;
 begin {effects;}inc(ticker);if ticker>=i0.t and$ff then begin ticker:=0;player2 end else e2;port[$20]:=$20 end;
function preclip (var x0,y0,x1,y1: byte): boolean;
begin if start.x and start.y and finish.x and finish.y=255 then begin x0:=cc;x1:=cc;y0:=cpos;y1:=cpos end else
   begin if start.y<finish.y then begin y0:=start.y;y1:=finish.y end else begin y1:=start.y;y0:=finish.y end;
   if start.x<finish.x then begin x0:=start.x;x1:=finish.x end else begin x1:=start.x;x0:=finish.x end; end;
   preclip:=TRUE
end;
function preclip1 (var x0,y0,x1,y1: byte): boolean;
begin if start1.x and start1.y and finish1.x and finish1.y=255 then preclip1:=FALSE else
 begin if start1.y<finish1.y then begin y0:=start1.y;y1:=finish1.y end else begin y1:=start1.y;y0:=finish1.y end;
  if start1.x<finish1.x then begin x0:=start1.x;x1:=finish1.x end else begin x1:=start1.x;x0:=finish1.x end;preclip1:=TRUE end
end;
procedure timer_prog; var freq: word;
begin if already<>$a2 then exit; freq:=round(18.2*65536/(i0.t shr 8)*2.5);
   getintvec ($8, ov); setintvec ($8, @player); asm cli end;
   port [$43]:=$36; port [$40]:=lo (freq); port [$40]:=hi (freq);
   asm sti end; already:=$79;
end;
procedure timer_only; var freq: word;
begin if already<>$79 then exit; freq:=round(18.2*65536/(i0.t shr 8)*2.5);
   asm cli end; port [$43]:=$36; port [$40]:=lo (freq); port [$40]:=hi (freq); asm sti end;
end;
procedure mute; var x: byte; begin if playback then begin playback:=FALSE; timer_reset end;
 for x:=0 to 255 do prog(x,0); cax:=0; cbx:=0; prog (1,32); end;
procedure exit17; var j: byte; begin mute; for j:=pn-1 downto 0 do dispose (m[j]); halt; end;
procedure fade; var w: word; begin for w:=0 to 25*80 do mem [$b800:w shl 1+1]:=mem[$b800:w shl 1+1] and $7 end;
procedure unsave_pat (p: byte); begin c_unsaved:=TRUE; if p>=i.p then i.p:=p+1 end;
procedure unsave_sam (s: byte); begin c_unsaved:=TRUE; if s>=i.s then i.s:=s+1 end;
procedure status; var x: byte;
begin color (16*5+14); for x:=0 to 58 do textxy (x, 23, ' ');
   textxy (1,23,'Status: '+ec[e]); color (16*5+12); textxy (59, 23, '(c) Hacker KAY, 2002 ');
end; procedure screen; forward;
procedure help; var hhh: array [0..150] of string[80]; f: text; w,q,a: word; c,x: byte;
begin assign (f, 'tanya1_d.hlp'); reset (f); if ioresult<>0 then exit;w:=0; while not eof (f) do
 begin readln(f, hhh[w]); inc (w) end; close (f); q:=0; c:=0;
  for a:=0 to w-1 do if hhh[a]<>'' then for x:=1 to length(hhh[a]) do begin inc (q, byte(hhh[a][x])); c:=c xor byte(hhh[a][x])
  end; q:=q xor (word(c) shl 8); if q<>31988 then exit;
  fade; chide; border(1); repeat waiting; color (30); for q:=hlppos to hlppos+24 do for x:=0 to 79 do
   if q>=w then textxy (x,q-hlppos,' ') else if x>=length(hhh[q]) then textxy (x,q-hlppos,' ') else
   textxy (x,q-hlppos,hhh[q][x+1]); scan; if kp and ek then begin hlppos:=hlppos+byte((keyv=80)and(hlppos<w-24))-
    byte((keyv=72)and(hlppos>0)); if keyv=81 then inc (hlppos,25); if (hlppos>w-24)or(keyv=79) then hlppos:=w-24;
    if (keyv=73)and(hlppos>24) then dec (hlppos,25) else if keyv in [71,73] then hlppos:=0 end; until kp and not ek and
    (keyv=27); keyv:=0; border(0); screen; cshow;
end;
function ask (s: string): boolean;
begin
   chide; color (16*4+15); s:=' '+s+' (Y/N)';
   while length(s)<80 do s:=s+' '; textxy (0, 23, s);
   repeat scan; if kp and not ek then case
      keyv of 27,byte('N'),byte('n'): begin ask:=FALSE;status;exit end;
      13,byte('Y'),byte('y'): begin ask:=TRUE;status;exit end end;
   until false;
end;
function hex (a: byte): string;
begin hex:=char(48+a shr 4+7*byte(a shr 4>9))+char(48+a and 15+7*byte(a and 15>9)) end;
function decb (a: byte): string; var s: string;
begin s:=''; repeat s:=char(a mod 10+48)+s; a:=a div 10 until a=0; decb:=s end;
function deci (a: integer): string; var s,s2: string; begin s:=''; if a<0 then begin s2:='-'; a:=abs(a); end else s2:='';
repeat s:=char(a mod 10+48)+s; a:=a div 10 until a=0; deci:=s2+s end;
function bin (a: byte): string; var s: string; i: byte;
begin s:=''; for i:=0 to 7 do s:=s+char(48+byte(a and (128 shr i)>0)); bin:=s end;
function decw (a: word): string; var s: string;
begin s:=''; repeat s:=char(a mod 10+48)+s; a:=a div 10 until a=0; decw:=s end;
procedure save (ss: string); {path\filename}
var f: file; u,w: word; j0,j1,b,n,n0,n1,n2,n3: byte; rt: row; sq:array[0..255]of byte;const ver:string=#$11'KAY';
 et:tune=(n:$ff;s:$ff;e:$ff00;v:$ff);
label no_space; function tcmp (t1,t2: tune): boolean; begin tcmp:=(t1.n=t2.n)and(t1.s=t2.s)and(t1.v=t2.v)and(t1.e=t2.e) end;
begin {$I-} e:=0; mute;
   assign (f, ss);
   reset (f, 1);
   if ioresult=0 then
    begin close (f); if not ask ('File exist. Overwrite?') then begin keyv:=0; exit; end; keyv:=0; end;
   rewrite (f,1);
   if ioresult<>0 then begin e:=1; exit end;
   i.p:=255;
   for n:=pn-1 downto 0 do if i.p=255 then
    begin if m[n]^.l<>64 then begin i.p:=n; break end;
       if i.p=255 then
        for n0:=0 to 63 do if i.p=255 then for n1:=0 to cn-1 do
         if (m[n]^.t[n0][n1].n<>255)or
            (m[n]^.t[n0][n1].s<>255)or
            (m[n]^.t[n0][n1].v<>255)or
            (m[n]^.t[n0][n1].e shr 8<>$ff) then
               begin i.p:=n; break end;
    end; inc (i.p);
   i.s:=255;
   for n:=sn-1 downto 0 do if i.s=255 then
    begin if s[n].t<>'                            ' then begin i.s:=n; break end;
       if s[n].dn<>'            ' then begin i.s:=n; break end;
       if s[n].f0<>0 then begin i.s:=n; break end;
       for n0:=0 to 10 do if i.s=255 then if sambyte(n,n0+2*byte(n0>7))<>0 then
          begin i.s:=n; break end;
    end; inc (i.s);
{   n:=$1d; blockwrite (f, n, 1, u); if u<>1 then goto no_space;
   blockwrite (f, i, sizeof(sysinfo), u);}{
New: }
   blockwrite (f, ver[1], length(ver), u); if u<>length(ver) then goto no_space;
   blockwrite (f, i.t, 2, u); if u<>2 then goto no_space;
   blockwrite (f, i.p, 1, u); if u<>1 then goto no_space;
   blockwrite (f, i.s, 1, u); if u<>1 then goto no_space;
   blockwrite (f, i.m, 2, u); if u<>2 then goto no_space;
   n1:=32; while (i.n[n1-1]=' ')and(n1>1) do dec (n1); if i.n[n1-1]=' ' then dec (n1);
   if n1>0 then begin blockwrite (f, i.n, n1, u); if u<>n1 then goto no_space end;
   n0:=0; blockwrite (f, n0, 1, u); if u<>1 then goto no_space;
   blockwrite (f, n0, 1, u); if u<>1 then goto no_space; {skip author name...}
   n:=0; repeat blockwrite (f, list[n], 1, u); if u<>1 then goto no_space; inc (n) until (list[n-1]=255)or(n=255);
   if n=255 then begin n0:=255; blockwrite (f, n0, 1, u); if u<>1 then goto no_space end;
   if i.p>0 then for n2:=0 to i.p-1 do begin {save pat N2}
     n0:=0; for n:=0 to cn-1 do rt[n]:=et;
     repeat j0:=0; for n:=0 to cn-1 do if not tcmp(m[n2]^.t[n0][n],et) then begin j0:=1; break end;
      if j0=0 then begin b:=123; {SL} blockwrite (f, b, 1, u); if u<>1 then goto no_space end else
      begin
       if tcmp(m[n2]^.t[n0][0],et) then begin b:=124; {ST} blockwrite (f, b, 1, u); if u<>1 then goto no_space end else
        if tcmp(m[n2]^.t[n0][0],rt[0]) then begin b:=125; {RLT} blockwrite(f,b,1,u); if u<>1 then goto no_space end else
         with m[n2]^.t[n0][0] do begin
          w:=n and $7f+$80*byte(v<>$ff)+word(s and $7f+$80*byte(e=rt[0].e)) shl 8;
          blockwrite(f,w,2,u); if u<>2 then goto no_space; {tune/smp}
          if w and $80>0 then begin blockwrite(f,v,1,u); if u<>1 then goto no_space; end; {vol}
          if w and $8000=0 then begin blockwrite(f,e,2,u); if u<>2 then goto no_space; end; {vol}
        end; n3:=0; for n:=1 to cn-1 do n3:=n3 shl 1+byte(tcmp(m[n2]^.t[n0][n],et)) xor 1;
       blockwrite(f,n3,1,u); if u<>1 then goto no_space;
       for n1:=1 to cn-1 do if boolean((n3 shr (cn-n1-1)) and 1) then
        begin
         if tcmp(m[n2]^.t[n0][n1],et) then begin b:=124; {ST} blockwrite (f, b, 1, u); if u<>1 then goto no_space end else
          if tcmp(m[n2]^.t[n0][n1],rt[n1]) then begin b:=125;{RLT}blockwrite(f,b,1,u); if u<>1 then goto no_space end else
           with m[n2]^.t[n0][n1] do begin
            w:=n and $7f+$80*byte(v<>$ff)+word(s and $7f+$80*byte(e=rt[n1].e)) shl 8;
            blockwrite(f,w,2,u); if u<>2 then goto no_space; {tune/smp}
            if w and $80>0 then begin blockwrite(f,v,1,u); if u<>1 then goto no_space; end; {vol}
            if w and $8000=0 then begin blockwrite(f,e,2,u); if u<>2 then goto no_space; end; {vol}
          end;
       end;
      end; for n:=0 to cn-1 do rt[n]:=m[n2]^.t[n0][n]; inc (n0);
     until n0=m[n2]^.l;
     b:=122; {EOP} blockwrite (f, b, 1, u); if u<>1 then goto no_space;
   end;
   if i.s>0 then for n:=0 to i.s-1 do {save sam N}
    with s[n] do begin
       sq[0]:=byte(f0);
       sq[1]:=a20;sq[2]:=a21;sq[3]:=a40;sq[4]:=a41;
       sq[5]:=a60;sq[6]:=a61;sq[7]:=a80;sq[8]:=a81;
       sq[9]:=ac;sq[10]:=ae0;sq[11]:=ae1;
       blockwrite (f, sq, 12, u); if u<>12 then goto no_space;
       for j0:=1 to 32 do begin if t[j0]=#0 then t[j0]:=' ';if j0<13 then if dn[j0]=#0 then dn[j0]:=' ' end;
       j0:=stn; while t[j0]=' ' do dec (j0);
       if j0>0 then begin blockwrite (f, t[1], j0, u); if u<>j0 then goto no_space end;
       j0:=0; blockwrite (f, j0, 1, u); if u<>1 then goto no_space;
       j0:=12; while dn[j0]=' ' do dec (j0);
       if j0>0 then begin blockwrite (f, dn[1], j0, u); if u<>j0 then goto no_space end;
       j0:=0; blockwrite (f, j0, 1, u); if u<>1 then goto no_space;
    end;
{   if u<>sizeof (sysinfo) then goto no_space;
   blockwrite (f, list, ml, u); if u<>ml then goto no_space;
   color (15); textxy (0,2,' '+hex(i.s)+'h samples, '+hex(i.p)+'h patterns '); waiting;waiting;waiting;waiting;waiting;
   if i.p>0 then
    for n:=0 to i.p-1 do
     begin
        blockwrite (f,m[n]^,sizeof(pattern),u);
        if u<>sizeof(pattern) then goto no_space;
     end;
    if i.s>0 then
    for n:=0 to i.s-1 do
     begin
        blockwrite (f,s[n],sizeof(sample),u);
        if u<>sizeof(sample) then goto no_space;
     end;}
   close (f);
   e:=0; c_unsaved:=FALSE;
   exit;
no_space:
   e:=2;
   close (f);
   erase (f);
end;
procedure samsave (sn: byte; ss: string); {path\filename}
var f: file; u: word;
begin {$I-} e:=0; mute;
   assign (f, ss); reset (f, 1);
   if ioresult=0 then
    begin close (f); if not ask ('File exist. Overwrite?') then begin keyv:=0; exit; end; keyv:=0; end;
   rewrite (f,1); if ioresult<>0 then begin e:=1; exit end;
   blockwrite (f, s[sn], sizeof(sample), u);
   close (f);
   if u<>sizeof (sample) then begin e:=2; erase (f) end;
end;
procedure samload (sn: byte; ss: string); {path\filename}
var f: file; u: word; l: longint; sto: sample_old;
begin e:=0; mute;
   assign (f, ss); reset (f,1); if ioresult<>0 then begin e:=3; exit end;
   l:=filesize(f); if l=sizeof(sample_old) then
    begin blockread (f,sto,sizeof(sample_old),u);
          s[sn].a20:=sto.a20;s[sn].a21:=sto.a21;s[sn].a40:=sto.a40;
          s[sn].a41:=sto.a41;s[sn].a60:=sto.a60;s[sn].a61:=sto.a61;
          s[sn].a80:=sto.a80;s[sn].a81:=sto.a81;s[sn].ae0:=sto.ae0;
          s[sn].ae1:=sto.ae1;s[sn].ac:=sto.ac;s[sn].t:=sto.t;
          while length(s[sn].t)<stn do s[sn].t:=s[sn].t+' ';
          s[sn].dn:='            '; if u<>sizeof(sample_old) then e:=4;
          close (f);
    end else
  begin
   blockread (f, s[sn], sizeof(sample), u);
   while length(s[sn].t)<20 do s[sn].t:=s[sn].t+' ';
   close (f);
   if u<>sizeof (sample) then e:=4;
  end;
end;
procedure init_vars; forward;
procedure load (ss: string); {path\filename}
var f:file; w,j2,wa,ws,wo,wi,wp,wf,wc,wffi,u:word; frst,n,j0,j1,b,b0,b1,b2:byte; tp:pattern_old; s3h:array [0..255] of byte;
    ii: array [0..sn-1] of longint; pp: array [0..pn-1] of longint; sto: sample_old; f0x: single;
    dps:array[0..255] of byte; lt: row;
const c=1.05946309435929526456182529494634; et:tune=(n:255;s:255;e:$ff00;v:255);
label bad_file; function getw(i: byte): word; begin getw:=word(s3h[i+1])shl 8+word(s3h[i]) end;
procedure warning(s: string); begin color (12*16+1); textxy (0,0,' WARNING! '+s+'. Press any key... '); pause end;
begin mute;
   assign (f, ss);
   reset (f,1);
   if ioresult<>0 then begin e:=3; exit end;
   if ss[length(ss)]='0' then
    begin {Load D00}
       blockread (f, s3h, $75, u); if u<>$75 then goto bad_file;
       if (s3h[0]<>byte('J'))or(s3h[1]<>byte('C'))or(s3h[2]<>byte('H'))or(s3h[3]<>$26)or(s3h[4]<>2)or(s3h[5]<>$66)
        then goto bad_file; {bad identifier}
       for n:=1 to 28 do i.n[n]:=char(s3h[10+n]); {name}
       wa:=getw($6b); {  arrangment_data^ }
       ws:=getw($6d); {    sequence_data^ }
       wi:=getw($6f); {  instrument_data^ }
       wc:=getw($71); { song_description^ }
       wf:=getw($73); {       sp_fx_data^ }


       close (f); exit;
    end else
   if ss[length(ss)]='D' then
    begin {Load RAD}
       blockread (f, s3h, 18, u); if u<>18 then goto bad_file;
       if (char(s3h[0])<>'R')or(char(s3h[1])<>'A')or(char(s3h[2])<>'D') then warning ('File header is incorrect');
       if s3h[17] and $40>0 then i.t:=46*256+s3h[17] and $1f else i.t:=125*256+s3h[17] and $1f;
       if s3h[17] and 128>0 then {skip description}
        repeat blockread (f, b, 1, u); if u<>1 then goto bad_file until b=0;
       j1:=0;
       repeat blockread (f, n, 1, u); if u<>1 then goto bad_file; {read samples}
          if n>0 then
           begin blockread (f, s3h, 11, u); if u<>11 then goto bad_file;
               if j1=0 then if n<=sn then begin
              for j0:=0 to 10 do samstore (n-1,j0 xor (1*byte(j0<8))+2*byte(j0>7)+byte(j0=9)-byte(j0=10),s3h[j0]); i.s:=n end
                else begin j1:=255; warning ('Too many samples') end;
           end;
       until n=0;
       blockread (f, n, 1, u); if u<>1 then goto bad_file; {read order list} j0:=0;
       if n>128 then warning ('Order list length='+hex(n)+'h'); j1:=0;
       while j0<n do begin blockread (f, b, 1, u); if u<>1 then goto bad_file;
          if j1=0 then if b<32 then list[j0]:=b else begin list[j0]:=255; j1:=255 end; inc (j0); end;
       list[j0]:=255; {pattern pointers: }
       for j0:=0 to 31 do begin blockread (f, w, 2, u); if u<>2 then goto bad_file;
        if j0<pn then pp[j0]:=w end;
       j0:=0; while (j0<32)and(j0<pn) do
        if pp[j0]<>0 then begin seek (f, pp[j0]);
         repeat blockread (f, b2, 1, u); if u<>1 then goto bad_file; {line no}
            repeat blockread (f, b1, 1, u); if u<>1 then goto bad_file; {channel}
               blockread (f, w, 2, u); if u<>2 then goto bad_file; {note/effect}
               if w and $f00<>0 then
                begin blockread (f, b, 1, u); if u<>1 then goto bad_file; {command info} end else b:=0;
               with m[j0]^.t[b2 and $7f][b1 and $f] do
                begin if byte(w and $f) in [1..12] then n:=(w and $f-1)+12*((w shr 4) and 7)+25 else n:=0;
                   if (w shr 12) + (w and $80) shr 3=0 then s:=255 else s:=(w shr 12) + (w and $80) shr 3-1;
                   if w and $f00=0 then e:=$ff00 else e:=w and $f00+b+100*256;
                   v:=$ff
                end;
            until b1 and $80>0;
         until b2 and $80>0; inc (j0);
        end else inc (j0);
       close (f); exit;
    end else
   if ss[length(ss)-1]='3' then
    begin {Load S3M}
       blockread (f, s3h, 96, u); if u<>96 then goto bad_file; n:=0;
       if s3h[$1c]<>$1a then goto bad_file;
       while (s3h[n]<>0)and(n<28) do begin i.n[n]:=char(s3h[n]); inc (n) end;
       for n:=n to 31 do i.n[n]:=' ';
       if (s3h[$1d]<>16)or(s3h[$2c]and $df<>byte('S'))or(s3h[$2d]and$df<>byte('C'))
        or(s3h[$2e] and $df<>byte('R'))or(s3h[$2f] and $df<>byte('M')) then warning ('Unknown format type: '+hex(s3h[$1d]));
       wo:=getw($20);wi:=getw($22);wp:=getw($24);wf:=getw($26);wc:=getw($28);wffi:=getw($2a);i.t:=getw($31);
       if wp>pn then warning ('Song contain too much patterns');
       if wi>sn then warning ('There are too many samples in song');
       if wo and 1<>0 then warning ('Ordnum is not even! Fatal error');
       if wo>ml then warning ('Too many positions in the song list');
       n:=0; w:=0; j1:=0; while w<wo do {position list}
        begin
           blockread (f, j0, 1, u); if u<>1 then goto bad_file;
           if j1=0 then if j0<>254 then if n<pn then
            begin list[n]:=j0; inc (n); j1:=byte(j0=255); end;
           inc (w);
        end;
       w:=0; n:=0; while w<wi do {sample parapointers}
        begin blockread (f,j2,2,u); if u<>2 then goto bad_file;
           if n<sn then begin ii[n]:=longint(j2)*16; inc (n) end;
           inc (w);
        end; i.s:=n;
       w:=0; n:=0; while w<wp do {pattern parapointers}
        begin blockread (f,j2,2,u); if u<>2 then goto bad_file;
           if n<pn then begin pp[n]:=longint(j2)*16; inc (n) end;
           inc (w);
        end; i.p:=n;
       for n:=0 to 255 do {channel setting}
        begin if n<32 then b:=s3h[$40+n] else b:=255;
           if b=255 then dps[n]:=255 else if b and $7f>15 then
            if b and $7f>24 then begin warning ('S3M Drums Channels are not supported'); dps[n]:=255 end
            else begin dps[n]:=b and $7f-16; i.m:=i.m and ($ffff xor (1 shl dps[n]) or (((b shr 7) xor 1)*(1 shl dps[n]))) end;
       end; j0:=0;
       for n:=0 to 8 do if j0=0 then for w:=0 to 256 do if w=256 then begin warning ('Channel '+char(n+49)+' is not located');
       j0:=1 end else if dps[w]=n then break; if j0=1 then if ask ('Do you want to ignore S3M channel set table?') then
       begin for n:=0 to 255 do if n<9 then dps[n]:=n else dps[n]:=255; i.m:=$ffff end;
       for n:=0 to i.s-1 do {read samples}
        begin seek (f, ii[n]); blockread (f, s3h, $50, u); if u<>$50 then goto bad_file; s[n].t:='';
           j0:=$30; while (length(s[n].t)<stn)and(s3h[j0]<>0) do begin s[n].t:=s[n].t+char(s3h[j0]); inc (j0) end;
           while length(s[n].t)<stn do s[n].t:=s[n].t+' ';
           if s3h[$4f] and $df=byte('S') then warning ('Sample '+hex(n)+' is digital');
           s[n].a20:=s3h[$10];s[n].a21:=s3h[$11];
           s[n].a40:=s3h[$12];s[n].a41:=s3h[$13];
           s[n].a60:=s3h[$14];s[n].a61:=s3h[$15];
           s[n].a80:=s3h[$16];s[n].a81:=s3h[$17];
           s[n].ae0:=s3h[$18];s[n].ae1:=s3h[$19];
           s[n].ac:=s3h[$1a]; f0x:=getw($20); s[n].f0:=0;
           if f0x<1 then f0x:=8363;
           while f0x/8363>sqrt(c) do begin inc (s[n].f0); f0x:=f0x/c end;
           while 8363/f0x>sqrt(c) do begin dec (s[n].f0); f0x:=f0x*c end;
           s[n].dn[0]:=#12; for j0:=1 to 12 do s[n].dn[j0]:=char(s3h[j0]);
        end;
       frst:=8;
       for n:=0 to i.p-1 do {read patterns}
        begin seek (f, pp[n]+2); j0:=0;
           while j0<64 do
            begin blockread(f,b,1,u); if u<>1 then goto bad_file;
               if b=0 then inc (j0) else
                begin cc:=dps[b and 31];
                   if b and $20>0 then {note/sample}
                    begin blockread(f,b1,1,u); if u<>1 then goto bad_file;
                       blockread(f,b2,1,u); if u<>1 then goto bad_file;
                       if cc<cn then begin
                        if b1<254 then b1:=(b1 shr 4)*12+b1 and $f+12+12; dec (b2); if b2>=sn then b2:=255;
                        m[n]^.t[j0][cc].n:=b1; m[n]^.t[j0][cc].s:=b2 end{ else
                         if b1<254 then if frst<cc then
                       begin warning ('Found channel '+hex(cc)); frst:=cc end};
                    end;
                   if b and $40>0 then {volume}
                    begin blockread(f,b1,1,u); if u<>1 then goto bad_file;
                       if cc<cn then begin if b1=64 then b1:=63 else if b1>63 then b1:=255;
                       m[n]^.t[j0][cc].v:=b1 end;
                    end;
                   if b and $80>0 then {command/info}
                    begin blockread(f,b1,1,u); if u<>1 then goto bad_file;
                       blockread(f,b2,1,u); if u<>1 then goto bad_file;
                       if cc<cn then
                        begin
                           case b1 of
                              0..254: m[n]^.t[j0][cc].e:=word(b1) shl 8+b2;
                              255: m[n]^.t[j0][cc].e:=$ff00+b2; {FF=..=00}
                           end;
                           m[n]^.t[j0][cc].e:=m[n]^.t[j0][cc].e and $ff00+b2;
                        end;
                    end;
                end;

            end
        end;
       close(f);cc:=0; exit;
    end;
   blockread (f, b, 1); if ioresult<>0 then begin e:=3; exit end;
   blockread (f, b1, 1); if ioresult<>0 then begin e:=3; exit end;
   if (b=byte('F'))and(b1=byte('M')) then if ask ('Is this module written in old beta-beta FM-Tracker?') then
    begin {Load 1998 year beta-beta tracker format}
       blockread (f, b, 1); if ioresult<>0 then begin e:=3; exit end;
       if b>32 then b:=32 else if b<2 then b:=2; i.t:=$af00+b;
       blockread (f, w, 2); if ioresult<>0 then begin e:=3; exit end;
       seek (f, w); j0:=0; repeat blockread (f, b, 1); if ioresult<>0 then begin e:=3; exit end;
        list[j0]:=b; inc (j0) until (b=255)or(j0=255); {list}
       for n:=1 to 15 do begin
        seek (f, 3+2*n); blockread (f, w, 2); if ioresult<>0 then begin e:=3; exit end; seek (f, w);
        for j0:=0 to 10 do begin {samples}
          blockread (f, b, 1); if ioresult<>0 then begin e:=3; exit end;
          case j0 of 0: s[n-1].ae0:=b; 1: s[n-1].ae1:=b; 2: s[n-1].ac:=b;
             3: s[n-1].a20:=b; 4: s[n-1].a40:=b; 5: s[n-1].a60:=b;
             6: s[n-1].a80:=b; 7: s[n-1].a21:=b; 8: s[n-1].a41:=b;
             9: s[n-1].a61:=b; 10: s[n-1].a81:=b; end; end;
        end;
       for n:=0 to 19 do begin
        seek (f, 65+2*n); blockread (f, w, 2); if ioresult<>0 then begin e:=3; exit end; seek (f, w);
        for j0:=0 to 8 do for j1:=0 to 63 do
         begin
            blockread (f, b0, 1); if ioresult<>0 then begin e:=3; exit end;
            blockread (f, b1, 1); if ioresult<>0 then begin e:=3; exit end;
            with m[n]^.t[j1][j0] do case b0 of
               253: begin n:=255; s:=255; v:=255; e:=$fe00+b1 end;
               254: begin n:=254; s:=255; v:=255; e:=$ff00 end;
               1..95: begin n:=b0+24; if s>$10 then s:=b1 shr 4-1 else s:=255; v:=255; e:=$ff00; end;
               else begin n:=255; s:=255; v:=255; e:=$ff00 end;
            end;
         end;
       end;
       close (f);cc:=0;exit
    end;
   if b=$11 then
    begin {Load packed FMM} seek (f, 4);
       blockread (f, i.t, 2, u); if u<>2 then goto bad_file;
       blockread (f, i.p, 1, u); if u<>1 then goto bad_file;
       blockread (f, i.s, 1, u); if u<>1 then goto bad_file;
       blockread (f, i.m, 2, u); if u<>2 then goto bad_file; j0:=0;
       repeat blockread (f, b, 1, u); if u<>1 then goto bad_file;
          if (j0<32)and(b>0) then begin i.n[j0]:=char(b); inc (j0) end
       until b=0; blockread (f, b, 1, u); if u<>1 then goto bad_file; {skip author...}
       n:=0; j0:=0; repeat blockread (f, b, 1, u); if u<>1 then goto bad_file;
        if j0=0 then begin list[n]:=b; inc (n); j0:=byte(n=0) end until b=255;
       if i.p>0 then for j2:=0 to i.p-1 do begin {load pat J2} j0:=0;
        for n:=0 to cn-1 do lt[n]:=et;
        repeat blockread (f, b, 1, u); if u<>1 then goto bad_file;
         if b=122 then begin m[j2]^.l:=j0; break end {EOP} else if j0<tn then
         if b<>123 then
          begin
             if b=125 then m[j2]^.t[j0][0]:=lt[0] else if b=124 then begin end else
             begin blockread (f, b1, 1, u); if u<>1 then goto bad_file;
                with m[j2]^.t[j0][0] do begin
                 n:=b and $7f or (128*byte(b and $7f>125));
                 s:=b1 and $7f or (128*byte(b1 and $7f=$7f));
                 if b and $80>0 then begin blockread (f, v, 1, u); if u<>1 then goto bad_file end;
                 if b1 and $80>0 then e:=lt[0].e else begin blockread (f, e, 2, u); if u<>2 then goto bad_file end;
                end;
             end;
             blockread (f, j1, 1, u); if u<>1 then goto bad_file;
             for b2:=1 to cn-1 do if boolean((j1 shr (cn-b2-1)) and 1) then
             begin
                blockread (f, b, 1, u); if u<>1 then goto bad_file;
                if b=125 then m[j2]^.t[j0][b2]:=lt[b2] else if b=124 then begin end else
                begin blockread (f, b1, 1, u); if u<>1 then goto bad_file;
                   with m[j2]^.t[j0][b2] do begin
                    n:=b and $7f or (128*byte(b and $7f>125));
                    s:=b1 and $7f or (128*byte(b1 and $7f=$7f));
                    if b and $80>0 then begin blockread (f, v, 1, u); if u<>1 then goto bad_file end;
                    if b1 and $80>0 then e:=lt[b2].e else begin blockread (f, e, 2, u); if u<>2 then goto bad_file end;
                   end;
                end;
             end;
          end;
         if j0<tn then begin lt:=m[j2]^.t[j0];inc (j0) end
        until false;
       end;

       if i.s>0 then for n:=0 to i.s-1 do {load sam N}
        with s[n] do begin blockread (f, s3h, 12, u); if u<>12 then goto bad_file;
          f0:=shortint(s3h[0]);
          a20:=s3h[1];a21:=s3h[2];a40:=s3h[3];a41:=s3h[4];
          a60:=s3h[5];a61:=s3h[6];a80:=s3h[7];a81:=s3h[8];
          ac:=s3h[9];ae0:=s3h[10];ae1:=s3h[11]; j0:=0;
          repeat blockread (f, b, 1, u); if u<>1 then goto bad_file;
             if (b<>0)and(j0<stn) then begin inc (j0); t[j0]:=char(b) end;
          until b=0; j0:=0;
          repeat blockread (f, b, 1, u); if u<>1 then goto bad_file;
             if (b<>0)and(j0<12) then begin inc (j0); dn[j0]:=char(b) end;
          until b=0;
        end;
       close (f); exit;
    end;
   if (b<>$1c)and(b<>$1d) then begin reset (f, 1); if ioresult<>0 then begin e:=3; exit end; end else seek (f, 1);
   blockread (f, i, sizeof(sysinfo), u);
   if u<>sizeof (sysinfo) then goto bad_file;
   blockread (f, list, ml, u); if u<>ml then goto bad_file;
   if (b<>$1c)and(b<>$1d) then
    begin {Load old module format}
       if i.p>0 then for n:=0 to i.p-1 do
         begin blockread (f,tp,sizeof(pattern_old),u);
            if u<>sizeof(pattern_old) then goto bad_file;
            for j0:=0 to tn-1 do for j1:=0 to cn-1 do
             begin m[n]^.t[j0][j1].n:=tp.t[j0][j1].n; m[n]^.t[j0][j1].s:=tp.t[j0][j1].s; m[n]^.t[j0][j1].e:=$ff00; end;
            m[n]^.l:=tp.l
         end;
    end else
   begin {Load new module format}
      if i.p>0 then for n:=0 to i.p-1 do
        begin blockread (f,m[n]^,sizeof(pattern),u);
           if u<>sizeof(pattern) then goto bad_file;
        end;
   end;
   if i.s>0 then
    if b<>$1d then {Load samples in 0x1C format (old)}
     for n:=0 to i.s-1 do
      begin blockread (f,sto,sizeof(sample_old),u);
           if u<>sizeof(sample_old) then goto bad_file;
           s[n].a20:=sto.a20;s[n].a21:=sto.a21;s[n].a40:=sto.a40;
           s[n].a41:=sto.a41;s[n].a60:=sto.a60;s[n].a61:=sto.a61;
           s[n].a80:=sto.a80;s[n].a81:=sto.a81;s[n].ae0:=sto.ae0;
           s[n].ae1:=sto.ae1;s[n].ac:=sto.ac;s[n].t:=sto.t;
         while length(s[n].t)<stn do s[n].t:=s[n].t+' ';
         s[n].dn:='            ';
      end else
    for n:=0 to i.s-1 do
     begin
        blockread (f,s[n],sizeof(sample),u);
        while length(s[n].t)<stn do s[n].t:=s[n].t+' ';
        if u<>sizeof(sample) then goto bad_file;
     end;
   close (f);
   e:=0; c_unsaved:=FALSE; cc:=0;
   exit;
bad_file:
   init_vars;
   e:=4;
   close (f);
end;
procedure edit_status; var x: byte;
begin color (7); for x:=0 to 79 do textxy (0,22,' ');
   color(7); textxy (0, 22, 'O='+char(co+48)); {3+1=4}
   color (12);
   textxy(4,22,char(97-32*byte(c_cursorstep))+'='+char(byte(char('+'))*byte(c_autostep>=0)+byte(char('-'))*byte(c_autostep<0))+
   char(abs(c_autostep) div 10+48)+char(abs(c_autostep) mod 10+48)); {5+1=6}
   color (7); textxy(10,22,'P='+hex(cp)); {4+1=5}
   color (14); textxy (15,22,'T='+hex(cpos)+'/'+hex(m[cp]^.l)); {7+1=8}
   color (12); if c_unsaved then textxy (23,22,'*'); {1+1=2}
   color (13); textxy (25,22,'V=');if cv=255 then textxy (27,22,'NP') else
    textxy (27,22,hex(cv)); {4+1=5}
   color (7); textxy (74-stn,22,'S='+hex(cs));
   if s[cs].t<>'                            ' then textxy (78-stn,22,'='+s[cs].t);
{   color (14); textxy (32,22,hex(m[cp]^.t[cpos][cc].e shr 8)+hex(m[cp]^.t[cpos][cc].e and $ff));}
   if playback then begin color (7); if not lockpat then textxy (32,22,hex(ppl)+':'); textxy (35,22,hex(ppn)+'/'+hex(ppp)) end;
 {8+1=9}
end;
procedure redraw_tunes;
var n, x, y, x0,y0,x1,y1,c,c0: byte; b: boolean;
begin b:=start.x and start.y and finish.x and finish.y<>255;
   if b then begin
    if start.y<finish.y then begin y0:=start.y;y1:=finish.y end else begin y1:=start.y;y0:=finish.y end;
    if start.x<finish.x then begin x0:=start.x;x1:=finish.x end else begin x1:=start.x;x0:=finish.x end;
   c0:=128 end else c:=0;
   color (16+10);
   for y:=0 to 19 do
    begin
       color (16+10+4*byte((y+coffs)mod m[cp]^.l=cpos));
       textxy (xs, y+2, hex((y+coffs) mod m[cp]^.l));
    end;
   for y:=19 downto 0 do
    begin
      if playback and(cp=ppn) then if (y+coffs) mod m[cp]^.l=ppp then color (16) else color (0) else color (0);
      for n:=0 to cn-1 do textxy (n*(rl+1)+xs+2,y+2,' ');
    end;
   for n:=0 to cn-1 do
    begin
       color (9+5*byte(n=cc));
       textxy (n*(rl+1)+xs+8,1,char(n+49));
{       for y:=0 to 19 do
        for x:=0 to rl-1 do
         textxy (xs+n*(rl+1)+x+3, y+2, ' ');}
       for y:=0 to 19 do
        begin if b then if (n in [x0..x1])and((y+coffs)mod m[cp]^.l in [y0..y1]) then c:=c0 else c:=0;
           x:=m[cp]^.t[(y+coffs) mod m[cp]^.l][n].n;
           color (c+16*byte(i.m and (1 shl n)>0)+12+2*byte((y+coffs)mod m[cp]^.l<>cpos));
           if x<120 then textxy (xs+n*(rl+1)+3,y+2,tune_text[x mod 12]+char(x div 12+48)+' ') else
            case x of
              254: textxy (xs+n*(rl+1)+3,y+2,'R.. ');
              255: textxy (xs+n*(rl+1)+3,y+2,'... ');
              else textxy (xs+n*(rl+1)+3,y+2,'??? ');
            end;
           case csh of
            0: if m[cp]^.t[(y+coffs)mod m[cp]^.l][n].s<$ff then
             textxy(xs+n*(rl+1)+7,y+2,hex(m[cp]^.t[(y+coffs)mod m[cp]^.l][n].s)) else textxy (xs+n*(rl+1)+7,y+2,'..');
            1: begin color (c+16*byte(i.m and (1 shl n)>0)+11+2*byte((y+coffs)mod m[cp]^.l<>cpos));
             if m[cp]^.t[(y+coffs)mod m[cp]^.l][n].v<$ff then
              textxy(xs+n*(rl+1)+7,y+2,hex(m[cp]^.t[(y+coffs)mod m[cp]^.l][n].v)) else textxy (xs+n*(rl+1)+7,y+2,'..'); end;
            2: begin color (c+16+12); if m[cp]^.t[(y+coffs)mod m[cp]^.l][n].e shr 8 in [1..26] then
             textxy (xs+n*(rl+1)+6,y+2,char(m[cp]^.t[(y+coffs)mod m[cp]^.l][n].e shr 8+96)) else
              if m[cp]^.t[(y+coffs)mod m[cp]^.l][n].e < 255 then textxy (xs+n*(rl+1)+6,y+2,'?');
               color (c+16*byte(i.m and (1 shl n)>0)+15-byte((y+coffs)mod m[cp]^.l<>cpos));
               textxy(xs+n*(rl+1)+7,y+2,hex(m[cp]^.t[(y+coffs)mod m[cp]^.l][n].e and $ff));
               end;
           end;
           if csh<>2 then if m[cp]^.t[(y+coffs) mod m[cp]^.l][n].e<$ff00 then
            begin color (16+c+9); textxy (xs+n*(rl+1)+6,y+2,'*'); end;
        end;
    end;
   edit_status;
end;
procedure hlpbar (a: hlpitems); var x,c: byte;
begin c:=gr256_col;color (48+15); for x:=0 to 9 do textxy (x*8+1+byte(x=9),24,a[x]);
   color (12); for x:=0 to 8 do textxy (x*8,24,char(49+x)); textxy (72,24,'10'); color (c);
end;
procedure screen;
var x, y: byte;
const hlp:hlpitems=('Help  ','Save  ','Open >','SmplEd','PLAY! ','PlyPat','PlyCur','Mute X','Compo>','[Quit]');
 sc:string='e1.0 aynaT';
begin
   color (7); for x:=0 to 79 do for y:=0 to 24 do textxy (x, y, ' ');
   hlpbar (hlp); status;
   color (16*5+14); for x:=0 to 79 do if x in [1..32] then textxy (x, 0, i.n[x-1]) else
    if (x>=79-length(sc))and(x<79) then begin color (5*16+12); textxy (x, 0, sc[79-x]) end else textxy (x, 0, ' ');
   redraw_tunes;
end;
procedure init_vars; var j,n,k: byte;
begin start.x:=255; start.y:=255; finish.x:=255; finish.y:=255;
  start1.x:=255; start1.y:=255; finish1.x:=255; finish1.y:=255;
   for j:=0 to 31 do i.n[j]:=' ';
   with i do begin
    n[0]:='N'; n[2]:='N';
    n[1]:='O'; n[3]:='A'; n[4]:='M'; n[5]:='E'; end;
   for j:=0 to pn-1 do
    begin m[j]^.l:=64; for n:=0 to tn-1 do
     for k:=0 to cn-1 do with m[j]^.t[n][k] do
      begin n:=255; s:=255; e:=$ff00; v:=255 end;
    end;
   for j:=0 to sn-1 do begin for n:=0 to 12 do samstore (j, n, 0); s[j].t:='                            '; s[j].f0:=0;
    s[j].dn:='            ' end;
   cp:=0; co:=4; cpos:=0; cc:=0; cs:=0; coffs:=0; cv:=255; vsc:=100; ceh:=$ff; cel:=0;
   cwhat:=0; c_autostep:=1; c_unsaved:=FALSE; c_cursorstep:=TRUE; csh:=0;
   e:=0; i.t:=125*256+6; for j:=0 to 255 do list[j]:=255; i.m:=$ffff; playback:=FALSE;
end;
procedure init; var j,i,n: byte; w: word; q: double; const c=1.05946309435929526456182529494634;
begin
   clgraph; mute; chide; repeat until port[$3da] and 8=0; port[$3c0]:=$30; port [$3c0]:=4;
   xs:=(78-cn*(rl+1)) shr 1;
   for j:=0 to pn-1 do new (m[j]);
   init_vars; q:=440*1.31/2/2/2/2/c/c/c/c/c/c/c/c/c;
   for j:=0 to 10*12-1 do
    begin w:=round(q); n:=2;
       while (w<$100)and(n>0) do begin dec (n); w:=round(q*(1 shl (2-n))); end;
       while (w>$3ff)and(n<7) do begin inc (n); w:=round(q/(1 shl (n-2))); end;
       if w>$3ff then begin writeln ('!!!'); exit17 end;
       w:=w or (word(n) shl 10); table[j]:=w; q:=q*c;
    end;
end;
procedure move_left;
begin if cwhat>0 then dec (cwhat) else
       if cc>0 then begin dec (cc); cwhat:=3+byte(csh=2) end
        else begin cc:=cn-1; cwhat:=3+byte(csh=2) end;
end;
procedure move_right;
begin if cwhat<3+byte(csh=2) then inc (cwhat) else
       if cc<cn-1 then begin inc (cc); cwhat:=0 end
        else begin cc:=0; cwhat:=0 end;
end;
procedure move_up;
begin if cpos>0 then dec (cpos) else cpos:=m[cp]^.l-1;
       while not (((cpos>=coffs)and(cpos-coffs<20)) or
        (((coffs+19) mod m[cp]^.l>=cpos) and ((coffs+19) mod m[cp]^.l<>coffs+19))) do
         if coffs>0 then dec (coffs) else coffs:=m[cp]^.l;
end;
procedure move_down;
begin if cpos<m[cp]^.l-1 then inc (cpos) else cpos:=0;
       while not (((cpos>=coffs)and(cpos-coffs<20)) or
        (((coffs+19) mod m[cp]^.l>=cpos) and ((coffs+19) mod m[cp]^.l<>coffs+19))) do
         if coffs<m[cp]^.l-1 then inc (coffs) else coffs:=0;
end;
procedure effect (c: byte; e: word); var a, n: byte;
begin a:=e shr 8; eff[c].c:='s'; if a=255 then exit; n:=e and $ff;
   case a of
    byte('A')-64: if n>1 then i0.t:=i0.t and $ff00+n else i0.t:=i0.t and $ff00+2;
    byte('C')-64: begin ppjl:=ppl; inc (ppjl); ppjp:=n; ppj:=TRUE end;
    byte('J')-64: begin eff[c].c:='J'; if n<>0 then eff[c].p:=n end;
    byte('T')-64: begin if n<46 then n:=46; i0.t:=word(n) shl 8 + (i0.t and $ff); timer_only; end;
   end;
end;
procedure playbar; var j: byte;
begin
   for j:=0 to cn-1 do if i.m and (1 shl j)>0 then
     begin effect (j, m[cp]^.t[cpos][j].e);
          if m[cp]^.t[cpos][j].n<10*12 then
          if m[cp]^.t[cpos][j].s<sn then
           splay (j, m[cp]^.t[cpos][j].s, m[cp]^.t[cpos][j].n, m[cp]^.t[cpos][j].v) else
          change_pitch (j, m[cp]^.t[cpos][j].n, m[cp]^.t[cpos][j].v) else
        if m[cp]^.t[cpos][j].n=254 then mute_channel (j)
     end else begin cbx:=0; prog(samtrans(j,9),cbx) end;
end;
procedure sample_editor;
var j,x, y, yc,xc,xxc: byte; ex_v: boolean;
const regs:array[0..12] of string[3]=
('A20','A21','A40','A41','A60','A61','A80','A81','AAx','ABx','ACx','AE0','AE1');
desc:array[0..12] of string=
('Op.1: AM / Vib / EG Type / Key Scl / Mult','Op.2: AM / Vib / EG Type / Key Scl / Mult',
 'Op.1: Key Scl Level / Op. Output Level','Op.2: Key Scl Level / Op. Output Level',
 'Op.1: Attack Rate / Decay Rate','Op.2: Attack Rate / Decay Rate',
 'Op.1: Sustain Level / Release Rate','Op.2: Sustain Level / Release Rate',
 '*Frequency (low 8 bits)','*Key On / Octave / Frequency (high 2 bits)','Feedback Strength / Connection Type',
 'Op.1: Wave Select','Op.2: Wave Select');
hlp:hlpitems=('      ','Save >','Open >','Copy  ','Test >','Paste ','      ','Mute X','      ','Return');
hlp2:hlpitems=('      ','      ','      ','      ','      ','      ','      ','Mute X','      ','Return');
procedure screen2; var x, y: byte; begin color (7); for x:=0 to 79 do for y:=0 to 24 do textxy (x, y, ' '); hlpbar(hlp);
 status; end; var ss: string;
begin screen2; yc:=0; xc:=0;
   repeat
      if xc<>255 then cursor (byte(xc>1)*3+xc+6, yc+2);
      waiting; edit_status;
      for y:=0 to 12 do
       begin x:=sambyte(cs, y); color (7+7*byte(y=yc));
          textxy (2, y+2,regs[y]+'='); color (10+5*byte(y=yc));
          textxy (6, y+2, hex(x)+'h, '+bin(x)+'b'); color (7+5*byte(y=yc));
          textxy (20, y+2,'; '+desc[y]);
       end;
      color (11); textxy (0,15,'OFFS='+deci(s[cs].f0)+'st   ');
      j:=0; while j+2<cs do inc (j); color (7); textxy (2,16,'No:  Title (Alt+E to change):  ');
      for y:=0 to 4 do
       begin color (7+6*byte(y+j=cs));
          if y+j<sn then textxy (2,17+y,hex(y+j)+':'+s[y+j].t) else
          textxy (2,17+y,'                               ');
       end;
      scan; ex_v:=kp and (not ek and (keyv=27)) or (ek and (keyv=68));
      if kp and not ek then case keyv of
       12,13: begin keyv:=61; ek:=TRUE end; {Load}
       19: begin keyv:=60; ek:=TRUE end; {Save}
       45: begin keyv:=30; ek:=TRUE end; {-}
       43: begin keyv:=16; ek:=TRUE end; {+}
      end;
      if xc=255 then
       begin
          if ex_v then begin xc:=xxc; hlpbar(hlp); mute; ex_v:=FALSE; cshow; keyv:=0; end else
          begin
             if kp and ek then
              case keyv of
                 66: mute;
              end else
             if kp and not ek then
              begin if char(keyv) in ['a'..'z'] then keyv:=keyv and $df;
                 for j:=1 to length(tune_keys) do
                  begin
                     if byte(tune_keys[j])=keyv then
                      begin
                         if (co*12+j-1) div 12>9 then
                          begin setpal (0, 63, 0, 0); e:=5; status;
                            waiting; waiting; setpal (0, 0, 0, 0); e:=0
                          end else splay(0,cs,co*12+j-1,255);
                         break;
                      end;
                  end;
                 if keyv=47 then begin if co>0 then dec (co); status end else
                 if keyv=42 then begin if co<9 then inc (co); status end
              end;
          end;
       end else
      if kp and ek then
       case keyv of
          75: begin if xc>0 then dec (xc) else xc:=9 end;
          77: begin if xc<9 then inc (xc) else xc:=0 end;
          72: begin if yc>0 then dec (yc) else yc:=12; if yc=9 then yc:=7 end;
          80: begin if yc<12 then inc (yc) else yc:=0; if yc=8 then yc:=10 end;
          66: mute;
          62: for j:=0 to 12 do samstore(sn,j,sambyte(cs,j));
          64: {F6} begin if ask ('Current data will be deleted! Do you want to continue?') then
                for j:=0 to 12 do samstore(cs,j,sambyte(sn,j)); cshow end;
          60: begin {F2} fade; ss:=br_save (3,'FMS'); if ss<>'' then samsave (cs,ss); screen2; cshow end;
          61: begin {F3} if not c_unsaved then begin fade; ss:=br_load (4,'FMS') end else
                 if ask ('Current data will be removed! Do you want to continue?') then begin fade; ss:=br_load (4,'FMS') end;
                 if ss<>'' then begin samload (cs, ss); end; screen2; cshow; keyv:=0; end;
          63: {F5} begin chide; hlpbar(hlp2); xxc:=xc; xc:=255; end;
          73: begin if cs>0 then dec (cs); end;
          81: begin if cs<sn-1 then inc (cs); end;
          30: {Alt+A} if s[cs].f0>-12*10+1 then begin dec (s[cs].f0); unsave_sam(cs) end;
          16: {Alt+Q} if s[cs].f0<12*10-1 then begin inc (s[cs].f0); unsave_sam(cs) end;
          18: {Alt+E} begin x:=stn-1; while (s[cs].t[x]=' ')and(x>0) do dec (x);
                 color (7); textxy (2,16,'Sample Text Editor. Esc to exit');
                 repeat j:=0; while j+2<cs do inc (j);for y:=0 to 4 do
                          begin color (7+6*byte(y+j=cs)); if y+j<sn then textxy (2,17+y,hex(y+j)+':'+s[y+j].t) else
                           textxy (2,17+y,'                               '); end; y:=cs-j;
                    cursor (x+5,17+y);waiting; scan;
                    if kp then
                     if not ek then
                      if keyv>31 then begin s[cs].t[x+1]:=char(keyv);unsave_sam(cs);if x<stn-1 then inc(x) else if cs<sn-1 then
                       begin x:=0;inc(cs) end; end else
                      case keyv of
                         8: if x or cs>0 then begin
                                if x=0 then begin x:=19; dec (cs) end else dec (x); s[cs].t[x+1]:=' '; unsave_sam(cs);
                             end;
                        13: if cs<sn-1 then begin x:=0; inc (cs) end;
                      end else
                     case keyv of
                        72: if cs>0 then dec (cs);
                        75: if x>0 then dec(x) else if cs>0 then begin x:=stn-1;dec(cs) end;
                        77: if x<stn-1 then inc(x) else if cs<sn-1 then begin x:=0;inc (cs) end;
                        80: if cs<sn-1 then inc (cs);
                     end;
                 until kp and not ek and (keyv=27); keyv:=0;
              end
       end else
      if kp and not ek then
       begin if char(keyv) in ['a'..'z'] then keyv:=keyv and $df;
          if (keyv in [48..57,65..70]) and (xc<2) then
           begin dec (keyv,48+7*byte(keyv>64));
              samstore(cs,yc,sambyte(cs,yc) and ($f shl (4*xc))+keyv shl (4*(xc xor 1)));
              unsave_sam (cs);
              xc:=xc xor 1; if xc=0 then begin inc (yc); if yc=8 then yc:=10; if yc>12 then yc:=0 end;
           end else

          if (keyv in [48,49]) and (xc>1) then
           begin dec (keyv,48);
              samstore(cs,yc,sambyte(cs,yc) and (($80 shr (xc-2))xor $ff)+keyv shl (9-xc));
              unsave_sam (cs);
              inc (xc); if xc>9 then begin xc:=2; inc (yc); if yc=8 then yc:=10; if yc>12 then yc:=0 end;
           end else
          if keyv=47 then begin if co>0 then dec (co); status end else
          if keyv=42 then begin if co<9 then inc (co); status end

       end;
   until ex_v; kp:=FALSE; ek:=TRUE; screen; cshow;
end;
function check1: boolean; {timer}
begin if (i.t shr 8<46)or(i.t and $ff<2) then begin e:=6;status; check1:=FALSE end else check1:=TRUE end;
function check2: boolean; {does playlist exist?} begin if list[ppl]<255 then check2:=TRUE else begin e:=7; status;
 check2:=FALSE end end;
procedure play (m: byte); var j: byte;
{m: 0=start2end; 1=pattern; 2=pos2end}
{    ppl, ppn, ppp: byte; {play positions: in list, pattern number, in pattern}
{    ppj: boolean; {jump: yes/no}
{    ppjl, ppjp: byte; {jump to position in list, in pattern}
begin if not playback then begin case m of
 0: if check1 then begin ppl:=0;if check2 then begin ppn:=list[ppl];ppp:=0;ppj:=FALSE;playback:=TRUE;lockpat:=FALSE end end;
 1: if check1 then begin ppn:=cp; ppp:=0; ppj:=FALSE; playback:=TRUE; lockpat:=TRUE end;
 2: if check1 then begin ppn:=cp; j:=0; while (list[j]<>255)and(list[j]<>cp) do inc (j); if list[j]<>255 then
     begin ppl:=j;ppp:=cpos;ppj:=FALSE;playback:=TRUE;lockpat:=FALSE end else begin e:=9; status end end;
end; if playback then begin i0:=i; dec (ppp); timer_prog;

end end end;
procedure main;
var shift,selecting,ex_v: boolean; warning,coffs0, cp0, j, n: byte; ss: string; jj: array [0..22] of byte; w,page: word;
const hlp3:hlpitems=('Help  ','      ','      ','      ','      ','      ','      ','      ','      ','Return');
procedure before; begin if shift then if not selecting then begin start.y:=cpos;start.x:=cc;selecting:=TRUE end
 else else selecting:=FALSE end;
procedure after; begin if shift then begin finish.y:=cpos;finish.x:=cc end end;
procedure paste; var h,w,x0,x1,y0,y1,x,y: byte;
begin if not preclip1 (x0,y0,x1,y1) then exit; {if start1.x and start1.y and finish1.x and finish1.y=255 then exit;
   if start.y<finish.y then begin y0:=start.y;y1:=finish.y end else begin y1:=start.y;y0:=finish.y end;
   if start.x<finish.x then begin x0:=start.x;x1:=finish.x end else begin x1:=start.x;x0:=finish.x end;}
   h:=y1-y0+1;w:=x1-x0+1;for y:=tn-1 downto cpos+h do for x:=cc to cc+w-1 do if x<cn then
      m[cp]^.t[y][x]:=m[cp]^.t[y-h][x]; unsave_pat(cp);
   for y:=0 to h-1 do for x:=0 to w-1 do
    if y+cpos<m[cp]^.l then if x+cc<cn then m[cp]^.t[y+cpos][x+cc]:=tp.t[y+y0][x+x0];
end;
procedure trans (a: shortint); var x0,x1,y0,y1,x,y: byte; s: integer;
begin if preclip (x0,y0,x1,y1) then begin
   if warning=0 then for y:=y0 to y1 do for x:=x0 to x1 do if m[cp]^.t[y][x].n<12*10 then
     begin s:=integer(a)+m[cp]^.t[y][x].n; if (s<0)or(s>=12*10) then warning:=1; end
      else else warning:=2; if warning=1 then begin e:=8; status; exit end; warning:=0; e:=0;
   for y:=y0 to y1 do for x:=x0 to x1 do if m[cp]^.t[y][x].n<12*10 then
     begin s:=integer(a)+m[cp]^.t[y][x].n; if (s>=0)and(s<12*10) then begin unsave_pat(cp);
      m[cp]^.t[y][x].n:=byte(s) end else warning:=2; end end;
end;
procedure change_sample; var x0,x1,y0,y1,x,y: byte; begin
 if preclip(x0,y0,x1,y1) then for y:=y0 to y1 do for x:=x0 to x1 do if m[cp]^.t[y][x].n<12*10 then m[cp]^.t[y][x].s:=cs end;
procedure set_volume; var x0,x1,y0,y1,x,y: byte; begin
 if preclip(x0,y0,x1,y1) then for y:=y0 to y1 do for x:=x0 to x1 do if m[cp]^.t[y][x].n<12*10 then m[cp]^.t[y][x].v:=cv end;
procedure scale_volume (sc: byte); var x0,x1,y0,y1,x,y,a: byte; begin
 if preclip(x0,y0,x1,y1) then for y:=y0 to y1 do for x:=x0 to x1 do if m[cp]^.t[y][x].n<12*10 then
  begin a:=m[cp]^.t[y][x].v; if a=255 then a:=63; a:=round(a/100*sc); if a>63 then a:=63; m[cp]^.t[y][x].v:=a end end;
procedure slide_volume; var x0,x1,y0,y1,x,y,v0,v1: byte; begin
 if preclip(x0,y0,x1,y1) then if y0+1<y1 then
  for x:=x0 to x1 do begin v0:=m[cp]^.t[y0][x].v;v1:=m[cp]^.t[y1][x].v;if v0>63 then v0:=63;if v1>63 then v1:=63;
   for y:=y0+1 to y1-1 do if m[cp]^.t[y][x].n<12*10 then
    m[cp]^.t[y][x].v:=byte(integer(v0)+round(((integer(v1)-v0))/(y1-y0+1)*(y-y0)));
   end;
end;
procedure clear_block; var x0,x1,y0,y1,x,y: byte; begin
 if preclip(x0,y0,x1,y1) then for y:=y0 to y1 do for x:=x0 to x1 do begin m[cp]^.t[y][x].n:=255;m[cp]^.t[y][x].s:=255 end end;
begin
   cshow; selecting:=FALSE; warning:=0;
   repeat
      color (15-csh); case csh of
         0: textxy (0, 1, 'RB=SAMPLES');
         1: textxy (0, 1, 'RB=VOLUMES');
         2: textxy (0, 1, 'RB=EFFECTS');
      end;
      coffs0:=coffs; cp0:=cp; ex_v:=FALSE;
{      textxy (1,1,char(cwhat+48));}
      if csh<>2 then cursor (xs+cc*(rl+1)+3+2*cwhat-byte(cwhat=3), (word(cpos)+m[cp]^.l-coffs) mod m[cp]^.l+2) else
      cursor(xs+cc*(rl+1)+3+cwhat+byte(cwhat>0), (word(cpos)+m[cp]^.l-coffs) mod m[cp]^.l+2);
      scan; asm mov ah,2; int 16h; and al,3; mov shift,al end;
      if kp and not ek then case keyv of
         12: begin keyv:=61; ek:=TRUE end;
         19: begin keyv:=60; ek:=TRUE end;
      end;
      if kp and ek then
       case keyv of
       {cursor: left/right/up/down}
          73: begin before; for j:=0 to 15 do move_up; after end;
          81: begin before; for j:=0 to 15 do move_down; after end;
          75: begin before; for j:=0 to byte(byte(shift)>0)*(3+byte(csh=2)) do move_left; after end;
          77: begin before; for j:=0 to byte(byte(shift)>0)*(3+byte(csh=2)) do move_right; after end;
          71: begin before; if cwhat<>0 then cwhat:=0 else {Home}
               if cpos<>0 then while cpos<>0 do move_up
                else begin coffs:=0; cc:=0 end; after end;
         119,132: begin before; cwhat:=0; cpos:=0; coffs:=0; cc:=0; after end; {Ctrl+Home}
          79,117,118: begin before; while cpos<>m[cp]^.l-1 do move_down; after end;
          72: begin before; if c_cursorstep then begin if c_autostep>0 then for n:=1 to c_autostep do move_up else
               if c_autostep<0 then for n:=1 to abs(c_autostep) do move_down end else move_up; after end;
          80: begin before; if c_cursorstep then begin if c_autostep>0 then for n:=1 to c_autostep do move_down else
               if c_autostep<0 then for n:=1 to abs(c_autostep) do move_up end else move_down; after end;
          68: ex_v:=TRUE; {F10}
          46: {Alt+C} begin tp:=m[cp]^; start1:=start; finish1:=finish end;
          25: {Alt+P} paste;
          47: {Alt+V} set_volume;
          37: {Alt+K} slide_volume;
          36: {Alt+J} begin chide; fade; color (159); repeat waiting; textxy (1, 23,' Volume Scale: '+decb(vsc)+'%'+'  ');
               scan; vsc:=vsc+byte(kp and ek and (keyv=77) and (vsc<255))-byte(kp and ek and (keyv=75) and (vsc>0));
               until kp and not ek and (keyv in [13,27]); if keyv=13 then scale_volume (vsc); screen; cshow end;
          22: {Alt+U} begin start.x:=255; start.y:=255; finish.x:=255; finish.y:=255 end;
          38: {Alt+L} if (start.x=cc)and(finish.x=cc)and(start.y=0)and(finish.y=m[cp]^.l-1) then begin start.x:=0;
              finish.x:=cn-1 end else begin start.x:=cc; finish.x:=cc; start.y:=0; finish.y:=m[cp]^.l-1 end;
          16: {Alt+Q} trans (1);
          30: {Alt+A} trans (-1);
          48: {Alt+B} begin start.x:=cc; start.y:=cpos; if finish.x=255 then begin finish.x:=cc; finish.y:=cpos end end;
          18: {Alt+E} begin finish.x:=cc; finish.y:=cpos; if start.x=255 then begin start.x:=cc; start.y:=cpos end end;
          31: {Alt+S} change_sample;
          44: {Alt+Z} clear_block;
          45: {Alt+X} ex_v:=TRUE;
         120..128: begin if c_autostep=keyv-119 then c_autostep:=keyv-109 else
            c_autostep:=keyv-119; screen end; {Alt+1,2,...,0}
         129: begin if c_autostep=0 then c_autostep:=10 else c_autostep:=0; screen end;
         130: begin c_autostep:=-c_autostep; screen end;
         131: begin c_cursorstep:=not c_cursorstep; setpal (0,0,0,63);waiting;waiting;setpal(0,0,0,0);end;
          60: begin {F2} fade; ss:=br_save (1,'FMM'); if ss<>'' then save (ss); screen; cshow;keyv:=0 end;
          59: help;
          61: begin {F3}
                 if c_unsaved then if not ask ('Current music is not saved. Load anyway?') then ss:=''
                  else begin fade;ss:=br_load(2,'FMM,S3M,RAD,D00') end else begin fade; ss:=br_load (2,'FMM,S3M,RAD,D00') end;
                 if ss<>'' then begin mute; init_vars; load (ss); end; screen; cshow; keyv:=0;
              end;
          62: sample_editor; {F4}
          63: {F5} play (0); {timer_reset; cshow; mute; end;}
          64: {F6} play (1);
          65: {F7} play (2);
          66: mute; {F8}
          82: {Ins} begin unsave_pat (cp); for j:=tn-2 downto cpos do
                m[cp]^.t[j+1][cc]:=m[cp]^.t[j][cc]; m[cp]^.t[cpos][cc].n:=255; m[cp]^.t[j][cc].s:=255 end;
          83: {Del} begin unsave_pat (cp); for j:=cpos to tn-2 do
                m[cp]^.t[j][cc]:=m[cp]^.t[j+1][cc]; m[cp]^.t[tn-1][cc].n:=255; m[cp]^.t[tn-1][cc].s:=255 end;

          67: begin {F9} fade; color (3); for j:=0 to 20 do textxy (4,j+2,'³  ³');
                 color(16*5+14); ss:=' Title: ';for j:=0 to 31 do ss:=ss+i.n[j]; while length(ss)<>80 do ss:=ss+' ';
                 textxy (0, 0, ss);
                 ss:=' Timer: '+hex(i.t shr 8)+hex(i.t and $ff); while length(ss)<>80 do ss:=ss+' ';
                 textxy (0, 1, ss); page:=0;
                 n:=0; jj[1]:=4; j:=0;
                 for j:=31 downto 0 do if i.n[j]<>' ' then begin jj[0]:=j+1; break end;
                 if list[0]=255 then w:=0;
                 for j:=255 downto 0 do if list[j]<>255 then begin w:=word(j)+1; break end;
                 hlpbar(hlp3); j:=0;
                 repeat if n<2 then cursor (jj[n]+8,n) else cursor (5+j,n); waiting; scan;
                    color (11); for cp0:=0 to 20 do
                     if list[page+cp0]<>$ff then textxy (5, cp0+2,hex(list[page+cp0])) else
                      textxy (5, cp0+2, '  '); color(16*5+14);
                    case n of
                      0: begin if kp and not ek then
                          begin if keyv>31 then begin if jj[n]<31 then
                              begin i.n[jj[n]]:=char(keyv); textxy (jj[n]+8,n,char(keyv)); inc (jj[n]) end; end
                             else if keyv=8 then begin if jj[n]>0 then begin dec (jj[n]); i.n[jj[n]]:=' ';
                                textxy (jj[n]+8,n,' '); end; end
                          end;
                         end;
                      1: begin if kp and not ek then
                          begin if char(keyv) in ['a'..'f'] then keyv:=keyv and $df;
                          if keyv in [48..57,65..70] then begin if jj[n]<4 then
                           begin i.t:=i.t shl 4+(keyv-48-7*byte(keyv>64)); textxy (jj[n]+8,n,char(keyv)); inc (jj[n]) end; end
                            else if keyv=8 then begin if jj[n]>0 then begin dec (jj[n]); i.t:=i.t shr 4;
                               textxy(jj[n]+8,n,' ');end;end
                          end;
                         end;
                      else begin if kp and not ek then begin
                                if char(keyv) in ['a'..'f'] then keyv:=keyv and $df;
                                if keyv in [48..57,65..70] then
                                 begin
                                    list[page+n-2]:=list[page+n-2] and($f shl (4*j)) +(keyv-48-7*byte(keyv>57)) shl (4*(1-j));
                                    if list[page+n-2]>pn-1 then list[page+n-2]:=pn-1;
                                    color (11); textxy (5, n, hex(list[page+n-2]));
                                    j:=1-j; if page+n-1>w then w:=page+n-1; if j=0 then begin if n<22 then inc (n) else
                                       if page+n-1<ml then inc (page);
                                    end;
                                 end;
                             end;
                             if kp and ek then
                              case keyv of
                                 82: begin for cp0:=255 downto page+n-1 do list[cp0]:=list[cp0-1];
                                     if page+n-2<255 then inc(w)end;
                                 83: begin for cp0:=page+n-1 to 254 do list[cp0-1]:=list[cp0];
                                     if w>0 then dec(w)end;
                              end;
                           end;
                    end;
                    if kp and ek and (keyv=72) then
                      if (n=2)and(page>0) then dec (page) else
                      if n>0 then dec (n);
                    if kp and ek and (keyv=80) then
                      if n<2+w*byte(w<20)+20*byte(w>=20) then inc (n) else
                      if (n=22) and (page+n<w+2) then inc (page);
                 until kp and (not ek and (keyv=27)) or (ek and (keyv=68));
                 screen; keyv:=0;
              end;
          15: {Shift+Tab} begin move_left;move_left;move_left;move_left;
              end;
         115: begin if m[cp]^.l>1 then dec (m[cp]^.l); {Ctrl+Left}
                 coffs:=0; cpos:=0; end;
         116: begin if m[cp]^.l<tn then inc (m[cp]^.l); {Ctrl+Right}
                 coffs:=0; cpos:=0; end;
         104..111: begin i.m:=i.m xor (1 shl (keyv-104)); end; {Alt+F1-F8}
         94..102: begin i.m:=i.m xor (1 shl (keyv-94)); end; {Ctrl+F1-F9}
         103: begin if i.m and 1=0 then i.m:=$ffff else i.m:=0 end; {Ctrl+F10}
         113: begin if i.m and $1ff<>1 shl cc then i.m:=1 shl cc else i.m:=$ffff end; {Alt+F10}
         112: i.m:=i.m xor (1 shl cc); {Alt+F9}
       end;
      if kp and not ek then
       begin
          case keyv of
             126: {~} begin n:=0; repeat color (10); if cv<>255 then textxy (27,22,hex(cv)) else textxy (27,22,'..');
             cursor (n+27,22); waiting; scan; if kp and not ek then begin if char(keyv) in ['a'..'f'] then keyv:=keyv and $df;
               if keyv in [48..57,65..70] then if n=0 then begin cv:=(keyv-48-7*byte(keyv>64))*16+cv and $f;
                 if cv>63 then cv:=63; n:=1; end else begin cv:=cv and $30+(keyv-48-7*byte(keyv>64));n:=2 end;
                  end; if kp and not ek and (char(keyv)='.') then begin cv:=255; n:=2 end;
                  until (kp and not ek and (keyv=27)) or (n=2); keyv:=0; end;
             96: {`} csh:=(csh+1) mod 3;
             32: {Space} begin m[cp]^.t[cpos][cc].e:=$FF00; end;
              9: {Tab} begin move_right;move_right;move_right;move_right;
                 end;
             13,56: begin playbar; if keyv=56 then move_down else {Enter,8}
               if c_cursorstep then begin if c_autostep>0 then for n:=1 to c_autostep do move_down else
               if c_autostep<0 then for n:=1 to abs(c_autostep) do move_up end else move_down end;
             14: begin {Ctrl+N} if c_unsaved then if ask ('Current music is not saved. Really clear?') then begin mute;
                  init_vars end else else begin mute;init_vars end; cshow; screen; status; keyv:=14; end;
             47: begin if co>0 then dec (co); status end;
             42: begin if co<9 then inc (co); status end;
             45: {-} begin if cp>0 then dec (cp); if coffs>=m[cp]^.l then begin coffs:=0; cpos:=0 end; end;
             43: {+} begin if cp<pn-1 then inc (cp); if coffs>=m[cp]^.l then begin coffs:=0; cpos:=0 end; end;
             49: if cwhat=0 then begin {1}
                    mute_channel(cn);
                    m[cp]^.t[cpos][cc].n:=254; m[cp]^.t[cpos][cc].s:=255;
                    if c_autostep>0 then for n:=1 to c_autostep do move_down else
                     if c_autostep<0 then for n:=1 to abs(c_autostep) do move_up;
                    unsave_pat (cp);
                 end;
          end;
          if char(keyv) in ['a'..'z'] then keyv:=keyv and $df;
          if cwhat=0 then
           begin
              for j:=1 to length(tune_keys) do
               begin
                  if byte(tune_keys[j])=keyv then
                   begin
                      if (co*12+j-1) div 12>9 then
                       begin setpal (0, 63, 0, 0); e:=5; status;
                          waiting; waiting; setpal (0, 0, 0, 0); e:=0
                       end else
                      begin
                         splay(cc,cs,co*12+j-1,cv);
                         m[cp]^.t[cpos][cc].n:=co*12+j-1;
                         m[cp]^.t[cpos][cc].s:=cs;
                         m[cp]^.t[cpos][cc].v:=cv;
                         if c_autostep>0 then for n:=1 to c_autostep do move_down else
                          if c_autostep<0 then for n:=1 to abs(c_autostep) do move_up;
                         unsave_pat (cp);
                      end;
                      break;
                   end;
               end;
              if keyv=46 then
               begin
                  m[cp]^.t[cpos][cc].n:=255;
                  m[cp]^.t[cpos][cc].s:=255;
                  m[cp]^.t[cpos][cc].v:=255;
                  m[cp]^.t[cpos][cc].e:=$ff00;
                  for n:=1 to c_autostep do move_down;
               end;
           end else
          if cwhat=1 then
           begin
              if keyv in [48..57] then
               if m[cp]^.t[cpos][cc].n<=12*9+11 then
                begin if co<>keyv-48 then begin co:=keyv-48; screen end;
                   m[cp]^.t[cpos][cc].n:=co*12+m[cp]^.t[cpos][cc].n mod 12;
                   for n:=1 to c_autostep do move_down; unsave_pat (cp);
                end else
            end else
          begin
             if (keyv in [46,48..57,65..70])or((csh=2)and(cwhat=2)and(keyv in [65..90])) then
              begin if keyv<>46 then j:=keyv-48-7*byte(keyv>57);
                case csh of
                0: if keyv<>46 then begin
                 n:=m[cp]^.t[cpos][cc].s;
                 if n>=sn then n:=0;
                 if cwhat=2 then n:=n and 15+j shl 4 else n:=n and $f0 + j;
                 if n>sn-1 then n:=sn-1;
                 m[cp]^.t[cpos][cc].s:=n; if cs<>n then cs:=n;
                 end;
                1: begin
                 n:=m[cp]^.t[cpos][cc].v;
                 if n=255 then n:=0;
                 if cwhat=2 then n:=n and 15+j shl 4 else n:=n and $f0 + j;
                 if n>=vn then n:=vn-1;
                 if keyv=46 then n:=255;
                 m[cp]^.t[cpos][cc].v:=n; if cv<>n then cv:=n;
                 end;
                2: if keyv<>46 then begin
                 n:=m[cp]^.t[cpos][cc].e and $ff;
                 if cwhat=2 then
                  begin if keyv in [65..65+25] then
                   begin m[cp]^.t[cpos][cc].e:=m[cp]^.t[cpos][cc].e and $ff + word(keyv-64) shl 8; ceh:=keyv-64; end;
                  end else begin if cwhat=3 then n:=n and 15+j shl 4 else n:=n and $f0 + j;
                 m[cp]^.t[cpos][cc].e:=m[cp]^.t[cpos][cc].e and $ff00 + n; if cel<>n then cel:=n; end;
                 end;
                end;
                if (keyv=46)and(csh=1)and(cwhat=2) then cwhat:=3;
                if csh<>2 then cwhat:=cwhat xor 1 else begin inc (cwhat); if cwhat>4 then cwhat:=2 end;
                if cwhat=2 then if c_autostep>0 then for n:=1 to c_autostep do move_down else
                 if c_autostep<0 then for n:=1 to abs(c_autostep) do move_up;
                unsave_pat (cp);
              end;
          end;
       end;
      waiting;
      redraw_tunes;
      if ex_v and c_unsaved then
       if not ask ('Current music is not saved. Really quit?') then
        begin ex_v:=FALSE; cshow; status end;
   until ex_v;
end;
begin
   init;
   screen;
   main;
   clgraph; writeln ('Bye!');
   exit17;
end.
