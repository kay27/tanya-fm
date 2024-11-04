unit browser;

interface

function br_load (n: word; s: string): string;
function br_save (n: word; s: string): string;

implementation

uses mb2, dos;
{$R-,Q-,I-,S-}

const ro=1;
      hd=1;
      sys=1;
      vol=0;
      dir=1;
      arc=1;
attr_1=ro+hd shl 1+sys shl 2+vol shl 3+dir shl 4+arc shl 5;
      files=1000;
      mask='*.*';
      remember=5;
var r1: searchrec;
    pp,ppp,p0: string;
    ppo: array [1..remember] of string;
type rec=record
   n: string [12];
   a: byte;
   s,t: longint
end;
type list=array [1..files] of rec;
var l: list;
    p1: word;
    preselect,cursorr,curs1: word;
    preset, redr: boolean;

function upcase (s: string): string; var i: byte; q: string;
begin
   q:='';
   for i:=1 to length(s) do
      if s[i] in ['a'..'z'] then q:=q+char(byte(s[i]) and $df)
       else q:=q+s[i];
   upcase:=q
end;

function valid_ext (e: string; l: string): boolean;
var i,p: byte;
begin valid_ext:=FALSE;
   if length(e)>length(l) then exit;
   p:=1;
   while p<=length(l) do
    begin
       i:=1;
       while i<=length(e) do
        if l[p]<>e[i] then break else begin inc (p); inc (i) end;
       if (i>length(e))and((l[p]=';')or(l[p]=',')or(p>length(l))) then begin valid_ext:=TRUE; exit end;
       while (l[p]<>';')and(l[p]<>',')and(p<length(l)) do inc (p);
       inc (p);
    end;
end;
function get_ext (s: string): string;
var i: byte; q: string;
begin
   q:='';
   i:=length(s);
   for i:=i downto 1 do
    begin
       if s[i]='.' then begin get_ext:=q; exit end;
       q:=s[i]+q;
    end;
   get_ext:='';
end;
procedure add_to_list (q: string);
begin
   if doserror<>0 then exit;
   if r1.attr or attr_1>attr_1 then exit;
   if r1.name='.' then exit;
   if r1.attr and $10=0 then
    if not valid_ext (upcase(get_ext(r1.name)), q) then exit;
   inc (p1);
   with l[p1] do begin
      a:=r1.attr;
      n:=r1.name;
      s:=r1.size;
      t:=r1.time
   end
end;

function get_name (i: word): string;
var n: string; j: byte;
begin
   n:=''; for j:=1 to length(l[i].n) do if l[i].n[j]='.' then break
    else n:=n+l[i].n[j];
   get_name:=n
end;

function get_extension (i: word): string;
var n: string; j: byte;
begin
   n:=''; for j:=length(l[i].n) downto 1 do if l[i].n[j]='.' then break;
   if l[i].n[j]='.' then for j:=j+1 to length(l[i].n) do
    n:=n+l[i].n[j];
   get_extension:=n
end;

procedure order(o: byte);
var i,j: word;
    ni, ei, nj, ej: string;
    ai, aj: byte;
procedure exchange;
var r: rec;
begin
   r:=l[i];
   l[i]:=l[j];
   l[j]:=r;
   ni:=get_name(i);
   ei:=get_extension(i);
   ai:=aj
end;
begin
   if o=0 then exit;
   if p1<2 then exit;
   for i:=1 to p1-1 do
     begin
        ni:=get_name (i); ei:=get_extension (i);
        ai:=l[i].a;
        for j:=i+1 to p1 do
        case o of
          1: begin aj:=l[j].a;
                if (aj and $10) > (ai and $10) then exchange
                 else
                if (ai and $10)=(aj and $10) then
                   begin
                    ej:=get_extension (j);
                    if ei>ej then exchange
                       else
                    if ei=ej then
                      begin
                        nj:=get_name (j);
                        if ni>nj then exchange
                      end
                   end
             end;
        end
     end
end;

procedure browse (p: string; q: string);
var s: string;
begin
   p1:=0;          {C:\GAME*.*}
   if (p='')or(p[byte(p[0])] in[':','\']) then s:=p+mask
    else s:=p+'\'+mask;
   findfirst(s, $3f, r1);
   add_to_list(q);
   if p1>=files then begin color (12); textxy (40, 0, 'Too many files...'); end;
   while (doserror=0)and(p1<files) do
     begin
        findnext (r1);
        add_to_list(q)
     end
end;

const cn=4; cl=15;
      px=(80-13*cn) shr 1; py=(24-cl) shr 1;
   c_back=1;
   c_bord=7;
   c_text=14;
   c_curs=9 shl 4+15;
var x, y, px2, py2: integer;

procedure panel;
begin
   repeat until port[$3da] and 8=0;
   port[$3c0]:=$30; port [$3c0]:=4; chide;
   px2:=px+13*cn; py2:=py+cl+1;
   for y:=py to py2 do for x:=px to px2 do
     begin
        color (c_back shl 4+c_bord);
        if (x=px)and(y=py) then textxy (x, y, 'ษ')
         else
        if (x=px2)and(y=py) then textxy (x, y, 'ป')
         else
        if (x=px)and(y=py2) then textxy (x, y, 'ศ')
         else
        if (x=px2)and(y=py2) then textxy (x, y, 'ผ')
         else
        if x=px then textxy (x, y, 'บ')
         else
        if x=px2 then textxy (x, y, 'บ')
         else
        if (y=py)and((x-px-1)mod 13=12) then textxy (x, y, 'ั')
         else
        if (y=py2)and((x-px-1)mod 13=12) then textxy (x, y, 'ฯ')
         else
        if (x-px-1) mod 13=12 then textxy (x, y, 'ณ')
         else
        if y=py then textxy (x, y, 'อ')
         else
        if y=py2 then textxy (x, y, 'อ')
         else
        begin
           color (c_back shl 4+c_text);
           textxy (x, y, ' ')
        end
     end
end;
procedure panel2 (b: byte);
begin
   color (c_back shl 4+c_bord+b);
   textxy (0, 1, 'ษอออออออออออออออออออป');
   textxy (0, 2, 'บFile:              บ');
   textxy (0, 3, 'ศอออออออออออออออออออผ');
end;

procedure reform (var r: string; a: byte);
var i: byte;
begin
   if a and 16<>0 then
    begin
     for i:=1 to length (r) do
        if r[i] in ['a'..'z'] then r[i]:=char(byte(r[i]) and $df)
         else
        if r[i]='๑' then r[i]:='๐'
         else
        if r[i] in [' '..'๏'] then r[i]:=char(byte(r[i])-32-byte(r[i]>'ฏ')*48)
    end
   else
  for i:=1 to length (r) do
     if r[i] in ['A'..'Z'] then r[i]:=char(byte(r[i]) or $20)
      else
     if r[i]='๐' then r[i]:='๑'
      else
     if r[i] in ['€'..''] then r[i]:=char(byte(r[i])+32+byte(r[i]>'')*48)
end;

procedure save_name;
var r: string;
    j: byte;
begin
   preset:=pp='..';
   if preset then
    begin
      getdir (0, r);
      p0:='';
      for j:=length (r) downto 1 do if r[j] in ['\',':'] then
        begin
           for j:=j+1 to length(r) do p0:=p0+r[j];
           break
        end
    end;
end;

var name: string;
    namef: boolean;

procedure vyvod (sf: boolean);
var i, max: word;
    x, y,j, k, ccx: byte;
    r, s: string;
    desc: word;
begin
   preselect:=1; ccx:=7; name:='            '; namef:=FALSE;
   cursorr:=1;
   if preset then for i:=1 to p1 do
    if l[i].a and $10=$10 then
     if p0=l[i].n then begin cursorr:=i; break end;
   panel;
   if sf then panel2(0);
   repeat
      if desc=0 then desc:=1;
      while cursorr<integer(preselect) do
         dec (preselect,desc);
      if integer(preselect)<1 then preselect:=1;
      while cursorr>=preselect+word(cn)*cl do
         inc (preselect, desc);
      if preselect>p1-word(cn)*cl+1 then
         if p1<word(cn)*cl then preselect:=1
       else preselect:=p1-word(cn)*cl+1;
      color (c_back shl 4+c_text);
      if p1=0 then exit;
      max:=cn*cl;
      if p1-preselect+1<max then max:=p1-preselect+1;
      for i:=preselect to max+preselect-1 do
         begin
            x:=((i-preselect) div cl)*13+px+1;
            y:=(i-preselect) mod cl+py+1;
            s:=l[i].n;
            if s<>'..' then
              begin
                 r:=get_name(i);
                 while length (r)<9 do r:=r+' ';
                 r:=r+get_extension(i)
              end else r:=s;
            while length(r)<12 do r:=r+' ';
            reform (r, l[i].a);
            textxy (x, y, r);
         end;
      x:=((cursorr-preselect) div cl)*13+px+1;
      y:=(cursorr-preselect) mod cl+py+1;
      for x:=x to x+11 do mem [$b800:y*160+x*2+1]:=c_curs;
      repeat
         repeat waiting; scan until kp;
         redr:=false;
         curs1:=cursorr;
         if ek then
            dec (cursorr, byte((keyv=72)and(cursorr>1))
               - byte((keyv=80)and(cursorr<p1))
               + cl*byte((keyv=75)and(cursorr>cl))
               + (cursorr-1)*byte(((keyv=75)and(cursorr<=cl)) or ((keyv=73)and(cursorr<=word(cl)*cn)))
               - cl*byte((keyv=77)and(cursorr<longint(p1)-cl))
               - (p1-cursorr)*byte(((keyv=77)and(cursorr>=longint(p1)-cl)) or ((keyv=81)and(cursorr>=longint(p1)-word(cl)*cn)))
               + (cursorr-1)*byte(keyv=71)
               - (p1-cursorr)*byte(keyv=79)
               + word(cl)*cn*byte((keyv=73)and(cursorr>word(cl)*cn))
               - word(cl)*cn*byte((keyv=81)and(cursorr<longint(p1)-word(cl)*cn)));
         desc:=cl*byte(keyv in [75,77])+word(cn)*cl*byte(keyv in [73,81]);
         if cursorr<>curs1 then begin
            x:=((curs1-preselect) div cl)*13+px+1;
            y:=(curs1-preselect) mod cl+py+1;
            for x:=x to x+11 do mem [$b800:y*160+x*2+1]:=c_back shl 4+c_text;
            redr:=(preselect>cursorr)or(cursorr>=preselect+cl*cn);
            if not redr then
              begin
                 x:=((cursorr-preselect) div cl)*13+px+1;
                 y:=(cursorr-preselect) mod cl+py+1;
                 for x:=x to x+11 do mem [$b800:y*160+x*2+1]:=c_curs
              end
         end;
         if sf then if kp and not ek and (keyv=9) then
          begin
             x:=((cursorr-preselect) div cl)*13+px+1;
             y:=(cursorr-preselect) mod cl+py+1;
             for x:=x to x+11 do mem [$b800:y*160+x*2+1]:=c_back shl 4+c_text;
             cshow; panel2(128+8); color (9*16+15); textxy (7, 2, name);
             cursor (ccx, 2);
             repeat
                waiting;
                scan;
                if kp and not ek and (keyv>31) and (ccx<19) then
                 begin
                    name[ccx-6]:=char(keyv);
                    color (9*16+15); textxy (7, 2, name);
                    inc (ccx); cursor (ccx, 2);
                 end;
                if kp and not ek and (keyv=8) and (ccx>7) then
                 begin
                    dec (ccx); name[ccx-6]:=' '; color (9*16+15); textxy (7, 2, name);
                    cursor (ccx, 2)
                 end;
                if kp and not ek and (keyv=13) and (name<>'            ') then
                 begin
                    while name[length(name)]=' ' do dec (name[0]);
                    namef:=true; exit
                 end;
             until kp and not ek and (keyv=9);
             panel2(0);
             color (16+7); textxy (7, 2, name);
             chide;
             x:=((cursorr-preselect) div cl)*13+px+1;
             y:=(cursorr-preselect) mod cl+py+1;
             for x:=x to x+11 do mem [$b800:y*160+x*2+1]:=c_curs;
          end;
      until (kp and not ek and (keyv in [27,13])) or redr;
   until not redr or (kp and not ek and (keyv=27));
end;
function br_load (n: word; s: string): string;
var q: string;
begin
   color (15); textxy (0, 0, 'Open file...');
   preset:=false;
   getdir(0, ppp);
   if n>0 then pp:=ppo[n] else pp:='';
   repeat
      save_name;
      chdir (pp);
      browse ('',upcase(s));
      order (1);
      vyvod(FALSE);
      pp:=l[cursorr].n;
   until kp and not ek and ((keyv=27) or (l[cursorr].a and $10=0));
   getdir (0,q); ppo[n]:=q;
   if q[length(q)]<>'\' then q:=q+'\';
   q:=q+pp;
   chdir (ppp);
   if kp and not ek and (keyv=27) then br_load:='' else
   br_load:=q;
end;
function br_save (n: word; s: string): string;
var q: string; j: byte;
begin
   color (15); textxy (0, 0, 'Saving file...');
   preset:=false;
   getdir(0, ppp);
   if n>0 then pp:=ppo[n] else pp:='';
   repeat
      save_name;
      chdir (pp);
      browse ('',upcase(s));
      order (1);
      vyvod(TRUE);
      pp:=l[cursorr].n;
   until (kp and not ek and ((keyv=27) or (l[cursorr].a and $10=0))) or namef;
   getdir (0,q); ppo[n]:=q;
   if q[length(q)]<>'\' then q:=q+'\';
   if namef then q:=q+name else q:=q+pp;
   chdir (ppp);
   if namef then
    begin
       for j:=1 to length(name) do if name[j]='.' then begin j:=255; break end;
       if j<>255 then if (s<>'')and(s[1]<>';')and(s[1]<>',') then
        begin q:=q+'.'; j:=1;
           while (s[j]<>';')and(s[j]<>',')and(j<=length(s)) do
            begin q:=q+s[j]; inc (j) end;
        end;
    end;
   if kp and not ek and (keyv=27) then br_save:='' else
   br_save:=q;
end;

begin
   getdir(0, ppp);
   for p1:=1 to remember do ppo[p1]:=ppp;
end.
