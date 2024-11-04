procedure s (a, b: byte);
begin port[$388]:=a; port[$389]:=b end;
begin
   s($20,$01);      {Set the modulator's multiple to 1                 }
   s($40,$10);      {Set the modulator's level to about 40 dB          }
   s($60,$D2);      {Modulator attack:  quick;   decay:   long         }
   s($80,$77);      {Modulator sustain: medium;  release: medium       }
   s($23,$01);      {Set the carrier's multiple to 1                   }
   s($43,$10);      {Set the carrier to maximum volume (about 47 dB)   }
   s($63,$D2);      {Carrier attack:  quick;   decay:   long           }
   s($83,$77);      {Carrier sustain: medium;  release: medium         }
   s($A0,$80);      {Set voice frequency's LSB (it'll be a D#)         }
   s($B0,$30);      {Turn the voice on; set the octave and freq MSB    }
end.