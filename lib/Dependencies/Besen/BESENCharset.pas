(*******************************************************************************
                        P R I M A R Y     L I C E N S E
********************************************************************************

BESEN is copyrighted free software by Benjamin Rosseaux <benjamin@rosseaux.com>.
You can redistribute it and/or modify it under either the terms of the AGPLv3
(see COPYING.txt file), or the conditions below:

  1. You may make and give away verbatim copies of the source form of this
     software without restriction, provided that you duplicate all of the
     original copyright notices and associated disclaimers.

  2. You may modify your copy of this software in any way, provided that
     you do at least ONE of the following:

       a) place your modifications in the Public Domain or otherwise
          make them freely available, such as by posting said
	        modifications to Usenet or an equivalent medium, or by allowing
	        the author to include your modifications in this software.

       b) use the modified software only within your corporation or
          organization.

       c) make other distribution arrangements with the author.

  3. You may distribute this software in object code or executable
     form, provided that you do at least ONE of the following:

       a) distribute the executables and library files of this software,
	        together with instructions (in the manual page or equivalent)
	        on where to get the original distribution.

       b) accompany the distribution with the machine-readable source of
      	  this software.

       c) make other distribution arrangements with the author.

  4. You are permitted to link this software with other software, to embed this
     software in your own software, or to build stand-alone binary or bytecode
     versions of applications that include this software, provided that you do
     at least ONE of the following:

       a) place the other software, if it is our own, in the Public Domain
          or otherwise make them together with machine-readable sources
          freely available, such as by posting said modifications to
          Usenet or an equivalent medium.

       b) use the other software, which includes this software, only within
          your corporation or organization and don't make it accessible or
          distribute it to the public / 3rd parties.

       c) make other distribution arrangements with the author.

  5. The scripts and library files supplied as input to or produced as
     output from the software do not automatically fall under the
     copyright of the software, but belong to whomever generated them,
     and may be sold commercially, and may be aggregated with this
     software.

  6. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
     "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
     LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
     FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
     COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
     INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
     BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
     OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
     AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
     OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
     THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
     DAMAGE.

********************************************************************************
                        S E C O N D A R Y    L I C E N S E
********************************************************************************

    BESEN - A ECMAScript Fifth Edition Object Pascal Implementation
    Copyright (C) 2009-2011, Benjamin 'BeRo' Rosseaux

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be usefufl,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*******************************************************************************)
unit BESENCharset;
{$i BESEN.inc}

interface

uses BESENConstants,BESENTypes;

type TBESENCharBitmap=array[0..$f] of byte;

     TBESENCharset=(ISO_8859_1,ISO_8859_2,ISO_8859_3,ISO_8859_4,ISO_8859_5,
                   ISO_8859_6,ISO_8859_7,ISO_8859_8,ISO_8859_9,ISO_8859_10,
                   CP1250,CP1251,CP1252,CP1253,CP1254,CP1255,CP1256,CP1257,CP1258,
                   KOI8_R,UCS_2,UCS_4,UTF_32,UTF_16,UTF_8,UTF_7);

     TBESENCharsetSet=set of TBESENCharset;

     TBESENCharsetTable=array[128..255] of word;

     TBESENCharsetTableCasted=array[128..255] of widechar;

const BESENAllCharsets:TBESENCharsetSet=[ISO_8859_1,ISO_8859_2,ISO_8859_3,ISO_8859_4,
                                         ISO_8859_5,ISO_8859_6,ISO_8859_7,ISO_8859_8,
                                         ISO_8859_9,ISO_8859_10,CP1250,CP1251,CP1252,
                                         CP1253,CP1254,CP1255,CP1256,CP1257,CP1258,
                                         KOI8_R,UCS_2,UCS_4,UTF_32,UTF_16,UTF_8,UTF_7];

      BESENCharISO_8859_1:TBESENCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00A0,$00A1,$00A2,$00A3,$00A4,$00A5,$00A6,$00A7,
        $00A8,$00A9,$00AA,$00AB,$00AC,$00AD,$00AE,$00AF,
        $00B0,$00B1,$00B2,$00B3,$00B4,$00B5,$00B6,$00B7,
        $00B8,$00B9,$00BA,$00BB,$00BC,$00BD,$00BE,$00BF,
        $00C0,$00C1,$00C2,$00C3,$00C4,$00C5,$00C6,$00C7,
        $00C8,$00C9,$00CA,$00CB,$00CC,$00CD,$00CE,$00CF,
        $00D0,$00D1,$00D2,$00D3,$00D4,$00D5,$00D6,$00D7,
        $00D8,$00D9,$00DA,$00DB,$00DC,$00DD,$00DE,$00DF,
        $00E0,$00E1,$00E2,$00E3,$00E4,$00E5,$00E6,$00E7,
        $00E8,$00E9,$00EA,$00EB,$00EC,$00ED,$00EE,$00EF,
        $00F0,$00F1,$00F2,$00F3,$00F4,$00F5,$00F6,$00F7,
        $00F8,$00F9,$00FA,$00FB,$00FC,$00FD,$00FE,$00FF);

      BESENCharISO_8859_2:TBESENCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0104,$02d8,$0141,$00a4,$013d,$015a,$00a7,
        $00a8,$0160,$015e,$0164,$0179,$00ad,$017d,$017b,
        $00b0,$0105,$02db,$0142,$00b4,$013e,$015b,$02c7,
        $00b8,$0161,$015f,$0165,$017a,$02dd,$017e,$017c,
        $0154,$00c1,$00c2,$0102,$00c4,$0139,$0106,$00c7,
        $010c,$00c9,$0118,$00cb,$011a,$00cd,$00ce,$010e,
        $0110,$0143,$0147,$00d3,$00d4,$0150,$00d6,$00d7,
        $0158,$016e,$00da,$0170,$00dc,$00dd,$0162,$00df,
        $0155,$00e1,$00e2,$0103,$00e4,$013a,$0107,$00e7,
        $010d,$00e9,$0119,$00eb,$011b,$00ed,$00ee,$010f,
        $0111,$0144,$0148,$00f3,$00f4,$0151,$00f6,$00f7,
        $0159,$016f,$00fa,$0171,$00fc,$00fd,$0163,$02d9);

      BESENCharISO_8859_3:TBESENCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0126,$02d8,$00a3,$00a4,$fffd,$0124,$00a7,
        $00a8,$0130,$015e,$011e,$0134,$00ad,$fffd,$017b,
        $00b0,$0127,$00b2,$00b3,$00b4,$00b5,$0125,$00b7,
        $00b8,$0131,$015f,$011f,$0135,$00bd,$fffd,$017c,
        $00c0,$00c1,$00c2,$fffd,$00c4,$010a,$0108,$00c7,
        $00c8,$00c9,$00ca,$00cb,$00cc,$00cd,$00ce,$00cf,
        $fffd,$00d1,$00d2,$00d3,$00d4,$0120,$00d6,$00d7,
        $011c,$00d9,$00da,$00db,$00dc,$016c,$015c,$00df,
        $00e0,$00e1,$00e2,$fffd,$00e4,$010b,$0109,$00e7,
        $00e8,$00e9,$00ea,$00eb,$00ec,$00ed,$00ee,$00ef,
        $fffd,$00f1,$00f2,$00f3,$00f4,$0121,$00f6,$00f7,
        $011d,$00f9,$00fa,$00fb,$00fc,$016d,$015d,$02d9);

      BESENCharISO_8859_4:TBESENCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0104,$0138,$0156,$00a4,$0128,$013b,$00a7,
        $00a8,$0160,$0112,$0122,$0166,$00ad,$017d,$00af,
        $00b0,$0105,$02db,$0157,$00b4,$0129,$013c,$02c7,
        $00b8,$0161,$0113,$0123,$0167,$014a,$017e,$014b,
        $0100,$00c1,$00c2,$00c3,$00c4,$00c5,$00c6,$012e,
        $010c,$00c9,$0118,$00cb,$0116,$00cd,$00ce,$012a,
        $0110,$0145,$014c,$0136,$00d4,$00d5,$00d6,$00d7,
        $00d8,$0172,$00da,$00db,$00dc,$0168,$016a,$00df,
        $0101,$00e1,$00e2,$00e3,$00e4,$00e5,$00e6,$012f,
        $010d,$00e9,$0119,$00eb,$0117,$00ed,$00ee,$012b,
        $0111,$0146,$014d,$0137,$00f4,$00f5,$00f6,$00f7,
        $00f8,$0173,$00fa,$00fb,$00fc,$0169,$016b,$02d9);

      BESENCharISO_8859_5:TBESENCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0401,$0402,$0403,$0404,$0405,$0406,$0407,
        $0408,$0409,$040a,$040b,$040c,$00ad,$040e,$040f,
        $0410,$0411,$0412,$0413,$0414,$0415,$0416,$0417,
        $0418,$0419,$041a,$041b,$041c,$041d,$041e,$041f,
        $0420,$0421,$0422,$0423,$0424,$0425,$0426,$0427,
        $0428,$0429,$042a,$042b,$042c,$042d,$042e,$042f,
        $0430,$0431,$0432,$0433,$0434,$0435,$0436,$0437,
        $0438,$0439,$043a,$043b,$043c,$043d,$043e,$043f,
        $0440,$0441,$0442,$0443,$0444,$0445,$0446,$0447,
        $0448,$0449,$044a,$044b,$044c,$044d,$044e,$044f,
        $2116,$0451,$0452,$0453,$0454,$0455,$0456,$0457,
        $0458,$0459,$045a,$045b,$045c,$00a7,$045e,$045f);

      BESENCharISO_8859_6:TBESENCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$fffd,$fffd,$fffd,$00a4,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$060c,$00ad,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$061b,$fffd,$fffd,$fffd,$061f,
        $fffd,$0621,$0622,$0623,$0624,$0625,$0626,$0627,
        $0628,$0629,$062a,$062b,$062c,$062d,$062e,$062f,
        $0630,$0631,$0632,$0633,$0634,$0635,$0636,$0637,
        $0638,$0639,$063a,$fffd,$fffd,$fffd,$fffd,$fffd,
        $0640,$0641,$0642,$0643,$0644,$0645,$0646,$0647,
        $0648,$0649,$064a,$064b,$064c,$064d,$064e,$064f,
        $0650,$0651,$0652,$fffd,$fffd,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd);

      BESENCharISO_8859_7:TBESENCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$2018,$2019,$00a3,$fffd,$fffd,$00a6,$00a7,
        $00a8,$00a9,$fffd,$00ab,$00ac,$00ad,$fffd,$2015,
        $00b0,$00b1,$00b2,$00b3,$0384,$0385,$0386,$00b7,
        $0388,$0389,$038a,$00bb,$038c,$00bd,$038e,$038f,
        $0390,$0391,$0392,$0393,$0394,$0395,$0396,$0397,
        $0398,$0399,$039a,$039b,$039c,$039d,$039e,$039f,
        $03a0,$03a1,$fffd,$03a3,$03a4,$03a5,$03a6,$03a7,
        $03a8,$03a9,$03aa,$03ab,$03ac,$03ad,$03ae,$03af,
        $03b0,$03b1,$03b2,$03b3,$03b4,$03b5,$03b6,$03b7,
        $03b8,$03b9,$03ba,$03bb,$03bc,$03bd,$03be,$03bf,
        $03c0,$03c1,$03c2,$03c3,$03c4,$03c5,$03c6,$03c7,
        $03c8,$03c9,$03ca,$03cb,$03cc,$03cd,$03ce,$fffd);

      BESENCharISO_8859_8:TBESENCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$fffd,$00a2,$00a3,$00a4,$00a5,$00a6,$00a7,
        $00a8,$00a9,$00d7,$00ab,$00ac,$00ad,$00ae,$00af,
        $00b0,$00b1,$00b2,$00b3,$00b4,$00b5,$00b6,$00b7,
        $00b8,$00b9,$00f7,$00bb,$00bc,$00bd,$00be,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$2017,
        $05d0,$05d1,$05d2,$05d3,$05d4,$05d5,$05d6,$05d7,
        $05d8,$05d9,$05da,$05db,$05dc,$05dd,$05de,$05df,
        $05e0,$05e1,$05e2,$05e3,$05e4,$05e5,$05e6,$05e7,
        $05e8,$05e9,$05ea,$fffd,$fffd,$200e,$200f,$fffd);

      BESENCharISO_8859_9:TBESENCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0104,$02d8,$0141,$00a4,$013d,$015a,$00a7,
        $00a8,$0160,$015e,$0164,$0179,$00ad,$017d,$017b,
        $00b0,$0105,$02db,$0142,$00b4,$013e,$015b,$02c7,
        $00b8,$0161,$015f,$0165,$017a,$02dd,$017e,$017c,
        $0154,$00c1,$00c2,$0102,$00c4,$0139,$0106,$00c7,
        $010c,$00c9,$0118,$00cb,$011a,$00cd,$00ce,$010e,
        $011e,$00d1,$00d2,$00d3,$00d4,$00d5,$00d6,$00d7,
        $00d8,$00d9,$00da,$00db,$00dc,$0130,$015e,$00df,
        $00e0,$00e1,$00e2,$00e3,$00e4,$00e5,$00e6,$00e7,
        $00e8,$00e9,$00ea,$00eb,$00ec,$00ed,$00ee,$00ef,
        $011f,$00f1,$00f2,$00f3,$00f4,$00f5,$00f6,$00f7,
        $00f8,$00f9,$00fa,$00fb,$00fc,$0131,$015f,$00ff);

      BESENCharISO_8859_10:TBESENCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0104,$0112,$0122,$012a,$0128,$0136,$00a7,
        $013b,$0110,$0160,$0166,$017d,$00ad,$016a,$014a,
        $00b0,$0105,$0113,$0123,$012b,$0129,$0137,$00b7,
        $013c,$0111,$0161,$0167,$017e,$2015,$016b,$014b,
        $0100,$00c1,$00c2,$00c3,$00c4,$00c5,$00c6,$012e,
        $010c,$00c9,$0118,$00cb,$0116,$00cd,$00ce,$00cf,
        $00d0,$0145,$014c,$00d3,$00d4,$00d5,$00d6,$0168,
        $00d8,$0172,$00da,$00db,$00dc,$00dd,$00de,$00df,
        $0101,$00e1,$00e2,$00e3,$00e4,$00e5,$00e6,$012f,
        $010d,$00e9,$0119,$00eb,$0117,$00ed,$00ee,$00ef,
        $00f0,$0146,$014d,$00f3,$00f4,$00f5,$00f6,$0169,
        $00f8,$0173,$00fa,$00fb,$00fc,$00fd,$00fe,$0138);

      BESENCharCP_1250:TBESENCharsetTable=
       ($20ac,$fffd,$201a,$fffd,$201e,$2026,$2020,$2021,
        $fffd,$2030,$0160,$2039,$015a,$0164,$017d,$0179,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $fffd,$2122,$0161,$203a,$015b,$0165,$017e,$017a,
        $00a0,$02c7,$02d8,$0141,$00a4,$0104,$00a6,$00a7,
        $00a8,$00a9,$015e,$00ab,$00ac,$00ad,$00ae,$017b,
        $00b0,$00b1,$02db,$0142,$00b4,$00b5,$00b6,$00b7,
        $00b8,$0105,$015f,$00bb,$013d,$02dd,$013e,$017c,
        $0154,$00c1,$00c2,$0102,$00c4,$0139,$0106,$00c7,
        $010c,$00c9,$0118,$00cb,$011a,$00cd,$00ce,$010e,
        $0110,$0143,$0147,$00d3,$00d4,$0150,$00d6,$00d7,
        $0158,$016e,$00da,$0170,$00dc,$00dd,$0162,$00df,
        $0155,$00e1,$00e2,$0103,$00e4,$013a,$0107,$00e7,
        $010d,$00e9,$0119,$00eb,$011b,$00ed,$00ee,$010f,
        $0111,$0144,$0148,$00f3,$00f4,$0151,$00f6,$00f7,
        $0159,$016f,$00fa,$0171,$00fc,$00fd,$0163,$02d9);

      BESENCharCP_1251:TBESENCharsetTable=
       ($0402,$0403,$201a,$0453,$201e,$2026,$2020,$2021,
        $20ac,$2030,$0409,$2039,$040a,$040c,$040b,$040f,
        $0452,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $fffd,$2122,$0459,$203a,$045a,$045c,$045b,$045f,
        $00a0,$040e,$045e,$0408,$00a4,$0490,$00a6,$00a7,
        $0401,$00a9,$0404,$00ab,$00ac,$00ad,$00ae,$0407,
        $00b0,$00b1,$0406,$0456,$0491,$00b5,$00b6,$00b7,
        $0451,$2116,$0454,$00bb,$0458,$0405,$0455,$0457,
        $0410,$0411,$0412,$0413,$0414,$0415,$0416,$0417,
        $0418,$0419,$041a,$041b,$041c,$041d,$041e,$041f,
        $0420,$0421,$0422,$0423,$0424,$0425,$0426,$0427,
        $0428,$0429,$042a,$042b,$042c,$042d,$042e,$042f,
        $0430,$0431,$0432,$0433,$0434,$0435,$0436,$0437,
        $0438,$0439,$043a,$043b,$043c,$043d,$043e,$043f,
        $0440,$0441,$0442,$0443,$0444,$0445,$0446,$0447,
        $0448,$0449,$044a,$044b,$044c,$044d,$044e,$044f);

      BESENCharCP_1252:TBESENCharsetTable=
       ($20ac,$fffd,$201a,$0192,$201e,$2026,$2020,$2021,
        $02c6,$2030,$0160,$2039,$0152,$fffd,$017d,$fffd,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $02dc,$2122,$0161,$203a,$0153,$fffd,$017e,$0178,
        $00A0,$00A1,$00A2,$00A3,$00A4,$00A5,$00A6,$00A7,
        $00A8,$00A9,$00AA,$00AB,$00AC,$00AD,$00AE,$00AF,
        $00B0,$00B1,$00B2,$00B3,$00B4,$00B5,$00B6,$00B7,
        $00B8,$00B9,$00BA,$00BB,$00BC,$00BD,$00BE,$00BF,
        $00C0,$00C1,$00C2,$00C3,$00C4,$00C5,$00C6,$00C7,
        $00C8,$00C9,$00CA,$00CB,$00CC,$00CD,$00CE,$00CF,
        $00D0,$00D1,$00D2,$00D3,$00D4,$00D5,$00D6,$00D7,
        $00D8,$00D9,$00DA,$00DB,$00DC,$00DD,$00DE,$00DF,
        $00E0,$00E1,$00E2,$00E3,$00E4,$00E5,$00E6,$00E7,
        $00E8,$00E9,$00EA,$00EB,$00EC,$00ED,$00EE,$00EF,
        $00F0,$00F1,$00F2,$00F3,$00F4,$00F5,$00F6,$00F7,
        $00F8,$00F9,$00FA,$00FB,$00FC,$00FD,$00FE,$00FF);

      BESENCharCP_1253:TBESENCharsetTable=
       ($20ac,$fffd,$201a,$0192,$201e,$2026,$2020,$2021,
        $fffd,$2030,$fffd,$2039,$fffd,$fffd,$fffd,$fffd,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $fffd,$2122,$fffd,$203a,$fffd,$fffd,$fffd,$fffd,
        $00a0,$0385,$0386,$00a3,$00a4,$00a5,$00a6,$00a7,
        $00a8,$00a9,$fffd,$00ab,$00ac,$00ad,$00ae,$2015,
        $00b0,$00b1,$00b2,$00b3,$0384,$00b5,$00b6,$00b7,
        $0388,$0389,$038a,$00bb,$038c,$00bd,$038e,$038f,
        $0390,$0391,$0392,$0393,$0394,$0395,$0396,$0397,
        $0398,$0399,$039a,$039b,$039c,$039d,$039e,$039f,
        $03a0,$03a1,$fffd,$03a3,$03a4,$03a5,$03a6,$03a7,
        $03a8,$03a9,$03aa,$03ab,$03ac,$03ad,$03ae,$03af,
        $03b0,$03b1,$03b2,$03b3,$03b4,$03b5,$03b6,$03b7,
        $03b8,$03b9,$03ba,$03bb,$03bc,$03bd,$03be,$03bf,
        $03c0,$03c1,$03c2,$03c3,$03c4,$03c5,$03c6,$03c7,
        $03c8,$03c9,$03ca,$03cb,$03cc,$03cd,$03ce,$fffd);

      BESENCharCP_1254:TBESENCharsetTable=
       ($20ac,$fffd,$201a,$0192,$201e,$2026,$2020,$2021,
        $02c6,$2030,$0160,$2039,$0152,$fffd,$fffd,$fffd,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $02dc,$2122,$0161,$203a,$0153,$fffd,$fffd,$0178,
        $00A0,$00A1,$00A2,$00A3,$00A4,$00A5,$00A6,$00A7,
        $00A8,$00A9,$00AA,$00AB,$00AC,$00AD,$00AE,$00AF,
        $00B0,$00B1,$00B2,$00B3,$00B4,$00B5,$00B6,$00B7,
        $00B8,$00B9,$00BA,$00BB,$00BC,$00BD,$00BE,$00BF,
        $00C0,$00C1,$00C2,$00C3,$00C4,$00C5,$00C6,$00C7,
        $00C8,$00C9,$00CA,$00CB,$00CC,$00CD,$00CE,$00CF,
        $011e,$00d1,$00d2,$00d3,$00d4,$00d5,$00d6,$00d7,
        $00d8,$00d9,$00da,$00db,$00dc,$0130,$015e,$00df,
        $00E0,$00E1,$00E2,$00E3,$00E4,$00E5,$00E6,$00E7,
        $00E8,$00E9,$00EA,$00EB,$00EC,$00ED,$00EE,$00EF,
        $011f,$00f1,$00f2,$00f3,$00f4,$00f5,$00f6,$00f7,
        $00f8,$00f9,$00fa,$00fb,$00fc,$0131,$015f,$00ff);

      BESENCharCP_1255:TBESENCharsetTable=
       ($20ac,$fffd,$201a,$0192,$201e,$2026,$2020,$2021,
        $02c6,$2030,$fffd,$2039,$fffd,$fffd,$fffd,$fffd,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $02dc,$2122,$fffd,$203a,$fffd,$fffd,$fffd,$fffd,
        $00a0,$00a1,$00a2,$00a3,$20aa,$00a5,$00a6,$00a7,
        $00a8,$00a9,$00d7,$00ab,$00ac,$00ad,$00ae,$00af,
        $00b0,$00b1,$00b2,$00b3,$00b4,$00b5,$00b6,$00b7,
        $00b8,$00b9,$00f7,$00bb,$00bc,$00bd,$00be,$00bf,
        $05b0,$05b1,$05b2,$05b3,$05b4,$05b5,$05b6,$05b7,
        $05b8,$05b9,$fffd,$05bb,$05bc,$05bd,$05be,$05bf,
        $05c0,$05c1,$05c2,$05c3,$05f0,$05f1,$05f2,$05f3,
        $05f4,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,
        $05d0,$05d1,$05d2,$05d3,$05d4,$05d5,$05d6,$05d7,
        $05d8,$05d9,$05da,$05db,$05dc,$05dd,$05de,$05df,
        $05e0,$05e1,$05e2,$05e3,$05e4,$05e5,$05e6,$05e7,
        $05e8,$05e9,$05ea,$fffd,$fffd,$200e,$200f,$fffd);

      BESENCharCP_1256:TBESENCharsetTable=
       ($20ac,$067e,$201a,$0192,$201e,$2026,$2020,$2021,
        $02c6,$2030,$0679,$2039,$0152,$0686,$0698,$0688,
        $06af,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $06a9,$2122,$0691,$203a,$0153,$200c,$200d,$06ba,
        $00a0,$060c,$00a2,$00a3,$00a4,$00a5,$00a6,$00a7,
        $00a8,$00a9,$06be,$00ab,$00ac,$00ad,$00ae,$00af,
        $00b0,$00b1,$00b2,$00b3,$00b4,$00b5,$00b6,$00b7,
        $00b8,$00b9,$061b,$00bb,$00bc,$00bd,$00be,$061f,
        $06c1,$0621,$0622,$0623,$0624,$0625,$0626,$0627,
        $0628,$0629,$062a,$062b,$062c,$062d,$062e,$062f,
        $0630,$0631,$0632,$0633,$0634,$0635,$0636,$00d7,
        $0637,$0638,$0639,$063a,$0640,$0641,$0642,$0643,
        $00e0,$0644,$00e2,$0645,$0646,$0647,$0648,$00e7,
        $00e8,$00e9,$00ea,$00eb,$0649,$064a,$00ee,$00ef,
        $064b,$064c,$064d,$064e,$00f4,$064f,$0650,$00f7,
        $0651,$00f9,$0652,$00fb,$00fc,$200e,$200f,$06d2);

      BESENCharCP_1257:TBESENCharsetTable=
       ($20ac,$fffd,$201a,$fffd,$201e,$2026,$2020,$2021,
        $fffd,$2030,$fffd,$2039,$fffd,$00a8,$02c7,$00b8,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $fffd,$2122,$fffd,$203a,$fffd,$00af,$02db,$fffd,
        $00a0,$fffd,$00a2,$00a3,$00a4,$fffd,$00a6,$00a7,
        $00d8,$00a9,$0156,$00ab,$00ac,$00ad,$00ae,$00c6,
        $00b0,$00b1,$00b2,$00b3,$00b4,$00b5,$00b6,$00b7,
        $00f8,$00b9,$0157,$00bb,$00bc,$00bd,$00be,$00e6,
        $0104,$012e,$0100,$0106,$00c4,$00c5,$0118,$0112,
        $010c,$00c9,$0179,$0116,$0122,$0136,$012a,$013b,
        $0160,$0143,$0145,$00d3,$014c,$00d5,$00d6,$00d7,
        $0172,$0141,$015a,$016a,$00dc,$017b,$017d,$00df,
        $0105,$012f,$0101,$0107,$00e4,$00e5,$0119,$0113,
        $010d,$00e9,$017a,$0117,$0123,$0137,$012b,$013c,
        $0161,$0144,$0146,$00f3,$014d,$00f5,$00f6,$00f7,
        $0173,$0142,$015b,$016b,$00fc,$017c,$017e,$02d9);

      BESENCharCP_1258:TBESENCharsetTable=
       ($20ac,$fffd,$201a,$0192,$201e,$2026,$2020,$2021,
        $02c6,$2030,$fffd,$2039,$0152,$fffd,$fffd,$fffd,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $02dc,$2122,$fffd,$203a,$0153,$fffd,$fffd,$0178,
        $00a0,$00a1,$00a2,$00a3,$00a4,$00a5,$00a6,$00a7,
        $00a8,$00a9,$00aa,$00ab,$00ac,$00ad,$00ae,$00af,
        $00b0,$00b1,$00b2,$00b3,$00b4,$00b5,$00b6,$00b7,
        $00b8,$00b9,$00ba,$00bb,$00bc,$00bd,$00be,$00bf,
        $00c0,$00c1,$00c2,$0102,$00c4,$00c5,$00c6,$00c7,
        $00c8,$00c9,$00ca,$00cb,$0300,$00cd,$00ce,$00cf,
        $0110,$00d1,$0309,$00d3,$00d4,$01a0,$00d6,$00d7,
        $00d8,$00d9,$00da,$00db,$00dc,$01af,$0303,$00df,
        $00e0,$00e1,$00e2,$0103,$00e4,$00e5,$00e6,$00e7,
        $00e8,$00e9,$00ea,$00eb,$0301,$00ed,$00ee,$00ef,
        $0111,$00f1,$0323,$00f3,$00f4,$01a1,$00f6,$00f7,
        $00f8,$00f9,$00fa,$00fb,$00fc,$01b0,$20ab,$00ff);

      BESENCharKOI8_R:TBESENCharsetTable=
       ($2500,$2502,$250c,$2510,$2514,$2518,$251c,$2524,
        $252c,$2534,$253c,$2580,$2584,$2588,$258c,$2590,
        $2591,$2592,$2593,$2320,$25a0,$2219,$221a,$2248,
        $2264,$2265,$00a0,$2321,$00b0,$00b2,$00b7,$00f7,
        $2550,$2551,$2552,$0451,$2553,$2554,$2555,$2556,
        $2557,$2558,$2559,$255a,$255b,$255c,$255d,$255e,
        $255f,$2560,$2561,$0401,$2562,$2563,$2564,$2565,
        $2566,$2567,$2568,$2569,$256a,$256b,$256c,$00a9,
        $044e,$0430,$0431,$0446,$0434,$0435,$0444,$0433,
        $0445,$0438,$0439,$043a,$043b,$043c,$043d,$043e,
        $043f,$044f,$0440,$0441,$0442,$0443,$0436,$0432,
        $044c,$044b,$0437,$0448,$044d,$0449,$0447,$044a,
        $042e,$0410,$0411,$0426,$0414,$0415,$0424,$0413,
        $0425,$0418,$0419,$041a,$041b,$041c,$041d,$041e,
        $041f,$042f,$0420,$0421,$0422,$0423,$0416,$0412,
        $042c,$042b,$0417,$0428,$042d,$0429,$0427,$042a);

var BESENLocaleCharset:TBESENCharset;

implementation

end.
