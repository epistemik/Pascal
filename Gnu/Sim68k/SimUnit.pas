(* SimUnit.pas *)
(* Library of Pascal functions & procedures used by Sim68k.pas - Nov 1999 *)
unit simunit;

INTERFACE

{ Constants and types. DO NOT REDEFINE them in Sim68k.pas }

const
    (* Data Size *)
     byteSize = 0 ;
     wordSize = 1 ;
     longSize = 2 ;

type
    bit     = boolean ;       (* True = 1, False = 0 *)
    twobits = 0..3 ;
    byte    = $0..$FF ;       (* $80..$7F, in 2's CF *)
    word    = $0..$FFFF ;     (* $8000..$7FFF, in 2's CF *)
    long    = $0..$FFFFFFFF ; (* $80000000..$7FFFFFFF, in 2's CF *)

function Byte2Hex(Binary:byte) : string;
(* Conversion of a byte to a hexadecimal string.  For display only.*)

function Word2Hex(Binary:word) : string;
(* Conversion of a word to a hexadecimal string.  For display only.*)

function Long2Hex(Binary:long) : string;
(* Conversion of a long word to a hexadecimal string.  For display only.*)

function Hex2Word(hex: string) : word;
(* Conversion of a hexadecimal string to a word.   *)

function Hex2dec(hex: string) : integer ;

function GetBits (V:word; FirstBit, LastBit:byte):word;
(* Returns a substring of bits between FirstBit and LastBit from V whose type is word  *)

procedure SetBit (var V:word; Position:byte; Value:bit);
(* Sets the bit of V indicated by Position to Value (false or true)    *)

procedure SetBits (var V:word; First, Last:byte; Value:word);
(* Sets the bits of V between First and Last to the least significant bits of Value.    *)

procedure SetByte (var V:word; MSB:bit; Value:byte);
(* Sets the byte of V whose type is word to Value                      *)
(* MSB: false = Least Significant Byte, true = Most Significant Byte   *)

function GetWord (V:long; MSW:bit):word;
(* Gets one word from V whose type is long. *)
(* MSW: false = Least Significant Word, true = Most Significant Word   *)

procedure SetWord (var V:long; MSW:bit; Value:word);
(* Sets one word of V whose type is long to Value.                     *)
(* MSW: false = Least Significant Word, true = Most Significant Word   *)

function GetBitsL (V:long; FirstBit, LastBit:byte):long;
(* Returns a substring of bits between FirstBit and LastBit from V whose type is long    *)

procedure SetBitL (var V:long; Position:byte; Value:bit);
(* Sets the bit of V (whose type is long) indicated by Position to Value (false or true)   *)

procedure SetBitsL (var V:long; First, Last:byte; Value:long);
(* Sets the bits of V (whose type is long) between First and Last to the least significant bits of Value  *)

procedure SetByteL (var V:long; Position:twobits; Value:byte);
(* Sets one byte of V (whose type is long) indicated by position to Value. *)


IMPLEMENTATION

{ **************************************************************************

    Functions for converting from a byte or word to hexadecimal string.

  *************************************************************************** }

function Byte2Hex(Binary:byte) : string;
(* Conversion of a byte to a hexadecimal string.  For display only.*)
var
   hexDigit : byte;
   hexString: string ;
   i        : integer;
begin
     hexString := '';
     for i := 0 to 1 do
        begin
	hexDigit := Binary MOD $10;
	if hexDigit < $A
          then hexString := chr(48+hexDigit) + hexString  (* 0 to 9 *)
	  else hexString := chr(55+hexDigit) + hexString; (* A to F *)
	Binary := Binary DIV $10
	end;{for}
     Byte2Hex := hexString  (* Here is the string *)
end;


function Word2Hex(Binary:word) : string;
(* Conversion of a word to a hexadecimal string.  For display only.*)
var
   hexDigit : byte;
   hexString: string ;
   i        : integer;
begin
     hexString := '';
     for i := 0 to 3 do
	begin
	hexDigit := Binary MOD $10;
	if hexDigit < $A
          then hexString := chr(48+hexDigit) + hexString  (* 0 to 9 *)
	  else hexString := chr(55+hexDigit) + hexString; (* A to F *)
	Binary := Binary DIV $10
	end;{for}
     Word2Hex := hexString  (* Here is the String *)
end;


function Long2Hex(Binary:long) : string;
(* Conversion of a long word to a hexadecimal string. For display only.*)
var
   hexDigit : byte;
   hexString: string ;
   i        : integer;
   tempW    : word;
begin
     hexString := '';

     (* the least significant word *)
     tempW := Binary AND $0000FFFF;
     for i := 0 to 3 do
	begin
	hexDigit := tempW MOD $10;
	if hexDigit < $A
          then hexString := chr(48+hexDigit) + hexString  (* 0 to 9 *)
	  else hexString := chr(55+hexDigit) + hexString; (* A to F *)
	tempW := tempW DIV $10
	end;{for}

     (* the most significant word *)
     tempW := (Binary AND $FFFF0000) SHR 16;
     for i := 0 to 3 do
	begin
	hexDigit := tempW MOD $10;
	if hexDigit < $A
          then hexString := chr(48+hexDigit) + hexString  (* 0 to 9 *)
	  else hexString := chr(55+hexDigit) + hexString; (* A to F *)
	tempW := tempW DIV $10
	end;{for}

     Long2Hex := hexString  (* Here is the string *)
end;


function Hex2Word(hex: string) : word;
(* Conversion of a hexadecimal string to a word.                       *)
var
   hexWord : word ;
   i       : integer;
begin
     hexWord := 0;
     for i := 1 to length(hex) do
	     begin
	     if hex[i] <= '9'
	       then hexWord := Ord(hex[i])-48 + hexWord*$10  (* 0 to 9 *)
	         else hexWord := Ord(hex[i])-55 + hexWord*$10; (* A to F *)
	     end;{for}
     Hex2Word := hexWord  (* Here is the Word *)
end;


function Hex2dec(hex: string) : integer ;
(* Conversion of a hexadecimal string to an integer.   *)
var
   temp2, temp : integer;
begin
     if hex[1] <= '9'
       then temp := ( Ord(hex[1]) - 48 ) * 16  (* 0 to 9 *)
       else temp := ( Ord(hex[1]) - 55 ) * 16 ; (* A to F *)
     if hex[2] <= '9'
       then temp2 := ( Ord(hex[2]) - 48 )  (* 0 to 9 *)
       else temp2 := ( Ord(hex[2]) - 55 )  ; (* A to F *)
     Hex2dec := temp2 + temp ;
end;

(***************************************************************************

 Functions for bit manipulation.

 Pascal, unlike C does not have operators that allow easy manipulation
 of bits within a byte or word. The following procedures and functions
 are designed to help you manipulate (extract and set) the bits.
 You may use or modify these procedures/functions or create others.

***************************************************************************)

function GetBits (V:word; FirstBit, LastBit:byte):word;
(* Returns a substring of bits between FirstBit and LastBit from V whose type is word *)
(* Ex:                    1111 11                          *)
(*     Bit Positions:     5432 1098 7654 3210              *)
(*     V = $1234     (or %0001 0010 0011 0100)             *)
(*     FirstBit = 3, LastBit = 9                           *)
(*     The bits from 3 to 9 are %10 0011 0,                *)
(*     The function returns $0046  (%0000 0000 0100 0110)  *)
begin
     GetBits := (V SHR FirstBit) AND ( (2 SHL (LastBit-FirstBit))-1 )
end;


procedure SetBit (var V:word; Position:byte; Value:bit);
(* Sets the bit of V indicated by Position to Value (false or true) *)
(* Example: If V=$0000, SetBit(V, 6, True) modifies V as $0040      *)
begin
     V := (V AND ($FFFF - (1 SHL Position))) OR (ORD(Value) SHL Position)
end;


procedure SetBits (var V:word; First, Last:byte; Value:word);
(* Sets the bits of V between First and Last to the least significant bits of Value. *)
(* Example: If V=$F0F0, SetBits(V, 4, 11, $00BA) modifies V as $FBA0 *)
var
   pos: Integer;
begin
     for pos := First to Last do
     	SetBit( V, pos, GetBits(Value, pos-First, pos-First)=1 );
end;


procedure SetByte (var V:word; MSB:bit; Value:byte);
(* Sets the byte of V whose type is word to Value                    *)
(* MSB: false = Least Significant Byte, true = Most Significant Byte *)
(* Example: If V=$1234, SetByte(V, True, $BD) modifies V as $BD34    *)
begin
     if MSB
       then V := (V AND $00FF) OR (Value SHL 8)
       else V := (V AND $FF00) OR Value
end;


function GetWord (V:long; MSW:bit):word;
(* Gets one word from V whose type is long. *)
(* MSW: false = Least Significant Word, true = Most Significant Word *)
var
   TmpW:word;
begin
     if MSW
       then TmpW := (V AND $FFFF0000) SHR 16
       else TmpW := V AND $0000FFFF;
     GetWord := TmpW;
end;


procedure SetWord (var V:long; MSW:bit; Value:word);
(* Sets one word of V whose type is long to Value. *)
(* MSW: false = Least Significant Word, true = Most Significant Word *)
begin
     if MSW
       then V := (V AND $0000FFFF) OR (Value SHL 16)
       else V := (V AND $FFFF0000) OR Value
end;


function GetBitsL (V:long; FirstBit, LastBit:byte):long;
(* Returns a substring of bits between FirstBit and LastBit from V whose type is long *)
begin
     GetBitsL := (V SHR FirstBit) AND ( (2 SHL (LastBit-FirstBit))-1 )
end;


procedure SetBitL (var V:long; Position:byte; Value:bit);
(* Sets the bit of V (whose type is long) indicated by Position to Value (false or true) *)
begin
     V := (V AND ($FFFFFFFF - (1 SHL Position))) OR (ORD(Value) SHL Position)
end;


procedure SetBitsL (var V:long; First, Last:byte; Value:long);
(* Sets the bits of V (whose type is long) between First and Last to the least significant bits of Value *)
var
   pos: Integer;
begin
     for pos := First to Last do
        SetBitL( V, pos, GetBitsL(value, pos-First, pos-First)=1 );
end;


procedure SetByteL (var V:long; Position:twobits; Value:byte);
(* Sets one byte of V (whose type is long) indicated by position to value. *)
begin
     case Position of
       0: V := (V AND $FFFFFF00) OR Value;
       1: V := (V AND $FFFF00FF) OR (Value SHL 8);
       2: V := (V AND $FF00FFFF) OR (Value SHL 16);
       3: V := (V AND $00FFFFFF) OR (Value SHL 24);
     end; { case }
end;

(* Initialization : not necessary. *)

END.
{ SimUnit.pas }
