program Sim68k;

uses
    SimUnit ; (* Library containing useful functions/procedures *)

const
    (* List of OpCode or OpNames to create *)
    iADD  = 0;  (* Regular binary integer addition                 *)
    iADDQ = 1;  (* Quick binary integer addition                   *)
    iSUB  = 2;  (* Regular binary integer subtraction              *)
    iSUBQ = 3;  (* Quick binary integer subtraction                *)
    iMULS = 4;  (* Signed binary multiplication                    *)
	  iDIVS = 5;  (* Signed binary division                          *)
	  iNEG  = 6;  (* Signed binary negation                          *)
	  iCLR  = 7;  (* Clear (set to 0)                                *)
	  iNOT  = 8;  (* Logical NOT                                     *)
	  iAND  = 9;  (* Logical AND                                     *)
	  iOR   = 10; (* Logical OR                                      *)
	  iEOR  = 11; (* Logical EXCLUSIVE-OR                            *)
	  iLSL  = 12; (* Logical Shift Left                              *)
	  iLSR  = 13; (* Logical Shift Right                             *)
	  iROL  = 14; (* Rotate Left                                     *)
	  iROR  = 15; (* Rotate Right                                    *)
	  iCMP  = 16; (* Compare (to adjust CVNZH according to D - S)    *)
	  iTST  = 17; (* Test   (to adjust CVNZH according to D)         *)
	  iBRA  = 18; (* Unconditional branch to the given address       *)
	  iBVS  = 19; (* Branch to the given address if overflow         *)
	  iBEQ  = 20; (* Branch to the given address if equal            *)
	  iBCS  = 21; (* Branch to the given address if carry            *)
	  iBGE  = 22; (* Branch to the given address if greater or equal *)
	  iBLE  = 23; (* Branch to the given address if less or equal    *)
	  iMOVE = 24; (* Regular move                                    *)
	 iMOVEQ = 25; (* Quick move                                      *)
	  iEXG  = 26; (* Exchange 2 registers                            *)
	 iMOVEA = 27; (* Move the given address into A[i]                *)
	  iINP  = 28; (* Read from keyboard (input)                      *)
	  iDSP  = 29; (* Display the name, the source and the contents   *)
	  iDSR  = 30; (* Display the contents of the Status bits         *)
	  iHLT  = 31; (* HALT                                            *)

(* Types already defined in SimUnit.pas. DO NOT REDEFINE                 *)
(*      bit      = boolean;           * True = 1, False = 0              *)
(*      twobits  = 0..3;                                                 *)
(*      byte     = $00..$FF;          * $80...$7F, in 2's CF             *)
(*      word     = $0000..$FFFF;      * $8000...$7FFF, in 2's CF         *)
(*      long = $00000000..$FFFFFFFF;  * $80000000..$7FFFFFFF, in 2's CF  *)

(* Size of Data to manipulate                                            *)
(*      byteSize = 0;                                                    *)
(*      wordSize = 1;                                                    *)
(*      longSize = 2;                                                    *)

type memorySize = $0000..$1000;  (* Memory *)

var
	 program_name: string;
	 option: char;     (* Option chosen from the menu by the user *)
	 Mnemo: array[0..31] of string;                  (* Mnemonics *)
	 memory: array[memorySize] of byte ;  (* 4097 bytes of memory *)

	 (* The CPU's registers *)
	 PC:               word ; (* Program Counter       *)
	 TMPS, TMPD, TMPR: long ; (* Temporary Registers   *)
	 OpCode:           word ; (* OPCODE of the current instruction *)
	 OpAddr1, OpAddr2: word ; (* Operand Addresses     *)
	 C:                bit  ; (* Status bit Carry      *)
	 V:                bit  ; (* Status bit Overflow   *)
	 Z:                bit  ; (* Status bit Zero       *)
	 N:                bit  ; (* Status bit Negative   *)
	 H:                bit  ; (* Status bit Halt       *)

	 D: array[0..1] of long ; (* Data Registers        *)
	 A: array[0..1] of word ; (* Address Registers     *)

	 MAR:     word ;  (* Memory Address Register *)
	 MDR:     long ;  (* Memory Data Register    *)
	 RW:      bit  ;  (* Read/Write Bit: READ=True;  WRITE=False *)
	 DS:      twobits;(* Twobits Data Size: ByteSize=0, WordSize=1, LongSize=2 *)


(* Procedures and functions to help you manipulate instruction formats     *)
(***************************************************************************)

function FormatF1 (OpName:byte): boolean;
(* Determines the format of the instruction. F1 = true, F2 = false *)
var
   check : boolean;
BEGIN
     if (OpName = 1) OR (OpName = 3) OR (OpName = 25) OR ((OpName >= 12) AND (OpName <= 15))
       then check := false
       else check := true;
     FormatF1 := check;
END;

procedure MnemoInit;
(* Initializes Mnemo with mnemonic codes corresponding  to            *)
(* each instruction  according to OpName (0..31)                      *)
(* Used by the translation procedure to determine the OpCode.         *)
BEGIN
     Mnemo[iADD]   := 'ADD';
     Mnemo[iADDQ]  := 'ADDQ';
     Mnemo[iSUB]   := 'SUB';
     Mnemo[iSUBQ]  := 'SUBQ';
     Mnemo[iMULS]  := 'MULS';
     Mnemo[iDIVS]  := 'DIVS';
     Mnemo[iNEG]   := 'NEG';
     Mnemo[iCLR]   := 'CLR';
     Mnemo[iNOT]   := 'NOT';
     Mnemo[iAND]   := 'AND';
     Mnemo[iOR]    := 'OR';
     Mnemo[iEOR]   := 'EOR';
     Mnemo[iLSL]   := 'LSL';
     Mnemo[iLSR]   := 'LSR';
     Mnemo[iROL]   := 'ROL';
     Mnemo[iROR]   := 'ROR';
     Mnemo[iCMP]   := 'CMP';
     Mnemo[iTST]   := 'TST';
     Mnemo[iBRA]   := 'BRA';
     Mnemo[iBVS]   := 'BVS';
     Mnemo[iBEQ]   := 'BEQ';
     Mnemo[iBCS]   := 'BCS';
     Mnemo[iBGE]   := 'BGE';
     Mnemo[iBLE]   := 'BLE';
     Mnemo[iMOVE]  := 'MOVE';
     Mnemo[iMOVEQ] := 'MOVEQ';
     Mnemo[iEXG]   := 'EXG';
     Mnemo[iMOVEA] := 'MOVEA';
     Mnemo[iINP]   := 'INP';
     Mnemo[iDSP]   := 'DSP';
     Mnemo[iDSR]   := 'DSR';
     Mnemo[iHLT]   := 'HLT';
END;
{proc MnemoInit}

procedure Loader(name: string);
(* Read into memory a machine language program contained in a file *)
var
   address: word;
   first_character, ch: char;
   file_68b: text; (* Your program in machine language *)
BEGIN
     assign(file_68b, name);
     reset(file_68b);
     address := $0000;
     while (not eof(file_68b)) do
        BEGIN
        read(file_68b, first_character);
        if first_character = '/' then  (* Beginning of comment *)
          repeat   (* Skip the comment *)
            read(file_68b, ch)
          until (ch = first_character) or eoln(file_68b);
        while (not eoln(file_68b)) do
           BEGIN
           read(file_68b, Memory[address]);
           address := address + $0001
           END;
        readln(file_68b)
        END;{while not eof}
     writeln ('Program loaded. ', address,' bytes in memory.');
     close(file_68b);
END;
{proc Loader}

procedure Access_Memory;
(* Copies an element (Byte, Word, Long) from memory\CPU to CPU\memory.  *)
(* Uses DS (byteSize, wordSize, longSize) and RW (read or write).       *)
(* Verifies if we are trying to access an address outside the range     *)
(* allowed for addressing [$0000..$1000].                               *)
BEGIN
     {MAR := $1001 ;}
     if ( (MAR >= $0000) AND (MAR <= $1000) ) then
     (* Valid Memory Address. *)
       if RW then
         (* RW = TRUE = Read = copy an element from memory to CPU *)
         case DS of
             byteSize: MDR := memory[MAR];
             wordSize: MDR := memory[MAR] * $100 + memory[MAR+1];
             longSize: MDR := ((memory[MAR]   * $1000000) AND $FF000000) OR
                              ((memory[MAR+1] * $10000)   AND $00FF0000) OR
                              ((memory[MAR+2] * $100)     AND $0000FF00) OR
                              ( memory[MAR+3] AND $000000FF);
             END{case}
       else
	  (* RW = FALSE = Write = copy an element from the CPU to memory *)
          if DS = byteSize then
            memory[MAR] := MDR mod $100     (* LSB: 8 last bits *)
          else
             if DS = wordSize then
               BEGIN  (* wordSize *)
               memory[MAR] := (MDR div $100) mod $100; (* MSB: 8 first bits *)
               memory[MAR+1] := MDR mod $100; (* LSB: 8 last bits *)
               END
             else BEGIN  (* longSize *)
                  memory[MAR] := (MDR SHR 24) AND $000000FF;(* MSB: 8 first bits *)
                  memory[MAR+1] := (MDR SHR 16) AND $000000FF;
                  memory[MAR+2] := (MDR SHR 8) AND $000000FF;
                  memory[MAR+3] := MDR mod $100
                  END
     else
       (* Invalid Memory Address. *)
        BEGIN
        writeln('*** ERROR *** AccessMemory uses the invalid address $', Word2Hex(MAR));
        writeln('PC Address = ', Word2Hex(PC-2));
        H := true;  (* End of simulation...! *)
        END;
END;
{proc Access_Memory}


procedure Controller ;
(* Fetch-Execute Cycle simulated by the simulator *)
var
   OpName                : Byte;
   Size                  : twobits; (* byteSize/wordSize/longSize *)
   NbOper                : Byte;    (* Number of necessary operands (P+1) *)
   M1, N1, M2, N2, Data  : Byte;
   Sm, Dm, Rm            : boolean; (* Most Significant Bits of TMPS, TMPD, and TMPR *)

 {/ PRIVATE ROUTINES /}

      { Private to Controller}
      procedure Init;
      (* Initialize the simulator (PC and status bits). *)
      BEGIN
           PC := $0000;
           C := False;
           V := False;
           Z := False;
           N := False;
           H := False;
      END;
      {private proc Init - used by proc Controller}

      { Private to Controller}
      procedure FetchOpCode;
      (* Fetch the OpCode from memory *)
      BEGIN
           RW := true; {read}
           DS := wordSize;
           MAR := PC;
           PC := PC + 2;
           Access_Memory;
           Opcode := GetWord(MDR, false); {get LSW from MDR}
      END;
      {private proc FetchOpCode - used by proc Controller}

      { Private to Controller}
      procedure DecodeInstr;
      (* Update the fields (OpName, Size, NbOper, M1, N1, M2, N2, Data) *)
      (* according to given format. Uses GetBits. *)
      BEGIN
           OpName := GetBits(OpCode, 11, 15);
           Size := GetBits(OpCode, 9, 10);
           NbOper := GetBits(OpCode, 8, 8) + 1;
           if (NbOper > 0) then   {SHOULD ALWAYS BE TRUE!}
             if not( FormatF1(OpName) ) then
               BEGIN
               Data := GetBits(OpCode, 4, 7);
               M2 := GetBits(OpCode, 1, 3);
               N2 := GetBits(OpCode, 0, 0)
               END
             else  {is FormatF1}
                if not(OpName = 30) or not(OpName = 31) then
                  BEGIN
                  M1 := GetBits(OpCode, 5, 7);
                  N1 := GetBits(OpCode, 4, 4);
                  M2 := GetBits(OpCode, 1, 3);
                  N2 := GetBits(OpCode, 0, 0)
                  END;
      END;
      {private proc DecodeInstr - used by proc Controller}

   { Private to Controller}
   procedure FetchOperands;
   (* Fetch the operands, according to their number (NbOper) and   *)
   (* addressing modes (M1 et M2)                                  *)
   BEGIN
      if (NbOper > 0) then { SHOULD ALWAYS BE TRUE!}
         (* There are operands *)
         BEGIN
         (* Fetch the address of first operand (in OpAddr1) *)
         if (FormatF1(OpName)) and (M1 = 3) then  {RELATIVE/ABSOLUTE ADDRESSING}
           BEGIN
           RW := true; {read}
           DS := wordSize;
           MAR := PC;
           PC := PC + 2;
           Access_Memory;
           OpAddr1 := GetWord(MDR, false); {get LSW of MDR}
           END;
         (* Fetch the address of second operand, if F1 and 2 operands. *)
         (* 2nd operand of an instruction with format F2 is in OpAddr2 *)
         if (M2 = 3) then
           BEGIN
           RW := true; {read}
           DS := wordSize;
           MAR := PC;
           Access_Memory;
           PC := PC + 2;
           OpAddr2 := GetWord(MDR, false); {get LSW of MDR}
           END;
         (* Check invalid number of operands. *)
         if (NbOper = 2) and ( not(FormatF1(OpName)) ) then
           BEGIN
           write( '*** ERROR *** Invalid number of operands for (', Mnemo[Opname] );
           writeln( ') at address PC = ', Word2Hex(PC-2) );
           H := true;
           END
         END
   END;
   {private proc FetchOperands - used by proc Controller}

   { Private to Controller}
   function NOB(Size:twobits):byte;
   (* return the "number of bytes" *)
   BEGIN
        if Size = 2
          then NOB := 4
          else NOB := Ord(Size)+1;
   END;
   {private func NOB - used by proc Controller}

(******************************************************************************)
(* Since many instructions will make local fetches between temporary registers*)
(* (TMPS,TMPDM TMPR) and the memory or the Dn and An registers it would be    *)
(* useful to create procedures to transfer the words/bytes between them.      *)
(* Here are 2 suggestions of procedures to do this.                           *)
(******************************************************************************)

   { Private to Controller}
   procedure FillTMP( var TmpReg: long;   (* Register to modify - TMPS, TMPD or TMPR *)
                          OpAddrNo: word; (* Address of operand (OpAddr1 or OpAddr2), for mode 11 *)
                          Size: twobits;  (* Data Size                             *)
			  ModeAdr,                (* Required Addressing Mode      *)
                                  RegNo: byte );  (* Register number for  An & Dn  *)
   (* Transfer data in the required temporary register *)
   BEGIN
   (* Depends on Addressing Mode *)
      case ModeAdr of
         $0: (* Data Register Direct *)
             BEGIN
             TmpReg := D[RegNo];
             if (Size = byteSize) then
               BEGIN
               SetByteL(TmpReg, 1, $00);
               SetWord(TmpReg, true, $0000)
               END
             else if (Size = wordSize)
		    then SetWord(TmpReg, true, $0000);
             END;

         $1: (* Address Register Direct *)
             TmpReg := A[RegNo] ;

         $3: (* Relative/Absolute Addressing *)
             (* We need to access memory, except for branching and MOVEA. *)
             BEGIN
               DS := Size;
               RW := true;
               MAR := OpAddrNo;
               Access_Memory;
               TmpReg := MDR;
             END;

         $4: (* Address Register Indirect *)
             (* We need to access memory. *)
             BEGIN
               DS := Size;
               RW := true;
               MAR := A[RegNo];
               Access_Memory;
               TmpReg := MDR;
             END;

         $6: (* Address Register Indirect with Post-Increment *)
             (* We need to access memory. *)
             BEGIN
	       DS := Size;
               RW := true;
               MAR := A[RegNo];
               Access_Memory;
               TmpReg := MDR;
               A[RegNo] := A[RegNo] + NOB(Size);
             END;

	 $7: (* Address Register Indirect with Pre-Decrement *)
	     (* We need to access memory. *)
	     BEGIN
               A[RegNo] := A[RegNo] - NOB(Size);
               DS := Size;
               RW := true;
               MAR := A[RegNo];
               Access_Memory;
               TmpReg := MDR;
             END;

	 else BEGIN
		(* This error should never occur, but just in case...! *)
		write( '*** ERROR *** Invalid addressing mode (', ModeAdr );
		writeln( ') at address PC = ', Word2Hex(PC-2) );
		H := true;
	      END
      END;{case ModeAdr}
   END;
   {private proc FillTMP - used by proc Controller}

   { Private to Controller}
   procedure SetResult( TmpReg: long;   (* Source Register(TMPD...)     *)
			OpAddrNo: word; (* Operand Address(OpAddr1...)  *)
			Size:twobits;   (* Data Size                    *)
			ModeAdr,        (* Required Addressing Mode     *)
			        RegNo: byte);  (* Register Number for An & Dn *)
     (* Transfer the contents of temporary register to Register or Memory *)
     BEGIN
       (* Depends on Addressing Mode *)
       case ModeAdr of
	 $0: (* Data Register Direct *)
	     case Size of
		byteSize: SetBitsL(D[RegNo], 0, 7, TmpReg);
                wordSize: SetWord(D[RegNo], false, GetWord(TmpReg, false));
                longSize: D[RegNo] := TmpReg;
             END;{case}

         $1: (* Address Register Direct *)
             A[RegNo]:= GetWord(TmpReg, false) ;

         $3: (* Relative/Absolute Addressing *)
             (* We need to access memory, except for branching and MOVEA. *)
             BEGIN
               DS := Size;
               RW := false;
               MAR := OpAddrNo;
               MDR := TmpReg;
               Access_Memory;
             END;

         $4: (* Address Register Indirect *)
             (* We need to access memory. *)
             BEGIN
               DS := Size;
               RW := false;
               MAR := A[RegNo];
               MDR := TmpReg;
               Access_Memory;
             END;

         $6: (* Address Register Indirect with Post-Increment          *)
             (* We need to access memory.                              *)
             (* ATTENTION: for some instructions, the address register *)
             (* has already been incremented by FillTMP                *)
             (* DO NOT increment it a 2nd time here                    *)
             BEGIN
               MAR := A[RegNo] - NOB(Size);
               DS := Size;
               RW := false;
               MDR := TmpReg;
               Access_Memory;
	     END;

	 $7: (* Address Register Indirect with Pre-Decrement           *)
	     (* We need to access memory.                              *)
	     (* ATTENTION: for some instructions, the address register *)
	     (* has already been decremented by FillTMP                *)
	     (* DO NOT decrement it a 2nd time here                    *)
	     BEGIN
	       DS := Size;
               RW := false;
               MAR := A[RegNo];
               MDR := TmpReg;
               Access_Memory;
	     END;

	 else {invalid ModeAdr}
             BEGIN
             write( '*** ERROR *** Invalid Addressing Mode (', ModeAdr );
             writeln( ') at address PC = ', Word2Hex(PC-2) );
             H := true;
	     END;
      END;{case ModeAdr}
   END;
   {private proc SetResult - used by proc Controller}

   { Private to Controller}
   function CheckCond(Cond: boolean; Message: string):boolean;
   (* Generic error verification function, with message display,          *)
   (* if Cond is false, display an error message (including the OpName)   *)
   (* The Halt Status bit will also be set to false if there is an Error. *)
   BEGIN
      if Cond
        then CheckCond := True
      else
         BEGIN
         writeln( '*** ERROR *** ', Message, ' for ',
                   Mnemo[OpName], ' at address $', Word2Hex(PC-2) );
         (* Set the H bit to True *)
         H := true;
         CheckCond := False
         END
   END;
   {private func CheckCond - used by proc Controller}

   { Private to Controller}
   procedure SetZN(TmpNo:long);
   (* Since Status bits Z and N are often set the same way in many *)
   (* instructions. A procedure would be useful to do this.        *)
   BEGIN
      (* Set Z *)
      case Size of
         byteSize: Z := ((GetBits(GetWord(TmpNo, false), 0, 7) OR $00) = $0);
         wordSize: Z := ((GetBits(GetWord(TmpNo, false), 0, 15) OR $0000) = $0);
         longsize: Z := ((TmpNo OR $00000000) = $0);
      END;

      (* Set N *)
      case Size of
         byteSize: N := (GetBits(GetWord(TmpNo, false), 7, 7) = $1);
         wordSize: N := (GetBits(GetWord(TmpNo, false), 15, 15) = $1);
         longsize: N := (GetBits(GetWord(TmpNo, true), 15, 15) = $1);
      END;
   END;
   {private proc SetZN - used by proc Controller}

   { Private to Controller}
   procedure SetSmDmRm(TSrc, TDest, TRes: long);
   (* The calculations to find V and C are more complex but are simplified   *)
   (* by the use of Sm, Dm, Rm.  It would be a good Idea to make a procedure *)
   (* to find these values                                                   *)
   var
      mostSigBit : byte;
   BEGIN
      case size of
         byteSize: mostSigBit := 7;
         wordSize: mostSigBit := 15;
         longSize: mostSigBit := 31;
         END;
      Sm := (GetBitsL(TSrc, mostSigBit, mostSigBit) = $1);
      Dm := (GetBitsL(TDest, mostSigBit, mostSigBit) = $1);
      Rm := (GetBitsL(TRes, mostSigBit, mostSigBit) = $1);
   END;
   {private proc SetSmDmRm - used by proc Controller}


   (**********************************************************************)
   (*  The execution of each instruction is done via its micro-program   *)
   (**********************************************************************)

   { Private to Controller}
   procedure ExecInstr;
   (* Execute the instruction according to opCode.        *)
   (* Use a CASE structure where each case corresponds to *)
   (* an instruction and its micro-program.               *)
   var
      i: byte;   (* Counter *)
      tmpA: word;

   BEGIN
     case OpName of

     iADD:  BEGIN
              (* EXAMPLE micro-program according to step 2.4.1 in section 3*)
              (* 1. Fill TMPS if necessary *)
                 FillTMP(TMPS, OpAddr1, Size, M1, N1);
              (* 2. Fill TMPD if necessary *)
                 FillTMP(TMPD, OpAddr2, Size, M2, N2);
              (* 3. Compute TMPR using TMPS and TMPD  *)
                 TMPR := TMPS + TMPD;
              (* 4. Update status bits HZNVC if necessary  *)
                 SetZN(TMPR);
                 SetSmDmRm (TMPS, TMPD, TMPR);
                 V := (Sm AND Dm AND NOT(Rm)) OR (NOT(Sm) AND NOT(Dm) AND Rm);
                 C := (Sm AND Dm) OR (NOT(Rm) AND Dm) OR (Sm AND NOT(Rm));
              (* 5. Store the result in the destination if necessary *)
                 SetResult(TMPR, OpAddr2, Size, M2, N2);
            END;

     iADDQ:  BEGIN
               FillTMP(TMPD, OpAddr2, Size, M2, N2);
               TMPS := $0 ;
	       SetByteL(TMPS, 0, Data) ;
	       {Sign extension if W or L ??}
               TMPR := TMPD + TMPS;
               SetZN(TMPR);
               SetSmDmRm (TMPS, TMPD, TMPR) ;
               V := (Sm and Dm and NOT(Rm)) or (NOT(Sm) and NOT(Dm) and Rm);
               C := (Sm and Dm) or (NOT(Rm) and Dm) or (Sm and NOT(Rm));
               SetResult(TMPR, OpAddr2, Size, M2, N2);
             END;

     iSUB:  BEGIN
               FillTMP(TMPS, OpAddr1, Size, M1, N1);
               FillTMP(TMPD, OpAddr2, Size, M2, N2);
               TMPR := TMPD - TMPS;
               SetZN(TMPR);
               SetSmDmRm (TMPS, TMPD, TMPR);
               V := (NOT(Sm) and Dm and NOT(Rm)) or (Sm and NOT(Dm) and Rm);
               C := (Sm and NOT(Dm)) or (Rm and NOT(Dm)) or (Sm and Rm);
               SetResult(TMPR, OpAddr2, Size, M2, N2) ;
            END;

     iSUBQ: BEGIN
               FillTMP(TMPD, OpAddr2, Size, M2, N2);
               TMPS := $0;
               SetByteL(TMPS, 0, Data);
               {Sign extension if W or L ??}
               TMPR := TMPD - TMPS;
               SetZN(TMPR);
               SetSmDmRm (TMPS, TMPD, TMPR);
               V := (NOT(Sm) and Dm and NOT(Rm)) or (Sm and NOT(Dm) and Rm);
               C := (Sm and NOT(Dm)) or (Rm and NOT(Dm)) or (Sm and Rm);
               SetResult(TMPR, OpAddr2, Size, M2, N2);
            END;

     iMULS: BEGIN
               if CheckCond( (Size = wordSize), 'Invalid Data Size' ) then
                  BEGIN
                     FillTMP(TMPS, OpAddr1, Size, M1, N1);
                     FillTMP(TMPD, OpAddr2, Size, M2, N2);
                     if (GetBitsL(TMPS,15,15) = $1)
                     then TMPS := TMPS or $FFFF0000;
                     if (GetBitsL(TMPD,15,15) = $1)
                     then TMPD := TMPD or $FFFF0000;
                     TMPR := TMPD * TMPS;
                     SetZN(TMPR);
                     V := false;
                     C := false;
                     SetResult(TMPR, OpAddr2, longSize, M2, N2);
                  END { if size = 1 }
            END;

     iDIVS: BEGIN
               if CheckCond( (Size = 2), 'Invalid Data Size' ) then
                 BEGIN
		 FillTMP(TMPS, OpAddr1, wordSize, M1, N1) ;
		 if CheckCond( (TMPS <> $0), 'Division by Zero' ) then
                   BEGIN
                   FillTMP(TMPD, OpAddr2, Size, M2, N2);
                   V := ((TMPD div TMPS) < -32768) OR ((TMPD div TMPS) > 32767);
                   if TMPS > $8000 then
                     BEGIN
                     i := $1;
                     TMPS := (TMPS xor $FFFF) + 1;
                     TMPD := (TMPD xor $FFFFFFFF) + 1
                     END;
                   if ((TMPD div TMPS) = 0) and (i = $1) then
                     BEGIN
                     SetWord(TMPR, false, $0000);
                     TMPD := (TMPD xor $FFFFFFFF) + 1;
                     SetWord(TMPR, true, TMPD mod TMPS)
                     END
                   else
                       BEGIN
                       TMPR := TMPD div GetWord(TMPS, false) ;
		       SetWord(TMPR, true, (TMPD mod GetWord(TMPS, false)))
                       END;
		   SetZN(TMPR) ;
		   C := false ;
		   SetResult(TMPR, OpAddr2, Size, M2, N2) ;
		   END { if div by 0 }
		 END { if size = 2 }
            END;

     iNEG: BEGIN
              FillTMP(TMPD, OpAddr1, Size, M1, N1);
              TMPR := - TMPD;
              SetZN(TMPR);
              SetSmDmRm (TMPS, TMPD, TMPR);
              V := Dm and Rm;
              C := Dm or Rm;
              SetResult(TMPR, OpAddr1, Size, M1, N1);
           END;

     iCLR: BEGIN
              TMPD := $0;
              SetZN(TMPD);
              V := false;
              C := false;
              SetResult(TMPD, OpAddr1, Size, M1, N1);
           END;

     iNOT: BEGIN
              FillTMP(TMPD, OpAddr1, Size, M1, N1);
              TMPR := not(TMPD);
              SetZN(TMPR);
              V := false;
              C := false;
              SetResult(TMPR, OpAddr1, Size, M1, N1);
           END;

     iAND: BEGIN
             FillTMP(TMPS, OpAddr1, Size, M1, N1);
             FillTMP(TMPD, OpAddr2, Size, M2, N2);
             TMPR := TMPD and TMPS;
             SetZN(TMPR);
             V := false;
             C := false;
             SetResult(TMPR, OpAddr2, Size, M2, N2);
           END;

     iOR: BEGIN
            FillTMP(TMPS, OpAddr1, Size, M1, N1);
            FillTMP(TMPD, OpAddr2, Size, M2, N2);
            TMPR := TMPD or TMPS;
            SetZN(TMPR);
            V := false;
            C := false;
            SetResult(TMPR, OpAddr2, Size, M2, N2);
          END;

     iEOR: BEGIN
            FillTMP(TMPS, OpAddr1, Size, M1, N1);
            FillTMP(TMPD, OpAddr2, Size, M2, N2);
            TMPR := TMPD xor TMPS;
            SetZN(TMPR);
            V := false;
            C := false;
            SetResult(TMPR, OpAddr2, Size, M2, N2);
           END;

     iLSL: BEGIN
            FillTMP(TMPD, OpAddr2, Size, M2, N2);
            TMPR := TMPD SHL Data;
            SetZN(TMPR);
            V := False;
            if (Data > 0) then
              BEGIN
              if GetBitsL(TMPD, NOB(Size)*8-Data, NOB(Size)*8-Data) = 1
                then C:= true
                else C:= false
              END
            else
                C := false;
            SetResult(TMPR, OpAddr2, Size, M2, N2);
           END;

     iLSR: BEGIN
            FillTMP(TMPD, OpAddr2, Size, M2, N2);
            TMPR := TMPD shr Data;
            SetZN(TMPR) ;
            V := false;
            if Data > 0 then
               C := ( GetBitsL(TMPD, Data-1, Data-1) = 1 )
            else
                C := false;
            SetResult(TMPR, OpAddr2, Size, M2, N2);
           END;

     iROL: BEGIN
            FillTMP(TMPD, OpAddr2, Size, M2, N2);
            Data := Data mod 8*NOB(Size);
            TMPR := TMPD shl Data;
            TMPS := TMPD shr 8*NOB(Size)-Data;
            SetBitsL( TMPR, 0, Data-1, TMPS );
            SetZN(TMPR);
            V := false;
            if Data > 0 then
               C := ( GetBitsL(TMPD, NOB(Size)*8-Data, NOB(Size)*8-Data) = 1 )
            else
                C := false;
            SetResult(TMPR, OpAddr2, Size, M2, N2);
           END;

     iROR: BEGIN
            FillTMP(TMPD, OpAddr2, Size, M2, N2);
            Data := Data mod 8*NOB(Size);
            TMPR := TMPD shr Data;
            SetBitsL( TMPR, 8*NOB(Size)-Data, 8*NOB(Size)-1, TMPD );
            SetZN(TMPR);
            V := false;
            if Data > 0 then
               C := ( GetBitsL(TMPD, Data-1, Data-1) = 1 )
            else
                C := false ;
            SetResult(TMPR, OpAddr2, Size, M2, N2);
           END;

     iCMP: BEGIN
            FillTMP(TMPS, OpAddr1, Size, M1, N1);
            FillTMP(TMPD, OpAddr2, Size, M2, N2);
            TMPR := TMPD - TMPS;
            SetZN(TMPR);
            SetSmDmRm (TMPS, TMPD, TMPR);
            V := ( NOT(Sm) and Dm and NOT(Rm) ) or ( Sm and NOT(Dm) and Rm );
            C := ( Sm and NOT(Dm) ) or ( Rm and NOT(Dm) ) or ( Sm and Rm );
           END;

     iTST: BEGIN
            FillTMP(TMPD, OpAddr1, Size, M1, N1);
            SetZN(TMPD);
            V := false;
            C := false;
           END;

     iBRA: BEGIN
            if CheckCond( (M1 = $3), 'Invalid Addressing Mode' )
              then if CheckCond( (Size = wordSize), 'Invalid Data Size' )
                     then PC := OpAddr1
           END;

     iBVS: BEGIN
            if CheckCond( (M1 = $3), 'Invalid Addressing Mode' )
              then if CheckCond( (Size = wordSize), 'Invalid Data Size' )
                     then if V then PC := OpAddr1
           END;

     iBEQ: BEGIN
            if CheckCond( (M1 = $3), 'Invalid Addressing Mode' )
              then if CheckCond( (Size = wordSize), 'Invalid Data Size' )
                     then if Z then PC := OpAddr1
           END;

     iBCS: BEGIN
            if CheckCond( (M1 = $3), 'Invalid Addressing Mode')
              then if CheckCond ( (Size = wordSize), 'Invalid Data Size')
                     then if C then PC := OpAddr1
           END;

     iBGE: BEGIN
            if CheckCond( (M1 = $3), 'Invalid Addressing Mode')
              then if CheckCond ( (Size = wordSize), 'Invalid Data Size')
                     then if not (N xor V) then PC := OpAddr1
           END;

     iBLE: BEGIN
            if CheckCond( (M1 = $3), 'Invalid Addressing Mode')
              then if CheckCond ( (Size = wordSize), 'Invalid Data Size')
                     then if (N xor V) then PC := OpAddr1
           END;

     iMOVE: BEGIN
             FillTMP(TMPS, OpAddr1, Size, M1, N1);
             SetResult(TMPS, OpAddr2, Size, M2, N2);
            END;

    iMOVEQ: BEGIN
              FillTMP(TMPD, OpAddr2, Size, M2, N2);
              SetByteL(TMPD, 0, Data);
              { Sign extension if W or L ??	}
              SetZN(TMPD);
              V := false;
              C := false;
              SetResult(TMPD, OpAddr2, Size, M2, N2);
            END;

     iEXG: BEGIN
            if CheckCond((((M1=$0) or (M1=$1)) and ((M2=$0) or (M2=$1))), 'Invalid Addressing Mode')
            then BEGIN
                 FillTMP(TMPS, OpAddr1, Size, M1, N1);
                 FillTMP(TMPD, OpAddr2, Size, M2, N2);
                 SetResult(TMPS, OpAddr1, Size, M2, N2);
                 SetResult(TMPD, OpAddr2, Size, M1, N1);
                 V := false;
                 C := false;
                 END { if valid address }
           END;

    iMOVEA: BEGIN
              if CheckCond( (Size=wordSize), 'Invalid Data Size')
                then if CheckCond( ((M1=$3) AND (M2=$1)), 'Invalid Addressing Mode')
                       then SetResult(OpAddr1, OpAddr2, Size, M2, N2)
            END;

     iINP: BEGIN
              write('Enter a value ');
              case Size of
                 byteSize: write('(byte) for ');
                 wordSize: write('(word) for ');
                 longSize: write('(long) for ');
                 END;{Size}
              case M1 of
		 $0: write('the register D', N1);
                 $1: write('the register A', N1);
                 $4: write('the memory address $', Word2Hex(A[N1]):4);
                 $6: write('the memory address $', Word2Hex(A[N1]):4);
                 $7: write('the memory address $', Word2Hex(A[N1]):4);
                 $3: write('the memory address $', Word2Hex(OpAddr1));
                 END;{M1}
              write(': ');
              readln(TMPD);
              SetZN(TMPD);
              C:= false;
              V:= false;
              SetResult(TMPD, OpAddr1, Size, M1, N1)
           END;

     iDSP: BEGIN
              FillTMP(TMPS, OpAddr1, Size, M1, N1);
              case M1 of
                 $0: write('[ D', N1,' ]  = $');
                 $1: write('[ A', N1,' ]  = $');
                 $4: write('[$',Word2Hex(A[N1]),'] = $');
                 $6: BEGIN
                       (* NOB(Size) subtracted to compensate post-incrementation *)
                       tmpA := A[N1] - NOB(Size);
                       write('[$',Word2Hex(tmpA),'] = $');
                     END;
                 $7: write('[$',Word2Hex(A[N1]),'] = $');
                 $3: write('[$',Word2Hex(OpAddr1):4,'] = $');
                 END;{M1}
              case Size of
                 byteSize: writeln(Byte2Hex(TMPS), ' (byte)');
                 wordSize: writeln(Word2Hex(TMPS), ' (word)');
                 longSize: writeln(Long2Hex(TMPS), ' (long)');
                 END{Size}
           END;

     iDSR: BEGIN writeln( 'Status Bits: H:', H, ' N:', N, ' Z:', Z, ' V:', V, ' C:', C ) END;

     iHLT: H := true ;  (* Set the Halt Status Bit to 1 (stops program) *)

     END; (* case *)
   END;
   {private proc ExecInstr - used by proc Controller}


BEGIN (* CONTROLLER *)
     Init;
     repeat
        FetchOpCode;
        DecodeInstr;
        FetchOperands;
        if not H then ExecInstr;
     until H;
     (* Repeat the Fetch-Execute Cycle until the H bit becomes 1. *)
     writeln;
     writeln ('End of program Execution.');
END;
{ proc Controller}

(***************************************************************************)

procedure Identification ;
BEGIN
     {clrscr;}
     writeln('*** Sim68k ***') ;
     writeln ;
     writeln('Project CSI 2111 (November 1999).') ;
     (* Insert your names and student numbers *)
     writeln('FirstName LastName: #StudentNumber');
     writeln('-------------------------------------');
     writeln('Tu        Nguyen    555104        ');
     writeln('Mark      Sattolo   428500        ');
     writeln('Thuy      Nguyen    1768590       ');
     writeln('-------------------------------------');
     writeln;
     writeln('Menu:               ');
     writeln('(e)xecute <file.68b>');
     writeln('(q)uit the simulator');
     writeln; writeln;
END ;
{proc Identification}

{ MAIN PROGRAM }
BEGIN
     program_name := '';
     option := ' ';
     MnemoInit;

     (* Menu *)
     while (option <> 'q') do
        BEGIN
        identification;
        write('Your Option : ');
        readln(option);
        case option of
           'e' : BEGIN
                 (* Execution on the simulator *)
                 write('Name of the 68k binary program (".68b" will be added automatically): ');
                 readln(program_name);
		 Loader( concat(program_name,'.68b') );
                 Controller; (* Start the simulator *)
                 END;

           'q' : writeln('Bye! and come again');

           else writeln('Invalid Option. Please enter e or q.');
           END;  (* case *)

        writeln;
        writeln('Press <Enter>.');
        readln;
        END; (* while *)
END.
{program Sim68k}
