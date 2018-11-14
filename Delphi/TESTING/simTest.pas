program  simTest (input,output);

var
   target, opcode, operand, result, modulus : long;
   opstring: array[1..6] of string;

BEGIN
   opstring[1] := 'ADD' ;
   opstring[2] := 'SUB' ;
   opstring[3] := 'MUL' ;
   opstring[4] := 'DIV' ;
   opstring[5] := '5' ;
   opstring[6] := '6' ;
   repeat
      write('Please enter a target value (-999 to exit): ');
      readln(target);
      writeln('Please enter a choice for the operation,');
      write('[1=ADD, 2=SUB, 3=MUL, 4=DIV]  CHOICE: ');
      readln(opcode);
      write('Please enter an operand value: ');
      readln(operand);
      if target <> -999 then
	BEGIN
	case opcode of
           1: result := target + operand ;
           2: result := target - operand ;
           3: result := target * operand ;
           4: BEGIN
              result := target div operand ;
              modulus := target mod operand ;
              END
	   else writeln('Error: target not in range -> changing to opcode 1 [ADD].');
           END;//case
        writeln(target, ' ', opstring[opcode], ' ', operand, ' is ', result);
        if opcode = 4 then writeln(target, ' MOD ', operand, ' is ', modulus);
        writeln;
	END;//if
      writeln('PROGRAM ENDED.') ;
   until N = -999 ;
END.
