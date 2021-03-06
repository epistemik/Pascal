program ListTest (input,output) ;

uses Ptr_fxns ;

label 310 ;

{ variable for the main program - NOT to be referenced anywhere else }

var YESorNO: char ;

procedure test ;
var
	L: list ;
	Val: element ;
	Same: boolean ;
	Num, Ins : real ;
	choice: char ;
begin
  create_empty( L ) ;
  writeln('Enter a list of reals, all values on one line: ') ;
  while (not eoln)
  do begin
       read(val) ;
       insert_at_end(L,val)
     end ;
  readln ; { clear the empty line }

  writeln ;
  writeln('Here is the list you entered');
  write_list(L);
  writeln ;

repeat
  writeln('Choose a procedure to test: ' );
  writeln('A: are values all the same?') ;
  writeln('B: insert number before val.') ;
  writeln('C: insert number after val.') ;
  writeln('D: delete first element of list.') ;
  writeln('E: insert at end of list.') ;
  writeln('F: insert at front of list.') ;
  writeln('I: is list sorted in increasing order?') ;
  writeln('L: delete last element of list.') ;
  writeln('V: delete all occurrences of val.') ;
  writeln('X : exit.') ;
  writeln ;
  write('Choice:  ') ;
  readln(choice) ;
  
  case choice of 
  
  	'A', 'a' :  begin
  				all_the_same(L, Same);
  				write ('The values ARE ') ;
  				if not Same then write('NOT ') ;
  				write ('all the same.') ;
  				writeln ;
  				end;
  				
  	'B', 'b' :  begin
  				write('Enter a number to insert:  ') ;
  				readln(num) ;
  				write('Enter value to insert before:  ') ;
  				readln(ins) ;
  				insert_before_val(L, num, ins) ;
  			end ;
  
  	'C', 'c' :  begin
  				write('Enter a number to insert:  ') ;
  				readln(num) ;
  				write('Enter value to insert after:  ') ;
  				readln(ins) ;
  				insert_after_val(L, ins, num) ;
  			end ;
  
	'D', 'd' :  Delete_first(L) ;
	
	'E', 'e' :  begin
  				write('Enter a number to insert:  ') ;
  				readln(num) ;
  				Insert_at_end(L, num) ;
  			end ;

	'F', 'f' :  begin
  				write('Enter a number to insert:  ') ;
  				readln(num) ;
  				Insert_at_front(L, num) ;
  			end ;

	'I', 'i' :  writeln('The list ', Is_increasing(L) ) ;

	'L', 'l' :  Delete_last(L) ;
	
	'V', 'v' :  begin
  				write('Enter a value to delete:  ') ;
  				readln(num) ;
  				Delete_all_vals(L, num) ;
  			end ;

	'X', 'x' :  begin
				writeln('Exiting list processing procedures.') ;
				choice := 'X' ;
			end ;
	else
		begin
		writeln('Invalid choice: ', choice, ': exiting procedure.') ;
		choice := 'X' ;
		end ;
		
	end;  { case }		

  if (choice <> 'X') then
  	begin
	writeln('Here is the modified list: ');
	write_list(L);
	writeln ;
	end ;

until choice = 'X' ;

destroy( L )		{ Return all the nodes in all lists back to the global pool }

end;  { procedure test }


begin  { main program }
	mem_count := 0 ;
repeat
	  test ;
  310: writeln('Amount of dynamic memory allocated but not returned (should be 0): ', mem_count:0) ;
	  writeln('Do you wish to continue (y or n) ?') ;
	  readln(YESorNO);
until (YESorNO in ['N', 'n'])

end.