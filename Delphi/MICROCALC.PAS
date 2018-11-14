program microcalc;

Uses Crt;
TYPE  Real=Extended;

{******** From Turbo-Pascal 3.0 'MICROCALC' (example-program) ************}
{******************* !!!!! recursiv !!!!! ********************************}

procedure Evaluate(var Formula: String;    { Fomula to evaluate}
                   x          : Real;      { Place for x in formula }
                   var Value: Real;        { Result of formula }
                   var ErrPos: Integer);   { Position of error }
const
  Numbers: set of Char = ['0'..'9'];
  EofLine  = ^M;

var
  Pos, i: Integer;    { Current position in formula                     }
  Ch: Char;           { Current character being scanned                 }
  dummy : String;
  SumR, SumL  : Integer;

{ Procedure NextCh returns the next character in the formula         }
{ The variable Pos contains the position ann Ch the character        }

  procedure NextCh;
  begin
    repeat
      Pos:=Pos+1;
      if Pos<=Length(Formula) then
      Ch:=Formula[Pos] else Ch:=eofline;
    until Ch<>' ';
  end  { NextCh };


  function Fak(I: Integer): Real;  { Fakultaet }
  var  dummy   : Real;
       j       : Integer;
  begin
    IF i=1 THEN BEGIN  Fak:=1;  Exit;  END;
    dummy:=1;
    FOR i:=1 TO I DO Dummy:=dummy*i;
    Fak:=dummy;
  end  { Fact };

  function Expression: Real;
  var
    E: Real;
    Opr: Char;

    function SimpleExpression: Real;
    var
      S: Real;
      Opr: Char;

      function Term: Real;
      var
        T: Real;

        function SignedFactor: Real;

          function Factor: Real;
          type
            StandardFunction = (fabs,fsqrt,fsqr,fsin,fcos,
            farctan,fln,flog,fexp,ffact,fPi, fE, fX);
            StandardFunctionList = array[StandardFunction] of string[6];

          const
            StandardFunctionNames: StandardFunctionList =('ABS','SQRT','SQR','SIN','COS',
                                                          'ARCTAN','LN','LOG','EXP','FAK','PI','E','X');
          var
            E,EE,L:  Integer;       { intermidiate variables }
            Found:Boolean;
            F: Real;
            Sf:StandardFunction;
            Start:Integer;

          begin { Function Factor }
            if Ch in Numbers then
            begin
              Start:=Pos;
              repeat NextCh until not (Ch in Numbers);
              if Ch='.' then repeat NextCh until not (Ch in Numbers);
              if Ch='E' then
              begin
                NextCh;
                repeat NextCh until not (Ch in Numbers);
              end;
              Val(Copy(Formula,Start,Pos-Start),F,ErrPos);
            end else
            if Ch='(' then
            begin
              NextCh;
              F:=Expression;
              if Ch=')' then NextCh else ErrPos:=Pos;
            end else
            begin
              found:=false;
              for sf:=fabs to fX do
              if not found then
              begin
                l:=Length(StandardFunctionNames[sf]);
                if copy(Formula,Pos,l)=StandardFunctionNames[sf] then
                begin
                  Pos:=Pos+l-1; NextCh;
                  F:=Factor;
                  case sf of
                    fabs:     f:=abs(f);
                    fsqrt:    f:=sqrt(f);
                    fsqr:     f:=sqr(f);
                    fsin:     f:=sin(f);
                    fcos:     f:=cos(f);
                    farctan:  f:=arctan(f);
                    fln :     f:=ln(f);
                    flog:     f:=ln(f)/ln(10);
                    fexp:     f:=exp(f);
                    ffact:    f:=fak(trunc(f));
                    fPi:      f:=Pi;
                    fE:       f:=Exp(1);
                    fX:       f:=X;
                  end;
                  Found:=true;
                end;
              end;
              if not Found then ErrPos:=Pos;
            end;
            Factor:=F;
          end { function Factor};

        begin { SignedFactor }
         if Ch='-' then
          begin
            NextCh; SignedFactor:=-Factor;
          end else SignedFactor:=Factor;
        end { SignedFactor };

      begin { Term }
        T:=SignedFactor;
        while Ch='^' do
        begin
          NextCh; t:=exp(ln(t)*SignedFactor);
        end;
        Term:=t;
      end { Term };

    begin { SimpleExpression }
      s:=term;
      while Ch in ['*','/'] do
      begin
        Opr:=Ch;  NextCh;
        case Opr of
          '*': s:=s*term;
          '/': s:=s/term;
        end;
      end;
      SimpleExpression:=s;
    end { SimpleExpression };

  begin { Expression }
    E:=SimpleExpression;
    while Ch in ['+','-'] do
    begin
      Opr:=Ch; NextCh;
      case Opr of
        '+': e:=e+SimpleExpression;
        '-': e:=e-SimpleExpression;
      end;
    end;
    Expression:=E;
  end { Expression };


begin { procedure Evaluate }
  {--first make the formula a little easier --}

  dummy:='';     { remove all blanks }
  FOR i:=1 TO Length(Formula) DO
    IF Formula[i]<>' ' THEN Dummy:=dummy+Formula[i];

  sumr:=0;  suml:=0;
  FOR i:=1 TO Length(dummy) DO      { brackets ok ? }
     CASE dummy[i] OF
       ',' : dummy[i]:='.';
       '(' : Inc(SumR);
       ')' : Inc(SumL);
        ELSE dummy[i]:=UpCase(dummy[i]);
     END;

  i:=1;
  repeat
    Inc(i);
    IF (dummy[i]='.') AND NOT (dummy[i-1] IN numbers) THEN
      BEGIN
        Insert('0',dummy,i);  Inc(i);
      END;
  until i>=Length(dummy);

  if dummy[1]='.' then Insert('0',dummy,1);
  if dummy[1]='+' then delete(dummy,1,1);

  IF dummy='' THEN
   BEGIN Value:=0;  ErrPos:=-1;  Exit;  END;
  IF sumR<>sumL THEN
   BEGIN Value:=0;  ErrPos:=-2;  Exit;  END;

  Formula:=Dummy;
  Pos:=0; NextCh;
  Value:=Expression;
  if Ch=EofLine then ErrPos:=0 else ErrPos:=Pos;
end { Evaluate };


var
    Value, x  : Real;
    ErrPos, i : Integer;
    Formula    : String;

BEGIN      { Main (Example) }
  ClrScr;
  Formula:='fak(49)/(fak(49-6)*fak(6))';
  Evaluate(Formula, 0, Value, ErrPos);
  Writeln(Formula,' =  ',Value:7:0,'  Possibilitys in "German Lotto"');

  Formula:='100000^(1/5)*cos(pi)';
  Evaluate(Formula, 0, Value, ErrPos);
  Writeln(Formula,' =  ',Value:7:2,'');

  Writeln;
  Formula:='exp(cos(-2*Sqr(x) - 4*x+ 3))/(1/(x+0.01))';
  FOR i:=0 TO 10 DO
   BEGIN
    x:=i*0.1;
    Evaluate(Formula, x, Value, ErrPos);
    Writeln('x = ',x:5:1,'  ',Formula,' =  ',Value:7:3);
  END;

END.
