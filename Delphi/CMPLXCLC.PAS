Unit CmplxClc; { CMPLXCLC.PAS unit version 1.0 Reinout Raymakers 1997 }

{$N+,E+}

{ This unit implements a full set of functions to work with complex numbers
  in Turbo Pascal. The type Complex is defined as a pointer to be able to use
  the type as a return value for functions. Values of complex numbers first
  have to be assigned to a variable using CDef, before you can use them in
  calculations...

  Two examples of how to work with this unit:


  The Complex calculation  (2.3 + 4.1i) + cos (-9.1 + 0.7i) can be done
  in several ways:

    1) An extended but understandable one:


     var A, B, C, D : Complex
     ....
     New(A);
     New(B);
     A := CDef(2.3,4.1);
     B := CDef(-9.1,0.7);
     C := CCos(B);
     D := CAdd(A,C);
     ....


    2) A compact but not very readable form:

    var D : Complex;
    ....
    New(D);
    D := CAdd(CDef(2.3,4.1),CCos(CDef(-9.1,0.7));
    ....


  In other words: you can do every step seperatly, but also implement
  all the functions in one line of code....

  P.S. Whether or not it is always necessary to first declare the
       variables with the New command is not clear to me....
       Try it out and see what works best..... (I'm not used to
       working with pointers....)

  You are free to use or change this unit in any way you like, but I would
  appreciate it, if you mailed me your suggestions / changes / bugs or
  anything else you want changed....

  Reinout Raymakers
  Nolensstraat 12
  5344 SK Oss
  The Netherlands
  reinoutr@sci.kun.nl

  RR, 24 December 1997
  
  Other units created by me         : - Full support unit for 256 color PCX files

  Projects that I'm still working on: - True color (24 bit) PCX files unit
                                      - Export routines for GIF / BMP formats


}     



Interface

Type ComplexValue = Record
                          R : Double; { Real part }
                          I : Double; { Complex part }
                    end;
     Complex      = ^ComplexValue;

Const iValue   : ComplexValue = (R: 0.0 ;I: 1.0);
      i        : Complex      = @iValue;
      _2iValue : ComplexValue = (R: 0.0 ;I: 2.0);
      _2i      : Complex      = @_2iValue;
      _1Value  : ComplexValue = (R: 1.0 ;I: 0.0);
      _1       : Complex      = @_1Value;
      _2Value  : ComplexValue = (R: 2.0 ;I: 0.0);
      _2       : Complex      = @_2Value;

Function CDef(R, I : Real) : Complex;         { CDef := R + Ii }

{ Remarks: - All functions marked with 'B' are basic functions, that are
             defined by only using the standard calculations provided by
             Turbo Pascal. All other functions are defined in terms of
             these basic functions and add no (mathematical) functionality
             to the unit.
           - Note that the CArg function has a non-defined situation, in
             which case it gives '666' as an answer (an impossible value
             for an argument).
}


Function CAbsSqr(C : Complex) : Real;         { ( Abs C )^2 } {B}

Function CAbs(C : Complex) : Real;            { Abs C }     {B}

Function CReal(C : Complex) : Real;           { Real C }    {B}
Function Re(C : Complex) : Real;              { Real C }

Function CImag(C : Complex) : Real;           { Imag C }    {B}
Function Im(C : Complex) : Real;              { Imag C }

Function CArg(C : Complex) : Real;            { Arg C }     {B}

Function CConj(C : Complex) : Complex;        { Conj C }    {B}

Function CMin(C : Complex) : Complex;         { - C }       {B}

Function CAdd(C1,C2 : Complex) : Complex;     { C1 + C2 }   {B}

Function CSub(C1,C2 : Complex) : Complex;     { C1 - C2 }   {B}

Function CMul(C1,C2 : Complex) : Complex;     { C1 * C2 }   {B}

Function CDiv(C1,C2 : Complex) : Complex;     { C1 / C2 }   {B}

Function CExp(C : Complex) : Complex;         { e^C }       {B}

Function CLn(C : Complex) : Complex;          { Ln C }      {B}

Function CPow(C1, C2 : Complex) : Complex;    { C1^C2 }

Function CSqr(C : Complex) : Complex;         { C^2 }

Function CLog(C1, C2 : Complex) : Complex;    { C1 Log C2 }

Function CRt(C1, C2 : Complex) : Complex;     { C1 û C2 }

Function CSqrt(C : Complex) : Complex;        { û C }

Function CSin(C : Complex) : Complex;         { Sin C }

Function CCos(C : Complex) : Complex;         { Cos C }

Function CTan(C : Complex) : Complex;         { Tan C }

Function CCot(C : Complex) : Complex;         { Cot C (COT = 1 / TAN) }

Function CSec(C : Complex) : Complex;         { Sec C (SEC = 1 / COS) }

Function CCsc(C : Complex) : Complex;         { Csc C (CSC = 1 / SIN) }

Function CASin(C : Complex) : Complex;        { ArcSin C }

Function CACos(C : Complex) : Complex;        { ArcCos C }

Function CATan(C : Complex) : Complex;        { ArcTan C }

Function CACot(C : Complex) : Complex;        { ArcCot C }

Function CASec(C : Complex) : Complex;        { ArcSec C }

Function CACsc(C : Complex) : Complex;        { ArcCsc C }

Function CSinh(C : Complex) : Complex;        { Sinh C }

Function CCosh(C : Complex) : Complex;        { Cosh C }

Function CTanh(C : Complex) : Complex;        { Tanh C }

Function CCoth(C : Complex) : Complex;        { Coth C }

Function CSech(C : Complex) : Complex;        { Sech C }

Function CCsch(C : Complex) : Complex;        { Csch C }

Function CASinh(C : Complex) : Complex;       { ArcSinh C }

Function CACosh(C : Complex) : Complex;       { ArcCosh C }

Function CATanh(C : Complex) : Complex;       { ArcTanh C }

Function CACoth(C : Complex) : Complex;       { ArcCoth C }

Function CASech(C : Complex) : Complex;       { ArcSech C }

Function CACsch(C : Complex) : Complex;       { ArcCsch C }


Implementation

Function CDef;
var U : Complex;
begin
     New(U);
     U^.R := R;
     U^.I := I;
     CDef := U;
end; { Fill the complex number with the values of R en I }

Function CAbsSqr;
begin
     CAbsSqr := C^.R * C^.R + C^.I * C^.I;
end; { Absolute value of C squared }


Function CAbs;
begin
     CAbs := Sqrt( C^.R * C^.R + C^.I * C^.I );
end; { Absolute value of C }

Function CReal;
begin
     CReal := C^.R;
end; { Real part of C }

Function Re;
begin
     Re := CReal(C);
end; { Real part of C }

Function CImag;
begin
     CImag := C^.I;
end; { Imaginairy part of C }

Function Im;
begin
     Im := CImag(C);
end; { Imaginairy part of C }

Function CArg;
var A, B : Real;
begin
     A := C^.R;
     B := C^.I;
     if (A > 0) then
        begin
             if (B > 0)
             or (B < 0) then CArg := ArcTan(B/A);
             if (B = 0) then CArg := 0;
        end; { if }
     if (A < 0) then
        begin
             if (B > 0) then CArg := ArcTan(B/A) + Pi;
             if (B < 0) then CArg := ArcTan(B/A) - Pi;
             if (B = 0) then CArg := Pi;
        end; { if }
     if (A = 0) then
        begin
             if (B > 0) then CArg :=  Pi / 2;
             if (B < 0) then CArg := -Pi / 2;
             if (B = 0) then CArg := 666;     { Not defined! }
        end; { if }
end; { Argument of C }

Function CConj;
var U : Complex;
begin
     New(U);
     U^.R :=  C^.R;
     U^.I := -C^.I;
     CConj := U;
end; { Complex conjugate of C }

Function CMin;
var U : Complex;
begin
     New(U);
     U^.R := -C^.R;
     U^.I := -C^.I;
     CMin := U;
end; { The negative of C }

Function CAdd;
var U : Complex;
begin
     New(U);
     U^.R := C1^.R + C2^.R;
     U^.I := C1^.I + C2^.I;
     CAdd := U;
end; { C1 and C2 summed }

Function CSub;
var U : Complex;
begin
     New(U);
     U^.R := C1^.R - C2^.R;
     U^.I := C1^.I - C2^.I;
     CSub := U;
end; { Difference between C1 and C2 }

Function CMul;
var U : Complex;
begin
     New(U);
     U^.R := C1^.R * C2^.R - C1^.I * C2^.I;
     U^.I := C1^.R * C2^.I + C1^.I * C2^.R;
     CMul := U;
end; { Product of C1 and C2 }

Function CDiv;
var U      : Complex;
    N      : Real;
begin
     New(U);
     N := C2^.R * C2^.R + C2^.I * C2^.I;
     U^.R := (C1^.R * C2^.R + C1^.I * C2^.I) / N ;
     U^.I := (C2^.R * C1^.I - C1^.R * C2^.I) / N ;
     CDiv := U;
end; { C1 divided by C2 }

Function CExp;
var U : Complex;
begin
     New(U);
     U^.R := exp(C^.R) * cos(C^.I);
     U^.I := exp(C^.R) * sin(C^.I);
     CExp := U;
end; { e to the power C }

Function CLn;
var U : Complex;
begin
     New(U);
     U^.R := Ln(CAbs(C));
     U^.I := CArg(C);
     CLn := U;
end; { Natural logarithm of C }

Function CPow;
var A, B, U : Complex;
begin
     New(U);
     New(A);
     New(B);
     A := CLn(C1);
     B := CMul(C2,A);
     U := CExp(B);
     CPow := U;
end; { C1 to the power C2 }

Function CSqr;
var U : Complex;
begin
     New(U);
     U := CMul(C,C);
     CSqr := U;
end; { C squared }

Function CLog;
var A, B, U : Complex;
begin
     New(U);
     A := CLn(C1);
     B := CLn(C2);
     U := CDiv(B,A);
     CLog := U;
end; { Logarithmic base C1 of C2 (with natural logarithm C1 = e }

Function CRt;
var A, U : Complex;
begin
     New(U);
     A   := CDiv(_1,C1);
     U   := CPow(C2,A);
     CRt := U;
end; { The C1th root of C2 }

Function CSqrt;
var U : Complex;
begin
     New(U);
     U := CRt(_2,C);
     CSqrt := U;
end; { Squareroot of C }

Function CSin;
var U, Z : Complex;
begin
     New(U);
     New(Z);
     Z := CMul(i,C);
     U := CDiv(CSub(CExp(Z),CExp(CMin(Z))),_2i);
     CSin := U;
end; { Sine of C }

Function CCos;
var U, Z : Complex;
begin
     New(U);
     Z := CMul(i,C);
     U := CDiv(CAdd(CExp(Z),CExp(CMin(Z))),_2);
     CCos := U;
end; { Cosine of C }

Function CTan;
var U : Complex;
begin
     New(U);
     U := CDiv(CSin(C),CCos(C));
     CTan := U;
end; { Tangent of C }

Function CCot;
var U : Complex;
begin
     New(U);
     U := CDiv(_1,CTan(C));
     CCot := U;
end; { Cotangent of C }

Function CSec;
var U : Complex;
begin
     New(U);
     U := CDiv(_1,CCos(C));
     CSec := U;
end; { Secant of C }

Function CCsc;
var U : Complex;
begin
     New(U);
     U := CDiv(_1,CSin(C));
     CCsc := U;
end; { Cosecant of C }

Function CASin;
var U : Complex;
begin
     New(U);
     U := CDiv(CLn(CAdd(CMul(i,C),CSqrt(CSub(_1,CSqr(C))))),i);
     CASin := U;
end; { ArcSine of C }

Function CACos;
var U : Complex;
begin
     New(U);
     U := CDiv(CLn(Cadd(C,CSqrt(CSub(Csqr(C),_1)))),i);
     CACos := U;
end; { ArcCosine of C }

Function CATan;
var U, Z : Complex;
begin
     New(U);
     New(Z);
     Z := CMul(i,C);
     U := CMul(CDiv(_1,_2i),CLn(CDiv(CSub(CMin(_1),Z),CSub(Z,_1))));
     CATan := U;
end; { ArcTangent of C }

Function CACot;
var U : Complex;
begin
     New(U);
     U := CATan(CDiv(_1,C));
     CACot := U;
end; { ArcCotangent of C }

Function CASec;
var U : Complex;
begin
     New(U);
     U := CACos(CDiv(_1,C));
     CASec := U;
end; { ArcSecant of C }

Function CACsc;
var U : Complex;
begin
     New(U);
     U := CASin(CDiv(_1,C));
     CACsc := U;
end; { ArcCosecant of C }

Function CSinh;
Var U : Complex;
begin
     New(U);
     U := CDiv(CSub(CExp(C),CExp(CMin(C))),_2);
     CSinh := U;
end; { Sinushyperbolicus of C }

Function CCosh;
Var U : Complex;
begin
     New(U);
     U := CDiv(CAdd(CExp(C),CExp(CMin(C))),_2);
     CCosh := U;
end; { CoSinushyperbolicus of C }

Function CTanh;
var U : Complex;
begin
     New(U);
     U := CDiv(CSinh(C),CCosh(C));
     CTanh := U;
end; { Tangenshyperbolicus of C }

Function CCoth;
var U : Complex;
begin
     New(U);
     U := CDiv(_1,CTanh(C));
     CCoth := U;
end; { Cotangenshyperbolicus of C }

Function CSech;
var U : Complex;
begin
     New(U);
     U := CDiv(_1,CCosh(C));
     CSech := U;
end; { Secanthyperbolicus of C }

Function CCSch;
var U : Complex;
begin
     New(U);
     U := CDiv(_1,CCosh(C));
     CCsch := U;
end; { Cosecanthyperbolicus of C }

Function CASinh;
Var A, U : Complex;
begin
     New(A);
     New(U);
     A := CSqrt(CAdd(CSqr(C),_1));
     U := CLn(CAdd(C,A));
     CASinh := U;
end; { ArcSinushyperbolicus of C }

Function CACosh;
Var A, U : Complex;
begin
     New(A);
     New(U);
     A := CSqrt(CSub(CSqr(C),_1));
     U := CLn(CAdd(C,A));
     CACosh := U;
end; { ArcCosinushyperbolicus of C }

Function CATanh;
var A, U : Complex;
begin
     New(U);
     New(A);
     A := CDiv(CSub(CMin(C),_1),CSub(C,_1));
     U := CDiv(CLn(A),_2);
     CATanh := U;
end; { ArcTangenshyperbolicus of C }

Function CACoth;
var U : Complex;
begin
     New(U);
     U := CATanh(CDiv(_1,C));
     CACoth := U;
end; { ArcCotangenshyperbolicus of C }

Function CASech;
var U : Complex;
begin
     New(U);
     U := CACosh(CDiv(_1,C));
     CASech := U;
end; { ArcSecanthyperbolicus of C }

Function CACsch;
var U : Complex;
begin
     New(U);
     U := CASin(CDiv(_1,C));
     CACsch := U;
end; { ArcCosecanthyperbolicus of C }

end.