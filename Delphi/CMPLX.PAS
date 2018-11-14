unit Cmplx;



{ Unit to provide Complex number services.



  Complex numbers are stored in a string, so that they can be passed
back
  from functions. *Don't* access the parts directly; use the Re, Im
and  
  Complex functions (which will be optimized for speed) so that the

  internal format can change. }



interface



uses

  floats3;  { This unit has basic floating point support, and defines
float = double; }


type

  TComplex = string[2*sizeof(float)];

  { Complex number.  Not a true string:  the values are stored in
binary 
    format within it. }



  TCmplx = record   { The internal storage format for TComplex } 
    len : byte;                                                  
    r,i : float;                                                 
  end;                                                           
                                                                 
function Re(z:TComplex):Float;                                   
{ Extract real part of z. }                                      
                                                                 
function Im(z:TComplex):Float;                                   
{ Extract imaginary part of z. }                                 
                                                                 
function Complex(x,y:float):TComplex;                            
{ Convert x + iy to complex number. }                            
                                                                 
function Polar2Cx(r,theta:float):TComplex;                       
{ Convert r*exp(i theta) to complex type. }                      

function Cx2Str(z:TComplex;d:byte):string;

{ Convert z to string, with d decimal places.  If d < 0, scientific
format
  is used with |d| decimal places. }


function CSum(x,y:TComplex):TComplex;    
{ Complex sum x+y }                      
                                         
function CDiff(x,y:TComplex):TComplex;   
{ Complex difference x-y }               
                                         
function CProd(x,y:TComplex):Tcomplex;   
{ Complex product x*y }                  

{...}

implementation

                                                         
function Complex(x,y:float):TComplex;                    
var                                                      
  result : TCmplx;                                       
begin                                                    
  with result do                                         
  begin                                                  
    len := 2*sizeof(float);                              
    r := x;                                              
    i := y;                                              
  end;                                                   
  Complex := TComplex(result);                           
end;                                                     
                                                         
function Polar2Cx(r,theta:float):TComplex;               
begin                                                    
  Polar2Cx := Complex(r*cos(theta), r*sin(theta));       
end;                                                     
                                                         
function CCiS(theta:float):TComplex;                     
begin                                                    
  CCiS := Complex(cos(theta), sin(theta));               
end;                                             
                                                 
function Cx2Str(z:TComplex;d:byte):String;       
var                                              
  result1,result2 : string;                      
begin                                            
  if d > 0 then                                  
  begin                                          
    Str(Re(z):0:d,result1);                      
    Str(Im(z):0:d,result2);                      
  end                                            
  else                                           
  begin                                          
    Str(Re(z):(d+6),result1);                    
    Str(Im(z):(d+6),result2);                    
  end;                                           
  if Im(z) < 0 then                              
    Cx2Str := result1 + result2 + 'i'            
  else                                           
    Cx2str := result1 + '+' + result2 + 'i';     
end;                                             
                                                 
function Re(z:TComplex):float;                   
begin                                            
  Re := TCmplx(z).r;                             
end;                                             
                                                 
function Im(z:TComplex):float;                   
begin                                            
  Im := TCmplx(z).i;                             
end;                                             
                                                 
function CSum(x,y:TComplex):TComplex;            
{ Complex sum x+y }                              
begin                                            
  CSum := Complex(re(x)+re(y),im(x)+im(y));      
end;                                             
                                                 
function CDiff(x,y:TComplex):TComplex;           
{ Complex difference x-y }                       
begin                                            
  CDiff := Complex(re(x)-re(y), im(x)-im(y));    
end;                                             
                                                 
function CProd(x,y:TComplex):Tcomplex;           
{ Complex product x*y }                          
begin                                                    
  CProd := Complex(Re(x)*Re(y) - Im(x)*Im(y),            
                   Re(x)*Im(y) + Im(x)*Re(y));           
end;                                                     

{...}
