{$keep 'fptest'}

{ This program checks to insure that all subroutines needed by	}
{ ORCA/Pascal are in the library, can be linked, and can be	}
{ called.  It does _not_ check the quality of the output.	}

program fptest (input, output, erroroutput);

var
   a: real;
   b, c: double;
   s: string[80];
   i: integer;
   l: longint;
   fl: text;


   function f: real;

   begin
   f := 1.0;
   end;


   function g: double;

   begin
   g := 2.0;
   end;


   procedure h (a: real; b: double);

   begin
   end;


begin
{I/O}
write('Please type two real numbers: '); readln(a, b);
c := a + b;
writeln(a, ' + ', b, ' = ', c);
c := a - b;
rewrite(fl);
writeln(fl, a, ' - ', b, ' = ', c);
reset(fl);
readln(fl, c);
c := a * b;
writeln(erroroutput, a, ' * ', b, ' = ', c);

{basic math}
a := f+g;
a := f-g;
a := f*g;
a := f/g;
a := -f;
a := b**c;
h(a, b);
a := i;
a := l;

{compares}
if a < b then c := a;
if a <= b then c := a;
if a > b then c := a;
if a >= b then c := a;
if a = b then c := a;
if a <> b then c := a;

{functions}
a := abs(b);
a := arccos(b);
a := arcsin(b);
a := arctan(b);
a := arctan2(b, c);
s := cnvds(g, 0, 0);
s := cnvrs(f, 0, 0);
b := cnvsd(s);
a := cnvsr(s);
a := cos(b);
a := exp(b);
a := ln(b);
a := Random;
b := RandomDouble;
i := round(a);
l := round4(a);
a := sin(b);
a := sqr(b);
a := sqrt(b);
a := tan(b);
end.
