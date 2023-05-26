#pragma keep "fptest"
#pragma lint -1

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* This program checks to insure that all subroutines needed by	*/
/* ORCA/C that are not also used by ORCA/Pascal are in the      */
/* library, can be linked, and can be called.  It does _not_    */
/* check the quality of the output.	                        */

float a, b, c;
double d1, d2, d3;
extended e1, e2, e3;
comp c1, c2, c3;
int i;
char s[100];

extern pascal setfpeslot (int);

float ff(void) {return 1.0;}

double df(void) {return 1.0;}

extended ef(void) {return 1.0;}

comp cf(void) {return 1;}


#pragma optimize -1

void f(void)

{
a = ff();
d1 = df();
e1 = ef();
c1 = cf();
}

#pragma optimize 0


void main (void)

{
long l; unsigned u; unsigned long ul;

/* set the FPE slot */
setfpeslot(3);

/* various assignment forms */
a = ff();
b = c = a;
d1 = df();
d2 = d3 = d1;
e1 = ef();
e2 = e3 = e1;
c1 = cf();
c2 = c3 = c1;
f();

/* type casts */

a = 1;
a = 1L;
a = 1u;
a = 1UL;
i = a;
l = a;
u = a;
ul = a;

/* math.h calls */
b = 1.0;
c = 2.0;

a = acos(b);
printf("acos(%f) = %f\n", b, a);
printf("asin(%f) = %f\n", b, asin(b));
printf("atan(%f) = %f\n", b, atan(b));
printf("cos(%f)  = %f\n", b, cos(b));
printf("cosh(%f) = %f\n", b, cosh(b));
printf("exp(%f)  = %f\n", b, exp(b));
printf("log(%f)  = %f\n", b, log(b));
printf("log10(%f) = %f\n", b, log10(b));
printf("sin(%f)  = %f\n", b, sin(b));
printf("sinh(%f) = %f\n", b, sinh(b));
printf("sqrt(%f) = %f\n", b, sqrt(b));
printf("tan(%f)  = %f\n", b, tan(b));
printf("tanh(%f) = %f\n", b, tanh(b));
printf("ceil(%f) = %f\n", b, ceil(b));
printf("fabs(%f) = %f\n", b, fabs(b));
printf("floor(%f) = %f\n", b, floor(b));
printf("atan2(%f,%f) = %f\n", b, c, atan2(b, c));
printf("fmod(%f,%f)  = %f\n", b, c, fmod(b, c));
printf("pow(%f,%f)   = %f\n", b, c, pow(b, c));
a = frexp(b, &i);
a = ldexp(b, i);
a = modf(b, &i);

/* stdio.h floating point converters */
sscanf("1.2", "%f", &a);
sprintf(s, "%f", a);
sprintf(s, "%g", a);

/* stdlib.h floating-point routines */
a = strtod("1.2", NULL);

/* time.h floating-point routines */
a = difftime(20000L, 10000L);
}
