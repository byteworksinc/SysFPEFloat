         keep  obj/math
         mcopy math.macros
         case  on

****************************************************************
*
*  Math - Math libraries for C
*
*  This code implements the tables and subroutines needed to
*  support the standard C library MATH.
*
*  January 1989
*  Mike Westerfield
*
*  Copyright 1989
*  Byte Works, Inc.
*
****************************************************************
*
Math     start                          dummy segment
         copy  equates.asm
         end

****************************************************************
*
*  MathCommon - common work areas for the math library
*
****************************************************************
*
MathCommon privdata
;
;  constants
;
pi       dc    h'35C2 6821 A2DA 0FC9 0040'
piover2  dc    h'35C2 6821 A2DA 0FC9 FF3F'
;
;  temporary work space
;
t1       ds    10
t2       ds    10
t3       ds    10
sign     ds    2
         end

****************************************************************
*
*  asin - return arcsin(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
asin     start

         lda   #$0C48
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  acos - return arccos(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
acos     start

         lda   #$1C48
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  atan - return arctan(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
atan     start

         lda   #$0A48
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  atan2 - return arctangent(y,x) scaled to -pi..pi
*
*  Inputs:
*        stack - extended numbers
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
atan2    start
         using MathCommon

         phb                            place the numbers in the work areas
         plx
         ply
         phk
         plb
         pla
         sta   t1
         pla
         sta   t1+2
         pla
         sta   t1+4
         pla
         sta   t1+6
         pla
         sta   t1+8
         pla
         sta   t2
         pla
         sta   t2+2
         pla
         sta   t2+4
         pla
         sta   t2+6
         pla
         sta   t2+8
         phy
         phx
;
;  do special processing for x=0
;
         lda   t2+8                     branch if x <> 0
         and   #$7FFF
         ora   t2+6
         ora   t2+4
         ora   t2+2
         ora   t2
         bne   lb2
         lda   t1+8                     if y = 0, report a range error
         and   #$7FFF
         ora   t1+6
         ora   t1+4
         ora   t1+2
         ora   t1
         bne   lb1
         lda   #ERANGE
         sta   >errno
         brl   lb5

lb1      lda   t1+8                     return pi/2 with the sign of t1
         and   #$8000
         ora   piover2+8
         sta   t1+8
         move  piover2,t1,#8
         bra   lb5
;
;  handle cases where x <> 0
;
lb2      lda   t2+8                     save the sign of t2 in sign
         and   #$8000
         sta   sign
         lda   t2+8                     t2 := abs(t2)
         and   #$7FFF
         sta   t2+8
         ph4   #t2                      t1 := t1/t2
         ph4   #t1
         fdivx
         ph4   #t1                      t1 := arctan(t1)
         fatanx
         lda   sign                     if t2 was less than zero then
         beq   lb5
         ph4   #pi
         ph4   #t1
         lda   t1+8                       if t1 < 0 then
         bpl   lb3
         faddx                              t1 := t1+pi
         bra   lb4
lb3      fsubx                            else t1 := t1-pi
lb4      lda   t1+8                       t1 := -t1
         eor   #$8000
         sta   t1+8

lb5      ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  ceil - round up
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
ceil     start
         using MathCommon

         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         pla
         sta   t1+2
         pla
         sta   t1+4
         pla
         sta   t1+6
         pla
         sta   t1+8
         phy
         phx
         fgetenv                        set the rounding direction to round up
         phx
         txa
         and   #$3FFF
         ora   #$4000
         pha
         fsetenv
         ph4   #t1
         frintx                         round the number
         fsetenv                        restore the environment
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  cos - return cos(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
cos      start

         lda   #$1D48
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  cosh - return hyperbolic cosine(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
cosh     start

         lda   #$1948
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  exp - return exp(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
exp      start

         lda   #$1048
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  fabs - absolute value of an extended number
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
fabs     start
         using MathCommon

         phb
         plx
         ply
         phk
         plb
         pla
         sta   t1
         pla
         sta   t1+2
         pla
         sta   t1+4
         pla
         sta   t1+6
         pla
         and   #$7FFF
         sta   t1+8
         phy
         phx
         plb
         ldx   #^t1
         lda   #t1
         rtl
         end

****************************************************************
*
*  floor - round down
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
floor    start
         using MathCommon

         phb                            place the number in a work area
         plx
         ply
         phk
         plb
         pla
         sta   t1
         pla
         sta   t1+2
         pla
         sta   t1+4
         pla
         sta   t1+6
         pla
         sta   t1+8
         phy
         phx
         plb
         fgetenv                        set the rounding direction to round down
         phx
         txa
         and   #$3FFF
         ora   #$8000
         pha
         fsetenv
         ph4   #t1
         frintx                         round the number
         fsetenv                        restore the environment
         ldx   #^t1                     return a pointer to the result
         lda   #t1
         rtl
         end

****************************************************************
*
*  fmod - return the floating point remainder
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
fmod     start
         using MathCommon

         phb                            place the numbers in the work areas
         plx
         ply
         phk
         plb
         pla
         sta   t1
         pla
         sta   t1+2
         pla
         sta   t1+4
         pla
         sta   t1+6
         pla
         sta   t1+8
         pla
         sta   t2
         pla
         sta   t2+2
         pla
         sta   t2+4
         pla
         sta   t2+6
         pla
         sta   t2+8
         phy
         phx

         lda   t2+8                     if t2 = 0 then
         and   #$7FFF
         ora   t2+6
         ora   t2+4
         ora   t2+2
         ora   t2
         jeq   lb1                        return t1

         fgetenv                        set the rounding direction to round down
         phx
         txa
         and   #$3FFF
         ora   #$8000
         pha
         fsetenv
         lda   t1+8                     get and save the sign of t1, setting t1
         and   #$8000                     to abs(t1)
         sta   sign
         asl   t1+8
         lsr   t1+8
         move  t1,t3,#10                t3 := t1/t2
         ph4   #t2
         ph4   #t3
         fdivx
         ph4   #t3                      t3 := round(t3)
         frintx
         ph4   #t2                      t3 = t3*t2
         ph4   #t3
         fmulx
         ph4   #t3                      t1 = t1-t3
         ph4   #t1
         fsubx
         lda   t1+8                     restore the sign of t1
         ora   sign
         sta   t1+8
         fsetenv                        restore the environment
lb1      ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  frexp - split a number into a fracrion and exponent
*
*  Inputs:
*        x - number
*        nptr - pointer to location to save integer
*
*  Outputs:
*        returns the address of the fraction
*
****************************************************************
*
frexp    start
         using MathCommon

         csubroutine (10:x,4:nptr),0

         phb
         phk
         plb

         lda   x+8                      get the exponent
         and   #$7FFF
         bne   lb1                      handle zero
         sta   [nptr]
         bra   lb2
lb1      sec
         sbc   #$3FFE
         sta   [nptr]
         lda   x+8                      set the fraction range
         and   #$8000
         ora   #$3FFE
         sta   x+8
lb2      ldx   #8                       set up to return the result
lb3      lda   x,X
         sta   t1,X
         dex
         dex
         bpl   lb3
         lla   nptr,t1

         plb
         creturn 4:nptr
         end

****************************************************************
*
*  ldexp - raise a number to an integer power of 2
*
*  Inputs:
*        x - number
*        n - integer power of 2
*
*  Outputs:
*        returns the address of the result
*
****************************************************************
*
ldexp    start
         using MathCommon

         csubroutine (10:x,2:n),0

         phb
         phk
         plb

         lda   x+8                      separate the sign from the exponent
         and   #$8000
         sta   sign
         clc                            add the value to the exponent
         lda   x+8
         and   #$7FFF
         adc   n
         bvs   err
         cmp   #$7FFF                   check for a range error
         blt   lb1
err      lda   #ERANGE
         sta   >errno
         bra   lb2
lb1      ora   sign                     replace the sign
         sta   x+8                      save the exponent

lb2      ldx   #8                       set up to return the result
lb3      lda   x,X
         sta   t1,X
         dex
         dex
         bpl   lb3
         lla   x,t1

         plb
         creturn 4:x
         end

****************************************************************
*
*  log - return ln(x) (base e)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
log      start

         lda   #$1448
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  log10 - return log(x) (base 10)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
log10    start

         lda   #$1548
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  modf - split a number into whole and fraction parts
*
*  Inputs:
*        x - number
*        nptr - ptr to double to store integer part
*
*  Outputs:
*        returns the address of the result
*
****************************************************************
*
modf     start
         using MathCommon

         csubroutine (10:x,4:nptr),0

         phb
         phk
         plb

         fgetenv                        set the rounding direction to round
         phx                             towards 0
         txa
         ora   #$C000
         pha
         fsetenv
         ldx   #8                       t1 := x
lb1      lda   x,X                      t2 := x
         sta   t1,X
         sta   t2,X
         dex
         dex
         bpl   lb1
         ph4   #t2                      t2 := round(t2)
         frintx
         fsetenv                        restore the environment
         ph4   #t2                      t1 := t1-t2
         ph4   #t1
         fsubx
         ph4   #t2                      convert t2 to a double
         ph4   #t3
         fx2d
         fgetenv                        check for a range error
         txa
         and   #$0D00
         beq   lb2
         lda   #ERANGE
         sta   >errno
lb2      ldy   #6
lb3      lda   t3,Y                     return the integer part
         sta   [nptr],Y
         dey
         dey
         bpl   lb3
         lla   nptr,t1                  set up to return the result

         plb
         creturn 4:nptr
         end

****************************************************************
*
*  pow - return x ** y
*
*  Inputs:
*        stack - extended numbers
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
pow      start
         using MathCommon

         phb                            place the numbers in the work areas
         plx
         ply
         phk
         plb
         pla
         sta   t1
         pla
         sta   t1+2
         pla
         sta   t1+4
         pla
         sta   t1+6
         pla
         sta   t1+8
         pla
         sta   t2
         pla
         sta   t2+2
         pla
         sta   t2+4
         pla
         sta   t2+6
         pla
         sta   t2+8
         phy
         phx

         ph4   #t2                      t1 := t1 ** t2
         ph4   #t1
         fxpwry
         fgetenv                        check for a range error
         txa
         and   #$0D00
         beq   lb1
         lda   #ERANGE
         sta   >errno
lb1      ldx   #^t1                     return a pointer to the result
         lda   #t1
         plb
         rtl
         end

****************************************************************
*
*  sin - return sin(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
sin      start

         lda   #$0E48
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  sinh - return hyperbolic sin(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
sinh     start

         lda   #$0248
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  sqrt - return sqrt(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
sqrt     start

         lda   #$0448
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  tan - return tan(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
tan      start

         lda   #$0F48
         brl   ~DOFPEFUNCTION
         end

****************************************************************
*
*  tanh - return hyperbolic tan(x)
*
*  Inputs:
*        stack - extended number
*
*  Outputs:
*        X-A - addr of the result
*
****************************************************************
*
tanh     start

         lda   #$0948
         brl   ~DOFPEFUNCTION
         end
