         keep  obj/fp
         mcopy fp.macros
****************************************************************
*
*  FPE Floating Point Libraries
*
*  These libraries are common routines used by the 65816 ORCA
*  native code compilers for accessing the Floating Point Engine.
*
*  Copyright 1987, 1990, 1993
*  Byte Works, Inc.
*  All rights reserved.
*
*  By Mike Westerfield
*
*  Notes: Some subroutines in this library do not contain FPE
*  calls, but they are called by other subroutines that do.
*  Many are also called from ORCALib or PasLib, so do not change
*  the call protocall or global variables.
*
*  SANE is still used in some places in this library.
*
****************************************************************
*
Dummy    start
         end

****************************************************************
*
*  ~FPECommon - Common data area for FPE routines
*
****************************************************************
*
~FPECommon privdata

slot     ds    2                        FPE slot number
         end

****************************************************************
*
*  ~DoFPEBinop - do a binary operation
*
*  Inputs:
*        op1,op2: numbers
*        A - FPE operation code
*
****************************************************************
*
~DoFPEBinop private
         using ~FPECommon
op1      equ   5                        first operand
op2      equ   15                       second operand/result

command  equ   $C088                    disp to the FPE command register
operand  equ   $C08C                    disp to the FPE operand register
response equ   $C080                    disp to the FPE response register

         phb                            set the data bank to bank 0
         ph2   #0
         plb
         plb
         tax                            save the operation

         lda   >FPEslot                 get the slot displacelement
         bne   fp1
         phb
         phk
         plb
         lda   slot
         asl   A
         asl   A
         asl   A
         asl   A
         sta   FPEslot
         ora   #command
         sta   c1+1
         sta   c2+1
         sta   c3+1
         lda   FPEslot
         ora   #response
         sta   lb1+1
         sta   lb2+1
         sta   lb3+1
         sta   lb4+1
         sta   lb5+1
         sta   lb6+1
         lda   FPEslot
         ora   #operand
         sta   o1+1
         sta   o2+1
         sta   o3+1
         sta   o4+1
         sta   o5+1
         sta   o6+1
         sta   o7+1
         sta   o8+1
         sta   o9+1
         lda   FPEslot
         ora   #operand+2
         sta   p1+1
         sta   p2+1
         sta   p3+1
         sta   p4+1
         sta   p5+1
         sta   p6+1
         sta   p7+1
         sta   p8+1
         sta   p9+1
         plb      
fp1      anop     

         lda   #$0048                   load the first operand
c1       sta   command
         lda   #$0089
lb1      cmp   response
         beq   lb1
         lda   op2+8,S
         xba
o1       sta   operand
         lda   #0
p1       sta   operand+2
         lda   op2+6,S
         xba
o2       sta   operand
         lda   op2+4,S
         xba
p2       sta   operand+2
         lda   op2+2,S
         xba
o3       sta   operand
         lda   op2,S
         xba
p3       sta   operand+2
lb2      lda   response
         and   #$0080
         bne   lb2

         txa                            perform the operation with the 2nd operand
c2       sta   command
         lda   #$0089
lb3      cmp   response
         beq   lb3
         lda   op1+8,S
         xba
o4       sta   operand
         lda   #0
p4       sta   operand+2
         lda   op1+6,S
         xba
o5       sta   operand
         lda   op1+4,S
         xba
p5       sta   operand+2
         lda   op1+2,S
         xba
o6       sta   operand
         lda   op1,S
         xba
p6       sta   operand+2
lb4      lda   response
         and   #$0080
         bne   lb4

         lda   #$0068
c3       sta   command
         lda   #$0089
lb5      cmp   response
         beq   lb5
o7       lda   operand
         xba
         sta   op2+8,S
p7       lda   operand
o8       lda   operand
         xba
         sta   op2+6,S
p8       lda   operand
         xba
         sta   op2+4,S
o9       lda   operand
         xba
         sta   op2+2,S
p9       lda   operand
         xba
         sta   op2,S
lb6      lda   response
         and   #$0080
         bne   lb6

         plb
         lda   1,S
         sta   11,S
         lda   2,S
         sta   12,S
         tsc
         clc
         adc   #10
         tcs
         rtl
;
;  Local data
;
FPEslot  ds    2                        FPE slot displacement
         end

****************************************************************
*
*  ~DoFPEFunction - do an FPE function
*
*  Inputs:
*        A - FPE operation code
*
****************************************************************
*
~DoFPEFunction start
         using ~FPECommon

command  equ   $C088                    disp to the FPE command register
operand  equ   $C08C                    disp to the FPE operand register
response equ   $C080                    disp to the FPE response register

save     equ   $0068                    op code for save of FPE0 to extended

val      equ   5                        start of value on stack

         phb                            set up for abs addressing to bank 0
         pea   0
         plb
         plb
         pha                            save the operation
         lda   >FPEslot                 get the slot displacelement
         bne   lb1
         phb
         phk
         plb
         lda   slot
         asl   A
         asl   A
         asl   A
         asl   A
         sta   FPEslot
         ora   #command
         sta   c1+1
         sta   c2+1
         lda   FPEslot
         ora   #response
         sta   lb2+1
         sta   lb3+1
         sta   lb4+1
         sta   lb5+1
         lda   FPEslot
         ora   #operand
         sta   o1+1
         sta   o2+1
         sta   o3+1
         sta   o4+1
         sta   o5+1
         sta   o6+1
         lda   FPEslot
         ora   #operand+2
         sta   p1+1
         sta   p2+1
         sta   p3+1
         sta   p4+1
         sta   p5+1
         sta   p6+1
         plb
lb1      pla                            do the operation
c1       sta   command
         ldx   #$0089
lb2      cpx   response
         beq   lb2
         lda   val+8,S
         xba
o1       sta   operand
p1       stz   operand+2
         lda   val+6,S
         xba
o2       sta   operand
         lda   val+4,S
         xba
p2       sta   operand+2
         lda   val+2,S
         xba
o3       sta   operand
         lda   val,S
         xba
p3       sta   operand+2
lb3      lda   response
         and   #$0080
         bne   lb3
         lda   #save
c2       sta   command                  save the result
lb4      cpx   response
         beq   lb4
o4       lda   operand
         xba
         sta   >FPEresult+8
p4       lda   operand+2
o5       lda   operand
         xba
         sta   >FPEresult+6
p5       lda   operand+2
         xba
         sta   >FPEresult+4
o6       lda   operand
         xba
         sta   >FPEresult+2
p6       lda   operand+2
         xba
         sta   >FPEresult
lb5      lda   response
         and   #$0080
         bne   lb5
         plx                            remove the operand from the stack
         ply
         tsc
         clc
         adc   #10
         tcs
         phy
         phx
         plb                            restore B
         ldx   #^FPEresult              return the addr of the result
         lda   #FPEresult
         rtl
;
;  Local data
;
FPEresult ds   10                       result of an FPE function
FPEslot  ds    2                        FPE slot displacement
         end

****************************************************************
*
*  ~GetFPEControlReg - get the value of an FPE control register
*
*  Inputs:
*        A - FPE operation code (FMOVE control register)
*
*  Outputs:
*        A-X - result value
*
****************************************************************
*
~GetFPEControlReg start
         using ~FPECommon

command  equ   $C088                    disp to the FPE command register
operand  equ   $C08C                    disp to the FPE operand register
response equ   $C080                    disp to the FPE response register

save     equ   $0068                    op code for save of FPE0 to extended

val      equ   5                        start of value on stack

         phb                            set up for abs addressing to bank 0
         pea   0
         plb
         plb
         pha                            save the operation
         lda   >FPEslot                 get the slot displacelement
         bne   lb1
         phb
         phk
         plb
         lda   slot
         asl   A
         asl   A
         asl   A
         asl   A
         sta   FPEslot
         ora   #command
         sta   c1+1
         lda   FPEslot
         ora   #response
         sta   lb2+1
         sta   lb3+1
         lda   FPEslot
         ora   #operand
         sta   o1+1
         lda   FPEslot
         ora   #operand+2
         sta   p1+1
         plb

lb1      pla                            issue the command
c1       sta   command
         ldx   #$0089
lb2      cpx   response
         beq   lb2
o1       lda   operand                  get the result
         xba
         tax
p1       lda   operand+2
         xba
         tay
lb3      lda   response
         and   #$0080
         bne   lb3
         plb                            restore B
         tya                            return the result
         rtl
;
;  Local data
;
FPEslot  ds    2                        FPE slot displacement
         end

****************************************************************
*
*  ~SetFPEControlReg - get the value of an FPE control register
*
*  Inputs:
*        A   - FPE operation code (FMOVE control register)
*        X-Y - Value to set
*
****************************************************************
*
~SetFPEControlReg start
         using ~FPECommon

command  equ   $C088                    disp to the FPE command register
operand  equ   $C08C                    disp to the FPE operand register
response equ   $C080                    disp to the FPE response register

save     equ   $0068                    op code for save of FPE0 to extended

val      equ   5                        start of value on stack

         phb                            set up for abs addressing to bank 0
         pea   0
         plb
         plb
         phx                            save the value
         phy
         pha                            save the operation
         lda   >FPEslot                 get the slot displacelement
         bne   lb1
         phb
         phk
         plb
         lda   slot
         asl   A
         asl   A
         asl   A
         asl   A
         sta   FPEslot
         ora   #command
         sta   c1+1
         lda   FPEslot
         ora   #response
         sta   lb2+1
         sta   lb3+1
         lda   FPEslot
         ora   #operand
         sta   o1+1
         lda   FPEslot
         ora   #operand+2
         sta   p1+1
         plb

lb1      pla                            issue the command
c1       sta   command
         ldx   #$0089
lb2      cpx   response
         beq   lb2
         pla                            write the value
         xba
o1       sta   operand
         pla
         xba
p1       sta   operand+2
lb3      lda   response
         and   #$0080
         bne   lb3
         plb                            restore B
         rtl
;
;  Local data
;
FPEslot  ds    2                        FPE slot displacement
         end

****************************************************************
*
*  SetFPESlot - Set the FPE slot number
*
*  Inputs:
*        slot - slot number
*
****************************************************************
*
SetFPESlot start
         using ~FPECommon

         phb
         phk
         plb
         plx
         ply
         pla
         sta   slot
         phy
         phx

         ldx   #0                       zero the FPCR
         txy
         lda   #$0090
         jsl   ~SETFPECONTROLREG
         
         ldx   #0                       zero the FPSR
         txy
         lda   #$0088
         jsl   ~SETFPECONTROLREG

         plb
         rtl
         end

****************************************************************
*
*  ~AddE - Add two SANE extended numbers
*
*  Inputs:
*        op1,op2: numbers
*
****************************************************************
*
~AddE    start

         lda   #$2248
         brl   ~DoFPEBinop
         end

****************************************************************
*
*  ~ArcCosE - ArcCos function
*
*  Inputs:
*        1,S - return address
*        4,S - number
*
****************************************************************
*
~ArcCosE start
         longa on
         longi on
         using ~RealCom

         lda   12,S                     t1 := arcsin(x)
         pha
         lda   12,S
         pha
         lda   12,S
         pha
         lda   12,S
         pha
         lda   12,S
         pha
         jsl   ~ArcSinE
         pla
         sta   >t1
         pla
         sta   >t1+2
         pla
         sta   >t1+4
         pla
         sta   >t1+6
         pla
         sta   >t1+8
         lda   >piover2                 x := pi/2
         sta   4,S
         lda   >piover2+2
         sta   6,S
         lda   >piover2+4
         sta   8,S
         lda   >piover2+6
         sta   10,S
         lda   >piover2+8
         sta   12,S
         ph4   #t1                      x := x-t1
         pea   0
         clc
         tsc
         adc   #10
         pha
         fsubx
         rtl
         end

****************************************************************
*
*  ~ArcSinE - ArcSin function
*
*  Inputs:
*        1,S - return address
*        4,S - number
*
****************************************************************
*
~ArcSinE start
         longa on
         longi on
         using ~RealCom
x        equ   6                        location of number

         phd                            set up local DP
         tsc
         tcd
         phb                            set up local data addressing
         phk
         plb

         ldx   #8                       t1 := one
lb1      lda   one,X                    t2 := x
         sta   t1,X
         lda   x,X
         sta   t2,X
         dex
         dex
         bpl   lb1
         pea   0                        push addresses onto the stack
         clc                             push(x)
         tdc
         adc   #x
         pha
         ldy   #^t1                      push(t1)
         ldx   #t1
         phy
         phx
         pea   0                         push(x)
         pha
         phy                             push(t1);
         phx
         lda   #t2                       push(t2)
         phy
         pha
         phy                             push(t1)
         phx
         phy                             push(t2)
         pha
         phy                             push(t2)
         pha
         fmulx                          t2 := t2*t2
         fsubx                          t1 := t1-t2
         fsqrtx                         t1 := sqrt(t1)
         fdivx                          x := x/t1
         fatanx                         x := arctan(x)

         plb                            reset data bank
         pld                            restore DP
         rtl
         end

****************************************************************
*
*  ~ArcTan2E - Take the two argument arc tangent of two SANE extended numbers
*
*  Inputs:
*        n1,n2: numbers
*
****************************************************************
*
~ArcTan2E start
         longa on
         longi on
         using ~RealCom

         lda   12,S                     y := arctan(y/x);
         pha                            (save signs)
         lda   24,S
         pha
         clc
         tsc
         adc   #8
         tay
         adc   #10
         pea   0
         pha
         pea   0
         phy
         pea   0
         pha
         fdivx
         fatanx
         ply                            if x < 0 then
         plx                              if y >= 0 then
         bpl   lb2                          y := y+pi
         pea   pi|-16                     else
         pea   pi                           y := y-pi
         pea   0
         clc
         tsc
         adc   #20
         pha
         tya
         bmi   lb1
         faddx
         bra   lb2
lb1      fsubx
lb2      lda   0,S                      remove the left over from the stack
         sta   10,S
         lda   2,S
         sta   12,S
         tsc
         clc
         adc   #10
         tcs
         rtl
         end

****************************************************************
*
*  ~AtnE - ArcTan function
*
*  Inputs:
*        1,S - return address
*        4,S - number
*
****************************************************************
*
~AtnE    start
         longa on
         longi on

         tsc                            push the addresse twice
         pea   0
         clc
         adc   #4
         pha
         fatanx                         compute function
         rtl
         end

****************************************************************
*
*  ~CCompRet - Save a comp function return value in ~RealVal
*
*  Inputs:
*        rval - comp value to save
*
*  Outputs:
*        X-Y - address of ~RealVal
*
****************************************************************
*
~CCompRet start
lcAfterMarkStack equ 1                  location of function return value

         pea   0                        push address of double value
         tdc
         clc
         adc   #lcAfterMarkStack
         pha
         ph4   #~RealVal                push address of extended value
         fc2x                           convert to extended
         ldx   #^~RealVal               load address of result
         ldy   #~RealVal
         rtl                            return
         end

****************************************************************
*
*  ~CDoubleRet - Save a double function return value in ~RealVal
*
*  Inputs:
*        rval - double value to save
*
*  Outputs:
*        X-Y - address of ~RealVal
*
****************************************************************
*
~CDoubleRet start
lcAfterMarkStack equ 1                  location of function return value

         pea   0                        push address of double value
         tdc
         clc
         adc   #lcAfterMarkStack
         pha
         ph4   #~RealVal                push address of extended value
         fd2x                           convert to extended
         ldx   #^~RealVal               load address of result
         ldy   #~RealVal
         rtl                            return
         end

****************************************************************
*
*  ~CExtendedRet - Save an extended function return value in ~RealVal
*
*  Inputs:
*        rval - extended value to save
*
*  Outputs:
*        X-Y - address of ~RealVal
*
****************************************************************
*
~CExtendedRet start
lcAfterMarkStack equ 1                  location of function return value

         ldx   #8                       move the value
lb1      lda   lcAfterMarkStack,X
         sta   >~RealVal,X
         dex
         dex
         bpl   lb1
         ldx   #^~RealVal               load address of result
         ldy   #~RealVal
         rtl                            return
         end

****************************************************************
*
*  ~CnvES - convert extended sane to string
*
*  Inputs:
*        1,S - return address
*        4,S - fixed precision digit count
*        6,S - field width
*        8,S - extended format real number
*
*  Outputs:
*        stack - length of result string
*        stack-1 - pointer to result string
*
****************************************************************
*
~CnvES   start
         using ~RealCom
         longa on
         longi on
sLen     equ   0
sAddr    equ   2                        string pointer
return   equ   0
decDig   equ   3
fw       equ   5
ext      equ   7
rLen     equ   11                       return string length
rAddr    equ   13                       return string address

         tdc                            set up stack and direct page
         tax
         tsc
         sec
         sbc   #$FFFF
         tcd
         dec   a
         tcs
         phx
         phb                            set data bank reg
         phk
         plb

         ldx   #12                      format the number
cv1      lda   decDig,X
         pha
         dex
         dex
         bpl   cv1
         jsl   ~FormatReal

         lda   pasString
         and   #$00FF
         sta   rLen
         beq   rts
         ldx   #0
         jsr   ~GetSBuffer
         sta   rAddr
         stx   rAddr+2
         bcs   err

         ldy   rLen
         dey
         short M
cv2      lda   pasString+1,y
         sta   [rAddr],y
         dey
         bpl   cv2
         long  M

rts      plb
         lda   return+1
         sta   rLen-2
         lda   return
         sta   rLen-3
         clc
         tdc
         adc   #rLen-4
         pld
         tcs
         rtl

err      error #5                       out of memory
         stz   sLen                     return 0
         bra   rts
         end

****************************************************************
*
*  ~CnvIntReal - convert an integer into an extended SANE real
*
*  Inputs:
*        A - integer
*
*  Outputs:
*        extended real on stack
*
****************************************************************
*
~CnvIntReal start
         longa on
         longi on

         phb                            recover return addr
         plx
         ply
         pha                            make room for extended value
         pha
         pha
         pha
         pha
         phy                            fix return address
         phx
         plb
         pha                            save integer on stack
         tsc                            push address of integer
         ina
         pea   0
         pha
         pea   0                        push address of extended real
         clc
         adc   #5
         pha
         fi2x                           convert
         pla                            remove integer
         rtl                            return
         end

****************************************************************
*
*  ~CnvLE - convert a longint into an extended SANE real
*
*  Inputs:
*        longint on stack
*
*  Outputs:
*        extended real on stack
*
****************************************************************
*
~CnvLE   start
         using ~RealCom
         longa on
         longi on

         phb                            recover return addr & set up bank addr
         phk
         plb
         pl4   t1+4
         pl4   t1                       fetch number to convert
         ph4   #t1                      push addr of integer
         ph4   #t2                      push addr to place result
         fl2x                           convert
         lda   t2+8                     push real result
         pha
         lda   t2+6
         pha
         lda   t2+4
         pha
         lda   t2+2
         pha
         lda   t2
         pha
         ph4   t1+4                     fix stack
         plb
         rtl                            return
         end

****************************************************************
*
*  ~CnvLongReal - convert a long integer into an extended SANE real
*
*  Inputs:
*        A - integer
*
*  Outputs:
*        extended real on stack
*
****************************************************************
*
~CnvLongReal start

         phb                            save the long value
         phk
         plb
         sta   lval
         stx   lval+2
         plx                            recover return addr
         ply
         pha                            make room for extended value
         pha
         pha
         pha
         pha
         phy                            fix return address
         phx
         plb
         ph4   #lval                    push address of integer
         pea   0                        push address of extended real
         clc
         tsc
         adc   #10
         pha
         fl2x                           convert
         rtl                            return

lval     ds    4                        long value
         end

****************************************************************
*
*  ~CnvRealInt - trunc and convert a real to an integer
*
*  Inputs:
*        extended real on stack
*
*  Outputs:
*        A - integer
*
****************************************************************
*
~CnvRealInt start
         longa on
         longi on

         tsc                            push address of real 2 times
         clc
         adc   #4
         pea   0
         pha
         pea   0
         pha
         pea   0                        push address of integer
         pha
         ftintx                         round
         fx2i                           convert
         phb                            move return address
         pla
         sta   9,S
         pla
         sta   9,S
         pla                            remove integer
         plx                            fix stack
         plx
         plb
         tax                            set condition codes
         rtl                            return
         end

****************************************************************
*
*  ~CnvRealLong - trunc and convert a real to a long
*
*  Inputs:
*        extended real on stack
*
*  Outputs:
*        X-A - integer
*
****************************************************************
*
~CnvRealLong start

         tsc                            push address of real 2 times
         clc
         adc   #4
         pea   0
         pha
         pea   0
         pha
         pea   0                        push address of integer
         pha
         ftintx                         round
         fx2l                           convert
         phb                            move return address
         pla
         sta   9,S
         pla
         sta   9,S
         pla                            remove integer
         plx
         ply                            fix stack
         plb
         tay                            set condition codes
         bne   lb1
         txy
lb1      rtl                            return
         end

****************************************************************
*
*  ~CnvRealUInt - trunc and convert a real to an integer
*
*  Inputs:
*        extended real on stack
*
*  Outputs:
*        A - integer
*
****************************************************************
*
~CnvRealUInt start
         longa on
         longi on

         tsc                            push address of real 2 times
         clc
         adc   #4
         pea   0
         pha
         pea   0
         pha
         pea   0                        push address of integer
         pha
         ftintx                         round
         fx2l                           convert
         phb                            move return address
         pla
         sta   9,S
         pla
         sta   9,S
         pla                            remove integer
         plx                            fix stack
         plx
         plb
         tax                            set condition codes
         rtl                            return
         end

****************************************************************
*
*  ~CnvSE - convert string to extended sane
*
*  Inputs:
*        1,S - return address
*        4,S - string len
*        6,S - string
*
*  Outputs:
*        stack - length of result string
*        stack-1 - pointer to result string
*
****************************************************************
*
~CnvSE   start
         longa on
         longi on
         using ~FileCom
         using ~RealCom
dpage    equ   1                        caller's direct page
return   equ   3                        return address
ext      equ   6
slen     equ   10                       string length
sAddr    equ   12                       string address

         lda   3,S                      move and save return address
         pha
         lda   3,S
         pha
         phd                            save direct page
         tsc                            set up direct page
         tcd

         phb                            save data bank
         phk                            set up local data bank
         plb
         ph4   <sAddr                   convert string to standard form
         ph2   <sLen
         jsr   ~StringToStandard
         pl2   sLen
         pl4   sAddr
         lda   sLen
         tax
         short M                        move over the string
         sta   pasString
         lda   #0
         sta   pasString+1,X
         txy
         beq   cv3
         bra   cv2
cv1      lda   [sAddr],Y
         sta   pasString+1,Y
cv2      dey
         bpl   cv1
;
;  Convert the string into a number
;
cv3      long  M
         ldy   #0                       skip leading blanks
         lda   #' '
         short M
cv4      cmp   pasString+1,Y
         bne   cv5
         iny
         bra   cv4
cv5      long  M
         sty   index
         ph4   #pasString+1             lvp^.rval := cnvsr(digit);
         ph4   #index                   {convert from ascii to decform}
         ph4   #decrec
         ph4   #valid
         stz   index
         fcstr2dec
         lda   valid                    {flag an error if SANE said to}
         beq   err1
         lda   pasString
         and   #$00FF
         cmp   index
         bne   err1
         ph4   #decrec                  {convert decform to real}
         pea   0
         clc
         tdc
         adc   #ext
         pha
         fdec2x
         bcc   rts
err1     error #10                      real math error
rts      plb
         pld
         rtl
         end

****************************************************************
*
*  ~CnvULongReal - convert an unsigned long integer into an
*        extended SANE real
*
*  Inputs:
*        A - integer
*
*  Outputs:
*        extended real on stack
*
****************************************************************
*
~CnvULongReal start

         phb                            save the long value
         phk
         plb
         sta   lval
         stx   lval+2
         plx                            recover return addr
         ply
         pha                            make room for extended value
         pha
         pha
         pha
         pha
         phy                            fix return address
         phx
         plb
         ph4   #lval                    push address of integer
         pea   0                        push address of extended real
         clc
         tsc
         adc   #10
         pha
         fc2x                           convert
         rtl                            return

lval     ds    4                        long value
         dc    i4'0'                    least significant bits of comp value
         end

****************************************************************
*
*  ~CompFix - Convert a parameter from extended to comp
*
*  Inputs:
*        rval - disp in stack frame of real value
*
****************************************************************
*
~CompFix start
         longa on
         longi on
rval     equ   4

         tdc                            push address of real value
         clc
         adc   rval,S
         pea   0
         pha
         pea   0
         pha
         fx2c                           convert from extended
         lda   2,S                      fix return addr
         sta   4,S
         pla
         sta   1,S
         rtl
         end

****************************************************************
*
*  ~CompRet2 - Save a comp function return value in ~RealVal
*
*  Inputs:
*        rval - real value to save
*
*  Outputs:
*        X-Y - address of ~RealVal
*
*  Notes: Pascal 2.0 version
*
****************************************************************
*
~CompRet2 start

         lda   1,S                      swap address of value
         tax                             and return address
         lda   2,S
         tay
         lda   4,S
         sta   1,S
         lda   6,S
         sta   3,S
         tya
         sta   6,S
         txa
         sta   5,S
         ph4   #~RealVal                push address of extended value
         fc2x                           convert to extended
         ldx   #^~RealVal               load address of result
         ldy   #~RealVal
         rtl                            return
         end

****************************************************************
*
*  ~CopyComp - Save a comp value
*
*  Inputs:
*        1,S - return address
*        4,S - address to save to
*        8,S - extended real value
*
****************************************************************
*
~CopyComp start

         csubroutine (4:addr),0
ext      equ   addr+4

         ph2   #0                       push addr of extended #
         clc
         tdc
         adc   #ext
         pha
         ph4   <addr                    push addr of real value
         fx2c                           convert and move
         creturn
         end

****************************************************************
*
*  ~CopyDouble - Copy a double value
*
*  Inputs:
*        1,S - return address
*        4,S - address to save to
*        8,S - extended real value
*
****************************************************************
*
~CopyDouble start
         using ~FPECommon

         csubroutine (4:addr),0
ext      equ   addr+4

command  equ   $C088                    disp to the FPE command register
operand  equ   $C08C                    disp to the FPE operand register
response equ   $C080                    disp to the FPE response register

         phb                            set up for abs addressing to bank 0
         pea   0
         plb
         plb
         lda   >FPEslot                 get the slot displacelement
         bne   lb1
         phb
         phk
         plb
         lda   slot
         asl   A
         asl   A
         asl   A
         asl   A
         sta   FPEslot
         ora   #command
         sta   c1+1
         sta   c2+1
         lda   FPEslot
         ora   #response
         sta   lb2+1
         sta   lb3+1
         sta   lb4+1
         sta   lb5+1
         lda   FPEslot
         ora   #operand
         sta   o1+1
         sta   o2+1
         sta   o3+1
         sta   o4+1
         sta   o5+1
         lda   FPEslot
         ora   #operand+2
         sta   p1+1
         sta   p2+1
         sta   p3+1
         sta   p4+1
         sta   p5+1
         plb
lb1      anop

         lda   #$0048                   load the extended value
c1       sta   command
         ldx   #$0089
lb2      cpx   response
         beq   lb2
         lda   ext+8
         xba
o1       sta   operand
p1       stz   operand+2
         lda   ext+6
         xba
o2       sta   operand
         lda   ext+4
         xba
p2       sta   operand+2
         lda   ext+2
         xba
o3       sta   operand
         lda   ext
         xba
p3       sta   operand+2
lb3      lda   response
         and   #$0080
         bne   lb3

         lda   #$0074                   save the double value
c2       sta   command
         lda   #$0089
lb4      cmp   response
         beq   lb4
o4       lda   operand
         xba
         ldy   #6
         sta   [addr],Y
p4       lda   operand+2
         xba
         ldy   #4
         sta   [addr],Y
o5       lda   operand
         xba
         ldy   #2
         sta   [addr],Y
p5       lda   operand+2
         xba
         sta   [addr]
lb5      lda   response
         and   #$0080
         bne   lb5
         plb                            restore B

         creturn
;
;  Local data
;
FPEslot  ds    2                        FPE slot displacement
         end

****************************************************************
*
*  ~CopyExtended - Save an extended value
*
*  Inputs:
*        1,S - return address
*        4,S - address to save to
*        8,S - extended real value
*
****************************************************************
*
~CopyExtended start

         csubroutine (4:addr),0
ext      equ   addr+4

         ldx   #8
lb1      txy
         lda   ext,X
         sta   [addr],Y
         dex
         dex
         bpl   lb1
         creturn
         end

****************************************************************
*
*  ~CopyReal - Save a real value
*
*  Inputs:
*        1,S - return address
*        4,S - address to save to
*        8,S - extended real value
*
****************************************************************
*
~CopyReal start
         using ~FPECommon

         csubroutine (4:addr),0
ext      equ   addr+4

command  equ   $C088                    disp to the FPE command register
operand  equ   $C08C                    disp to the FPE operand register
response equ   $C080                    disp to the FPE response register

         phb                            set up for abs addressing to bank 0
         pea   0
         plb
         plb
         lda   >FPEslot                 get the slot displacelement
         bne   lb1
         phb
         phk
         plb
         lda   slot
         asl   A
         asl   A
         asl   A
         asl   A
         sta   FPEslot
         ora   #command
         sta   c1+1
         sta   c2+1
         lda   FPEslot
         ora   #response
         sta   lb2+1
         sta   lb3+1
         sta   lb4+1
         sta   lb5+1
         lda   FPEslot
         ora   #operand
         sta   o1+1
         sta   o2+1
         sta   o3+1
         sta   o4+1
         lda   FPEslot
         ora   #operand+2
         sta   p1+1
         sta   p2+1
         sta   p3+1
         sta   p4+1
         plb
lb1      anop

         lda   #$0048                   load the extended value
c1       sta   command
         ldx   #$0089
lb2      cpx   response
         beq   lb2
         lda   ext+8
         xba
o1       sta   operand
p1       stz   operand+2
         lda   ext+6
         xba
o2       sta   operand
         lda   ext+4
         xba
p2       sta   operand+2
         lda   ext+2
         xba
o3       sta   operand
         lda   ext
         xba
p3       sta   operand+2
lb3      lda   response
         and   #$0080
         bne   lb3

         lda   #$0064                   save the real value
c2       sta   command
         lda   #$0089
lb4      cmp   response
         beq   lb4
o4       lda   operand
         xba
         ldy   #2
         sta   [addr],Y
p4       lda   operand+2
         xba
         sta   [addr]
lb5      lda   response
         and   #$0080
         bne   lb5
         plb                            restore B

         creturn
;
;  Local data
;
FPEslot  ds    2                        FPE slot displacement
         end

****************************************************************
*
*  ~CosE - Cosine function
*
*  Inputs:
*        1,S - return address
*        4,S - number
*
****************************************************************
*
~CosE    start
         longa on
         longi on

         tsc                            push the addresse twice
         pea   0
         clc
         adc   #4
         pha
         fcosx                          compute function
         rtl
         end

****************************************************************
*
*  ~CRealRet - Save a real function return value in ~RealVal
*
*  Inputs:
*        rval - real value to save
*
*  Outputs:
*        X-Y - address of ~RealVal
*
****************************************************************
*
~CRealRet start
lcAfterMarkStack equ 1                  size of default part of stack frame

         pea   0                        push address of real value
         tdc
         clc
         adc   #lcAfterMarkStack
         pha
         ph4   #~RealVal                push address of extended value
         fs2x                           convert to extended
         ldx   #^~RealVal               load address of result
         ldy   #~RealVal
         rtl                            return
         end

****************************************************************
*
*  ~DivE - Divide two SANE extended numbers
*
*  Inputs:
*        n1,n2: numbers
*
****************************************************************
*
~DivE    start

         lda   #$2048
         brl   ~DoFPEBinop
         end

****************************************************************
*
*  ~DoubleFix - Convert a parameter from extended to double
*
*  Inputs:
*        rval - disp in stack frame of double value
*
****************************************************************
*
~DoubleFix start
         longa on
         longi on
rval     equ   4

         tdc                            push address of double value
         clc
         adc   rval,S
         pea   0
         pha
         pea   0
         pha
         fx2d                           convert from extended
         lda   2,S                      fix return addr
         sta   4,S
         pla
         sta   1,S
         rtl
         end

****************************************************************
*
*  ~DoubleRet - Save a double function return value in ~RealVal
*
*  Inputs:
*        rval - double value to save
*
*  Outputs:
*        X-Y - address of ~RealVal
*
****************************************************************
*
~DoubleRet start
         longa on
         longi on
lcAfterMarkStack equ 22                 location of function return value

         pea   0                        push address of double value
         tdc
         clc
         adc   #lcAfterMarkStack
         pha
         ph4   #~RealVal                push address of extended value
         fd2x                           convert to extended
         ldx   #^~RealVal               load address of result
         ldy   #~RealVal
         rtl                            return
         end

****************************************************************
*
*  ~DoubleRet2 - Save a double function return value in ~RealVal
*
*  Inputs:
*        rval - double value to save
*
*  Outputs:
*        X-Y - address of ~RealVal
*
*  Notes: Pascal 2.0 version
*
****************************************************************
*
~DoubleRet2 start

         lda   1,S                      swap address of double value
         tax                             and return address
         lda   2,S
         tay
         lda   4,S
         sta   1,S
         lda   6,S
         sta   3,S
         tya
         sta   6,S
         txa
         sta   5,S
         ph4   #~RealVal                push address of extended value
         fd2x                           convert to extended
         ldx   #^~RealVal               load address of result
         ldy   #~RealVal
         rtl                            return
         end

****************************************************************
*
*  ~EquE - Test two SANE extended numbers for equality
*
*  Inputs:
*        n1,n2: numbers
*
*  Outputs:
*        A - 1 if equal, else 0
*        Z - 0 if equal, else 1
*
****************************************************************
*
~EquE    start
         longa on
         longi on

         tsc                            push the addresses
         pea   0
         clc
         adc   #4
         pha
         pea   0
         adc   #10
         pha
         fcpxx                          add the two numbers
         lda   0,S                      remove the left over from the stack
         sta   20,S
         lda   2,S
         sta   22,S
         tsc
         clc
         adc   #20
         tcs
         txa                            set the result
         lsr   A
         and   #1
         rtl
         end

****************************************************************
*
*  ~ExpE - Exponent function
*
*  Inputs:
*        1,S - return address
*        4,S - number
*
****************************************************************
*
~ExpE    start
         longa on
         longi on

         tsc                            push the addresse twice
         pea   0
         clc
         adc   #4
         pha
         fexpx                          compute function
         rtl
         end

****************************************************************
*
*  ~ExtendedRet2 - Save an extended function return value in ~RealVal
*
*  Inputs:
*        rval - real value to save
*
*  Outputs:
*        X-Y - address of ~RealVal
*
*  Notes: Pascal 2.0 version
*
****************************************************************
*
~ExtendedRet2 start

         lda   1,S                      swap address of value
         tax                             and return address
         lda   2,S
         tay
         lda   4,S
         sta   1,S
         lda   6,S
         sta   3,S
         tya
         sta   6,S
         txa
         sta   5,S
         tsc                            set up a stack frame
         phd
         tcd
         phb
         phk
         plb
         ldy   #8                       move the extended value
lb1      lda   [1],Y
         sta   ~RealVal,Y
         dey
         dey
         bpl   lb1
         plb                            get rid of the stack frame
         pld
         pla
         pla
         ldx   #^~RealVal               load address of result
         ldy   #~RealVal
         rtl                            return
         end

****************************************************************
*
*  ~FileCom - common area for files
*
****************************************************************
*
~FileCom data
;
;  Definition of a file structure
;
~flLen   equ   $0                       length of the buffer
~flRef   equ   $4                       ProDOS reference number
~flKind  equ   $6                       Open for input (1), output (2),
!                                         text input (5), text output (6)
~flEOLN  equ   $8                       end of line flag
~flEOF   equ   $A                       end of file flag
~flNameLen equ $C                       length of the file name
~flName  equ   $E                       pointer to name of file
~flHeader equ  $12                      length of the file header; buffer
;
;  File buffer variables
;
~fileBuff ds   4                        pointer to next file buffer
~fileRecBuff ds 4                       pointer to next file record buffer
         end

****************************************************************
*
*  ~FormatReal - convert an extended SANE number to a string
*
*  Inputs:
*        ext - extended format real number
*        fw - field width
*        decDig - fixed precision digit count
*
*  Outputs:
*        passtring - string with leading length byte
*
****************************************************************
*
~FormatReal start
         longa on
         longi on
         using ~RealCom

         sub   (2:decDig,2:fw,10:ext),0
         phb
         phk
         plb

         lda   decDig                   if exponential format then
         bne   lb3
         stz   style                      set style to exponential
         sec                              set # sig digits
         lda   fw
         bmi   lb1
         sbc   #7
         bmi   lb1
         cmp   #2
         bge   lb2
lb1      lda   #2
lb2      sta   digits
         bra   lb4                      else
lb3      sta   digits                     set # decimal digits
         lda   #1                         set style to fixed
         sta   style
lb4      anop                           endif

         ph4   #decForm                 convert to decimal record
         ph2   #0
         clc
         tdc
         adc   #ext
         pha
         ph4   #decRec
         fx2dec
         ph4   #decForm                 convert to string
         ph4   #decRec
         ph4   #pasString
         fdec2str
         lda   decDig                   if exponential format then
         bne   lb7
         short I,M
         ldx   pasString                  if format is e+0 then
         lda   pasString-2,X
         cmp   #'e'
         bne   lb5
         lda   pasString,X                  make it e+00
         sta   pasString+1,X
         lda   #'0'
         sta   pasString,X
         inc   pasString
lb5      anop                             endif
         ldx   pasString                  if format is e+00 then
         lda   pasString-3,X
         cmp   #'e'
         bne   lb6
         lda   pasString,X                  make it e+000
         sta   pasString+1,X
         lda   pasString-1,X
         sta   pasString,X
         lda   #'0'
         sta   pasString-1,X
         inc   pasString
lb6      anop                             endif
         long  I,M
lb7      anop                           endif

         lda   #80                      if fw > 80 then fw := 80;
         cmp   fw
         bge   lb8
         sta   fw
lb8      anop
lb9      lda   pasString                while len(string) < fw do
         and   #$00FF
         cmp   fw
         bge   lb11
         short I,M                        insert(' ',string);
         ldx   pasString
lb10     lda   pasString,X
         sta   pasString+1,X
         dex
         bne   lb10
         lda   #' '
         sta   pasString+1
         inc   pasString
         long  I,M
         bra   lb9
lb11     anop

         plb
         return
         end

****************************************************************
*
*  ~GeqE - Test two SANE extended numbers for A >= B
*
*  Inputs:
*        n1,n2: numbers
*
*  Outputs:
*        A - 1 if true, else 0
*        Z - 0 if true, else 1
*
****************************************************************
*
~GeqE    start
         longa on
         longi on

         tsc                            push the addresses
         pea   0
         clc
         adc   #14
         pha
         pea   0
         sec
         sbc   #10
         pha
         fcpxx                          compare the two numbers
         lda   0,S                      remove the left over from the stack
         sta   20,S
         lda   2,S
         sta   22,S
         tsc
         clc
         adc   #20
         tcs
         txa                            set the result
         and   #$0042
         beq   lb1
         lda   #1
lb1      rtl
         end

****************************************************************
*
*  ~GetCharInput - read a character from standard in
*  ~ReadCharInput - read a character from standard in
*
*  Outputs:
*        ~EOLNInput - eoln(input)
*        ~EOFInput - eof(input)
*        A - character read
*        ~InputChar - character read
*
****************************************************************
*
~GetCharInput start
~ReadCharInput entry
         longa on
         longi on
return   equ   13                       RETURN key code

         phb
         phk
         plb
         lda   ~EOFInput                check for read at EOF
         beq   lb0
         error #3                       (read while at end of file)
         stz   ~EOFInput
lb0      jsl   SysKeyin                 get a character
         and   #$00FF
         stz   ~EOLNInput               eoln(input) := false
         cmp   #return                  if its a return then
         bne   lb1                        eoln(input) := true
         inc   ~EOLNInput
         lda   #' '                       return a space
lb1      cmp   #0                       if its a null then
         bne   lb2                        eof(input) := true
         inc   ~EOFInput
         inc   ~EOLNInput
         lda   #' '                       return a space
lb2      short M                        return the char
         sta   ~InputChar
         long  M
         plb
         rtl
         end

****************************************************************
*
*  ~GetSBuffer - allocate a string buffer
*
*  Inputs:
*        a,x - number of bytes to allocate
*
*  Outputs:
*        a,x - pointer to buffer
*        ~StringList - buffer is added to string list
*
****************************************************************
*
~GetSBuffer start
         longa on
         longi on
r0       equ   0                        save 0 page pointer

         phb                            set data bank reg
         phk
         plb

         clc                            add the node pointer
         adc   #4
         bcc   gs1
         inx
gs1      phx                            get a buffer
         pha
         jsl   ~New
         sta   buff
         stx   buff+2
         ora   buff+2
         beq   err
         ph4   <r0                      save 0 page
         move4 buff,r0                  insert the buffer into the list
         lda   ~StringList
         sta   [r0]
         ldy   #2
         lda   ~StringList+2
         sta   [r0],y
         move4 buff,~StringList
         add4  buff,#4                  adjust buffer pointer past node
         pl4   r0
         lda   buff                     return the pointer
         ldx   buff+2
         plb
         clc
         rts

err      plb                            restore data bank reg
         sec
         rts

buff     ds    4                        buffer pointer
         end

****************************************************************
*
*  ~GrtE - Test two SANE extended numbers for A > B
*
*  Inputs:
*        n1,n2: numbers
*
*  Outputs:
*        A - 1 if true, else 0
*        Z - 0 if true, else 1
*
****************************************************************
*
~GrtE    start
         longa on
         longi on

         tsc                            push the addresses
         pea   0
         clc
         adc   #14
         pha
         pea   0
         sec
         sbc   #10
         pha
         fcpxx                          compare the two numbers
         lda   0,S                      remove the left over from the stack
         sta   20,S
         lda   2,S
         sta   22,S
         tsc
         clc
         adc   #20
         tcs
         txa                            set the result
         and   #$0040
         beq   lb1
         lda   #1
lb1      rtl
         end

****************************************************************
*
*  ~LoadComp - Load a comp value
*
*  Inputs:
*        4,S - address of the comp value
*
*  Outputs:
*        extended # on stack
*
****************************************************************
*
~LoadComp start

         tsc                            make room on stack
         sec
         sbc   #14
         tcs
         lda   18,S                     move passed address
         sta   5,S
         lda   20,S
         sta   7,S
         lda   16,S                     move return address
         sta   10,S
         lda   14,S
         sta   8,S
         clc                            set addr of extended number
         tsc
         adc   #12
         sta   1,S
         lda   #0
         sta   3,S
         fc2x                           convert to extended
         rtl                            return
         end

****************************************************************
*
*  ~LoadDouble - Load a double value
*
*  Inputs:
*        1,S - return address
*        4,S - address of the double value
*
*  Outputs:
*        extended # on stack
*
****************************************************************
*
~LoadDouble start
         using ~FPECommon

command  equ   $C088                    disp to the FPE command register
operand  equ   $C08C                    disp to the FPE operand register
response equ   $C080                    disp to the FPE response register

ext      equ   5                        position of extended number
addr     equ   5                        direct page address of the float pointer

         phb                            set up for abs addressing to bank 0
         pea   0
         plb
         plb
         lda   >FPEslot                 get the slot displacelement
         bne   lb1
         phb
         phk
         plb
         lda   slot
         asl   A
         asl   A
         asl   A
         asl   A
         sta   FPEslot
         ora   #command
         sta   c1+1
         sta   c2+1
         lda   FPEslot
         ora   #response
         sta   lb2+1
         sta   lb3+1
         sta   lb4+1
         sta   lb5+1
         lda   FPEslot
         ora   #operand
         sta   o1+1
         sta   o2+1
         sta   o3+1
         sta   o4+1
         sta   o5+1
         lda   FPEslot
         ora   #operand+2
         sta   p1+1
         sta   p2+1
         sta   p3+1
         sta   p4+1
         sta   p5+1
         plb
lb1      anop

         lda   #$0054                   load the double value
c1       sta   command
         tsc
         phd
         tcd
         ldx   #$0089
lb2      cpx   response
         beq   lb2
         ldy   #6
         lda   [addr],Y
         xba
o1       sta   operand
         ldy   #4
         lda   [addr],Y
         xba
p1       sta   operand+2
         ldy   #2
         lda   [addr],Y
         xba
o2       sta   operand
         lda   [addr]
         xba
p2       sta   operand+2
         pld
lb3      lda   response
         and   #$0080
         bne   lb3
    
         lda   3,S                      make room on the stack
         pha
         pha
         lda   5,S
         pha
         lda   #$0068                   save the extended value
c2       sta   command
         lda   #$0089
lb4      cmp   response
         beq   lb4
o3       lda   operand
         xba
         sta   ext+8,S
p3       lda   operand+2
o4       lda   operand
         xba
         sta   ext+6,S
p4       lda   operand+2
         xba
         sta   ext+4,S
o5       lda   operand
         xba
         sta   ext+2,S
p5       lda   operand+2
         xba
         sta   ext,S
lb5      lda   response
         and   #$0080
         bne   lb5
         plb                            restore B

         rtl
;
;  Local data
;
FPEslot  ds    2                        FPE slot displacement
         end

****************************************************************
*
*  ~LoadExtended - Load an extended value
*
*  Inputs:
*        4,S - address of the comp value
*
*  Outputs:
*        extended # on stack
*
****************************************************************
*
~LoadExtended start
ext      equ   10                       addr of extended value on stack frame
addr     equ   3                        ptr to extended value to load

         tsc                            make room on stack
         sec
         sbc   #10
         tcs
         lda   14,S                     move passed address
         sta   1,S
         lda   16,S
         sta   3,S
         lda   11,S                     move return address
         sta   5,S
         lda   12,S
         sta   6,S
         phd                            set up addressing
         tsc
         tcd
         ldx   #8                       move the value
lb1      txy
         lda   [addr],Y
         sta   ext,X
         dex
         dex
         bpl   lb1
         pld                            return
         pla
         pla
         rtl
         end

****************************************************************
*
*  ~LoadReal - Load a real value
*
*  Inputs:
*        4,S - address of the real value
*
*  Outputs:
*        extended # on stack
*
****************************************************************
*
~LoadReal start
         using ~FPECommon

command  equ   $C088                    disp to the FPE command register
operand  equ   $C08C                    disp to the FPE operand register
response equ   $C080                    disp to the FPE response register

ext      equ   5                        position of extended number
addr     equ   5                        direct page address of the float pointer

         phb                            set up for abs addressing to bank 0
         pea   0
         plb
         plb
         lda   >FPEslot                 get the slot displacelement
         bne   lb1
         phb
         phk
         plb
         lda   slot
         asl   A
         asl   A
         asl   A
         asl   A
         sta   FPEslot
         ora   #command
         sta   c1+1
         sta   c2+1
         lda   FPEslot
         ora   #response
         sta   lb2+1
         sta   lb3+1
         sta   lb4+1
         sta   lb5+1
         lda   FPEslot
         ora   #operand
         sta   o1+1
         sta   o2+1
         sta   o3+1
         sta   o4+1
         lda   FPEslot
         ora   #operand+2
         sta   p1+1
         sta   p2+1
         sta   p3+1
         sta   p4+1
         plb
lb1      anop

         lda   #$0044                   load the real value
c1       sta   command
         tsc
         phd
         tcd
         ldx   #$0089
lb2      cpx   response
         beq   lb2
         ldy   #2
         lda   [addr],Y
         xba
o1       sta   operand
         lda   [addr]
         xba
p1       sta   operand+2
         pld
lb3      lda   response
         and   #$0080
         bne   lb3
    
         lda   3,S                      make room on the stack
         pha
         pha
         lda   5,S
         pha
         lda   #$0068                   save the extended value
c2       sta   command
         lda   #$0089
lb4      cmp   response
         beq   lb4
o2       lda   operand
         xba
         sta   ext+8,S
p2       lda   operand+2
o3       lda   operand
         xba
         sta   ext+6,S
p3       lda   operand+2
         xba
         sta   ext+4,S
o4       lda   operand
         xba
         sta   ext+2,S
p4       lda   operand+2
         xba
         sta   ext,S
lb5      lda   response
         and   #$0080
         bne   lb5
         plb                            restore B

         rtl
;
;  Local data
;
FPEslot  ds    2                        FPE slot displacement
         end

****************************************************************
*
*  ~LogE - Natural log function
*
*  Inputs:
*        1,S - return address
*        4,S: number
*
****************************************************************
*
~LogE    start
         longa on
         longi on

         tsc                            push the addresse twice
         pea   0
         clc
         adc   #4
         pha
         flnx                           compute function
         rtl
         end

****************************************************************
*
*  ~MulE - Multiply two SANE extended numbers
*
*  Inputs:
*        n1,n2: numbers
*
****************************************************************
*
~MulE    start

         lda   #$2348
         brl   ~DoFPEBinop
         end

****************************************************************
*
*  ~Power - Raise one number to the power of another
*
*  Inputs:
*        n1,n2: numbers
*
****************************************************************
*
~Power   start
         longa on
         longi on

         tsc                            push the addresses
         pea   0
         clc
         adc   #4
         pha
         pea   0
         adc   #10
         pha
         fxpwry                         do the operation
         lda   0,S                      remove the left over from the stack
         sta   10,S
         lda   2,S
         sta   12,S
         tsc
         clc
         adc   #10
         tcs
         rtl
         end

****************************************************************
*
*  ~PutCharInput - Put back a character, checking for EOL
*
*  Inputs:
*        A - character to put back
*        ~EOLNInput - EOLN flag
*
****************************************************************
*
~PutCharInput start
return   equ   13                       RETURN key code

         tax
         lda   >~EOLNInput
         beq   lb1
         ldx   #return
         lda   #0
         sta   >~EOLNInput
lb1      phx
         jsl   SysPutback
         rtl
         end

****************************************************************
*
*  ~ReadReal - Read a real
*
*  Inputs:
*        filePtr - pointer to file buffer
*
*  Outputs:
*        4,S - result
*
****************************************************************
*
~ReadReal start
         longa on
         longi on
         using ~FileCom
         using ~RealCom
tab      equ   9                        TAB key code

filePtr  equ   7                        file pointer/result pointer

         phb
         phk
         plb
         phd
         tsc
         tcd

         sub4  filePtr,#~flHeader       point to file variable, not buffer
         ldy   #~flHeader               skip leading white space
         lda   [filePtr],Y
         and   #$00FF
lb1      cmp   #tab
         beq   lb2
         cmp   #' '
         bne   lb3
lb2      jsr   NextCh
         bra   lb1
lb3      stz   pasString                read the leading sign, if any
         cmp   #'+'
         beq   lb3a
         cmp   #'-'
         bne   lb4
         sta   pasString+1
         inc   pasString
lb3a     jsr   NextCh
lb4      jsr   NMID                     read the leading digits
         bcc   lb6
         jsr   WriteCh
         jcs   err1
         jsr   NextCh
         bra   lb4
lb6      cmp   #'.'                     allow for the decimal point
         bne   lb8
         jsr   WriteCh
         jcs   err1
         jsr   NextCh
lb7      jsr   NMID                     read the decimal digits
         bcc   lb8
         jsr   WriteCh
         jcs   err1
         jsr   NextCh
         bra   lb7
lb8      cmp   #'e'                     allow for an exponent
         beq   lb9
         cmp   #'E'
         bne   cv1
lb9      jsr   WriteCh
         jcs   err1
         jsr   NextCh
         cmp   #'-'                     allow for a sign on the exponent
         beq   lb10
         cmp   #'+'
         bne   lb12
lb10     jsr   WriteCh
         bcs   err1
         jsr   NextCh
lb12     jsr   NMID                     read the exponent digits
         bcc   cv1
         jsr   WriteCh
         bcs   err1
         jsr   NextCh
         bra   lb12
;
;  Convert the string into a number
;
cv1      ph4   #pasString+1                     lvp^.rval := cnvsr(digit);
         ph4   #index                           {convert from ascii to decform}
         ph4   #decrec
         ph4   #valid
         stz   index
         fcstr2dec
         lda   valid                            {flag an error if SANE said to}
         beq   err1
         lda   pasString
         and   #$00FF
         cmp   index
         bne   err1
         ph4   #decrec                          {convert decform to real}
         ph4   #t1
         fdec2x
         bcc   rts
err1     error #10                      real math error
rts      pld                            return the result
         plx
         ply
         pla
         pla
         lda   t1+8
         pha
         lda   t1+6
         pha
         lda   t1+4
         pha
         lda   t1+2
         pha
         lda   t1
         pha
         phy
         phx
         plb
         rtl
;
;  Read the next character from the input file
;
NextCh   ldy   #~flEOF
         lda   [filePtr],Y
         beq   nc1
         lda   #' '
         rts

nc1      ph4   <filePtr
         jsl   ~GetBuffer
         ldy   #~flHeader
         lda   [filePtr],Y
         and   #$00FF
         rts
;
;  Save a character in the output buffer - return C set if overflow
;
WriteCh  pha
         inc   pasString
         lda   pasString
         and   #$00FF
         tax
         pla
         sta   pasString,X
         cpx   #l:pasString-1
         rts
;
;  Return C set if the character is a number
;
NMID     cmp   #'0'
         blt   no
         cmp   #'9'+1
         bge   no
         sec
         rts
no       clc
         rts
         end

****************************************************************
*
*  ~ReadRealInput - Read a real from standard in
*
*  Outputs:
*        4,S - result
*
****************************************************************
*
~ReadRealInput start
         longa on
         longi on
         using ~FileCom
         using ~RealCom
tab      equ   9                        TAB key code

         phb   
         phk
         plb

lb1      jsl   ~GetCharInput            skip leading white space
         cmp   #tab
         beq   lb1
         cmp   #' '
         beq   lb1
         stz   pasString                read the leading sign, if any
         cmp   #'+'
         beq   lb3a
         cmp   #'-'
         bne   lb4
         sta   pasString+1
         inc   pasString
lb3a     jsl   ~GetCharInput
lb4      jsr   NMID                     read the leading digits
         bcc   lb6
         jsr   WriteCh
         jcs   err1
         jsl   ~GetCharInput
         bra   lb4
lb6      cmp   #'.'                     allow for the decimal point
         bne   lb8
         jsr   WriteCh
         jcs   err1
         jsl   ~GetCharInput
lb7      jsr   NMID                     read the decimal digits
         bcc   lb8
         jsr   WriteCh
         jcs   err1
         jsl   ~GetCharInput
         bra   lb7
lb8      cmp   #'e'                     allow for an exponent
         beq   lb9
         cmp   #'E'
         bne   cv1
lb9      jsr   WriteCh
         jcs   err1
         jsl   ~GetCharInput
         cmp   #'-'                     allow for a sign on the exponent
         beq   lb10
         cmp   #'+'
         bne   lb12
lb10     jsr   WriteCh
         jcs   err1
         jsl   ~GetCharInput
lb12     jsr   NMID                     read the exponent digits
         bcc   cv1
         jsr   WriteCh
         bcs   err1
         jsl   ~GetCharInput
         bra   lb12
;
;  Convert the string into a number
;
cv1      jsl   ~PutCharInput
         ph4   #pasString+1                     lvp^.rval := cnvsr(digit);
         ph4   #index                           {convert from ascii to decform}
         ph4   #decrec
         ph4   #valid
         stz   index
         fcstr2dec
         lda   valid                            {flag an error if SANE said to}
         beq   err1
         lda   pasString
         and   #$00FF
         cmp   index
         bne   err1
         ph4   #decrec                          {convert decform to real}
         ph4   #t1
         fdec2x
         bcc   rts
err1     error #10                      real math error
rts      plx
         ply
         lda   t1+8
         pha
         lda   t1+6
         pha
         lda   t1+4
         pha
         lda   t1+2
         pha
         lda   t1
         pha
         phy
         phx
         plb
         rtl
;
;  Save a character in the output buffer - return C set if overflow
;
WriteCh  pha
         inc   pasString
         lda   pasString
         and   #$00FF
         tax
         pla
         sta   pasString,X
         cpx   #l:pasString-1
         rts
;
;  Return C set if the character is a number
;
NMID     cmp   #'0'
         blt   no
         cmp   #'9'+1
         bge   no
         sec
         rts
no       clc
         rts
         end

****************************************************************
*
*  ~RealFix - Convert a parameter from extended to real
*
*  Inputs:
*        rval - disp in stack frame of real value
*
****************************************************************
*
~RealFix start
         longa on
         longi on
rval     equ   4

         tdc                            push address of real value
         clc
         adc   rval,S
         pea   0
         pha
         pea   0
         pha
         fx2s                           convert from extended
         lda   2,S                      fix return addr
         sta   4,S
         pla
         sta   1,S
         rtl
         end

****************************************************************
*
*  ~RealRet - Save a real function return value in ~RealVal
*
*  Inputs:
*        rval - real value to save
*
*  Outputs:
*        X-Y - address of ~RealVal
*
****************************************************************
*
~RealRet start
         longa on
         longi on
lcAfterMarkStack equ 22                 size of default part of stack frame

         pea   0                        push address of real value
         tdc
         clc
         adc   #lcAfterMarkStack
         pha
         ph4   #~RealVal                push address of extended value
         fs2x                           convert to extended
         ldx   #^~RealVal               load address of result
         ldy   #~RealVal
         rtl                            return
         end

****************************************************************
*
*  ~RealRet2 - Save a real function return value in ~RealVal
*
*  Inputs:
*        rval - real value to save
*
*  Outputs:
*        X-Y - address of ~RealVal
*
*  Notes: Pascal 2.0 version
*
****************************************************************
*
~RealRet2 start

         lda   1,S                      swap address of double value
         tax                             and return address
         lda   2,S
         tay
         lda   4,S
         sta   1,S
         lda   6,S
         sta   3,S
         tya
         sta   6,S
         txa
         sta   5,S
         ph4   #~RealVal                push address of extended value
         fs2x                           convert to extended
         ldx   #^~RealVal               load address of result
         ldy   #~RealVal
         rtl                            return
         end

****************************************************************
*
*  ~RandomE - Return a random sane extended format number
*
*  Outputs:
*        extended random real on stack
*
****************************************************************
*
~RandomE start
         longa on
         longi on
mantisa  equ   4
exponent equ   12

         phb                            get return address
         plx
         ply
         pea   $3FFE                    set up exponent
         lda   #0
         pha                            make room for mantisa
         pha
         pha
         pha
         phy                            set the return address
         phx
         plb
         tsc                            get the stack pointer
         phd                            save direct page
         tcd                            set direct page

         jsl   ~ranx                    get the random mantisa
         lda   >~seed
         sta   mantisa
         lda   >~seed+2
         sta   mantisa+2
         lda   >~seed+4
         sta   mantisa+4
         lda   >~seed+6
         sta   mantisa+6
         ora   mantisa+4                if mantisa = 0
         ora   mantisa+2
         ora   mantisa
         beq   re2
re1      lda   mantisa+6                  while msb of mantisa <> 1
         bmi   re3
         dec   exponent                     exponent = exponent - 1
         asl   mantisa                      mantisa = mantisa << 1
         rol   mantisa+2
         rol   mantisa+4
         rol   mantisa+6
         bra   re1

re2      stz   exponent                 else exponent = 0
         bra   rts

re3      lda   >~seed+8                 set the sign
         and   #$8000
         ora   exponent
         sta   exponent
rts      pld
         rtl
         end

****************************************************************
*
*  ~RealCom - Real common area
*
****************************************************************
*
~RealCom privdata
decForm  anop                           decForm record
style    ds    2                        0 -> exponential; 1 -> fixed
digits   ds    2                        significant digits; decimal digits

decRec   anop                           decimal record
sgn      ds    2                        sign
exp      ds    2                        exponent
sig      ds    29                       significant digits

pasString ds   81                       printable string

index    ds    2                        index for number conversions
valid    ds    2                        valid flag for SANE scanner

one      dc    h'0000000000000080FF3F'  SANE extended 1
pi       dc    h'35C26821A2DA0FC90040'  pi/2
piover2  dc    h'35C26821A2DA0FC9FF3F'  pi/2
t1       ds    10                       temporary numbers
t2       ds    10
         end

****************************************************************
*
*  ~RealFn - place the return value from a real function on the stack
*  ~DoubleFn - place the return value from a double function on the stack
*
*  Inputs:
*        X-A - pointer to function value
*
****************************************************************
*
~RealFn  start
~DoubleFn entry
         longa on
         longi on

         phb                            use local addressing
         phk
         plb
         stz   lb1+2                    save the source address
         sta   lb1+1
         txa
         xba
         ora   lb1+2
         sta   lb1+2
         plx                            save return address
         ply
         tsc                            make room for value
         sec
         sbc   #10
         tcs
         phy                            fix return address
         phx
         plb
         clc                            set up the store addr
         tsc
         adc   #4
         sta   >lb2+1
         ldx   #8                       move the value
lb1      lda   >0,X
lb2      sta   >0,X
         dex
         dex
         bpl   lb1
         rtl
         end

****************************************************************
*
*  ~Round - round and convert a real to an integer
*
*  Inputs:
*        extended real on stack
*
*  Outputs:
*        A - integer
*
****************************************************************
*
~Round   start
         longa on
         longi on

         tsc                            push address of real 2 times
         clc
         adc   #4
         pea   0
         pha
         pea   0
         pha
         pea   0                        push address of integer
         pha
         frintx                         round
         fx2i                           convert
         phb                            move return address
         pla
         sta   9,S
         pla
         sta   9,S
         pla                            remove integer
         plx                            fix stack
         plx
         plb
         tax                            set condition codes
         rtl                            return
         end

****************************************************************
*
*  ~Round4 - round and convert a real to a long
*
*  Inputs:
*        extended real on stack
*
*  Outputs:
*        X,A - integer
*
****************************************************************
*
~Round4  start
         longa on
         longi on

         tsc                            push address of real 2 times
         clc
         adc   #4
         pea   0
         pha
         pea   0
         pha
         pea   0                        push address of integer
         pha
         frintx                         round
         fx2l                           convert
         phb                            move return address
         pla
         sta   9,S
         pla
         sta   9,S
         pla                            remove integer
         plx
         ply                            fix stack
         plb
         tay                            set condition codes
         bne   lb1
         txy
lb1      rtl                            return
         end

****************************************************************
*
*  ~SaveComp - Save a comp value
*
*  Inputs:
*        1,S - return address
*        4,S - address to save to
*        8,S - extended real value
*
****************************************************************
*
~SaveComp start

         csubroutine (4:addr,10:ext),0

         ph2   #0                       push addr of extended #
         clc
         tdc
         adc   #ext
         pha
         ph4   <addr                    push addr of real value
         fx2c                           convert and move
         creturn
         end

****************************************************************
*
*  ~SaveDouble - Save a double value
*
*  Inputs:
*        1,S - return address
*        4,S - address to save to
*        8,S - extended real value
*
****************************************************************
*
~SaveDouble start
         using ~FPECommon

         csubroutine (4:addr,10:ext),0

command  equ   $C088                    disp to the FPE command register
operand  equ   $C08C                    disp to the FPE operand register
response equ   $C080                    disp to the FPE response register

         phb                            set up for abs addressing to bank 0
         pea   0
         plb
         plb
         lda   >FPEslot                 get the slot displacelement
         bne   lb1
         phb
         phk
         plb
         lda   slot
         asl   A
         asl   A
         asl   A
         asl   A
         sta   FPEslot
         ora   #command
         sta   c1+1
         sta   c2+1
         lda   FPEslot
         ora   #response
         sta   lb2+1
         sta   lb3+1
         sta   lb4+1
         sta   lb5+1
         lda   FPEslot
         ora   #operand
         sta   o1+1
         sta   o2+1
         sta   o3+1
         sta   o4+1
         sta   o5+1
         lda   FPEslot
         ora   #operand+2
         sta   p1+1
         sta   p2+1
         sta   p3+1
         sta   p4+1
         sta   p5+1
         plb
lb1      anop

         lda   #$0048                   load the extended value
c1       sta   command
         ldx   #$0089
lb2      cpx   response
         beq   lb2
         lda   ext+8
         xba
o1       sta   operand
p1       stz   operand+2
         lda   ext+6
         xba
o2       sta   operand
         lda   ext+4
         xba
p2       sta   operand+2
         lda   ext+2
         xba
o3       sta   operand
         lda   ext
         xba
p3       sta   operand+2
lb3      lda   response
         and   #$0080
         bne   lb3

         lda   #$0074                   save the double value
c2       sta   command
         lda   #$0089
lb4      cmp   response
         beq   lb4
o4       lda   operand
         xba
         ldy   #6
         sta   [addr],Y
p4       lda   operand+2
         xba
         ldy   #4
         sta   [addr],Y
o5       lda   operand
         xba
         ldy   #2
         sta   [addr],Y
p5       lda   operand+2
         xba
         sta   [addr]
lb5      lda   response
         and   #$0080
         bne   lb5
         plb                            restore B

         creturn
;
;  Local data
;
FPEslot  ds    2                        FPE slot displacement
         end

****************************************************************
*
*  ~SaveExtended - Save an extended value
*
*  Inputs:
*        1,S - return address
*        4,S - address to save to
*        8,S - extended real value
*
****************************************************************
*
~SaveExtended start

         csubroutine (4:addr,10:ext),0

         ldx   #8                       move the value
lb1      txy
         lda   ext,X
         sta   [addr],Y
         dex
         dex
         bpl   lb1
         creturn
         end

****************************************************************
*
*  ~SaveReal - Save a real value
*
*  Inputs:
*        1,S - return address
*        4,S - address to save to
*        8,S - extended real value
*
****************************************************************
*
~SaveReal start
         using ~FPECommon

         csubroutine (4:addr,10:ext),0

command  equ   $C088                    disp to the FPE command register
operand  equ   $C08C                    disp to the FPE operand register
response equ   $C080                    disp to the FPE response register

         phb                            set up for abs addressing to bank 0
         pea   0
         plb
         plb
         lda   >FPEslot                 get the slot displacelement
         bne   lb1
         phb
         phk
         plb
         lda   slot
         asl   A
         asl   A
         asl   A
         asl   A
         sta   FPEslot
         ora   #command
         sta   c1+1
         sta   c2+1
         lda   FPEslot
         ora   #response
         sta   lb2+1
         sta   lb3+1
         sta   lb4+1
         sta   lb5+1
         lda   FPEslot
         ora   #operand
         sta   o1+1
         sta   o2+1
         sta   o3+1
         sta   o4+1
         lda   FPEslot
         ora   #operand+2
         sta   p1+1
         sta   p2+1
         sta   p3+1
         sta   p4+1
         plb
lb1      anop

         lda   #$0048                   load the extended value
c1       sta   command
         ldx   #$0089
lb2      cpx   response
         beq   lb2
         lda   ext+8
         xba
o1       sta   operand
p1       stz   operand+2
         lda   ext+6
         xba
o2       sta   operand
         lda   ext+4
         xba
p2       sta   operand+2
         lda   ext+2
         xba
o3       sta   operand
         lda   ext
         xba
p3       sta   operand+2
lb3      lda   response
         and   #$0080
         bne   lb3

         lda   #$0064                   save the real value
c2       sta   command
         lda   #$0089
lb4      cmp   response
         beq   lb4
o4       lda   operand
         xba
         ldy   #2
         sta   [addr],Y
p4       lda   operand+2
         xba
         sta   [addr]
lb5      lda   response
         and   #$0080
         bne   lb5
         plb                            restore B

         creturn
;
;  Local data
;
FPEslot  ds    2                        FPE slot displacement
         end

****************************************************************
*
*  ~SinE - Sin function
*
*  Inputs:
*        1,S - return address
*        4,S - number
*
****************************************************************
*
~SinE    start
         longa on
         longi on

         tsc                            push the addresse twice
         pea   0
         clc
         adc   #4
         pha
         fsinx                          compute function
         rtl
         end

****************************************************************
*
*  ~SqrE - Square a SANE extended number
*
*  Inputs:
*        1,S - return address
*        4,S - number
*
****************************************************************
*
~SqrE    start
         longa on
         longi on

         tsc                            push the addresse twice
         pea   0
         clc
         adc   #4
         pha
         pea   0
         pha
         fmulx                          multiply
         rtl
         end

****************************************************************
*
*  ~SqtE - Square root function
*
*  Inputs:
*        1,S - return address
*        4,S - number
*
****************************************************************
*
~SqtE    start
         longa on
         longi on

         tsc                            push the addresse twice
         pea   0
         clc
         adc   #4
         pha
         fsqrtx                         compute function
         rtl
         end

****************************************************************
*
*  StringToStandard - Convert a string to standard form
*
*  "Standard form" means the pointer points to the first char
*  of the string, and the length is the current length.  See
*  also ~StringToMaxStandard.
*
*  Inputs:
*        addr - address of string
*        len - length of string
*
*  Outputs:
*        addr - ptr to first char
*        len - current length of string
*
****************************************************************
*
~StringToStandard start
addr     equ   7                        string address
len      equ   5                        string length

         phd                            set up local DP
         tsc
         tcd
         lda   len                      if length < 0 then
         beq   lb5
         bpl   lb2
         inc   a                          if length = -1 then
         bne   lb1
         lda   addr+2                       if char = 0 then
         and   #$00FF                         {string is a null character}
         bne   lb0
         stz   len                            len := 0
         bra   lb5                          endif
lb0      lda   #1                           len := 1
         sta   len                          {string is a single character}
         lda   addr+2
         sta   >char
         lla   addr,char
         bra   lb5                          endif
!                                         endif
!                                         {string has a length byte}
lb1      lda   [addr]                     len := addr^
         and   #$00FF
         sta   len
         inc4  addr                       ++addr {skip length byte}
         bra   lb5                      else
!                                         {string is a nul terminated string}
lb2      ldx   len                        scan string for nul to find length
         ldy   #0
         short M
lb3      lda   [addr],Y
         beq   lb4
         iny
         dex
         bne   lb3
lb4      sty   len
         long  M                        endif

lb5      pld
         rts

char     ds    2                        value of a character string
         end

****************************************************************
*
*  ~SubE - Subtract two SANE extended numbers
*
*  Inputs:
*        n1,n2: numbers
*
****************************************************************
*
~SubE    start

         lda   #$2848
         brl   ~DoFPEBinop
         end

****************************************************************
*
*  ~TanE - Tan function
*
*  Inputs:
*        1,S - return address
*        4,S - number
*
****************************************************************
*
~TanE    start
         longa on
         longi on

         tsc                            push the addresse twice
         pea   0
         clc
         adc   #4
         pha
         ftanx                          compute function
         rtl
         end

****************************************************************
*
*  ~WriteReal - Write a real value to a file
*
*  Inputs:
*        ext - extended format real number
*        fw - field width
*        decDig - fixed precision digit count
*        filePtr - pointer to the file buffer
*
****************************************************************
*
~WriteReal start
         longa on
         longi on
         using ~RealCom
fPtr     equ   3                        file pointer for use by ~_COut
len      equ   0                        length of the string
disp     equ   7                        displacement into the string

         sub   (4:filePtr,2:decDig,2:fw,10:ext),9
         phb
         phk
         plb

         move4 filePtr,fPtr             set up file pointer for ~_COut
         ldx   #12                      format the number
lb1      lda   decDig,X
         pha
         dex
         dex
         bpl   lb1
         jsl   ~FormatReal
         lda   pasString                write the string
         and   #$00FF
         sta   len
         lda   #1
         sta   disp
lb5      ldx   disp
         lda   pasString,X
         jsl   ~_COut
         inc   disp
         dbne  len,lb5

         plb
         return
         end

****************************************************************
*
*  ~WriteRealEO - Write a real value to error out
*
*  Inputs:
*        ext - extended format real number
*        fw - field width
*        decDig - fixed precision digit count
*
****************************************************************
*
~WriteRealEO start
         longa on
         longi on
         using ~RealCom

         sub   (2:decDig,2:fw,10:ext),0
         phb
         phk
         plb

         ldx   #12                      format the number
lb1      lda   decDig,X
         pha
         dex
         dex
         bpl   lb1
         jsl   ~FormatReal
         ph4   #pasString-1             write the string
         ph2   #1
         ph2   #0
         ph2   #1
         jsl   ~Puts

         plb
         return
         end

****************************************************************
*
*  ~WriteRealOutput - Write a real value to standard out
*
*  Inputs:
*        ext - extended format real number
*        fw - field width
*        decDig - fixed precision digit count
*
****************************************************************
*
~WriteRealOutput start
         longa on
         longi on
         using ~RealCom

         sub   (2:decDig,2:fw,10:ext),0
         phb
         phk
         plb

         ldx   #12                      format the number
lb1      lda   decDig,X
         pha
         dex
         dex
         bpl   lb1
         jsl   ~FormatReal
         ph4   #pasString-1             write the string
         ph2   #1
         ph2   #0
         ph2   #0
         jsl   ~Puts

         plb
         return
         end
