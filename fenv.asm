         keep  obj/fenv
         mcopy fenv.macros
         case  on

****************************************************************
*
*  Fenv - Floating-point environment access
*
*  This code provides routines to query and modify the
*  floating-point environment.
*
*  Note: This relies on and only works with SANE.
*
****************************************************************
*
fenv     private                        dummy segment
         end

FE_ALL_EXCEPT gequ $001F

****************************************************************
*
*  ~GETENV - Get floating-point environment
*
*  Outputs: Returns environment in same format as SANE FGETENV
*
****************************************************************
*
~GETENV  private
         phb
         phk
         plb
         lda   #$00A8                   get FPSR value
         jsl   ~GETFPECONTROLREG
         
         stz   excepts                  set SANE-style exception bits
         clc
         bit   #$0008                   set inexact bit
         beq   lb1
         sec
lb1      rol   excepts
         bit   #$0010                   set div-by-zero bit
         beq   lb2
         sec
lb2      rol   excepts
         bit   #$0040                   set overflow bit
         beq   lb3
         sec
lb3      rol   excepts
         bit   #$0020                   set underflow bit
         beq   lb4
         sec
lb4      rol   excepts
         bit   #$0080                   set invalid bit
         beq   lb5
         sec
lb5      rol   excepts

         FGETENV                        OR in FPE exceptions with SANE env word
         txa
         ora   space
         tax                            leave results in X+Y regs, like SANE
         xba
         and   #$00FF
         tay

         plb
         rts                            return

space    dc    i1'0'                    one zero byte before excepts
excepts  ds    2                        exceptions read from FPE
         end

****************************************************************
*
*  ~SETENV - Set floating-point environment
*
*  Inputs: New environment value on stack, like SANE FSETENV
*
****************************************************************
*
~SETENV  private
         lda   3,s                      convert rounding precision
         and   #$00C0
         cmp   #$0080
         bne   lb1
         lsr   a
         bra   lb2
lb1      cmp   #$0040
         bne   lb2
         asl   a
lb2      pha

         lda   3+2,s                    convert rounding direction
         and   #$C000
         bit   #$4000
         beq   lb3
         eor   #$8000
lb3      xba
         lsr   a
         lsr   a

         ora   1,s                      construct FPCR value to set
         plx
         tax
         ldy   #0
         lda   #$0090                   set the FPCR
         jsl   ~SETFPECONTROLREG
         
         ldx   #0                       zero the FPSR
         txy
         lda   #$0088
         jsl   ~SETFPECONTROLREG

         lda   3,s                      set SANE environment
         pha
         FSETENV

         pla                            clean up stack and return
         sta   1,s
         rts
         end

****************************************************************
*
*  int feclearexcept(int excepts);
*
*  Clear floating-point exceptions
*
*  Inputs:
*        excepts - floating-point exceptions to clear
*
*  Outputs:
*        Returns 0 if successful, non-zero otherwise
*
****************************************************************
*
feclearexcept start

         csubroutine (2:excepts),0
         
         jsr   ~GETENV                         get current environment
         phx
         
         lda   excepts
         and   #FE_ALL_EXCEPT
         eor   #$FFFF                          mask off excepts to clear
         xba
         and   1,S
         sta   1,S
         jsr   ~SETENV                         clear them

         creturn 2:#0
         end

****************************************************************
*
*  int fegetexceptflag(fexcept_t *flagp, int excepts);
*
*  Get floating-point exception flags.
*
*  Inputs:
*        flagp - pointer to location to store exception flags
*        excepts - floating-point exceptions to get
*
*  Outputs:
*        Returns 0 if successful, non-zero otherwise
*
****************************************************************
*
fegetexceptflag start

         csubroutine (4:flagp,2:excepts),0
         
         jsr   ~GETENV                         get current environment
         tya
         and   excepts                         get desired exceptions
         and   #FE_ALL_EXCEPT
         sta   [flagp]                         store them in *flagp
         
         creturn 2:#0
         end

****************************************************************
*
*  int feraiseexcept(int excepts);
*
*  Raise floating-point exceptions
*
*  Inputs:
*        excepts - floating-point exceptions to raise
*
*  Outputs:
*        Returns 0 if successful, non-zero otherwise
*
****************************************************************
*
feraiseexcept start

         csubroutine (2:excepts),0
         
         lda   excepts
         and   #FE_ALL_EXCEPT
         beq   done
         pha
         FSETXCP                        raise exceptions
         
done     creturn 2:#0
         end

****************************************************************
*
*  int fesetexceptflag(fexcept_t *flagp, int excepts);
*
*  Set (but do not raise) floating-point exception flags
*
*  Inputs:
*        flagp - pointer to stored exception flags
*        excepts - floating-point exceptions to set
*
*  Outputs:
*        Returns 0 if successful, non-zero otherwise
*
****************************************************************

fesetexceptflag start

         csubroutine (4:flagp,2:excepts),0
         
         jsr   ~GETENV                  get env with excepts masked off
         phx
         lda   excepts
         and   #FE_ALL_EXCEPT
         eor   #$FFFF
         xba
         and   1,S
         sta   1,S
         
         lda   [flagp]                  set new exceptions
         and   excepts
         and   #FE_ALL_EXCEPT
         xba
         ora   1,S
         sta   1,S
         jsr   ~SETENV
         
         creturn 2:#0
         end

****************************************************************
*
*  int fetestexcept(int excepts);
*
*  Test if floating-point exception flags are set
*
*  Inputs:
*        excepts - floating-point exceptions to test for
*
*  Outputs:
*        Bitwise or of exceptions that are set
*
****************************************************************
*
fetestexcept start

         csubroutine (2:excepts),0
         
         jsr   ~GETENV                  get exception flags
         tya
         and   excepts                  mask to just the ones we want
         and   #FE_ALL_EXCEPT
         sta   excepts
         
         creturn 2:excepts
         end

****************************************************************
*
*  int fegetround(void);
*
*  Get the current rounding direction
*
*  Outputs:
*        The current rounding direction
*
****************************************************************
*
fegetround start
         jsr   ~GETENV                  get high word of environment
         tya
         and   #$00C0                   just rounding direction
         rtl
         end

****************************************************************
*
*  int fesetround(int round);
*
*  Set the current rounding direction
*
*  Inputs:
*        round - the rounding direction to set
*
*  Outputs:
*        Returns 0 if successful, non-zero otherwise
*
****************************************************************
*
fesetround start

         csubroutine (2:round),0

         lda   round                    flip words
         xba
         sta   round
         and   #$3FFF                   do nothing if not a valid rounding dir
         bne   done

         jsr   ~GETENV                  set the rounding direction
         txa
         and   #$3FFF
         ora   round
         pha
         jsr   ~SETENV

         stz   round
done     creturn 2:round
         end

****************************************************************
*
*  int fegetenv(fenv_t *envp);
*
*  Get the current floating-point environment
*
*  Inputs:
*        envp - pointer to location to store environment
*
*  Outputs:
*        Returns 0 if successful, non-zero otherwise
*
****************************************************************
*
fegetenv start

         csubroutine (4:envp),0
         
         jsr   ~GETENV                  get the environment
         txa
         sta   [envp]                   store it in *envp
         
         creturn 2:#0
         end

****************************************************************
*
*  int feholdexcept(fenv_t *envp);
*
*  Get environment, then clear status flags and disable halts
*
*  Inputs:
*        envp - pointer to location to store environment
*
*  Outputs:
*        Returns 0 if successful, non-zero otherwise
*
****************************************************************
*
feholdexcept start

         csubroutine (4:envp),0
         
         jsr   ~GETENV                  get the environment
         txa
         sta   [envp]                   store it in *envp
         
         and   #$E0E0                   clear exception flags and disable halts
         pha
         jsr   ~SETENV                  set the new environment
         
         creturn 2:#0
         end

****************************************************************
*
*  int fesetenv(const fenv_t *envp);
*
*  Set the floating-point environment
*
*  Inputs:
*        envp - pointer to environment to set
*
*  Outputs:
*        Returns 0 if successful, non-zero otherwise
*
****************************************************************
*
fesetenv start

         csubroutine (4:envp),0

         lda   [envp]                   set the environment
         pha
         jsr   ~SETENV

         creturn 2:#0
         end

****************************************************************
*
*  int feupdateenv(const fenv_t *envp);
*
*  Save exceptions, set environment, then re-raise exceptions
*
*  Inputs:
*        envp - pointer to environment to set
*
*  Outputs:
*        Returns 0 if successful, non-zero otherwise
*
****************************************************************
*
feupdateenv start

         csubroutine (4:envp),0

         jsr   ~GETENV                  get exceptions from current environment
         txa
         and   #$1F00

         ora   [envp]                   OR exceptions into new environment
         pha                            set the environment
         jsr   ~SETENV

         creturn 2:#0
         end

****************************************************************
*
*  Default floating-point environment
*
****************************************************************
*
__FE_DFL_ENV start
         dc i2'0'
         end

****************************************************************
*
*  int __get_flt_rounds(void);
*
*  Get the value of FLT_ROUNDS, accounting for rounding mode
*
*  Outputs:
*        Current value of FLT_ROUNDS
*
****************************************************************
*
__get_flt_rounds start
         jsr   ~GETENV
         tya                            get rounding direction in low bits of A
         asl   a
         asl   a
         xba
         inc   a                        convert to values used by FLT_ROUNDS
         and   #$0003
         rtl
         end
