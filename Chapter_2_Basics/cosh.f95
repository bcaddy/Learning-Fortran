PROGRAM cosher
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       Calculate the cosh of a user supplied number
!       Source: Exercise 2-22

!   Variable Declarations
REAL :: x  ! number to find the cosh of
REAL :: cs ! cosh of x

!   Get number
WRITE (*,*) 'Input value of number to find the cosh of'
READ (*,*) x

!   Calculations
cs = (EXP(x) + EXP(-x)) / 2

!   Return results
WRITE (*,*) 'The cosh of',x,'is',cs, 'using scratch written routines'

!   Built in value
WRITE (*,*) 'The cosh of',x,'is',cs, 'using the intrinsic function'


!   Finish up.
END PROGRAM cosher