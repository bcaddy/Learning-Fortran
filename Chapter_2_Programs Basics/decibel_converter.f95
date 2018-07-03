PROGRAM decibel_converter
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       Convert a given input power to decibels with a reference level of 1mW
!       Source: Exercise 2-21

!   Variable Declarations
REAL :: p1 = 1 ! reference level (mW)
REAL :: p2     ! input level (mW)
REAL :: d      ! decibel level

!   Get input level from user
WRITE (*,*) 'Input the power level to be converted to decibels in units of milliwatts'
READ (*,*) p2

!   Calculations
d = 10 * LOG10(p2/p1)

!   Return values
WRITE (*,*) 'The decibel value of a signal of',p2,'mW with a reference level of',p1,'mW is',d,'decibels'




!   Finish up.
END PROGRAM decibel_converter