PROGRAM decibel_converter_chap3
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       Convert a given input power to decibels with a reference level of 1mW and trap values that are undefined
!       Source: Exercise 3-11

!   Variable Declarations
REAL :: p1 = 1 ! reference level (mW)
REAL :: p2     ! input level (mW)
REAL :: d      ! decibel level
REAL :: ratio  ! p2/p1 unitless

!   Get input level from user
WRITE (*,*) 'Input the power level to be converted to decibels in units of milliwatts'
READ (*,*) p2

!   Calculations and exception handling
ratio = p2/p1

exception_handler: IF (ratio <= 0) THEN
    WRITE (*,*) 'The decibel value is undefined since the ratio of the input and reference power is',ratio,'This value must be > 0'
ELSE
    d = 10 * LOG10(ratio)
    !   Return values
    WRITE (*,*) 'The decibel value of a signal of',p2,'mW with a reference level of',p1,'mW is',d,'decibels'
END IF exception_handler




!   Finish up.
END PROGRAM decibel_converter_chap3