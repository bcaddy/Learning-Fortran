PROGRAM ln_loop
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       Calculate values of ln(1/(1-x))
!       Source: Exercise 4-13

!   Variable Declarations
REAL :: x  ! user input value
REAL :: arg ! argument of ln()

DO
    WRITE (*,*) 'Input a value for x'
    READ (*,*) x

    arg = 1/(1-x)

    IF (arg<=0) EXIT

    WRITE (*,*) 'The value of ln(1/(1-x)) is',LOG(arg)
END DO



!   Finish up.
WRITE (*,*) 'Program complete'
END PROGRAM ln_loop