PROGRAM log_base
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       to find the log of x with base b
!       Source: Exercise 2-18

!   Variable Declarations
REAL :: b  ! base of log
REAL :: x  ! argument of log
!REAL :: lg ! log

!   get values from user
WRITE (*,*) 'Input base then argument in that order:'
READ (*,*) b,x

!   Perform calculations
lg = LOG10(x)/LOG10(b)

!   return results
WRITE (*,*) 'The base',b,'logarithm of',x,'is',lg


!   Finish up.
END PROGRAM log_base