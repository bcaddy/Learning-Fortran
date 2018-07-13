PROGRAM radio_reciever
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       Calculate resonent frequency of a RLC circuit
!       Source: 2-23

!   Parameter Decrations
REAL, PARAMETER :: pi = 3.141593

!   Variable Declarations
REAL :: L = 0.1e-3  ! inductance (H)
REAL :: C = 0.25e-9 ! capacitance (F)
REAL :: f0          ! resonent frequency

!   Calculations
f0 = 1/(2*pi*SQRT(L*C))

!   Return result
WRITE (*,*) 'The resonent frequency is',f0,'Hz'

!   Finish up.
END PROGRAM radio_reciever