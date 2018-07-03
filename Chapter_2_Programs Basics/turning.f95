PROGRAM turning
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To illustrate some of the basic features of a Fortran Program
!       Source: Exercise 2-24

!   Parameter Decrations
REAL, PARAMETER :: g = 9.81 ! acceleration due to gravity (m/s/s)
REAL, PARAMETER :: mach = 340. ! speed of sound (m/s)

!   Variable Declarations
REAL :: ra ! turning radius for part a (m)
REAL :: rb ! turning radius for part b (m)
REAL :: rc ! turning radius for part c (m)

!   Calculations for part a
ra = ((0.85*mach)**2) / (2*g)
WRITE (*,*) 'the turning radius at 0.85 Mach with an acceleration of 2g is', ra,'meters'

!   Calculations for part b
rb = ((1.5*mach)**2) / (2*g)
WRITE (*,*) 'the turning radius at 0.85 Mach with an acceleration of 2g is', rb,'meters'

!   Calculations for part c
rc = ((1.5*mach)**2) / (7*g)
WRITE (*,*) 'the turning radius at 0.85 Mach with an acceleration of 2g is', rc,'meters'


!   Finish up.
END PROGRAM turning