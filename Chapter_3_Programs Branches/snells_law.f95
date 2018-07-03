PROGRAM snells_law
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       Calculate angle of incidence using Snell's Law
!       Source: Exercise 3-12

!   Parameter definition
REAL, PARAMETER :: pi = 3.141592653 ! pi

!   Variable Declarations
REAL :: n1      ! index of refraction in medium 1
REAL :: n2      ! index of refraction in medium 2
REAL :: theta1  ! angle of incidence in medium 1
REAL :: theta2  ! angle of incidence in medium 2
REAL :: quant   ! (n1/n2)*sin(theta1)


!   Gather user input
WRITE (*,*) 'Please input n1, n2, and theta1 (degrees)'
READ (*,*) n1, n2, theta1

!   Convert theta 1 into radians
theta1 = theta1 * pi/180

!   Determine theta2
quant = (n1/n2) * SIN(theta1)

exception: IF (quant < -1 .OR. quant > 1.) THEN
    WRITE (*,*) 'The light was totally reflected since (n1/n2)*sin(theta1)=',quant, 'The angle of reflection is',theta1
ELSE
    theta2 = ASIN(quant)
    WRITE (*,*) 'theta2 is',theta2 * 180/pi, 'degrees'
END IF exception

!   Finish up.
WRITE (*,*) CHAR(10),'Program complete'
END PROGRAM snells_law