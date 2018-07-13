PROGRAM polar2rec
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To convert from polar to cartesian coordinates
!       Source: Exercise 6-10

!   Parameter Decrations
REAL, PARAMETER :: pi = 3.141592653 ! pi
REAL, PARAMETER :: deg_2_rad = pi/180 ! Degree to radian conversion facter.  Used like rad=deg*deg_2_rad
INTEGER, PARAMETER :: d = 2   ! dimensionality of the space

!   Variable Declarations
REAL, DIMENSION(d) :: polar  ! polar vector.  polar(1) is the mag and polar(2) is the angle in degrees
REAL, DIMENSION(d) :: rect   ! cartesian vector. rect(1) is x and rect(2) is y

!   Read in polar vector
WRITE (*,*) 'Please input the polar vector in the form "mag, angle"'
READ (*,*) polar

!   Convert to cartesian coordinates
polar(2) = polar(2) * deg_2_rad
rect = (/ polar(1)* COS(polar(2)), polar(1) * SIN(polar(2)) /)

!   Return values
WRITE (*,100) polar, rect
100 FORMAT ('The polar coordinates',2(F6.2), 'deg converted to cartesian coordinates are', 2(F6.2))


!   Finish up.
WRITE (*,'(//, A)') 'Program complete'
END PROGRAM polar2rec
