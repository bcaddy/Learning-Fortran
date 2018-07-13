PROGRAM escape_vel
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To calculate escape velocities
!       Source: Exercise 2-25

!   Parameter Decrations
REAL, PARAMETER :: G = 6.67408e-11 ! gravitational constant (m^3 kg^-1 s^-2)
!   the following parameters are the masses and radii of some solar system bodies.  Masses are the 'm' variables and radii are the
!   'r' variables where masses are in kg and radii are in meters
REAL, PARAMETER :: Mearth = 6.0e24,   Rearth = 6.4e6
REAL, PARAMETER :: Mmoon = 7.4e22,    Rmoon  = 1.7e6
REAL, PARAMETER :: Mceres = 8.7e20,   Rceres = 4.7e5
REAL, PARAMETER :: Mjupiter = 1.9e27, Rjupiter = 7.1e7

!   Variable Declarations
REAL :: Vearth  ! escape velocity of the earth (m/s)
REAL :: Vmoon   ! escape velocity of the moon (m/s)
REAL :: Vceres  ! escape velocity of ceres (m/s)
REAL :: Vjupiter! escape velocity of jupiter (m/s)


!   Calculations
Vearth = SQRT(2*G*Mearth / Rearth)
Vmoon = SQRT(2*G*Mmoon / Rmoon)
Vceres = SQRT(2*G*Mceres / Rceres)
Vjupiter = SQRT(2*G*Mjupiter / Rjupiter)

!   Return values
WRITE (*,*) 'The escape velocity of the Earth is',Vearth,'m/s'
WRITE (*,*) 'The escape velocity of the Moon is',Vmoon,'m/s'
WRITE (*,*) 'The escape velocity of Ceres is',Vceres,'m/s'
WRITE (*,*) 'The escape velocity of Jupiter is',Vjupiter,'m/s'



!   Finish up.
END PROGRAM escape_vel