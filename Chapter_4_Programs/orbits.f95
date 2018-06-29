PROGRAM orbits
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To calculate the distance between the center of the earth and a satalite with varying angles
!       Source: Exercise 4-15

!   Parameter Decrations
REAL, PARAMETER :: pi = 3.141592653 ! pi
REAL, PARAMETER :: deg_2_rad = pi/180 ! Degree to radian conversion facter

!   Variable Declarations
REAL :: r       ! Distance between centers of mass (m)
REAL :: eta     ! eccentricity
REAL :: p       ! the semi-latus rectum (m)
REAL :: near    ! minimum separation (m)
REAL :: far = 0 ! maximum separation (m)
INTEGER :: deg  ! angle in degrees


!   Gather input info
WRITE (*,*) 'Please input a value for p and eta'
READ (*,*) p, eta

near = 10 * p ! set an absurdly large nearest distance


DO deg = 0, 360
    ! calculate r
    r = p / (1 - eta * COS(deg*deg_2_rad))

    ! find min and max values
    IF (r < near) near = r
    IF (r > far) far = r
END DO

WRITE (*,*) 'The max distance is',far/1000,'km. The min distance is', near/1000, 'km'

!   Finish up.
WRITE (*,*) 'Program complete'
END PROGRAM orbits

















































