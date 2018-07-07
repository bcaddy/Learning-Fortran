MODULE grav_force_wrapper

CONTAINS
    REAL FUNCTION grav_force (m1, m2, r)
    IMPLICIT NONE !requires me to declare all variables
    !   Purpose:
    !       To find the gravitational force between two objects
    !       Source: Exercise 7-34

    !   Parameter Decrations
    REAL, PARAMETER :: G = 6.672E-11 ! Gravitational constant (N m^2 kg^-2)

    !   Variable Declarations
    REAL, INTENT(IN) :: m1   ! mass of object 1 (kg)
    REAL, INTENT(IN) :: m2   ! mass of object 2 (kg)
    REAL, INTENT(IN) :: r    ! distance between the two objects (m)

    !   Compute the gravitational force
    grav_force = G * m1 * m2 / (r**2)

    !   Finish up.
    END FUNCTION grav_force
END MODULE grav_force_wrapper
! ========================================================================================================================
PROGRAM grav_force_test_driver
USE grav_force_wrapper
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To find the gravitational force between two objects using a Fortran function
!       Source: Exercise

!   Parameter Decrations
REAL, PARAMETER :: M_earth = 5.98E24    ! mass of the earth (kg)

!   Variable Declarations
!REAL :: grav_force  ! function declaration
REAL :: m_sat = 800 ! mass of the satalite (kg)
REAL :: r = 3.8E7   ! separation (m)
REAL :: F

!   Use the function
F = grav_force (M_earth, m_sat, r)

WRITE (*,*) F


!   Finish up.
WRITE (*,'(//, A)') 'Program complete'
END PROGRAM grav_force_test_driver