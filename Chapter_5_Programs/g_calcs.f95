PROGRAM g_calcs
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To Calculate the acceleration due to gravity at many points above the Earth's surface and write the results to a file
!       Source: Exercise 5-14

!   Parameter Decrations
REAL, PARAMETER :: G = 6.672E-11 ! Gravitational constant (N m^2 kg^-2)
REAL, PARAMETER :: M = 5.98E24   ! Mass of the earth (kg)
REAL, PARAMETER :: R = 6371.E3    ! Mean radius of the earth (m)

!   Variable Declarations
INTEGER :: h  ! height above the surface (m)
REAL :: ga  ! acceleration due to gravity (m s^-2)
INTEGER :: status ! IO status


! Open the save file and check for errors on open
OPEN (UNIT=3, FILE='g_calc_output.txt', STATUS='REPLACE', ACTION='WRITE', IOSTAT=status)
openif: IF (status == 0) THEN ! Check if the file opened correctly

    !   Write table headers
    WRITE (3,100)
    100 FORMAT ('Height (km)', T13, '|', T15, 'Acceleration due to gravity (m/s^2)',/, 49('='))




    !   Calculate values of g
    calcloop: DO h = 0, 40000000, 500000

        ga = - G * M / ((R + h)**2)

        WRITE (3,200) h/1000, ga
        200 FORMAT (I5,' km', T13, '|', T25, ES9.2, ' m/s^2')


    end do calcloop




ELSE openif
    WRITE (*,999) status
    999 FORMAT ('Error opening file: IOSTAT = ', I6)
END IF openif

! Close the file
CLOSE (UNIT=3)

!   Finish up.
WRITE (*,'(//, A)') 'Program complete'
END PROGRAM g_calcs
