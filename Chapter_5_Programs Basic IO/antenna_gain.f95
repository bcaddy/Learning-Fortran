PROGRAM antenna_gain
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       Calculate the gain of an antenna as a function of angle
!       Source: Exercise 5-29

!   Parameter Decrations
REAL, PARAMETER :: pi = 3.141592653   ! pi
REAL, PARAMETER :: deg_2_rad = pi/180 ! Degree to radian conversion facter.  Used like rad=deg*deg_2_rad

!   Variable Declarations
INTEGER :: deg    ! angle in degrees
INTEGER :: status ! io status exit code
REAL :: x         ! argument of gain function
REAL :: G         ! gain


! Open the save file and check for errors on open
OPEN (UNIT=3, FILE='antenna_gain_put.txt', STATUS='REPLACE', ACTION='WRITE', IOSTAT=status)
openif: IF (status == 0) THEN ! Check if the file opened correctly

    !   Write table headers
    WRITE (3,100)
    WRITE (3,101)
    100 FORMAT (28('='),/,'Antenna Gain vs. Angle (deg)',/, 28('='),//)
    101 FORMAT ('Angle (deg)', T13, '|', T15, 'Gain (db?)',/, 24('='))

    angle_loop: DO deg = 0, 90
        x = 6 * deg * deg_2_rad

        ! Calculate g
        G_if: IF (x < 0.0005) THEN
            G = 1
        ELSE
            G = ABS(SIN(x)/x)
        END IF G_if

        WRITE (3,200) deg, G
        200 FORMAT (T10, I2, T13, '|', T15, ES10.2)


    END DO angle_loop


ELSE openif
    WRITE (*,999) status
    999 FORMAT ('Error opening file: IOSTAT = ', I6)
END IF openif

! Close the file
CLOSE (UNIT=3)



!   Finish up.
WRITE (*,'(//, A)') 'Program complete'
END PROGRAM antenna_gain