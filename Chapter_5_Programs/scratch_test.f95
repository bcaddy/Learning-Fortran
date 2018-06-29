PROGRAM scratch_test
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To play with scratch files and moving around in a file and learn how BACKSPACE works
!       Source: Exercise 5-20


!   Variable Declarations
INTEGER :: i  ! indexing
INTEGER :: x
INTEGER :: y
INTEGER :: status ! io status


OPEN (UNIT=3, STATUS='SCRATCH', IOSTAT=status)
openif: IF (status == 0) THEN ! Check if the file opened correctly

    !   Write values to the file
    write_loop: DO i = 1, 10
        WRITE (3,*) i
    end do write_loop

    !   assign a value to x
    DO i = 1,6
        BACKSPACE (UNIT=3)
    END DO
    READ (3,*) x

    !   assign a value to y
    DO i = 1,3
        BACKSPACE (UNIT=3)
    END DO
    READ (3,*) y

    !   Return the value of x*y
    WRITE (*,100) x*y
    100 FORMAT ('The value of x*y is ',I2)




ELSE openif
    WRITE (*,999) status
    999 FORMAT ('Error opening file: IOSTAT = ', I6)
END IF openif

! Close the file
CLOSE (UNIT=3)

!   Finish up.
WRITE (*,'(//, A)') 'Program complete'
END PROGRAM scratch_test