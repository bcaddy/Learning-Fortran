PROGRAM read_file
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To illustrate how to read an unknown number of values from an input data file, detecting both any formatting errors and the
!       end of the file
!       Source: Example 5-3

!   Variable Declarations
CHARACTER(100) :: filename  ! Name of file to open
INTEGER        :: nvals = 0 ! Number of vaqlues read in
INTEGER        :: status    ! I/O status
REAL           :: value     ! the real value read in

!   Get the file name and echo it back to the user
WRITE (*,*) 'Please enter the input file name'
READ (*,'(A)') filename
WRITE (*,1000) filename
1000 FORMAT ('The input filename is: ', A)

! Open the file and check for errors on open
OPEN (UNIT=3, FILE=TRIM(filename), STATUS='OLD', ACTION='READ', IOSTAT=status)

openif: IF (status == 0) THEN
    ! Open was ok
    readloop: DO
        READ (3,*,IOSTAT=status) value  !Get next value
        IF (status /= 0) EXIT ! EXIT if not valid
        nvals = nvals+1
        WRITE (*,1010) nvals, value
        1010 FORMAT ('Line', I6, ': Value= ',F10.4)
    END DO readloop

    ! The WHILE loop has terminated.  Was it because of a READ error or becuase of the end of input file?
    readif: IF (status > 0) THEN ! a read error occured. Tell user

        WRITE (*,1020) nvals+1
        1020 FORMAT ('An error occured reading line ',I6)
    ELSE ! the end of the data was reached. Tell user
        WRITE (*,1030) nvals
        1030 FORMAT ('End of the file reached. There were ', I6, ' values in the file.')

    END IF readif

ELSE openif
    WRITE (*,1040) status
    1040 FORMAT ('Error opening file: IOSTAT = ', I6)
END IF openif

! Close the file
CLOSE (UNIT=3)

!   Finish up.
WRITE (*,*) 'Program complete'
END PROGRAM read_file
