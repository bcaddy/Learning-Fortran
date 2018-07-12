PROGRAM set_theory
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       Learning how to read and write arrays from files
!       Source: Exercise 6-19

!   Variable Declarations
INTEGER, DIMENSION(10) :: setA    ! Set A
INTEGER, DIMENSION(9) :: setB    ! Set B
INTEGER, DIMENSION(19) :: union=-999   ! union of A and B
INTEGER, DIMENSION(19) :: inter=-999   ! intersection of A and B
INTEGER :: i, k                ! indexing variables
INTEGER :: status

!   #####  Read in Data   ######
1040 FORMAT ('Error opening file: IOSTAT = ', I6) ! opening error format
! ################################################################
! Read in Set A
! Open the file and check for errors on open
OPEN (UNIT=3, FILE='setA.dat', STATUS='OLD', ACTION='READ', IOSTAT=status)

openifA: IF (status == 0) THEN ! Check if the open was ok
    READ (3,*) (setA(i), i = 1, 10)  ! read in the set A data
ELSE openifA
    WRITE (*,1040) status
END IF openifA

! Close the file
CLOSE (UNIT=3)
! Finished reading in Set A

! Read in Set B
! Open the file and check for errors on open
OPEN (UNIT=3, FILE='setB.dat', STATUS='OLD', ACTION='READ', IOSTAT=status)

openifB: IF (status == 0) THEN ! Check if the open was ok
    READ (3,*) (setB(i), i = 1, 9)  ! read in the set A data
ELSE openifB
    WRITE (*,1040) status
END IF openifB

! Close the file
CLOSE (UNIT=3)
! Finished reading in Set A
! ################################################################




!   #####  Calculations  #####
! ################################################################
! Find the intersection
k=1
interloop: DO i = 1, 10
    IF ( ANY(setB == setA(i)) .AND. (.NOT. ANY(inter==setA(i))) ) THEN
        inter(k) = setA(i)
        k = k + 1
    END IF
END DO interloop

! Find the union
k=11
union(:10) = setA
unionloop: DO i= 1, 9
    IF ( .NOT. ANY(union == setB(i)) ) THEN
        union(k) = setB(i)
        k = k+1
    END if
END DO unionloop
! ################################################################



!   #####  Write out the Data   ######
! ################################################################
! Write out the interesection of A and B
! Open the file and check for errors on open
OPEN (UNIT=3, FILE='intersection.dat', STATUS='REPLACE', ACTION='WRITE', IOSTAT=status)

openif_inter: IF (status == 0) THEN ! Check if the open was ok
    DO i = 1, 19
        IF (inter(i) /= -999) WRITE (3,'(I3)') inter(i)
    END DO
ELSE openif_inter
    WRITE (*,1040) status
END IF openif_inter

! Close the file
CLOSE (UNIT=3)
! Finished reading in Set A

! Write out the union of A and B
! Open the file and check for errors on open
OPEN (UNIT=3, FILE='union.dat', STATUS='REPLACE', ACTION='WRITE', IOSTAT=status)

openif_union: IF (status == 0) THEN ! Check if the open was ok
    DO i = 1, 19
        IF (union(i) /= -999) WRITE (3,'(I3)') union(i)
    END DO

ELSE openif_union
    WRITE (*,1040) status
END IF openif_union

! Close the file
CLOSE (UNIT=3)
! Finished reading in Set A
! ################################################################

!   Finish up.
WRITE (*,'(//, A)') 'Program complete'
END PROGRAM set_theory