PROGRAM timer_test
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To illustrate how to time a section of Fortran code
!       Source: Exercise


!   Variable Declarations
INTEGER :: i          ! index
INTEGER :: start_t    ! starting time for the program
INTEGER :: current_t  ! current time
INTEGER :: elapsed_t  ! elapsed time
INTEGER :: end_t      ! final time

!   Timing section
CALL SYSTEM_CLOCK(start_t)

DO i = 1, 5
    CALL SLEEP(1)
    CALL SYSTEM_CLOCK(current_t)
    WRITE (*,*) current_t-start_t,'milliseconds'
END DO


!   Finish up.
WRITE (*,'(//, A)') 'Program complete'
END PROGRAM timer_test