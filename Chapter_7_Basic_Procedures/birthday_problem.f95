MODULE helper
!test code
    CONTAINS
        FUNCTION probability_finder(n)
            !   Purpose:
            !       To find the probability of n people in a room having the same birthday

            ! declarations
            REAL :: probabiity_finder
            INTEGER, INTENT(IN) :: n            ! number of people
            INTEGER, DIMENSION(n) :: bdays ! the array of birthdays
            INTEGER :: bday ! one single birthday in the array
            REAL, DIMENSION(n) :: rndm ! birthdays normalized between 0 and 1
            INTEGER, PARAMETER :: nsims = 5000 ! number of simulations to run
            INTEGER :: n_dupe ! number with duplicates
            INTEGER :: i,j        ! loop indices

            ! reset variables
            n_dupe=0

            ! loop over each simulation
            sim_loop: DO i = 1, nsims
                ! generate array of birthdays
                CALL RANDOM_NUMBER(rndm)
                bdays = INT(1 + 364*rndm)

                ! check for duplicates
                dup_loop: DO j = 1, n
                    bday = bdays(j)
                    IF (ANY(bdays(i+1:) == bday)) THEN
                        n_dupe = n_dupe + 1
                        EXIT
                    END IF
                END DO dup_loop
            END DO sim_loop

            probability_finder = REAL(n_dupe) / REAL(nsims)

        END FUNCTION probability_finder
END MODULE helper

PROGRAM birthday_problem
    USE helper
    IMPLICIT NONE !requires me to declare all variables

	!   Purpose:
	!       To find the probability of n people in a room having the same birthday and do so for many values of n
	!       Source: Exercise 7-28

	!   Variable Declarations
    INTEGER :: start_time   ! execution start time
    INTEGER :: end_time     ! execution end time
    REAL    :: prob         ! probability of two people having the same birthday in a group of n people
    INTEGER :: n            ! number of people

    !	start timer
    CALL SYSTEM_CLOCK(start_time)

    DO n = 1,40
        prob = probability_finder(n)
        WRITE (*,100) n, 100*prob
        100 FORMAT ('The chance that two people in a group of ', I3, ' people will have the same birthday is ', F6.2,' percent')
    END DO


	!   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! execution time
    end_time = end_time - start_time
    WRITE (*,999) end_time/1000., end_time/60000., end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM birthday_problem
