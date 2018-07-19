MODULE die_thrower
    ! This module provides an explicit interface to the throw sub subroutine which returns two random values, each between 1 and 6,
    ! to simulate throwing actual dice
    !       Source: Exercise 9-11

    IMPLICIT NONE

    Contains
        SUBROUTINE throw(d1, d2)
            ! This subroutine simulates rolling two D6 dice.  It returns those values in the variables d1 and d2 and contains a internal
            ! subroutine for the actual random number generation

            ! Variable Declarations
            INTEGER, INTENT(OUT)               :: d1, d2 ! the values of the dice
            INTEGER                            :: size   ! size of the random seed
            INTEGER, ALLOCATABLE, DIMENSION(:) :: seed   ! the random seed
            INTEGER, DIMENSION(8)              :: times  ! get a value for random seed
            INTEGER                            :: err    ! error code

            ! before calling RANDOM_NUMBER we need to set the RANDOM_SEED
            CALL RANDOM_SEED(size=size) ! get the size of the seed

            ALLOCATE(seed(size), stat=err) ! allocate seed array
            IF ( err /= 0) WRITE (*,*) ": Allocation request denied"

            CALL DATE_AND_TIME(values=times) ! get value for seed
            seed(:) = times(8) ! set all values of seed to the millisecond value of time

            CALL RANDOM_SEED(put=seed) ! set the seed

            IF (ALLOCATED(seed)) DEALLOCATE(seed, stat=err) ! deallocate seed array
            IF ( err /= 0) WRITE (*,*) ": Deallocation request denied"

            ! get the random values
            d1 =  die()
            d2 =  die()

        END SUBROUTINE throw

        INTEGER FUNCTION die ()
            ! this function returns a random integer in the range [1, 6]

            ! Variable Decrations
            REAL                 :: dfloat ! float value of the die



            CALL RANDOM_NUMBER(dfloat) ! generate a random number [0,1]

            ! convert that random float to a random int in the right range
            die = FLOOR(dfloat*6.) + 1
        END FUNCTION die

END MODULE die_thrower
gfortran -std=f95 -Wextra -Wall -pedantic -fcheck=all -Og -fbacktrace .f95 -o 
