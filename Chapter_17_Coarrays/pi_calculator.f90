PROGRAM pi_calculator
  !   Purpose:
  !       To calculate the value of pi using a random points method
  !       Exercise: 17.4

  ! Import Modules
  !USE module_name, ONLY: procedure_name, data_type, etc
  IMPLICIT NONE !requires me to declare all variables

  !   Parameter Decrations
  INTEGER, PARAMETER        :: dbl      = SELECTED_REAL_KIND(p=15)   ! kind number of a double precision floating point number
  REAL(kind=dbl), PARAMETER :: pi_true  = 3.1415926535897932_dbl     ! pi
  INTEGER, PARAMETER        :: Nsamples = 10**6                      ! number of position samples to get at once

  !   Variable Declarations
  INTEGER                               :: start_time           ! execution start time
  INTEGER                               :: end_time             ! execution end time
  INTEGER                               :: i=1                  ! loop indices
  REAL(kind=dbl), CODIMENSION[*]        :: Nsq, Ncir            ! Counter for the number of points in the square and circle respectively
  REAL(kind=dbl)                        :: Nsq_total, Ncir_total! Counter for the total number of points in the square and circle respectively
  REAL(kind=dbl), DIMENSION(Nsamples,2) :: pos                  ! positions for each trial, pos(n,1) is the nth x value
  REAL(kind=dbl)                        :: radius               ! variable for calculating the radius that a point is at
  REAL(kind=dbl), CODIMENSION[*]        :: pi                   ! the value of pi we are calculating
  ! ===== END OF DECLARTION SECTION ===========================================

  !   start timer
  IF (THIS_IMAGE() == 1) CALL SYSTEM_CLOCK(start_time)
  SYNC ALL

  main: DO
    ! This loop generates the random numbers, scales them, and determines which positions are within the circle

    ! CRITICAL ! this critical block seems to slow down execution a little bit, less noticable on longer runs
      ! before calling RANDOM_NUMBER we need to set the RANDOM_SEED
      CALL RANDOM_SEED()

      CALL RANDOM_NUMBER(pos)
    ! END CRITICAL

    ! set the range of the position correctly
    pos = pos*2 - 1

    DO i = 1, Nsamples
      ! scan through each position and update the N values
      Nsq = Nsq+1.

      radius = SQRT(pos(i,1)**2 + pos(i,2)**2)
      IF (radius < 1_dbl) Ncir = Ncir+1.

    END DO

    ! get everybody synced up to pass their data to Image 1
    SYNC ALL


    IF (THIS_IMAGE() == 1) THEN
      ! sum all the different images values for Nsq and Ncir
      Nsq_total  = 0.
      Ncir_total = 0.

      DO i = 1, NUM_IMAGES()
        Nsq_total  = Nsq_total + Nsq[i]
        Ncir_total = Ncir_total + Ncir[i]
      END DO

      pi = 4. * Ncir_total / Nsq_total
    END IF

    ! Before we do another iteration lets get everybody synced again
    SYNC ALL

    IF (ABS(pi[1]-pi_true) <= 5E-8_dbl) THEN
      IF (THIS_IMAGE() == 1) WRITE(unit=*, fmt=*) 'The calculated value for pi is: ', pi
      EXIT
    END IF


  END DO main




  IF (THIS_IMAGE() == 1) THEN
    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
  END IF

END PROGRAM pi_calculator
