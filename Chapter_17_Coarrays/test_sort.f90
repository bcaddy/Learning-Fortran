PROGRAM test_sort
  !   Purpose:
  !       Sorting arrays in parallel using coarray fortran
  !       Exercise: 17.1-17.3
  !
  !   Written June 18th, 2019.  Revised June 18th, 2019

  USE merge_module, ONLY: merge_sub
  USE sort_module, ONLY: sort
  IMPLICIT NONE !requires me to declare all variables

  ! Parameters
  INTEGER,PARAMETER :: N_SAMPLES = 100000

  ! Declare Variables
  REAL,DIMENSION(N_SAMPLES)   :: a   ! Input values to sort
  REAL,DIMENSION(N_SAMPLES/2) :: b[*]! Coarray for parallel sorting
  REAL,DIMENSION(N_SAMPLES)   :: out ! Sorted output values
  INTEGER                     :: i   ! Loop index
  ! INTEGER                     :: m   ! Number of values to sort

  ! ===========================================================================
  ! Now sort using 2 images
  ! ===========================================================================

  ! ===== Create the input array using Image 1 ================================
  IF ( this_image() == 1 ) THEN
    ! Allocate the data to sort
    CALL RANDOM_NUMBER(a)

    ! Copy the data into the working arrays ! for each image.
    b(:)[1] = a(1:N_SAMPLES/2)
    b(:)[2] = a(N_SAMPLES/2+1:N_SAMPLES)

  END IF

  !*************************************************
  ! Synchronize all images during the creation of
  ! the input data
  !*************************************************
  SYNC ALL

  !*************************************************
  ! Now all images can run in parallel.
  ! Sort the data in each image
  !*************************************************
  CALL sort( b, N_SAMPLES/2 )

  !*************************************************
  ! Wait until all images are finished
  !*************************************************
  SYNC ALL

  !*************************************************
  ! Now merge the data back into a common output
  ! array using image 1, and display the results.
  !*************************************************
  IF ( this_image() == 1 ) THEN
    ! Merge the data
    CALL merge_sub ( b(:)[1], N_SAMPLES/2, b(:)[2], N_SAMPLES/2, out, N_SAMPLES )

    ! Display first 5 samples
    WRITE (*,'(A)') 'First 5 samples:'
    DO i = 1, 5
      WRITE (*,'(F10.6)') out(i)
    END DO

    ! Display last 5 samples
    WRITE (*,'(A)') 'Last 5 samples:'
    DO i = N_SAMPLES-4, N_SAMPLES
      WRITE (*,'(F10.6)') out(i)
    END DO

  END IF


END PROGRAM test_sort
