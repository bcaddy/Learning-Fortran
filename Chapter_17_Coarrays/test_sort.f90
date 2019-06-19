PROGRAM test_sort
  ! NOTE:  Something about the merge_sub subroutine is fucked.  This program doesn't work and that is why. All the coarray shit 
  ! works great
  !   Purpose:
  !       Sorting arrays in parallel using coarray fortran
  !       Exercise: 17.1-17.3
  !
  !   Written June 18th, 2019.  Revised June 18th, 2019

  USE merge_module, ONLY: merge_sub
  USE sort_module,  ONLY: sort
  IMPLICIT NONE !requires me to declare all variables

  ! Parameters
  INTEGER,PARAMETER :: N_SAMPLES = 100  ! Total number of samples

  ! Declare Variables
  REAL,DIMENSION(N_SAMPLES)      :: a         ! Input values to sort
  REAL,DIMENSION(:), ALLOCATABLE :: b[:]      ! Coarray for parallel sorting
  REAL,DIMENSION(:), ALLOCATABLE :: bfinal[:] ! sorting array for last image incase N_SAMPLES/NUM_IMAGES() has a remainder
  REAL,DIMENSION(N_SAMPLES)      :: out       ! Sorted output values
  REAL,DIMENSION(N_SAMPLES)      :: input    ! Sorted output values
  INTEGER                        :: i         ! Loop index
  INTEGER                        :: t1, t2, t3! temp variables
  INTEGER                        :: err       ! error code variable
  INTEGER                        :: length    ! length of each coarray b


  ! ===========================================================================
  ! Now sort using n images
  ! ===========================================================================


  ! ===== First Allocate all the Arrays =======================================
  length = N_SAMPLES/NUM_IMAGES() ! length of each coarray

  ! this just goes through and allocates the coarrays in all the images
  ALLOCATE(b(length)[*], stat=err)
  IF (err /= 0) WRITE(unit=*, fmt=*) ": Allocation request denied for coarray"
  SYNC ALL ! make sure to sync everybody


  t1 = N_SAMPLES-NUM_IMAGES()*length
  ALLOCATE(bfinal(t1)[*], stat=err)
  IF (err /= 0) WRITE(unit=*, fmt=*) ": Allocation request denied for bfinal"
  SYNC ALL


  ! ===== Create the input array using Image 1 ================================
  IF ( THIS_IMAGE() == 1 ) THEN
    ! create the data to sort
    CALL RANDOM_NUMBER(a)

    ! Copy the data into the working arrays for each image.
    t1 = 1      ! the variable for the lower limit of each array slice
    t2 = length ! the variable for the upper limit of each array slice
    DO i = 1, NUM_IMAGES()-1
      ! this takes consecutive slices of length 'length' and assigns them
      ! to each image.  To handle numbers that don't divide evenly the last
      ! image gets all the extra elements in the array
      b(:)[i] = a(t1:t2) ! assignment
      t1 = t2 + 1      ! set new lower bound
      t2 = t2 + length ! set new upper bound
    END DO

    ! copy data to the last image
    bfinal(:)[NUM_IMAGES()] = a(t1:)
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
  IF (THIS_IMAGE() /= NUM_IMAGES()) THEN
    CALL sort(b, length)
  ELSE
    CALL sort(bfinal, t1)
  END IF

  !*************************************************
  ! Wait until all images are finished
  !*************************************************
  SYNC ALL

  !*************************************************
  ! Now merge the data back into a common output
  ! array using image 1, and display the results.
  !*************************************************
  IF ( THIS_IMAGE() == 1 ) THEN
    input(:length) = b(:)[1]

    ! Merge the data
    DO i = 2, NUM_IMAGES()
      t2 = (i-1)*length
      t3 = SIZE(input(:(i-1)*length))+SIZE(b(:)[i])

      IF (i /= NUM_IMAGES()) THEN
        CALL merge_sub (input(:t2), t2, b(:)[i], SIZE(b(:)[i]), out(:t3), t3)
        write(unit=*, fmt=*) input
        write(unit=*, fmt=*) out
        input(:) = out(:)
        out(:) = 0

      ELSE
        CALL merge_sub (input(:t2), t2, bfinal(:)[i], SIZE(bfinal(:)[i]), out(:), N_SAMPLES)
      END IF

    END DO


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
