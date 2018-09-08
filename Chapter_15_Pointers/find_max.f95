MODULE helper
    ! contains the function Arr2PtrMax and Ptr2PtrMax, thus providing an explicit interface for them
    IMPLICIT NONE

    CONTAINS
        FUNCTION Arr2PtrMax (input_values,arr_size) RESULT (return_ptr)
            ! Takes and input of a real array and returns a pointer pointing at the largest element in the array

            ! Variable Declarations
            INTEGER, INTENT(IN)                           :: arr_size     ! size of the array
            REAL, TARGET, DIMENSION(arr_size), INTENT(IN) :: input_values ! the array to be scanned
            INTEGER                                       :: i            ! loop index
            REAL, POINTER                                 :: return_ptr

            return_ptr => input_values(1)

            DO i = 2, arr_size
                IF ( input_values(i) > return_ptr ) return_ptr => input_values(i)
            END DO

        END FUNCTION Arr2PtrMax

        FUNCTION Ptr2PtrMax (input_values,arr_size) RESULT (return_ptr)
            ! Takes and input of a real array and returns a pointer pointing at the largest element in the array

            ! Variable Declarations
            INTEGER, INTENT(IN)         :: arr_size     ! size of the array
            REAL, POINTER, DIMENSION(:) :: input_values ! the array to be scanned
            INTEGER                     :: i            ! loop index
            REAL, POINTER               :: return_ptr

            return_ptr => input_values(1)

            DO i = 2, arr_size
                IF ( input_values(i) > return_ptr ) return_ptr => input_values(i)
            END DO

        END FUNCTION Ptr2PtrMax

END MODULE helper


PROGRAM find_max
    USE helper
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To test several functions that find the maximum value in an array a couple of different ways.  The function Arr2PtrMax
    !       takes a real array and returns a pointer to the maxumum value.  The function Ptr2PtrMax takes a pointer to a real array
    !       and returns a pointer to the maximum value in that array
    !       Source: Exercise 15-17 and 15-18

    ! Parameter Declarations
    INTEGER, PARAMETER :: arr_size=100 ! the size of the array

    !   Variable Declarations
    REAL, TARGET, DIMENSION(arr_size) :: values = (/6.17,47.62,56.48,94.24,10.07,72.85,31.52,24.30,75.69,48.05,95.01,67.47, &
    52.77,93.01,80.51,24.27,98.71,57.59,17.28,98.86,66.72, 5.84,12.39,88.31,14.83,73.87,15.15,52.19,8.10,54.83,96.16,56.05,70.43, &
    13.06,82.81,96.76,41.43,93.28,77.51,32.89,18.04,29.15, 1.41,40.79,14.62,38.22,26.35,53.06,25.63,86.21,87.34,3.33,3.36,25.50,  &
    29.89,32.76,49.46,30.82,20.65,95.44,16.99,82.67,19.66,67.92,58.37,41.09,23.44,97.15,4.92,92.27,90.32,65.42,85.82,62.87,51.84, &
    50.71, 2.19,94.33,73.47,63.97,97.86,42.36,58.11,88.96,30.01,18.33,59.24,19.11,21.30,60.53,19.61,52.80,86.26,41.02,74.78, 0.49,&
    49.84,18.06,38.31, 12.35/) ! the values to find the max of
    REAL, POINTER, DIMENSION(:) :: ptr_values ! a pointer to point at the varaible values
    INTEGER                     :: start_time ! execution start time
    INTEGER                     :: end_time   ! execution end time
    REAL, POINTER               :: output1    ! result of Arr2PtrMax
    REAL, POINTER               :: output2    ! result of Ptr2PtrMax

    !   start timer
    CALL SYSTEM_CLOCK(start_time)


    ! point at the array with the array pointer
    ptr_values => values

    ! call the Arr2PtrMax function and return the result
    output1 => Arr2PtrMax(values, arr_size)
    WRITE (unit=*, fmt=*) 'The maximum value from Arr2PtrMax is', output1

    ! call the Ptr2PtrMax function and return the result
    output2 => Ptr2PtrMax(ptr_values, arr_size)
    WRITE (unit=*, fmt=*) 'The maximum value from Ptr2PtrMax is', output2


    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM find_max
