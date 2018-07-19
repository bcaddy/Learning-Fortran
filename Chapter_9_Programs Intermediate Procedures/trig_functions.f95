MODULE function_container
    ! Purpose
    !   To hold a bunch of elemental functions
    IMPLICIT NONE
    CONTAINS
        ELEMENTAL REAL FUNCTION cosine(x)
            ! calculates the cosine of x
            REAL, INTENT(IN) :: x ! input value

            ! calculate trig functions
            cosine = COS(x)

        END FUNCTION cosine

        ELEMENTAL REAL FUNCTION sine(x)
            ! calculates the sine of x
            REAL, INTENT(IN) :: x ! input value

            ! calculate trig functions
            sine = SIN(x)
        END FUNCTION sine

        ELEMENTAL REAL FUNCTION tangent(x)
            ! calculates the tangent of x
            REAL, INTENT(IN) :: x ! input value

            ! calculate trig functions
            tangent = TAN(x)
        END FUNCTION tangent

        ELEMENTAL REAL FUNCTION arccosine(x)
            ! calculates the arccosine of x
            REAL, INTENT(IN) :: x ! input value

            ! calculate trig functions
            arccosine = ACOS(x)
        END FUNCTION arccosine

        ELEMENTAL REAL FUNCTION arcsine(x)
            ! calculates the arcsine of x
            REAL, INTENT(IN) :: x ! input value

            ! calculate trig functions
            arcsine = ASIN(x)
        END FUNCTION arcsine

        ELEMENTAL REAL FUNCTION arctangent(x)
            ! calculates the arctangent of x
            REAL, INTENT(IN) :: x ! input value

            ! calculate trig functions
            arctangent = ATAN(x)
        END FUNCTION arctangent

END MODULE function_container


PROGRAM trig_functions
    USE function_container
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To define and use elemental functions to calculate sine, cosine, tangent, arcsine, arccosine, and arctangent
    !       Source: Exercise 9-12 and 9-13

    !   Variable Declarations
    INTEGER              :: start_time ! execution start time
    INTEGER              :: end_time   ! execution end time
    INTEGER              :: i          ! loop index
    REAL, DIMENSION(2,3) :: arr1 = RESHAPE((/10., 40., 20., 50., 30., 60./), shape=(/2,3/)) ! source array
    REAL, DIMENSION(2,3) :: storage ! temporary storage array

    !   start timer
    CALL SYSTEM_CLOCK(start_time)

    WRITE (*,*) "The array is"
    WRITE (unit=*, fmt='(3F14.9,/)') (arr1(i,:), i = 1, 2)

    ! execute functions
    storage = cosine(arr1)
    WRITE (*,*) "The cosine of the array is"
    WRITE (unit=*, fmt='(3F14.9,/)') (storage(i,:), i=1,2)

    storage = sine(arr1)
    WRITE (*,*) "The sine of the array is"
    WRITE (unit=*, fmt='(3F14.9,/)') (storage(i,:), i=1,2)

    storage = tangent(arr1)
    WRITE (*,*) "The tangent of the array is"
    WRITE (unit=*, fmt='(3F14.9,/)') (storage(i,:), i=1,2)

    storage = arccosine(cosine(arr1))
    WRITE (*,*) "The arccosine of the array is"
    WRITE (unit=*, fmt='(3F14.9,/)') (storage(i,:), i=1,2)

    storage = arcsine(sine(arr1))
    WRITE (*,*) "The arcsine of the array is"
    WRITE (unit=*, fmt='(3F14.9,/)') (storage(i,:), i=1,2)

    storage = arctangent(tangent(arr1))
    WRITE (*,*) "The arctangent of the array is"
    WRITE (unit=*, fmt='(3F14.9,/)') (storage(i,:), i=1,2)



    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM trig_functions
