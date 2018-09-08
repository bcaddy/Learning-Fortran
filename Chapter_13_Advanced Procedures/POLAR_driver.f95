MODULE helper
    ! Contains the type definition for the POLAR type along with the extension for it to include the
    ! operators =, *, and /
    IMPLICIT NONE

    ! set which things are public vs private
    PRIVATE
    PUBLIC :: POLAR, ASSIGNMENT(=), OPERATOR(*), OPERATOR(/)

    ! define POLAR type
    TYPE :: POLAR
        REAL :: mag
        REAL :: phase
    END TYPE POLAR

    ! extend assignment
    INTERFACE ASSIGNMENT (=)
        MODULE PROCEDURE converter
    END INTERFACE

    ! extend multiplication
    INTERFACE OPERATOR (*)
        MODULE PROCEDURE multiplication
    END INTERFACE

    ! extend division
    INTERFACE OPERATOR (/)
        MODULE PROCEDURE division
    END INTERFACE


    CONTAINS
        SUBROUTINE converter (polar_value, cartesian_value)
            ! this subroutine converts a complex number from polar to cartesian coordinates
            ! for use in extended variable assignment to support the POLAR data type

            ! declarations
            COMPLEX, INTENT(IN)       :: cartesian_value ! the cartesian complex number
            TYPE (POLAR), INTENT(OUT) :: polar_value     ! the complex number that is in polar form

            polar_value%mag   = CABS(cartesian_value)
            polar_value%phase = ATAN2(AIMAG(cartesian_value), REAL(cartesian_value))
        END SUBROUTINE converter

        TYPE (POLAR) FUNCTION multiplication(lhs, rhs)
            ! perform complex multiplication on complex numbers in polar format

            ! Declarations
            TYPE (POLAR), INTENT(IN) :: lhs ! left hand side of operator
            TYPE (POLAR), INTENT(IN) :: rhs ! right hand side of operator

            multiplication%mag = lhs%mag * rhs%mag
            multiplication%phase = lhs%phase + rhs%phase

        END FUNCTION multiplication

        TYPE (POLAR) FUNCTION division(lhs, rhs)
            ! perform complex division on complex numbers in polar format

            ! Declarations
            TYPE (POLAR), INTENT(IN) :: lhs ! left hand side of operator
            TYPE (POLAR), INTENT(IN) :: rhs ! right hand side of operator

            division%mag = lhs%mag / rhs%mag
            division%phase = lhs%phase - rhs%phase

        END FUNCTION division

END MODULE helper


PROGRAM POLAR_driver
    USE helper
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To convert a complex number to from cartesian to polar coordinates
    !       Source: Exercise 11-10

    !   Variable Declarations
    INTEGER      :: start_time             ! execution start time
    INTEGER      :: end_time               ! execution end time
    COMPLEX      :: cartesian1, cartesian2 ! complex number in cartesian coordinates
    TYPE (POLAR) :: result1, result2       ! complex number in polar coordinates
    TYPE (POLAR) :: test_value             ! value of tests

    !   start timer
    CALL SYSTEM_CLOCK(start_time)

    ! intialize the complex number
    cartesian1 = (3., 4.)
    cartesian2 = (5., 6.)

    ! test assignment
    result1 = cartesian1
    result2 = cartesian2
    WRITE (unit=*, fmt=*) 'The complex number result1 in polar coordinates is', result1
    WRITE (unit=*, fmt=*) 'The complex number result2 in polar coordinates is', result2

    ! test multiplication
    test_value = result1 * result2
    WRITE (unit=*, fmt=*) 'result1 * result2 is', test_value

    ! test division
    test_value = result1 / result2
    WRITE (unit=*, fmt=*) 'result1 / result2 is', test_value


    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM POLAR_driver
