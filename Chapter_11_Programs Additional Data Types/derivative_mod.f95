MODULE derivative_mod
    ! Purpose
    !   module for derivative.f95
    !   exercise:  11-6


    IMPLICIT NONE

    ! define double precision
    INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(p=13) ! kind number of double

    CONTAINS
        REAL(kind=dbl) FUNCTION func (x)
            ! the function to which the derivative is taken of.  This can be any mathematical function

            ! Declarations
            REAL(kind=dbl), INTENT(IN) :: x ! the point at which to find the value of the function

            func = 10._dbl*SIN(20._dbl*x)

        END FUNCTION func

        SUBROUTINE derivative_calculator (func, x0, dx, deriv)
            ! This subroutine calculates the derivative of the function func at the point x0 with steps dx

            ! Variable Declarations
            REAL(kind =dbl), INTENT(IN)  :: x0    ! point at which to find the derivative
            REAL(kind =dbl), INTENT(IN)  :: dx    ! step size
            REAL(kind =dbl)              :: func  ! the function to take the derivative of
            REAL(kind =dbl), INTENT(OUT) :: deriv ! the result of taking the derivative

            deriv = (func(x0+dx) - func(x0))/dx


        END SUBROUTINE derivative_calculator
END MODULE derivative_mod
