PROGRAM currency_converter
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To illustrate some of the basic features of a Fortran Program
!       Source: Exercise 3-10

!   Parameter Decrations
REAL, PARAMETER :: a2u = 0.56 ! Australian to USD conversion A$*A2U=USD
REAL, PARAMETER :: e2u = 0.97 ! Euros to USD conversion e$*e2U=USD
REAL, PARAMETER :: p2u = 1.62 ! UK pounds to USD conversion P$*p2U=USD

!   Variable Declarations
REAL :: in_amount  ! amount of input currency to convert
REAL :: out_amount ! amount of output currency
CHARACTER(9) :: input_cur  ! type of input currency
CHARACTER(9) :: output_cur ! type of output currency

!   Gather info from user
WRITE (*,*) 'How much currency is to be converted and what currency is it? (USD, Euro, or UKP)'
READ (*,*) in_amount, input_cur

WRITE (*,*) 'What is the desired output currency? (USD, Euro, or UKP)'
READ (*,*) output_cur

!   Convert to USD
x2u: SELECT CASE (input_cur)
    CASE ('USD','usd')
        out_amount = in_amount
    CASE ('Euro','euro')
        out_amount = in_amount  * e2u
    CASE ('UKP','ukp')
        out_amount = in_amount * p2u
    CASE DEFAULT
        WRITE (*,*) 'Invalid Input'
END SELECT x2u

!   Convert to desired output
u2x: SELECT CASE (output_cur)
    CASE ('USD', 'usd')
        WRITE (*,*) 'That is equivalent to',out_amount,'USD'
    CASE ('Euro', 'euro')
        out_amount = out_amount / e2u
        WRITE (*,*) 'That is equivalent to',out_amount,'Euros'
    CASE ('UKP', 'ukp')
        out_amount = out_amount / p2u
        WRITE (*,*) 'That is equivalent to',out_amount,'UK Pounds'
    CASE DEFAULT
        WRITE (*,*) 'Invalid Input'
END SELECT u2x



!   Finish up.
WRITE (*,*) 'Program complete'
END PROGRAM currency_converter


























