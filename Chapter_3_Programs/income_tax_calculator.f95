PROGRAM income_tax_calculator
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To determine the amount of income tax an individual must pay in Australia
!       Source: Exercise 3-9

!   Parameter Decrations
REAL, PARAMETER :: medicare = 0.015 ! percent of income that is taxed for medicare

!   Variable Declarations
REAL :: income  ! annual income (A$)
REAL :: levy    ! amount in medicare levy
REAL :: tax     ! amount taxed

!   Determine user's income
WRITE (*,*) 'Input your income'
READ (*,*) income

!   Determine medicare levy
levy = medicare * income

!   Determine income tax
taxer: IF (income < 6001.) THEN
    tax = 0
    WRITE (*,*) 'Your income tax is',tax,'.  Your medicare levy is',levy,'.  Your total tax payable is',levy+tax
ELSE IF (6001 <= income .AND. income < 20001) THEN
    tax = (income-6000) * .17
    WRITE (*,*) 'Your income tax is',tax,'.  Your medicare levy is',levy,'.  Your total tax payable is',levy+tax
ELSE IF (20001 <= income .AND. income < 50001) THEN
    tax = 2380 + (income-20000) * .30
    WRITE (*,*) 'Your income tax is',tax,'.  Your medicare levy is',levy,'.  Your total tax payable is',levy+tax
ELSE IF (50001 <= income .AND. income <= 60000) THEN
    tax = 11380 + (income-50000) * .42
    WRITE (*,*) 'Your income tax is',tax,'.  Your medicare levy is',levy,'.  Your total tax payable is',levy+tax
ELSE IF (60000 < income) THEN
    tax = 15580 + (income-60000) * .47
    WRITE (*,*) 'Your income tax is',tax,'.  Your medicare levy is',levy,'.  Your total tax payable is',levy+tax
END IF taxer


!   Finish up.
WRITE (*,*) 'Program complete'
END PROGRAM income_tax_calculator