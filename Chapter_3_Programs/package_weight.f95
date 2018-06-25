PROGRAM package_weight
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       To find the cost to ship a package
!       Source: Exercise 3-5

!   Variable Declarations
REAL :: w  ! weight (lbs)

!   Gather weight from user
WRITE (*,*) 'Please input the weight of the package in pounds:'
READ (*,*) w

!   If statement to determine cost
price_selector: IF (w <= 2) THEN
        WRITE (*,*) 'The cost is $10.00'
    ELSE IF (2 < w .AND. w <= 70) THEN
        WRITE (*,*) 'The cost is',10. + 3.75 * (w-2.)
    ELSE IF (70 < w .AND. w <= 100) THEN
        WRITE (*,*) 'The cost is',20. + 3.75 * (w-2.)
    ELSE IF (100 < w) THEN
        WRITE (*,*) 'The package is overweight. The maximum weight is 100lbs'
END IF price_selector


!   Finish up.
END PROGRAM package_weight