PROGRAM my_first_program
!   Purpose:
!       To illustrate some of the basic features of a Fortran Program
!       Source: Page 23

!   Declare the variables used in the program
INTEGER :: i,j,k  ! All variables are integers

!   Get two values to store in variables i and J
WRITE (*,*) 'Enter the numbers to multiply: '
READ (*,*) i,j

!   Multiply the numbers together
k = i * j

!   Write out the result
WRITE (*,*) 'Result = ',k

!   Finish up.
STOP
END PROGRAM my_first_program




