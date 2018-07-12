PROGRAM class_signup
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       Example of how to use SELECT CASE construct
!       Source: Exercise 3-8

!   Variable Declarations
CHARACTER(10) :: classy  ! Class to sign up for

!   Get user input
WRITE (*,*) 'Please enter the name of the class you want to sign up to take.  Your options are English, History, Astronomy, or &
&Literature'
READ (*,*) classy


!   Respond
confirmer: SELECT CASE (classy)
    CASE ('English')
        WRITE (*,*) 'You have registered for English'
    CASE ('History')
        WRITE (*,*) 'You have registered for History'
    CASE ('Astronomy')
        WRITE (*,*) 'You have registered for Astronomy'
    CASE ('Literature')
        WRITE (*,*) 'You have registered for Literature'
    CASE DEFAULT
        WRITE (*,*) 'Invalid input.  Exiting'
END SELECT confirmer





!   Finish up.
WRITE (*,*) 'Program complete'
END PROGRAM class_signup