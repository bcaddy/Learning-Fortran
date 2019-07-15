PROGRAM double_tester
  !   Purpose:
  !       To demonstrate different kinds of utilizing doubles
  !       Project:

  !   Parameter Decrations
  INTEGER, PARAMETER        :: dbl       = SELECTED_REAL_KIND(p = 15) ! kind number of a double precision floating point number
  INTEGER, PARAMETER        :: dblint    = SELECTED_INT_KIND(18)      ! kind number of a double precision floating interger number

  !   Variable Declarations
  REAL(kind=dbl) :: a,b,c,d
  REAL*8 :: g
  REAL :: f
  ! ===== END OF DECLARTION SECTION ===========================================

  a = 3.1415926535897932_dbl
  g = 3.14159265358979323846D0
  f = 3.14159265358979323846E0



  WRITE (unit=*, fmt=*) a, f
  WRITE (unit=*, fmt='(2F20.16)') g, acos(-1._dbl)
  WRITE (unit=*, fmt=*) 3.14159265358979323846 !single precision
  WRITE (unit=*, fmt=*) 3.14159265358979323846D0 !double
  WRITE (unit=*, fmt=*) 3.14159265358979323846_dbl !double
  WRITE (unit=*, fmt=*) 3.14159265358979323846E0_dbl !double
  ! WRITE (unit=*, fmt=*) 3.14159265358979323846D0_dbl !does not work




END PROGRAM double_tester
