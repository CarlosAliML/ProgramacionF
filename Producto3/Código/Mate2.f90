!Math . f90 : demo some Fortran math functions
! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
Program Math_test ! Begin main program
  Real *8 :: x = -1.0 , y=2.0, z=0 ! Declare variables x, y, z, a, b, c
  a = sqrt (x) ! Call the sine function
  b = asin (y) ! Call the exponential function
  c = log (z)
  print * , x, y, z, a, b, c ! Print x, y, z
End Program Math_test ! End main program 
