!------------------------------------------------------------------
!	This program provides testing coverage for the isequal function  
!			 
!	Uses: FUNCTION isequal(a, b, tolerance) 
!   	  FUNCTION asserttrue(cond1, counter)
!	      FUNCTION assertfalse(cond1, counter) 	
!	      	
!	Test cases are written using two assert functions to determine whether
!	the isequal function is correctly evaluating to the expected Boolean 
!	for two floating point values. The assert functions are purely for code 
!	readability, and written to	avoid importing external libraries. The second 
! argument to the assertion functions is to keep track of failing test cases
! and the ability to provide an exit code if any tests do fail.
!	For testing purposes, the tolerance value will be set at .000001 (.000001D0).
!
!	Note- When tolerance value is increased, or is greater than the floats
!	being compared, isequal() does not behave as expected.
!------------------------------------------------------------------
program equaltest

  use isequalmodule

  implicit none

  real*8  :: tol
  integer :: failureCounter
  
  ! Set the tolerance value for the isequal function and set failureCounter to zero. 
  tol = .000001D0
  failureCounter = 0

  print*, "Tests for large Numbers"
  print*, asserttrue(isequal(10000000D0, 10000001D0, tol), failureCounter), "a=", 10000000D0,"b=", 10000001D0, "tol=,  ", tol
  print*, asserttrue(isequal(10000001D0, 10000000D0, tol), failureCounter), "a=", 10000001D0,"b=", 10000000D0 , "tol=,  ", tol
  print*, assertfalse(isequal(10001D0, 10000D0, tol), failureCounter), "a=", 10001D0,"b=", 10000D0 , "tol=,  ", tol
  print*, assertfalse(isequal(10000D0, 10001D0, tol), failureCounter), "a=", 10000D0,"b=", 10001D0 , "tol=,  ", tol
  print*, "_______________________________________"

  print*, "Tests for Negative large numbers"
  print*, asserttrue(isequal(-10000000D0, -10000001D0, tol), failureCounter), "a=", -10000000D0, "b=", -10000001D0, "tol=,  ", tol
  print*, asserttrue(isequal(-10000000D0, -10000001D0, tol), failureCounter), "a=", -10000000D0, "b=", -10000001D0, "tol=,  ", tol
  print*, assertfalse(isequal(-10000D0, -10001D0, tol), failureCounter) , "a=", -10000D0, "b=", -10001D0, "tol=,  ", tol
  print*, assertfalse(isequal(-10000D0, -10001D0, tol), failureCounter), "a=", -10000D0, "b=", -10001D0, "tol=,  ", tol
  print*, "_______________________________________"

  print*, "Tests for Numbers around -1"
  print*, asserttrue(isequal(-1.0000001D0, -1.0000002D0, tol), failureCounter), "a=", -1.0000001D0, "b=", -1.0000002D0
  print*, "tol=,  ", tol
  print*, asserttrue(isequal(-1.0000002D0, -1.0000001D0, tol), failureCounter), "a=", -1.0000002D0, "b=", -1.0000001D0
  print*, "tol=,  ", tol
  print*, assertfalse(isequal(-10000D0, -10001D0, tol), failureCounter) , "a=", -10000D0, "b=", -10001D0, "tol=,  ", tol
  print*, assertfalse(isequal(-10000D0, -10001D0, tol), failureCounter), "a=", -10000D0, "b=", -10001D0, "tol=,  ", tol
  print*, "_______________________________________" 

  print*, "Tests for Numbers around 1"
  print*, asserttrue(isequal(1.0000001D0, 1.0000002D0, tol), failureCounter), "a=", 1.0000001D0, "b=", 1.0000002D0, "tol=,  ", tol
  print*, asserttrue(isequal(1.0000002D0, 1.0000001D0, tol), failureCounter), "a=", 1.0000002D0, "b=", 1.0000001D0, "tol=,  ", tol
  print*, assertfalse(isequal(10000D0, 10001D0, tol), failureCounter) , "a=", 10000D0, "b=", 10001D0, "tol=,  ", tol
  print*, assertfalse(isequal(10000D0, 10001D0, tol), failureCounter), "a=", 10000D0, "b=", 10001D0, "tol=,  ", tol
  print*, "_______________________________________" 

  print*, "Tests for Numbers between 1 and 0"
  print*, asserttrue(isequal(0.000000001000001D0, 0.000000001000002D0, tol), failureCounter)
  print*,  "a=", 0.000000001000001D0, "b=", 0.000000001000002D0, "tol=,  ", tol
  print*, asserttrue(isequal(0.000000001000002D0, 0.000000001000001D0, tol), failureCounter)
  print*, "a=", 0.000000001000002D0, "b=", 0.000000001000001D0, "tol=,  ", tol
  print*, assertfalse(isequal(0.00000000001001D0, 0.000000000001002D0, tol), failureCounter)
  print*,  "a=", 0.000000000001002D0, "b=", 0.000000000001001D0, "tol=,  ", tol
  print*, assertfalse(isequal(0.00000000001002D0, 0.00000000001001D0, tol), failureCounter)
  print*, "a=", 0.00000000001002D0, "b=", 0.00000000001001D0, "tol=,  ", tol
  print*, "_______________________________________" 

  print*, "Tests for Numbers between -1 and 0"
  print*, asserttrue(isequal(-0.000000001000001D0, -0.000000001000002D0, tol), failureCounter)
  print*,  "a=", -0.000000001000001D0, "b=", -0.000000001000002D0, "tol=,  ", tol
  print*, asserttrue(isequal(-0.000000001000002D0, -0.000000001000001D0, tol), failureCounter)
  print*, "a=", -0.000000001000002D0, "b=", -0.000000001000001D0, "tol=,  ", tol
  print*, assertfalse(isequal(-0.00000000001001D0, -0.000000000001002D0, tol), failureCounter)
  print*,  "a=", -0.000000000001002D0, "b=", -0.000000000001001D0, "tol=,  ", tol
  print*, assertfalse(isequal(-0.00000000001002D0, -0.00000000001001D0, tol), failureCounter)
  print*, "a=", -0.00000000001002D0, "b=", -0.00000000001001D0, "tol=,  ", tol
  print*, "_______________________________________" 

  print*, "Tests for Small differences away from zero"
  print*, asserttrue(isequal(.3D0, 0.30000003D0, tol), failureCounter), "a=", .3D0, "b=", 0.30000003D0, "tol=,  ", tol
  print*, asserttrue(isequal(-.3D0, -0.30000003D0, tol), failureCounter), "a=", -.3D0, "b=", -0.30000003D0, "tol=,  ", tol
  print*, "_______________________________________" 

  print*, "Tests for Comparisons involving zero"
  print*, asserttrue(isequal(0.0D0, 0.0D0, tol), failureCounter), "a=", 0.0D0, "b=", 0.0D0, "tol=,  ", tol
  print*, asserttrue(isequal(0.0D0, -0.0D0, tol), failureCounter), "a=", 0.0D0, "b=", -0.0D0, "tol=,  ", tol
  print*, asserttrue(isequal(-0.0D0, 0.0D0, tol), failureCounter), "a=", -0.0D0, "b=", 0.0D0, "tol=,  ", tol
  print*, assertfalse(isequal(0.00000001D0, 0.0D0,tol), failureCounter), "a=", 0.00000001D0, "b=", 0.0D0, "tol=,  ", tol
  print*, assertfalse(isequal(0.0D0, 0.00000001D0, tol), failureCounter), "a=", 0.0D0, "b=", 0.00000001D0,  "tol=,  ", tol
  print*, assertfalse(isequal(-0.00000001D0, 0.0D0, tol), failureCounter), "a=", -0.00000001D0, "b=", 0.0D0, "tol=,  ", tol
  print*, assertfalse(isequal(0.0D0, -0.00000001D0,tol), failureCounter), "a=", 0.00D0, "b=",-0.00000001D0, "tol=,  ", tol
  ! isequal does not behave as intended in the next two tests. When a real*8 is very close to zero (1D-40) 
  ! and the tolerance value (.01D0) is greater (closer to zero) than the real*8's value, isequal evaluates to false. 
  ! This shows that the function does not perform as intended and expected for this test case 
  print*, assertfalse(isequal(0.0D0, 1D-40, .01D0), failureCounter), "a=", 0.0D0, "b=", 1D-40, "tol=,  ", .01D0 
  print*, assertfalse(isequal(1D-40, 0.0D0, .01D0), failureCounter), "a=", 1D-40, "b=", 0.0D0, "tol=,  ", .01D0 
  print*, assertfalse(isequal(1D-40, 0.0D0, 0.1D-6), failureCounter), "a=", 1D-40, "b=", 0.0D0, "tol=,  ", 0.1D-6 
  print*, assertfalse(isequal(0.0D0, 1D-40, 0.1D-6), failureCounter), "a=", 0.0D0, "b=", 1D-40, "tol=,  ", 0.1D-6 
  ! Following four tests portray the same behavior as described above 
  print*, assertfalse(isequal(0.0D0, -1D-40, .1D0), failureCounter), "a=", 0.0D0, "b=", -1D-40, "tol=,  ", .1D0
  print*, assertfalse(isequal(-1D-40, 0.0D0, .1D0), failureCounter), "a=", -1D-40, "b=", 0.0D0, "tol=,  ", .1D0
  print*, assertfalse(isequal(-1D-40, 0.0D0, 0.1D-8), failureCounter), "a=", -1D-40, "b=", 0.0D0, "tol=,  ", 0.1D-8 
  print*, assertfalse(isequal(0.0D0, -1D-40, 0.1D-8),failureCounter), "a=", 0.0D0, "b=", -1D-40, "tol=,  ", 0.1D-8
  print*, "_______________________________________" 

  print*, "Tests for extreme values (overflow potential)"
  print*, asserttrue(isequal(HUGE(1D0), HUGE(1D0), tol), failureCounter), "a=",HUGE(1D0), "b=", HUGE(1D0), "tol=,  ", tol
  print*, assertfalse(isequal(HUGE(1D0), -HUGE(1D0), tol), failureCounter), "a=",HUGE(1D0), "b=", -HUGE(1D0), "tol=,  ", tol
  print*, assertfalse(isequal(-HUGE(1D0), HUGE(1D0), tol), failureCounter), "a=",-HUGE(1D0), "b=", HUGE(1D0), "tol=,  ", tol
  print*, assertfalse(isequal(HUGE(1D0), HUGE(1D0)/2, tol), failureCounter), "a=",HUGE(1D0), "b=", HUGE(1D0)/2, "tol=,  ", tol
  print*, assertfalse(isequal(HUGE(1D0), -HUGE(1D0)/2, tol), failureCounter), "a=",HUGE(1D0), "b=", -HUGE(1D0)/2, "tol=,  ", tol
  print*, assertfalse(isequal(-HUGE(1D0), HUGE(1D0)/2, tol), failureCounter), "a=",-HUGE(1D0), "b=", HUGE(1D0)/2, "tol=,  ", tol
  print*, "_______________________________________" 

  ! "Tests for infinities and NaN"
  ! Fortran does not have native inifity and NaN values supported without the ieee_arithmetic module, so these tests have been omitted.
  ! There are compiler errors ("Arithmetic overflow", "Division by zero") when attempting to implement tests with Nan and Infinity.
  ! Resources: https://stackoverflow.com/questions/5010538/infinity-in-fortran, 
  ! https://stackoverflow.com/questions/17389958/is-there-a-standard-way-to-check-for-infinite-and-nan-in-fortran-90-95,
  ! https://docs.oracle.com/cd/E19957-01/805-4940/6j4m1u7pg/index.html
  
  print*, "Tests for numbers on opposite sides of 0"
  ! isequal does not behave as intended in the next two tests. Either the tolerance value is significantly higher than the double precision values
  ! being tested and/or isequal() is incorrectly equating for values on either side of zero
  print*, assertfalse(isequal(10 * TINY(1D0), 10 * (-TINY(1D0)), tol), failureCounter)
  print*, "a=", 10 * TINY(1D0), "b=", 10 * (-TINY(1D0)), "tol=,  ", tol
  print*, assertfalse(isequal(10000 * TINY(1D0), 10000 * (-TINY(1D0)), tol), failureCounter)
  print*, "a=", 10000 * TINY(1D0), "b=", 10000 * (-TINY(1D0)), "tol=,  ", tol
  print*, assertfalse(isequal(1.000000001D0, -1D0, tol), failureCounter), "a= ", 1.000000001D0, "b= ", -1D0, "tol=,  ", tol
  print*, assertfalse(isequal(-1D0, 1.000000001D0, tol), failureCounter), "a= ", -1D0, "b= ", 1.000000001D0, "tol=,  ", tol
  print*, assertfalse(isequal(-1.000000001D0, 1D0, tol), failureCounter), "a= ", -1.000000001D0, "b= ", -1D0, "tol=,  ", tol
  print*, assertfalse(isequal(1D0, -1.000000001D0, tol), failureCounter), "a= ", 1D0, "b= ", -1.000000001D0, "tol=,  ", tol
  print*, "_______________________________________" 

  print*, "Tests for numbers very close to zero"
  print*, asserttrue(isequal(TINY(1D0), TINY(1D0), tol), failureCounter), "a=", TINY(1D0), "b=", TINY(1D0), "tol=,  ", tol
  ! isequal does not behave as intended in the next six tests. When a real*8 is very close to zero (1D-40) 
  ! and the tolerance value (.01D0) is greater (closer to zero) than the real*8's value, isequal evaluates to false. 
  print*, assertfalse(isequal(TINY(1D0), -TINY(1D0), tol), failureCounter), "a=", TINY(1D0), "b=", -TINY(1D0), "tol=,  ", tol  
  print*, assertfalse(isequal(-TINY(1D0), TINY(1D0), tol), failureCounter), "a=", -TINY(1D0), "b=", TINY(1D0), "tol=,  ", tol
  print*, assertfalse(isequal(TINY(1D0), 0D0, tol), failureCounter), "a=", TINY(1D0), "b=",0D0, "tol=,  ", tol
  print*, assertfalse(isequal(0D0, TINY(1D0), tol), failureCounter), "a=", 0D0, "b=", TINY(1D0), "tol=,  ", tol
  print*, assertfalse(isequal(-TINY(1D0), 0D0, tol), failureCounter), "a=", -TINY(1D0), "b=",0D0, "tol=,  ", tol
  print*, assertfalse(isequal(0D0, -TINY(1D0), tol), failureCounter), "a=", 0D0, "b=", -TINY(1D0), "tol=,  ", tol
  print*, assertfalse(isequal(1D-09, -TINY(1D0), tol), failureCounter), "a=", 1D-09, "b=", -TINY(1D0), "tol=,  ", tol
  print*, assertfalse(isequal(1D-09, TINY(1D0), tol), failureCounter), "a=", 1D-09, "b=", TINY(1D0), "tol=,  ", tol
  print*, assertfalse(isequal(TINY(1D0), 1D-09, tol), failureCounter), "a=", TINY(1D0), "b=", 1D-09, "tol=,  ", tol
  print*, assertfalse(isequal(-TINY(1D0), 1D-09, tol), failureCounter), "a=", -TINY(1D0), "b=", 1D-09, "tol=,  ", tol
  print*, "_______________________________________"

  ! Tests for the assertion functions 
  print*, "This is a test for asserttrue",  asserttrue(.TRUE., failureCounter)
  print*, "This is a test for assertfalse",  assertfalse(.FALSE., failureCounter)

  if(failureCounter > 0) then
    print*, failureCounter, "tests failed."
	  stop 1
  end if

end program equaltest
