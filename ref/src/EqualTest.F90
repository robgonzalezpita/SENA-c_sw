PROGRAM Equal_Test
!------------------------------------------------------------------
!
!       This program compares equality for floating point values 
!       using a given tolerance.  
!
!       Uses: FUNCTION Is_Equal(a, b, tolerance) 
!             FUNCTION AssertTrue(cond1, cond2)
!	      FUNCTION AssertFalse(cond1, cond2) 	
!	      	
!	Test cases are written using two Assert Functions to determine whether
!	the IsEqual function is correctly evaluating to the expected Boolean for two floats.
!	Assertion Functions are purely for code readability, and written to avoid importing external libraries.
!	For testing purposes, the tolerance value will be set at .000001 (.000001D0).
!
!	Note- When tolerance value is increased, or is greater than the floats, IsEqual() does not behave as expected.
!
!------------------------------------------------------------------

use IsEqualModule

IMPLICIT NONE

	REAL*8    :: a, b, tol
!        LOGICAL   :: IsEqual, AssertTrue, AssertFalse

tol = .000001D0


PRINT*, IsEqual(1.12345D13, 1.12346D13, 1D-6), "IsEqual comparison test, run with", 1.12345D13, 1.12346D13, 1D-6

PRINT*, "_______________________________________"
PRINT*, "Tests for large Numbers"
PRINT*, AssertTrue(IsEqual(10000000D0, 10000001D0, tol), .TRUE.), "a=", 10000000D0,"b=", 10000001D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(10000001D0, 10000000D0, tol), .TRUE.), "a=", 10000001D0,"b=", 10000000D0 , "tol= ", tol
PRINT*, AssertFalse(IsEqual(10001D0, 10000D0, tol), .FALSE.), "a=", 10001D0,"b=", 10000D0 , "tol= ", tol
PRINT*, AssertFalse(IsEqual(10000D0, 10001D0, tol), .FALSE.), "a=", 10000D0,"b=", 10001D0 , "tol= ", tol
PRINT*, "_______________________________________"

PRINT*, "Tests for Negative large numbers"
PRINT*, AssertTrue(IsEqual(-10000000D0, -10000001D0, tol), .TRUE.), "a=", -10000000D0, "b=", -10000001D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(-10000000D0, -10000001D0, tol), .TRUE.), "a=", -10000000D0, "b=", -10000001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(-10000D0, -10001D0, tol), .FALSE.) , "a=", -10000D0, "b=", -10001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(-10000D0, -10001D0, tol), .FALSE.), "a=", -10000D0, "b=", -10001D0, "tol= ", tol

PRINT*, "_______________________________________"
PRINT*, "Tests for Numbers around -1"
PRINT*, AssertTrue(IsEqual(-1.0000001D0, -1.0000002D0, tol), .TRUE.), "a=", -1.0000001D0, "b=", -1.0000002D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(-1.0000002D0, -1.0000001D0, tol), .TRUE.), "a=", -1.0000002D0, "b=", -1.0000001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(-10000D0, -10001D0, tol), .FALSE.) , "a=", -10000D0, "b=", -10001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(-10000D0, -10001D0, tol), .FALSE.), "a=", -10000D0, "b=", -10001D0, "tol= ", tol
     
PRINT*, "_______________________________________" 
PRINT*, "Tests for Numbers around 1"
PRINT*, AssertTrue(IsEqual(1.0000001D0, 1.0000002D0, tol), .TRUE.), "a=", 1.0000001D0, "b=", 1.0000002D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(1.0000002D0, 1.0000001D0, tol), .TRUE.), "a=", 1.0000002D0, "b=", 1.0000001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(10000D0, 10001D0, tol), .FALSE.) , "a=", 10000D0, "b=", 10001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(10000D0, 10001D0, tol), .FALSE.), "a=", 10000D0, "b=", 10001D0, "tol= ", tol

PRINT*, "_______________________________________" 
PRINT*, "Tests for Numbers between 1 and 0"
PRINT*, AssertTrue(IsEqual(0.000000001000001D0, 0.000000001000002D0, tol), .TRUE.)
PRINT*,  "a=", 0.000000001000001D0, "b=", 0.000000001000002D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(0.000000001000002D0, 0.000000001000001D0, tol), .TRUE.)
PRINT*, "a=", 0.000000001000002D0, "b=", 0.000000001000001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(0.00000000001001D0, 0.000000000001002D0, tol), .FALSE.)
PRINT*,  "a=", 0.000000000001002D0, "b=", 0.000000000001001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(0.00000000001002D0, 0.00000000001001D0, tol), .FALSE.)
PRINT*, "a=", 0.00000000001002D0, "b=", 0.00000000001001D0, "tol= ", tol

PRINT*, "_______________________________________" 
PRINT*, "Tests for Numbers between -1 and 0"
PRINT*, AssertTrue(IsEqual(-0.000000001000001D0, -0.000000001000002D0, tol), .TRUE.)
PRINT*,  "a=", -0.000000001000001D0, "b=", -0.000000001000002D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(-0.000000001000002D0, -0.000000001000001D0, tol), .TRUE.)
PRINT*, "a=", -0.000000001000002D0, "b=", -0.000000001000001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(-0.00000000001001D0, -0.000000000001002D0, tol), .FALSE.)
PRINT*,  "a=", -0.000000000001002D0, "b=", -0.000000000001001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(-0.00000000001002D0, -0.00000000001001D0, tol), .FALSE.)
PRINT*, "a=", -0.00000000001002D0, "b=", -0.00000000001001D0, "tol= ", tol

PRINT*, "_______________________________________" 
PRINT*, "Tests for Small differences away from zero"
PRINT*, AssertTrue(IsEqual(.3D0, 0.30000003D0, tol), .TRUE.), "a=", .3D0, "b=", 0.30000003D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(-.3D0, -0.30000003D0, tol), .TRUE.), "a=", -.3D0, "b=", -0.30000003D0, "tol= ", tol

PRINT*, "_______________________________________" 
PRINT*, "Tests for Comparisons involving zero"
PRINT*, AssertTrue(IsEqual(0.0D0, 0.0D0, tol), .TRUE.), "a=", 0.0D0, "b=", 0.0D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(0.0D0, -0.0D0, tol), .TRUE.), "a=", 0.0D0, "b=", -0.0D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(-0.0D0, 0.0D0, tol), .TRUE.), "a=", -0.0D0, "b=", 0.0D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(0.00000001D0, 0.0D0,tol), .FALSE.), "a=", 0.00000001D0, "b=", 0.0D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(0.0D0, 0.00000001D0, tol), .FALSE.), "a=", 0.0D0, "b=", 0.00000001D0,  "tol= ", tol
PRINT*, AssertFalse(IsEqual(-0.00000001D0, 0.0D0, tol), .FALSE.), "a=", -0.00000001D0, "b=", 0.0D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(0.0D0, -0.00000001D0,tol), .FALSE.), "a=", 0.00D0, "b=",-0.00000001D0, "tol= ", tol

! Test fails when a real*8 is very close to zero (1D-40) but the tolerance value (.01D0) is greater (close to zero) than the real*8's value 
! This shows that the function does not perform as intended and expected for this test case 
PRINT*, AssertTrue(IsEqual(0.0D0, 1D-40, .01D0), .TRUE.), "a=", 0.0D0, "b=", 1D-40, "tol= ", .01D0 
PRINT*, AssertTrue(IsEqual(1D-40, 0.0D0, .01D0), .TRUE.), "a=", 1D-40, "b=", 0.0D0, "tol= ", .01D0 
PRINT*, AssertFalse(IsEqual(1D-40, 0.0D0, 0.1D-6), .FALSE.), "a=", 1D-40, "b=", 0.0D0, "tol= ", 0.1D-6 
PRINT*, AssertFalse(IsEqual(0.0D0, 1D-40, 0.1D-6), .FALSE.), "a=", 0.0D0, "b=", 1D-40, "tol= ", 0.1D-6 
! Following four tests portray the same behaviour as described above 
PRINT*, AssertTrue(IsEqual(0.0D0, -1D-40, .1D0), .TRUE.), "a=", 0.0D0, "b=", -1D-40, "tol= ", .1D0
PRINT*, AssertTrue(IsEqual(-1D-40, 0.0D0, .1D0), .TRUE.), "a=", -1D-40, "b=", 0.0D0, "tol= ", .1D0
PRINT*, AssertFalse(IsEqual(-1D-40, 0.0D0, 0.1D-8), .FALSE.), "a=", -1D-40, "b=", 0.0D0, "tol= ", 0.1D-8 
PRINT*, AssertFalse(IsEqual(0.0D0, -1D-40, 0.1D-8), .FALSE.), "a=", 0.0D0, "b=", -1D-40, "tol= ", 0.1D-8


PRINT*, "_______________________________________" 
PRINT*, "Tests for extreme values (overflow potential)"
PRINT*, AssertTrue(IsEqual(HUGE(1D0), HUGE(1D0), tol), .TRUE.), "a=",HUGE(1D0), "b=", HUGE(1D0), "tol= ", tol
PRINT*, AssertFalse(IsEqual(HUGE(1D0), -HUGE(1D0), tol), .FALSE.), "a=",HUGE(1D0), "b=", -HUGE(1D0), "tol= ", tol
PRINT*, AssertFalse(IsEqual(-HUGE(1D0), HUGE(1D0), tol), .FALSE.), "a=",-HUGE(1D0), "b=", HUGE(1D0), "tol= ", tol
PRINT*, AssertFalse(IsEqual(HUGE(1D0), HUGE(1D0)/2, tol), .FALSE.), "a=",HUGE(1D0), "b=", HUGE(1D0)/2, "tol= ", tol
PRINT*, AssertFalse(IsEqual(HUGE(1D0), -HUGE(1D0)/2, tol), .FALSE.), "a=",HUGE(1D0), "b=", -HUGE(1D0)/2, "tol= ", tol
PRINT*, AssertFalse(IsEqual(-HUGE(1D0), HUGE(1D0)/2, tol), .FALSE.), "a=",-HUGE(1D0), "b=", HUGE(1D0)/2, "tol= ", tol

PRINT*, "_______________________________________" 
!PRINT*, "Tests for infinities and NaN"
! Fortran does not easily support inifite and NaN values standardized across compilers, so these tests have been omitted
! There are compiler errors ("Arithmetic overflow", "Division by zero") when attempting to implement tests with Nan and Infinity
! Resources: https://stackoverflow.com/questions/5010538/infinity-in-fortran, https://docs.oracle.com/cd/E19957-01/805-4940/6j4m1u7pg/index.html
!PRINT*, "_______________________________________" 

PRINT*, "Tests for numbers on opposite sides of 0"
! The following two tests will fail as either the tolerance value is significantly higher than the double precision values
! being tested and/or IsEqual() is equating for the negative and positive values
PRINT*, AssertTrue(IsEqual(10 * TINY(1D0), 10 * (-TINY(1D0)), tol), .TRUE.)
PRINT*, "a=", 10 * TINY(1D0), "b=", 10 * (-TINY(1D0)), "tol= ", tol
PRINT*, AssertFalse(IsEqual(10000 * TINY(1D0), 10000 * (-TINY(1D0)), tol), .TRUE.)
PRINT*, "a=", 10000 * TINY(1D0), "b=", 10000 * (-TINY(1D0)), "tol= ", tol
!_____________________________________________
PRINT*, AssertFalse(IsEqual(1.000000001D0, -1D0, tol), .FALSE.), "a= ", 1.000000001D0, "b= ", -1D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(-1D0, 1.000000001D0, tol), .FALSE.), "a= ", -1D0, "b= ", 1.000000001D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(-1.000000001D0, 1D0, tol), .FALSE.), "a= ", -1.000000001D0, "b= ", -1D0, "tol= ", tol
PRINT*, AssertFalse(IsEqual(1D0, -1.000000001D0, tol), .FALSE.), "a= ", 1D0, "b= ", -1.000000001D0, "tol= ", tol

PRINT*, "_______________________________________" 
PRINT*, "Tests for numbers very close to zero"
PRINT*, AssertTrue(IsEqual(TINY(1D0), TINY(1D0), tol), .TRUE.), "a=", TINY(1D0), "b=", TINY(1D0), "tol= ", tol
!___The following five tests fail______ changed one test below (tol value)
PRINT*, AssertTrue(IsEqual(TINY(1D0), -TINY(1D0), tol), .TRUE.), "a=", TINY(1D0), "b=", -TINY(1D0), "tol= ", tol 
PRINT*, AssertTrue(IsEqual(-TINY(1D0), TINY(1D0), tol), .TRUE.), "a=", -TINY(1D0), "b=", TINY(1D0), "tol= ", tol
PRINT*, AssertTrue(IsEqual(TINY(1D0), 0D0, tol), .TRUE.), "a=", TINY(1D0), "b=",0D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(0D0, TINY(1D0), tol), .TRUE.), "a=", 0D0, "b=", TINY(1D0), "tol= ", tol
PRINT*, AssertTrue(IsEqual(-TINY(1D0), 0D0, tol), .TRUE.), "a=", -TINY(1D0), "b=",0D0, "tol= ", tol
PRINT*, AssertTrue(IsEqual(0D0, -TINY(1D0), tol), .TRUE.), "a=", 0D0, "b=", -TINY(1D0), "tol= ", tol
!___________________
PRINT*, AssertFalse(IsEqual(1D-09, -TINY(1D0), tol), .FALSE.), "a=", 1D-09, "b=", -TINY(1D0), "tol= ", tol
PRINT*, AssertFalse(IsEqual(1D-09, TINY(1D0), tol), .FALSE.), "a=", 1D-09, "b=", TINY(1D0), "tol= ", tol
PRINT*, AssertFalse(IsEqual(TINY(1D0), 1D-09, tol), .FALSE.), "a=", TINY(1D0), "b=", 1D-09, "tol= ", tol
PRINT*, AssertFalse(IsEqual(-TINY(1D0), 1D-09, tol), .FALSE.), "a=", -TINY(1D0), "b=", 1D-09, "tol= ", tol

PRINT*, "_______________________________________"


! Tests for the assertion functions 
PRINT*, "this is the test for assertTrue, T and T=",  AssertTrue( .TRUE. , .TRUE.)
PRINT*, "this is the test for assertTrue, T and F=",  AssertTrue( .TRUE. , .FALSE.)
PRINT*, "this is the test for assertTrue, F and F=",  AssertTrue( .FALSE. , .FALSE.)
PRINT*, "this is the test for assertFalse, F and F=",  AssertFalse( .FALSE. , .FALSE.)
PRINT*, "this is the test for assertFalse, T and T=",  AssertFalse( .TRUE. , .TRUE.)
PRINT*, "this is the test for assertFalse, T and F=",  AssertFalse( .TRUE. , .FALSE.)

STOP 

END PROGRAM Equal_Test
