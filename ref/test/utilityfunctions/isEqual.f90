Module IsEqualModule

CONTAINS 

LOGICAL FUNCTION IsEqual(a, b, tolerance)
IMPLICIT NONE

        real*8, intent(in)   :: a, b, tolerance
        real*8               :: absA, absB, diff

absA = abs(a)
absB = abs(b)
diff = abs(a - b)

IF (a == b) THEN
        IsEqual = .TRUE.
ELSE IF ( a == 0 .OR. b == 0 .OR. (absA + absB < TINY(a))) THEN
         ! a or b is zero or both are extremely close to it, relative error is less meaningful
         IsEqual = diff < (tolerance * TINY(a))
ELSE            ! use relative error
        IsEqual = (diff / MIN((absA + absB), HUGE(a))) < tolerance
ENDIF

END FUNCTION IsEqual

LOGICAL FUNCTION AssertTrue(cond1) 
IMPLICIT NONE

        logical, intent(in) :: cond1

AssertTrue = cond1 .EQV. .TRUE.

END FUNCTION AssertTrue

LOGICAL FUNCTION AssertFalse(cond1) 
IMPLICIT NONE

        logical, intent(in) :: cond1

AssertFalse = cond1 .EQV. .FALSE.

END FUNCTION AssertFalse

END Module  IsEqualModule
