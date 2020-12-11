module isequalmodule
  
 contains 

  !------------------------------------------------------------
  !       isequal 
  !
  !       Function which compares equality of two real*8 floats
  !       within a given tolerance value.
  !------------------------------------------------------------

  logical function isequal(a, b, tolerance)
    implicit none

    real*8, intent(in)   :: a, b, tolerance
    real*8               :: absA, absB, diff

    absA = abs(a)
    absB = abs(b)
    diff = abs(a - b)

    if (a == b) then
        isequal = .true.
    else if ( a == 0 .or. b == 0 .or. (absA + absB < TINY(a))) then
        ! a or b is zero or both are extremely close to it, relative error is less meaningful
        isequal = diff < (tolerance * TINY(a))
    else    ! use relative error
        isequal = (diff / MIN((absA + absB), HUGE(a))) < tolerance
    endif

  end function isequal

  !-----------------------------------------------------------
  !       asserttrue 
  !
  !       Helper function to improve readability in evalutaing 
  !       isequal's expected behavior
  !-----------------------------------------------------------

  logical function asserttrue(cond1) 
    implicit none

    logical, intent(in) :: cond1

    asserttrue = cond1 .eqv. .true.

    ! insert an if statement w a stop 1 is assertion fails

  end function asserttrue

  !-----------------------------------------------------------
  !       assertfalse 
  !
  !       Helper function to improve readability in evalutaing 
  !       isequal's expected behavior
  !-----------------------------------------------------------

  logical function assertfalse(cond1) 
    implicit none

    logical, intent(in) :: cond1

    assertfalse = cond1 .eqv. .false.

    ! insert an if statement w a stop 1 is assertion fails

  end function assertfalse

end module  isequalmodule
