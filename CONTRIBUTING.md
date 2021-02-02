# Contributing to c_sw

NOTE: If you are reading this with a plain text editor, please note that this 
document is formatted with Markdown syntax elements.  See 
https://www.markdownguide.org/cheat-sheet/ for more information. It is
recommended to view [this document](https://github.com/NOAA-GSL/SENA-c_sw/blob/develop/CONTRIBUTING.md)
on GitHub.


## Contents

[How to Contribute](#how-to-contribute)

[Contributing an Alternative Implementation](#contributing-an-alternative-implementation)

[Branch Management](#branch-management)

[Pull Request Rules and Guidelines](#pull-request-rules-and-guidelines)

[Fortran Style Guide](#fortran-style-guide)

## How to Contribute

Contributions to c_sw will be accepted via [pull request](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request),
either from branches in this repository, or from a fork of this repository. Pull
requests will be reviewed and evaluated based on the technical merit of the
proposed changes as well as conformance to the style guidelines outlined in this
document. Code reviewers may request changes as a condition of acceptance of the
pull request.

## Contributing an Alternative Implementation

Contributors who wish to provide an alternative implementation of `c_sw`
can do so by issuing a pull request to the `develop` branch from their
own forks of this repository. Due to NOAA policy, write access to this
repository can not be granted for external collaborators.

All pull requests for new implementations should include:

* A portable build system that functions across platforms and (if applicable)
across multiple compilers.
* Source code that is well written, properly formatted, and documented.
* A test suite that verifies the implementation produces the same results as
the reference implementation. At present, 11 digits of accuracy are expected.
* A GitHub Actions continuous integeration configuration to automate the new
implementation's test suite (we can help with this if needed).
* A documentation README.md describing the new implementation, including how
to run and test it.

## Branch Management

External collaborators are, of course, free to use the branch management
strategy of their choice in their own forks.  This section applies to branches
created in this repository by internal collaborators, but exertnal collaborators
are encouraged to adopt a similar approach.

This repository follows the [GitHub Flow](https://guides.github.com/introduction/flow/)
branching model with the following modifications borrowed from
[Git Flow](https://nvie.com/posts/a-successful-git-branching-model/):

* All branches that add new features, capabilities, or enhancements must be named:
`feature/name-of-my-feature`
* All branches that fix defects must be named: `bugfix/name-of-my-bugfix`

A rule of thumb for choosing a branch type: If it isn't a bugfix, it is a feature.

In addition to the naming conventions, all branches must have a clearly defined,
singular purpose, described by their name. It is also prefered, but not required,
that branches correspond to an issue documented in the issue tracking system. (Issues
can be added after branch creation, if necessary.)  All branches shall result in a
pull request and shall be deleted immediately after that pull request is merged.

## Pull Request Rules and Guidelines

We ask contributors to please be mindful of the following when submitting a Pull
Request. The following will help reviewers give meaningful and timely feedback.

In order to maintain quality standards, the following rules apply:

* Pull requests will not be accepted for branches that are not up-to-date with
the current `develop` branch.
* Pull requests will not be accepted unless all tests pass.  This include manual
execution of test suites on local platforms as well as the automated continuous
integration tests.
* Pull requests that add new capabilities will not be accepted without the inclusion
of tests that verify those capabilities work properly. (We can help with this if needed)

We also ask contributors to consider the following when proposing changes:

* Provide a thorough explanation of the changes you are proposing and reference any
issues they resolve. Also link to other related or dependent pull requests in
the description.
* Pull requests should be as small as possible. Break large changes into multiple
smaller changes if possible. Put yourself in the reviewers' shoes. If your pull
request is too big, reviewers may ask you to break it into smaller, more digestable,
pieces.
* Group changes that logically contribute to the branch's singular purpose
together.
* Do not group unrelated changes together; create separate branches and submit
pull requests for unrelated changes separately.

## Fortran Style Guide

Unfortunately, there appears not to be a reliable linter for Fortran that can be
used to automate conformance of Fortran coding style. The code in this repository
should use a consistent style throughout. The code should appear as if it were 
written by a single person.

NOTE: There is a tool, [fprettify](https://github.com/pseewald/fprettify), for automatically
formatting Fortran code. However, its output does not comply with this style guide
so we do not recommend its use for contributions to this repository.

We ask collaborators to follow these style guidelines when modifying existing code,
or contributing new code to this repository. Pull request reviewers may ask
collaborators to fix style violations as a condition of approval.

* Do not use upper case

  ```
  ! Use this
  program foo
    integer :: foobar
    integer :: foo_bar  ! This is also okay
    integer :: fooBar   ! This is also okay
  end program foo

  ! Instead of this
  PROGRAM FOO
    INTEGER :: FOOBAR
  END PROGRAM FOO
  ```

* Use two spaces to indent all code inside `program`

  ```
  ! Use this
  program foo
    integer :: foobar
  end program foo

  ! Instead of this
  program foo
  integer :: foobar
  end program foo

  ! Please do not use this, either
  program foo
        integer :: foobar
  end program foo
  ```

* Use two spaces to indent all code inside `module`

  ```
  ! Use this
  module foo
    foo :: integer
  contains
    subroutine bar
    end subroutine bar
  end module foo
  
  ! Instead of this
  module foo
  foo :: integer
  contains
  subroutine bar
  end subroutine bar
  end module foo

* Use two spaces to indent all code inside `subroutine` and `function`

  ```
  ! Use this
  subroutine foo(bar)
    bar :: integer
    bar = bar + 1
  end subroutine bar

  ! Instead of this
  subroutine foo(bar)
  bar :: integer
  bar = bar + 1
       bar = bar + 1  ! Please do not do this, either
  end subroutine bar
  ```

* Use two spaces to indent all code inside `if`, `do`, `while`, etc.

  ```
  ! Use this
  if (bar > 1) then
    do while (bar < 10)
      write(*, *) "Bar"
      bar = bar + 1
    end do
  end if

  ! Instead of this
  if (bar > 1) then
  do while (bar < 10)
  write(*, *) "Bar"
  bar = bar + 1
  end do
  end if

  ! Please do not do this, either
  if (bar > 1) then
       do while (bar < 10)
            write(*, *) "Bar"
            bar = bar + 1
       end do
  end if
  ```

* Use spaces after commas

  ```
  ! Use this
  write(*, '(A, I)') "The number is", a(i, j)

  ! Instead of this
  write(*,'(A,I)') "The number is",a(i,j)
  ```
  
* Use spaces around operators

  ```
  ! Use this
  x = a(i, j) * 1.0 - pi / (rho + phi)

  ! Instead of this
  x=a(i,j)*1.0-pi/(rho+phi)
  ```

* Do NOT use spaces before the open parenthesis when calling a function

  ```
  ! Use this
  write(*, *) "Foo"
  call bar(x)

  ! Instead of this
  write (*, *) "Foo"
  call bar (x)
  ```

* Align variable and intent declarations

  ```
  ! Use this
  subroutine foo(x, y, z)
    integer, intent(   in) :: x
    real,    intent(  out) :: y
    logical, intent(inout) :: z

    real, allocatable :: foobar(:,:)
    real              :: baz
    integer           :: zap

  ! Instead of this
  subroutine foo(x, y, z)
    integer, intent(in) :: x
    real, intent(out) :: y
    logical, intent(inout) :: z

    real, allocatable :: fobar(:,:)
    real :: baz
    integer :: zap
  ```

* Declare subroutine arguments in the same order they appear in the argument list

  ```
  ! Use this
  subroutine foo(a, b, c)
    integer :: a
    real    :: b
    logical :: c

  ! Instead of this
  subroutine foo(a, b, c)
    logical :: c
    integer :: a
    real    :: b    
  ```

* Specify full name in `end` statements

  ```
  ! Use this
  program foo
  end program foo

  ! Instead of this
  program foo
  end program
  ```

  And

  ```
  ! Use this
  module foo
    subroutine bar
    end subroutine bar
  end module foo

  ! Instead of this
  module foo
    subroutine bar
    end subroutine
  end module
  ```

* Use comment header blocks above subroutines

```
! Use this
!****************************************
!
! foo
!
! Description of what foo does.
!
!****************************************
subroutine foo()

! Instead of this
subroutine foo()
```