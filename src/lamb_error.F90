!******************************************************************************
! This source code is part of the LAMB library.
!
! Written by Urban Borstnik.  This file also written by Valery Weber and
! other CP2K developers.
!
! Inspired by CP2K and DBCSR.
!
! Copyright (C) 2012 Urban Borstnik. Parts of this file Copyright (C) 2008,
! 2009, 2010, 2011, 2012, and 2013 by CP2K developers and Urban Borstnik.
!
! The LAMB library is free software: you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation, either version 2 of the
! License, or (at your option) any later version.
!
! LAMB is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with LAMB.  If not, see <http://www.gnu.org/licenses/>.
!
! If you want to redistribute modifications, please note that derived
! work must not be called LAMB, Lamb, lamb, libLAMB, libLamb, liblamb
! nor any other case variation.  Details are found in the README &
! COPYING files.  If they are missing, get the official version at the
! http://lamb.borstnik.net/ website.
!
! We ask you to cite the published articles on this software.  You can
! find a list in the README file.
!
!******************************************************************************
!-----------------------------------------------------------------------------!
!   CP2K: A general program to perform molecular dynamics simulations         !
!   Copyright (C) 2000 - 2011  CP2K developers group                          !
!-----------------------------------------------------------------------------!

! *****************************************************************************
!> \brief LAMB error handling
!> \author Urban Borstnik
!>
!> <b>Modification history:</b>
!> - 2011-02-11 [UB] Created as copy from DBCSR error.
!> - 2012-10-24 [UB] Support for recoverable errors.
! Todo:
!  helper functions for openmp--error merging)
! New : was_error, was_warning
! Added? not_ok, set_die_level
! *****************************************************************************
MODULE lamb_error

  USE lamb_kinds,                     ONLY: def_str_len, int_1, int_2, int_4, int_8

#include "lamb_defs.h"

  IMPLICIT NONE
  PRIVATE

  logical, parameter :: careful_lamb = .true.
  logical, parameter :: debug_lamb   = .false.
  logical, parameter :: trace_lamb   = .false.

  logical, parameter :: debug_mod    = .false.

  PUBLIC :: careful_lamb, trace_lamb, debug_lamb

  ! Error types
  PUBLIC :: lamb_error_type
  PUBLIC :: lamb_error_location
  PUBLIC :: lamb_error_stack_type

  ! Basic calls
  PUBLIC :: lamb_error_set, lamb_error_stop
  ! Assertions, checks
  PUBLIC :: lamb_assert
  PUBLIC :: lamb_error_throw, lamb_error_catch
  PUBLIC :: lamb_memory_check
  ! Error status checks and fine-tuning
  public :: lamb_error_not_ok, lamb_error_ok
  public :: lamb_error_set_die_level, lamb_error_make_ok
  public :: lamb_error_save_level, lamb_error_update_level
  public :: lamb_error_set_message
  ! Tracing
  public :: lamb_error_trace_on, lamb_error_trace_off
  ! Helpers
  PUBLIC :: lamb_error_here, lamb_here
  ! Debug, info
  PUBLIC :: lamb_error_print_stack

  INTERFACE lamb_assert
     MODULE PROCEDURE lamb_int_assert, lamb_int8_assert
     MODULE PROCEDURE lamb_logical_assert
     MODULE PROCEDURE lamb_not_assert
     MODULE PROCEDURE lamb_char_assert
     MODULE PROCEDURE lamb_true_assert
  END INTERFACE

  INTERFACE lamb_memory_check
     MODULE PROCEDURE lamb_error_memalloc4, lamb_error_memalloc8
     MODULE PROCEDURE lamb_error_memalloc1, lamb_error_memalloc2
     MODULE PROCEDURE lamb_error_memdealloc
  END INTERFACE

  interface lamb_error_here
     module procedure lamb_error_here_auto
     module procedure lamb_error_here_manual
  end interface lamb_error_here

  interface lamb_here
     module procedure lamb_error_here_auto
     module procedure lamb_error_here_manual
  end interface lamb_here

  INTEGER, PARAMETER, PUBLIC :: lamb_error_stack_size = 20

  ! Error levels.
  ! Severity level of an error from 0 to 5.
  integer, parameter, public :: lamb_full_error = 5
  ! Generic fatal error, generally unrecoverable.
  INTEGER, PARAMETER, PUBLIC :: lamb_fatal_level = 5
  ! Memory error, generally unrecoverable.
  integer, parameter, PUBLIC :: lamb_memory_error = 4
  ! Generic failure, may be recoverable.
  INTEGER, PARAMETER, PUBLIC :: lamb_failure_level = 3
  ! Generic warning, may be recoverable.
  INTEGER, PARAMETER, PUBLIC :: lamb_warning_level = 2
  ! Generic warning, generally recoverable.
  INTEGER, PARAMETER, PUBLIC :: lamb_note_level = 1
  ! No detectable problems.
  INTEGER, PARAMETER, PUBLIC :: lamb_ok_level = 0
  ! No detectable problems.
  INTEGER, PARAMETER, PUBLIC :: lamb_no_error = 0

  ! Default level at which an error causes self-termination.
  INTEGER, PARAMETER, PUBLIC :: default_die_level = lamb_failure_level

  ! Error types.
  ! Generic error on the side of the caller.
  INTEGER, PARAMETER, PUBLIC :: lamb_caller_error = 1
  ! Some argument is invalid or has an invalid value.
  INTEGER, PARAMETER, PUBLIC :: lamb_wrong_args = 100
  ! Precondition is unsatisfied.
  INTEGER, PARAMETER, PUBLIC :: lamb_precondition_failed = 200
  ! An error internal to LAMB.
  INTEGER, PARAMETER, PUBLIC :: lamb_internal_error = -1
  ! An overflow error
  INTEGER, PARAMETER, PUBLIC :: lamb_overflow_error = -2
  ! Postcondition is unsatisfied.
  INTEGER, PARAMETER, PUBLIC :: lamb_postcondition_failed = -200
  ! Invariant is unsatisfied.
  INTEGER, PARAMETER, PUBLIC :: lamb_invariant_failed = -100
  ! Invalid assertion.
  INTEGER, PARAMETER, PUBLIC :: lamb_assertion_failed = -300
  ! Unimplemented feature in code path.
  INTEGER, PARAMETER, PUBLIC :: lamb_unimplemented = -1000
  ! Error calling external routines.
  INTEGER, PARAMETER, PUBLIC :: lamb_external_error = -2000

  CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'lamb_error'

  ! Defaults
  integer, private :: default_output_unit = 6
  logical, private, parameter :: die_by_segfault = .false.

! *****************************************************************************
!> \brief stack of the error type (LAMB library)
! *****************************************************************************
  TYPE lamb_error_stack_type
     CHARACTER(def_str_len) :: scope_name = ''
     INTEGER :: handle = 0
     INTEGER :: happened_level = lamb_ok_level
     INTEGER :: die_level = default_die_level
  END TYPE lamb_error_stack_type

  type lamb_error_location
     character(def_str_len) :: scope
     integer :: line
  end type lamb_error_location

! *****************************************************************************
!> \brief error type for the LAMB library
! *****************************************************************************
  TYPE lamb_error_type
     INTEGER :: stack_level = 0
     CHARACTER(def_str_len) :: message
     type(lamb_error_location) :: location
     integer :: error_level
     integer :: error_type
     INTEGER :: handle = 0
     INTEGER :: happened_level = lamb_ok_level
     INTEGER :: die_level = default_die_level
     TYPE(lamb_error_stack_type), DIMENSION(lamb_error_stack_size) :: stack
!#if defined(__USE_CP2K_TRACE)
!     INTEGER, DIMENSION(lamb_error_stack_size) :: cp2k_handler = 0
!     INTEGER, DIMENSION(lamb_error_stack_size) :: dbcsr_handle = 0
!     TYPE(dbcsr_error_type), DIMENSION(lamb_error_stack_size) :: dbcsr_error
!#endif
     integer :: output_unit
     logical :: i_am_printing = .true.
     logical :: tracing = .false.
  END TYPE lamb_error_type

CONTAINS

! *****************************************************************************
!> \brief Sets an error-catcher on subroutine or section entry.
!> \par OpenMP: Each thread must have its own private error structure.
!> \param[in,out] error   LAMB error.
!> \param[in] handle      Error handle
!> \param[in] name        Name of the routine.
! *****************************************************************************
  SUBROUTINE lamb_error_set(error, handle, scope_name)
    TYPE(lamb_error_type), INTENT(inout)     :: error
    INTEGER, INTENT(out)                     :: handle
    CHARACTER(len=*), INTENT(in)             :: scope_name
    CHARACTER(len=20)                        :: fmt_str

    integer :: l

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_error_set', &
         routineP = moduleN//':'//routineN

    error%stack_level = error%stack_level + 1
    l = error%stack_level
    if (l .gt. lamb_error_stack_size) then
       call lamb_error_throw (error, lamb_failure_level, lamb_internal_error, &
            "Size of error stack is too low.", HERE)
    elseif (l .lt. 1) then
       call lamb_error_throw (error, lamb_failure_level, lamb_internal_error, &
            "Error misuse.", HERE)
    endif
    IF(error%stack_level .gt. lamb_error_stack_size.OR.&
       error%stack_level .lt. 1)&
       STOP "Error level out of bounds."
    ! Set name.
    error%stack(l)%scope_name = scope_name
    ! Set handle.
    handle = error%stack(l)%handle
    error%stack(l)%handle = handle
    ! Propagate the die and happened level from the previous level.
    if (l > 1) then
       error%stack(l)%die_level = error%die_level
       error%stack(l)%happened_level = error%happened_level
    else
       error%output_unit = default_output_unit
       error%tracing = .false.
       error%stack(l)%die_level = default_die_level
       error%stack(l)%happened_level = lamb_ok_level
    endif
    if (error%tracing) then
       ! We want a format string like '(5X,A4,1X,A)'
       write(fmt_str,'("(",I3,"X,A4,1X,A)")')l
       write(error%output_unit,fmt=fmt_str)'ENTR',scope_name
    endif
  END SUBROUTINE lamb_error_set

! *****************************************************************************
!> \brief Stops error catcher on subroutine/section exit.
!> \param[in,out] error   lamb error
!> \param[in,out] handle  error handle
! *****************************************************************************
  SUBROUTINE lamb_error_stop(error, handle)
    TYPE(lamb_error_type), intent(inout)     :: error
    INTEGER, INTENT(inout)                   :: handle
    integer :: l
    character(len=*), parameter :: routineN = "lamb_error_stop"
    CHARACTER(len=20)                        :: fmt_str

    IF(error%stack_level.GT.lamb_error_stack_size.OR.&
       error%stack_level.LT.1) THEN
       call lamb_assert (error%stack_level, "LE", lamb_error_stack_size,&
            lamb_failure_level, lamb_internal_error, routineN,&
            "Requested error stack element too high.", __LINE__, error)
       call lamb_assert (error%stack_level, "GE", 1,&
            lamb_failure_level, lamb_internal_error, routineN,&
            "Error stack level too low.", __LINE__, error)
       if (debug_mod) then
          WRITE(*,*) 'lamb_error_stop'
          WRITE(*,*) 'level=',error%stack_level
          WRITE(*,*) 'lamb_error_stack_size=',lamb_error_stack_size
       endif
       STOP "Error level out of bounds."
    ENDIF
    IF(handle.NE.error%stack(error%stack_level)%handle) THEN
       call lamb_assert (handle, "EQ", error%stack(error%stack_level)%handle,&
            lamb_warning_level, lamb_caller_error, routineN,&
            "Improper handle given.", __LINE__, error)
       WRITE(*,*) 'get handle=',handle
       WRITE(*,*) 'expected handle=',error%stack(error%stack_level)%handle
       WRITE(*,*) 'you may have forget to call lamb_error_stop (may be a RETURN)...'
       STOP "Invalid handle given."
    ENDIF
!#if defined(__USE_CP2K_TRACE)
!    !CALL timestop_mp(error%stack%cp2k_handler(error%stack%level) )
!    CALL dbcsr_error_stop(error%stack%dbcsr_handle(error%stack%level),&
!         error%stack%dbcsr_error(error%stack%level))
!#endif
    l = error%stack_level
    if (error%tracing) then
       write(fmt_str,'("(",I3,"X,A4,1X,A)")')l
       write(error%output_unit,fmt=fmt_str)'EXIT',error%stack(l)%scope_name
    endif
    error%happened_level = error%stack(l)%happened_level
    ! Clear previous error stack.
    error%stack(l)%scope_name = '/'
    error%stack(l)%handle = 0
    error%stack_level = error%stack_level - 1
    ! Destroy the handle
    handle = -HUGE(0)
  END SUBROUTINE lamb_error_stop

  !pure function lamb_was_error(error)
  !  type(lamb_error_type), intent(in) :: error
  !  logical :: lamb_was_error
  !  lamb_was_error = error%error_happened
  !end function lamb_was_error
  !
  !pure function lamb_was_warning(error)
  !  type(lamb_error_type), intent(in) :: error
  !  logical :: lamb_was_warning
  !  lamb_was_warning = error%warning_happened
  !end function lamb_was_warning

  subroutine lamb_error_memalloc1 (status, object_name, memory_size, error, location)
    integer, intent(in) :: status
    character(len=*), intent(in) :: object_name
    integer(kind=int_1), intent(in) :: memory_size
    type(lamb_error_type), intent(inout) :: error
    type(lamb_error_location), intent(in), optional :: location
    character(len=*), parameter :: routineN = "lamb_error_memalloc1"
    if (present (location)) then
       call lamb_assert(status, "EQ", 0,&
            lamb_failure_level, lamb_memory_error,&
            "Memory allocation error for "//object_name,&
            location%scope, location%line, error)
    else
       call lamb_assert(status, "EQ", 0,&
            lamb_failure_level, lamb_memory_error,&
            "Memory allocation error for "//object_name,&
            routineN, __LINE__, error)
    endif
  end subroutine lamb_error_memalloc1
  subroutine lamb_error_memalloc2 (status, object_name, memory_size, error, location)
    integer, intent(in) :: status
    character(len=*), intent(in) :: object_name
    integer(kind=int_2), intent(in) :: memory_size
    type(lamb_error_type), intent(inout) :: error
    type(lamb_error_location), intent(in), optional :: location
    character(len=*), parameter :: routineN = "lamb_error_memalloc2"
    if (present (location)) then
       call lamb_assert(status, "EQ", 0,&
            lamb_failure_level, lamb_memory_error,&
            "Memory allocation error for "//object_name,&
            location%scope, location%line, error)
    else
       call lamb_assert(status, "EQ", 0,&
            lamb_failure_level, lamb_memory_error,&
            "Memory allocation error for "//object_name,&
            routineN, __LINE__, error)
    endif
  end subroutine lamb_error_memalloc2
  subroutine lamb_error_memalloc4 (status, object_name, memory_size, error, location)
    integer, intent(in) :: status
    character(len=*), intent(in) :: object_name
    integer(kind=int_4), intent(in) :: memory_size
    type(lamb_error_type), intent(inout) :: error
    type(lamb_error_location), intent(in), optional :: location
    character(len=*), parameter :: routineN = "lamb_error_memalloc4"
    if (present (location)) then
       call lamb_assert(status, "EQ", 0,&
            lamb_failure_level, lamb_memory_error,&
            "Memory allocation error for "//object_name,&
            location%scope, location%line, error)
    else
       call lamb_assert(status, "EQ", 0,&
            lamb_failure_level, lamb_memory_error,&
            "Memory allocation error for "//object_name,&
            routineN, __LINE__, error)
    endif
  end subroutine lamb_error_memalloc4
  subroutine lamb_error_memalloc8 (status, object_name, memory_size, error, location)
    integer, intent(in) :: status
    character(len=*), intent(in) :: object_name
    integer(kind=int_8), intent(in) :: memory_size
    type(lamb_error_type), intent(inout) :: error
    type(lamb_error_location), intent(in), optional :: location
    character(len=*), parameter :: routineN = "lamb_error_memalloc8"
    if (present (location)) then
       call lamb_assert(status, "EQ", 0,&
            lamb_failure_level, lamb_memory_error,&
            "Memory allocation error for "//object_name,&
            location%scope, location%line, error)
    else
       call lamb_assert(status, "EQ", 0,&
            lamb_failure_level, lamb_memory_error,&
            "Memory allocation error for "//object_name,&
            routineN, __LINE__, error)
    endif
  end subroutine lamb_error_memalloc8

  subroutine lamb_error_memdealloc (status, object_name, error, location)
    integer, intent(in) :: status
    character(len=*), intent(in) :: object_name
    type(lamb_error_type), intent(inout) :: error
    type(lamb_error_location), intent(in), optional :: location
    character(len=*), parameter :: routineN = "lamb_error_memdealloc"

    if (present (location)) then
       call lamb_assert(status, "EQ", 0,&
            lamb_warning_level, lamb_memory_error,&
            "Memory deallocation error "//object_name,&
            location%scope, location%line, error=error)
    else
       call lamb_assert(status, "EQ", 0,&
            lamb_warning_level, lamb_memory_error,&
            "Memory deallocation error "//object_name,&
            routineN, __LINE__, error=error)
    endif
  end subroutine lamb_error_memdealloc


  pure function lamb_error_here_auto(file_name, function_name, line)&
       result (location)
    character(len=*), intent(in) :: file_name, function_name
    integer, intent(in) :: line
    type(lamb_error_location) :: location
    location%scope = TRIM(file_name)//" "//TRIM(function_name)
    location%line = line
  end function lamb_error_here_auto

  pure function lamb_error_here_manual(loc, line) result (location)
    character(len=*), intent(in) :: loc
    integer, intent(in) :: line
    type(lamb_error_location) :: location
    location%scope = loc
    location%line = line
  end function lamb_error_here_manual

! *****************************************************************************
!> \brief stop the error
!> \param[inout] error   lamb error
! *****************************************************************************
  SUBROUTINE lamb_error_print_stack(error, output_unit, print)
    TYPE(lamb_error_type), intent(in)                   :: error
    integer, intent(in), optional :: output_unit
    logical, intent(in), optional :: print

    INTEGER                                  :: level, ou

    if (present (output_unit)) then
       ou = output_unit
    else
       ou = error%output_unit
    endif
    IF(error%stack_level.LE.lamb_error_stack_size.OR.&
       error%stack_level.GE.1) THEN
       write(ou,*)"Error"
       WRITE(ou,*)"error%message=",error%message
       WRITE(ou,*)"error%location%scope=",error%location%scope
       WRITE(ou,*)"error%location%line=",error%location%line
       WRITE(ou,*)"error%happened_level=",error%happened_level
       WRITE(ou,*)"error%stack_level=",error%stack_level
       RETURN
    ENDIF
    write(ou,*)"Error Stack"
    do level = 1, error%stack_level
       write(ou,*)"Error stack at level", level
       write(ou,*)"stack%scope_name=",error%stack(level)%scope_name
       write(ou,*)"stack%handle=",error%stack(level)%handle
       write(ou,*)"stack%happened_level=",error%stack(level)%happened_level
       write(ou,*)"stack%die_level=",error%stack(level)%die_level
    enddo
    CALL flush(output_unit)
  END SUBROUTINE lamb_error_print_stack

! *****************************************************************************
!> \brief 
!> \param[inout] 
! *****************************************************************************
  SUBROUTINE lamb_error_throw (error, throw_level, error_type, message, location)
    TYPE(lamb_error_type), INTENT(inout)     :: error
    INTEGER, INTENT(in)                      :: throw_level, error_type
    CHARACTER(*), INTENT(in)                 :: message
    type(lamb_error_location), intent(in)    :: location

    error%message = message
    error%location = location
    error%error_level = throw_level
    error%error_type = error_type
    error%happened_level = MAX(throw_level, error%happened_level)
    error%stack(error%stack_level)%happened_level = error%happened_level
    CALL lamb_error_print_stack(error)
    ! Now die if necessary.
    IF (lamb_error_deadly (error, error%error_level)) then
       ! Segmentation faults can be handy for debugging.
       if (die_by_segfault) then
          write(*,*)error%stack(HUGE(3)-error%stack_level)%handle
       else
          STOP "Self-termination."
       endif
    ENDIF
    !
  END SUBROUTINE lamb_error_throw

! *****************************************************************************
!> \brief Assertion
!> \param[in] left            left value
!> \param[in] rel             relation
!> \param[in] right           right value
!> \param[in] level           error level
!> \param[in] etype           error type
!> \param[in] routine         Routine name
!> \param[in] msg   Message to display if the assertion fails
! *****************************************************************************
  SUBROUTINE lamb_int_assert(left, rel, right, level, etype, msg, routine, line, error)
    INTEGER, INTENT(IN)                      :: left
    CHARACTER(len=2), INTENT(IN)             :: rel
    INTEGER, INTENT(IN)                      :: right, level, etype
    CHARACTER(len=*), INTENT(IN)             :: msg, routine
    INTEGER, INTENT(IN)                      :: line
    TYPE(lamb_error_type), INTENT(inout)    :: error
    character(len=*), parameter :: routineN = "lamb_int_assert"

    LOGICAL                                  :: l

!   ---------------------------------------------------------------------------

    SELECT CASE (rel)
    CASE ("EQ")
       l = left .EQ. right
    CASE ("LT")
       l = left .LT. right
    CASE ("LE")
       l = left .LE. right
    CASE ("GT")
       l = left .GT. right
    CASE ("GE")
       l = left .GE. right
    CASE ("NE")
       l = left .NE. right
    CASE default
       call lamb_error_throw(error, lamb_failure_level, lamb_wrong_args,&
            "Invalid relation specified: "//"rel", HERE)
       l = .FALSE.
    END SELECT
    IF (.NOT. l) THEN
       WRITE(*,'(1X,A,1X,I9,A4,I9)')"ASSERTION FAILED:",&
            left, "."//rel//".", right
       CALL lamb_error_throw (error, level, etype, msg, lamb_here(routine,line))
    ENDIF
  END SUBROUTINE lamb_int_assert

  SUBROUTINE lamb_int8_assert(left, rel, right, level, etype, msg, routine, line, error)
    INTEGER(kind=int_8), INTENT(IN)                      :: left, right
    CHARACTER(len=2), INTENT(IN)             :: rel
    INTEGER, INTENT(IN)                      :: level, etype
    CHARACTER(len=*), INTENT(IN)             :: msg, routine
    INTEGER, INTENT(IN)                      :: line
    TYPE(lamb_error_type), INTENT(inout)    :: error
    character(len=*), parameter :: routineN = "lamb_int8_assert"

    LOGICAL                                  :: l

!   ---------------------------------------------------------------------------

    SELECT CASE (rel)
    CASE ("EQ")
       l = left .EQ. right
    CASE ("LT")
       l = left .LT. right
    CASE ("LE")
       l = left .LE. right
    CASE ("GT")
       l = left .GT. right
    CASE ("GE")
       l = left .GE. right
    CASE ("NE")
       l = left .NE. right
    CASE default
       call lamb_error_throw(error, lamb_failure_level, lamb_wrong_args,&
            "Invalid relation specified: "//"rel", HERE)
       l = .FALSE.
    END SELECT
    IF (.NOT. l) THEN
       WRITE(*,'(1X,A,1X,I18,A4,I18)')"ASSERTION FAILED:",&
            left, "."//rel//".", right
       CALL lamb_error_throw (error, level, etype, msg, lamb_here(routine,line))
    ENDIF
  END SUBROUTINE lamb_int8_assert

! *****************************************************************************
!> \brief Assertion
!> \param[in] left            left value
!> \param[in] rel             relation
!> \param[in] right           right value
!> \param[in] level           error level
!> \param[in] etype           error type
!> \param[in] routine         Routine name
!> \param[in] msg   Message to display if the assertion fails
! *****************************************************************************
  SUBROUTINE lamb_logical_assert(left, rel, right, level, etype, msg, routine, line, error)
    LOGICAL, INTENT(IN)                      :: left
    CHARACTER(len=*), INTENT(IN)             :: rel
    LOGICAL, INTENT(IN)                      :: right
    INTEGER, INTENT(IN)                      :: level, etype
    CHARACTER(len=*), INTENT(IN)             :: msg, routine
    INTEGER, INTENT(IN)                      :: line
    TYPE(lamb_error_type), INTENT(inout)    :: error
    character(len=*), parameter :: routineN = "lamb_logical_assert"

    LOGICAL                                  :: l

!   ---------------------------------------------------------------------------

    SELECT CASE (rel)
    CASE ("EQV")
       l = left .EQV. right
    CASE ("NEQV")
       l = left .NEQV. right
    CASE ("XOR")
       l = left .NEQV. right
    CASE ("OR")
       l = left .OR. right
    CASE ("AND")
       l = left .AND. right
    CASE ("IMP")
       l = .NOT. left .OR. right
    CASE default
       call lamb_error_throw(error, lamb_failure_level, lamb_wrong_args,&
            "Invalid relation specified: "//"rel", HERE)
       l = .FALSE.
    END SELECT
    IF (.NOT. l) THEN
       WRITE(*,'(1X,A,1X,L1,A,L1)')"ASSERTION FAILED:",&
            left, "."//rel//".", right
       CALL lamb_error_throw (error, level, etype, msg, lamb_here(routine,line))
    ENDIF
  END SUBROUTINE lamb_logical_assert

! *****************************************************************************
!> \brief Assertion
!> \param[in] left            left value
!> \param[in] rel             relation
!> \param[in] right           right value
!> \param[in] level           error level
!> \param[in] etype           error type
!> \param[in] routine         Routine name
!> \param[in] msg   Message to display if the assertion fails
! *****************************************************************************
  SUBROUTINE lamb_char_assert(left, rel, right, level, etype, msg, routine, line, error)
    CHARACTER, INTENT(IN)                    :: left
    CHARACTER(len=2), INTENT(IN)             :: rel
    CHARACTER, INTENT(IN)                    :: right
    INTEGER, INTENT(IN)                      :: level, etype
    CHARACTER(len=*), INTENT(IN)             :: msg, routine
    INTEGER, INTENT(IN)                      :: line
    TYPE(lamb_error_type), INTENT(inout)    :: error
    character(len=*), parameter :: routineN = "lamb_char_assert"

    LOGICAL                                  :: l

!   ---------------------------------------------------------------------------

    SELECT CASE (rel)
    CASE ("EQ")
       l = left .EQ. right
    CASE ("NE")
       l = left .NE. right
    CASE default
       call lamb_error_throw(error, lamb_failure_level, lamb_wrong_args,&
            "Invalid relation specified: "//"rel", HERE)
       l = .FALSE.
    END SELECT
    IF (.NOT. l) THEN
       WRITE(*,'(1X,A,1X,A1,1X,A4,1X,A1)')"ASSERTION FAILED:",&
            left, "."//rel//".", right
       CALL lamb_error_throw (error, level, etype, msg, lamb_here(routine,line))
    ENDIF
  END SUBROUTINE lamb_char_assert


! *****************************************************************************
!> \brief Assertion
!> \param[in] right           right value
!> \param[in] level           error level
!> \param[in] etype           error type
!> \param[in] routine         Routine name
!> \param[in] msg   Message to display if the assertion fails
! *****************************************************************************
  SUBROUTINE lamb_true_assert(right, level, etype, msg, routine, line, error)
    LOGICAL, INTENT(IN)                      :: right
    INTEGER, INTENT(IN)                      :: level, etype
    CHARACTER(len=*), INTENT(IN)             :: msg, routine
    INTEGER, INTENT(IN)                      :: line
    TYPE(lamb_error_type), INTENT(inout)    :: error
    character(len=*), parameter :: routineN = "lamb_true_assert"

    LOGICAL                                  :: l

!   ---------------------------------------------------------------------------

    l = right
    IF (.NOT.l) THEN
       WRITE(*,'(1X,A,1X,L1)')"ASSERTION FAILED:",&
            right
       CALL lamb_error_throw (error, level, etype, msg, lamb_here(routine,line))
    ENDIF
  END SUBROUTINE lamb_true_assert


! *****************************************************************************
!> \brief Assertion
!> \param[in] rel             relation
!> \param[in] right           right value
!> \param[in] level           error level
!> \param[in] etype           error type
!> \param[in] routine         Routine name
!> \param[in] msg   Message to display if the assertion fails
! *****************************************************************************
  SUBROUTINE lamb_not_assert(rel, right, level, etype, msg, routine, line, error)
    CHARACTER(len=3), INTENT(IN)             :: rel
    LOGICAL, INTENT(IN)                      :: right
    INTEGER, INTENT(IN)                      :: level, etype
    CHARACTER(len=*), INTENT(IN)             :: msg, routine
    INTEGER, INTENT(IN)                      :: line
    TYPE(lamb_error_type), INTENT(inout)    :: error

    LOGICAL                                  :: l
    character(len=*), parameter :: routineN = "lamb_not_assert"

!   ---------------------------------------------------------------------------

    SELECT CASE (rel)
    CASE ("NOT")
       l = .NOT. right
    CASE default
       call lamb_error_throw(error, lamb_failure_level, lamb_wrong_args,&
            "Invalid relation specified: "//"rel", HERE)
       l = .FALSE.
    END SELECT
    IF (.NOT. l) THEN
       WRITE(*,'(1X,A,1X,A,L1)')"ASSERTION FAILED:",&
            "."//rel//".", right
       CALL lamb_error_throw (error, level, etype, msg, lamb_here(routine,line))
    ENDIF
  END SUBROUTINE lamb_not_assert

  subroutine lamb_error_set_message(error, message)
    type(lamb_error_type), intent(inout) :: error
    character(len=*), intent(in) :: message
    error%message = message
    write(*,*)message
  end subroutine lamb_error_set_message

  subroutine lamb_error_catch(error, level, message, location)
    type(lamb_error_type), intent(inout) :: error
    integer, intent(in), optional :: level
    character(len=*), intent(in), optional :: message
    type(lamb_error_location), intent(in) :: location
    logical :: die
    if (present (level)) then
       die = error%happened_level .GE. level
    else
       die = error%happened_level .GE. error%die_level
    endif
    if (die) then
       if (present (message)) then
          call lamb_error_throw (error, error%error_level,&
               error%error_type, message, location)
       else
          call lamb_error_throw (error, error%error_level,&
               error%error_type, error%message, location)
       endif
    endif
  end subroutine lamb_error_catch



! *****************************************************************************
!> \brief Sets the level at which error levels to die
!>
!> An error at or above the given level causes the program to abort.
!> If level is not specified, default_die_level is set.
!> \param[in,out] error       error
!> \param[in] level           (optional) Error level at which to trigger death.
! *****************************************************************************
  SUBROUTINE lamb_error_set_die_level (error, level)
    TYPE(lamb_error_type), INTENT(INOUT)    :: error
    INTEGER, INTENT(IN), OPTIONAL            :: level

    INTEGER                                  :: l

    IF (PRESENT (level)) THEN
       l = level
    ELSE
       l = default_die_level
    ENDIF
    IF (debug_mod) THEN
       WRITE(*,*)"set_die_level: current die level", error%die_level
    ENDIF
    error%stack(error%stack_level)%die_level = l
    error%die_level = MAX(error%die_level, l)
    IF (debug_mod) THEN
       WRITE(*,*)"set_die_level: new die level", error%die_level
    ENDIF
  END SUBROUTINE lamb_error_set_die_level


! *****************************************************************************
!> \brief Returns whether an error is deadly
!>
!> \param[in] error           error
!> \param[in] level           Check deadliness for this error level.
!> \result deadly             True if the specified error level is deadly.
! *****************************************************************************
  FUNCTION lamb_error_deadly (error, level) RESULT (deadly)
    TYPE(lamb_error_type), INTENT(INOUT)    :: error
    INTEGER, INTENT(IN)                      :: level
    LOGICAL                                  :: deadly
    deadly = level .GE. error%die_level
  END FUNCTION lamb_error_deadly


! *****************************************************************************
!> \brief Returns whether some error condition has happened.
!>
!> If the error has registered an event that was not enough to trigger
!> death, this function returns true.  Can be used for graceful error
!> handling.
!> \param[in,out] error       error
!> \param[out] level          (optional) Error level encountered.
!> \result not_ok             An error condition has occurred.
! *****************************************************************************
  FUNCTION lamb_error_not_ok (error, level) RESULT (not_ok)
    TYPE(lamb_error_type), INTENT(IN)        :: error
    INTEGER, INTENT(OUT), OPTIONAL           :: level
    LOGICAL                                  :: not_ok

    IF (PRESENT (level)) level = error%happened_level
    not_ok = error%happened_level .GT. lamb_ok_level
  END FUNCTION lamb_error_not_ok

  FUNCTION lamb_error_ok (error, level) RESULT (ok)
    TYPE(lamb_error_type), INTENT(IN)        :: error
    INTEGER, INTENT(OUT), OPTIONAL           :: level
    LOGICAL                                  :: ok

    IF (PRESENT (level)) level = error%happened_level
    ok = error%happened_level .LE. lamb_ok_level
  END FUNCTION lamb_error_ok

  FUNCTION lamb_error_save_level (error) RESULT (level)
    TYPE(lamb_error_type), INTENT(IN)    :: error
    INTEGER            :: level

    level = error%happened_level
  END FUNCTION lamb_error_save_level

  SUBROUTINE lamb_error_update_level (error, level)
    TYPE(lamb_error_type), INTENT(INOUT)    :: error
    INTEGER, INTENT(IN)            :: level

    error%happened_level = MAX(level, error%happened_level)
  END SUBROUTINE lamb_error_update_level


  SUBROUTINE lamb_error_make_ok (error, prev_level)
    TYPE(lamb_error_type), INTENT(INOUT)    :: error
    INTEGER, INTENT(OUT), OPTIONAL :: prev_level

    if (present (prev_level)) prev_level = error%happened_level
    error%happened_level = lamb_ok_level
    error%stack(error%stack_level)%happened_level = lamb_ok_level
  END SUBROUTINE lamb_error_make_ok

  subroutine lamb_error_trace_on(error)
    TYPE(lamb_error_type), INTENT(INOUT)    :: error
    error%tracing = .true.
  end subroutine lamb_error_trace_on
  subroutine lamb_error_trace_off(error)
    TYPE(lamb_error_type), INTENT(INOUT)    :: error
    error%tracing = .false.
  end subroutine lamb_error_trace_off

END MODULE lamb_error
