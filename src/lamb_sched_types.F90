!******************************************************************************
!
! This source code is part of the LAMB library.
!
! Written by Urban Borstnik.
!
! Inspired by CP2K and DBCSR.
!
! Copyright (C) 2012, 2013, 2014 Urban Borstnik.
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
! find a list in the README file in the main directory.
!
!******************************************************************************

!> \brief A class to describe a communication schedule.

MODULE lamb_sched_types

#include "lamb_defs.h"

  use lamb_constants
  USE lamb_error
  use lamb_kinds

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_sched_type
  PUBLIC :: lamb_sched_type_p

  PUBLIC :: lamb_sched_create
  PUBLIC :: lamb_sched_copy
  PUBLIC :: lamb_sched_destroy

  PUBLIC :: lamb_sched_new
  PUBLIC :: lamb_sched_hold
  PUBLIC :: lamb_sched_release

  PUBLIC :: lamb_sched_valid

  PUBLIC :: lamb_sched_verify

  integer, parameter, public :: lamb_sched_action_end = -1
  integer, parameter, public :: lamb_sched_action_get = 1
  integer, parameter, public :: lamb_sched_action_put = 2
  integer, parameter, public :: lamb_sched_action_snd = 3
  integer, parameter, public :: lamb_sched_action_rcv = 4


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'sched'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: sched_id

  !> Describes a communication schedule.
  !> \var n_steps    The number of steps in the schedule
  !> \var pair       Communication pair
  TYPE lamb_sched_type
     INTEGER :: refcount
     INTEGER :: id
     integer :: n_steps
     integer(kind=proc_k), dimension(:,:), allocatable :: pairs
     integer, dimension(:,:), allocatable :: what
     integer, dimension(:), allocatable :: actions, eras
  END TYPE lamb_sched_type

  TYPE lamb_sched_type_p
     TYPE(lamb_sched_type), POINTER :: p
  END TYPE lamb_sched_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[out] sched  Object to create
!> \param[in] n_descriptors  (optional) Number of descriptors in each schedule step.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sched_create (sched, n_descriptors, error)
    TYPE(lamb_sched_type), INTENT(OUT):: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer, intent(in), optional :: n_descriptors

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat, n_desc
    integer, parameter :: n_steps = 1

    CALL lamb_error_set (error, error_handle, routineN)

    sched%id = sched_id
    sched_id = sched_id + 1
    sched%refcount = 0

    if (present (n_descriptors)) then
       n_desc = n_descriptors
    else
       n_desc = 1
    endif

    sched%n_steps = 1
    allocate(sched%pairs(2, n_steps), stat=stat)
    call lamb_memory_check(stat, "sched%pairs", 2*n_steps, error)
    allocate(sched%what(n_desc, n_steps), stat=stat)
    call lamb_memory_check(stat, "sched%what", n_desc*n_steps, error)
    allocate(sched%actions(n_steps), stat=stat)
    call lamb_memory_check(stat, "sched%actions", n_steps, error)
    allocate(sched%eras(n_steps), stat=stat)
    call lamb_memory_check(stat, "sched%eras", n_steps, error)
    sched%actions(1) = lamb_sched_action_get
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sched_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] old_sched       Object to copy from
!> \param[out] sched  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sched_copy (old_sched, sched, error)
    TYPE(lamb_sched_type), INTENT(IN):: old_sched
    TYPE(lamb_sched_type), INTENT(OUT):: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    sched = old_sched
    sched%refcount = 0
    sched%id = 0
    STOP routineN//" The copy method is not implemented."

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sched_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] sched  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sched_destroy (sched, error)
    TYPE(lamb_sched_type), INTENT(INOUT) :: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    deallocate(sched%pairs, stat=stat)
    call lamb_memory_check(stat, "sched%pairs", error)
    deallocate(sched%what, stat=stat)
    call lamb_memory_check(stat, "sched%what", error)
    deallocate(sched%actions, stat=stat)
    call lamb_memory_check(stat, "sched%actions", error)
    deallocate(sched%eras, stat=stat)
    call lamb_memory_check(stat, "sched%eras", error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sched_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] sched  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sched_new (sched, n_descriptors, error)
    TYPE(lamb_sched_type), POINTEROUT :: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer, intent(in), optional :: n_descriptors

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (sched, stat=stat)
    CALL lamb_memory_check (stat, "sched", -1, error)

    CALL lamb_sched_create (sched, n_descriptors=n_descriptors, error=error)
    sched%refcount = 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sched_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] sched  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sched_hold (sched, error)
    TYPE(lamb_sched_type), POINTERINOUT :: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    sched%refcount = sched%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sched_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] sched  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sched_release (sched, error)
    TYPE(lamb_sched_type), POINTERINOUT :: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    sched%refcount = sched%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (sched%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            routineN, "Reference count below 0.", __LINE__, error)
    ENDIF
    IF (sched%refcount <= 0) THEN
       CALL lamb_sched_destroy (sched, error)
       DEALLOCATE (sched, stat=stat)
       CALL lamb_memory_check (stat, "sched", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sched_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] sched  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_sched_valid (sched, error) RESULT (valid)
    TYPE(lamb_sched_type), POINTERIN :: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (sched)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_sched_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] sched  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sched_verify (sched, error)
    TYPE(lamb_sched_type), INTENT(IN) :: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sched_verify


END MODULE lamb_sched_types
