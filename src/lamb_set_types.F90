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

!> \brief Describes the matrix data along one dimension.

MODULE lamb_set_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_set_type
  PUBLIC :: lamb_set_type_p

  PUBLIC :: lamb_set_create
  PUBLIC :: lamb_set_destroy

  PUBLIC :: lamb_set_new
  PUBLIC :: lamb_set_hold
  PUBLIC :: lamb_set_release

  PUBLIC :: lamb_set_valid

  PUBLIC :: lamb_set_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'set_types'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: set_id


  !> The matrix structure along each matrix dimension is stored in the
  !> set structure.
  TYPE lamb_set_type
     INTEGER :: refcount
     INTEGER :: id
     integer(kind=allatm_k) :: n
     integer(kind=blksz_k), dimension(:), allocatable :: sizes
     real(kind=space_k), dimension(:,:), allocatable :: coordinates
  END TYPE lamb_set_type

  TYPE lamb_set_type_p
     TYPE(lamb_set_type), POINTER :: p
  END TYPE lamb_set_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[out] set  Object to create
!> \param[in] setsize   Number of items (atoms) in the set.
!> \param[in] sizes     The sizes associated with individual items.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_create (set, setsize, sizes, error)
    TYPE(lamb_set_type), INTENT(OUT):: set
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=allatm_k), INTENT(IN) :: setsize
    integer(kind=blksz_k), dimension(:), intent(IN) :: sizes

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    set%id = 0
    set%refcount = 0

    set%n = setsize
    allocate (set%sizes(setsize), stat=stat)
    call lamb_memory_check(stat, "set%sizes", setsize, error)
    if (careful_lamb) then
       call lamb_assert(int(setsize,int_big), "EQ", int(size(sizes),int_big),&
            lamb_failure_level, lamb_wrong_args,&
            "Declared set size does not match number of sizes given.",&
            routineN, __LINE__, error)
    endif
    allocate (set%coordinates(3,setsize), stat=stat)
    call lamb_memory_check(stat, "set%coordinates", int(3*setsize,kind=int_big), error)
    set%coordinates(:,:) = 0.0_space_k

    set%sizes(:) = sizes(:)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_create


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] set  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_destroy (set, error)
    TYPE(lamb_set_type), INTENT(INOUT) :: set
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    deallocate (set%sizes, stat=stat)
    call lamb_memory_check(stat, "set%sizes", error)

    deallocate (set%coordinates, stat=stat)
    call lamb_memory_check(stat, "set%coordinates", error)

    if (careful_mod) then
       set%n = 0
       set%id = -1
       set%refcount = 0
    endif
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[out] set  Object to allocate.
!> \param[in] setsize   Number of items (atoms) in the set.
!> \param[in] sizes     The sizes associated with individual items.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_new (set, setsize, sizes, error)
    TYPE(lamb_set_type), POINTEROUT :: set
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=allatm_k), INTENT(IN) :: setsize
    integer(kind=blksz_k), dimension(:), intent(IN) :: sizes

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (set, stat=stat)
    CALL lamb_memory_check (stat, "set", -1, error)

    CALL lamb_set_create (set, setsize, sizes, error)
    set%refcount = 1
    set%id = set_id
    set_id = set_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] set  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_hold (set, error)
    TYPE(lamb_set_type), POINTERINOUT :: set
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    set%refcount = set%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] set  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_release (set, error)
    TYPE(lamb_set_type), POINTERINOUT :: set
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    set%refcount = set%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (set%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error, routineN,&
            "Reference count below 0.", __LINE__, error)
    ENDIF
    IF (set%refcount <= 0) THEN
       CALL lamb_set_destroy (set, error)
       DEALLOCATE (set, stat=stat)
       CALL lamb_memory_check (stat, "set", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] set  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_set_valid (set, error) RESULT (valid)
    TYPE(lamb_set_type), POINTERIN :: set
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (set)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_set_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] set  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_verify (set, error)
    TYPE(lamb_set_type), INTENT(IN) :: set
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_verify


END MODULE lamb_set_types
