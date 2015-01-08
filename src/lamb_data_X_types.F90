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

!> \brief Type definitions and basic methods for data objects.

!MAKE s d c z
MODULE lamb_data_X_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_1d_util
  use lamb_1d_X_util

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_data_X_type
  PUBLIC :: lamb_data_X_type_p

  PUBLIC :: lamb_data_X_create
  PUBLIC :: lamb_data_X_copy
  PUBLIC :: lamb_data_X_destroy

  PUBLIC :: lamb_data_X_new
  PUBLIC :: lamb_data_X_hold
  PUBLIC :: lamb_data_X_release

  PUBLIC :: lamb_data_X_valid

  PUBLIC :: lamb_data_X_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'data_X'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: data_X_id

  !> \var d  Data is stored in this array.
  !> \var data_len  Size of data stored in array (which must be less
  !>                than or equal to the actual array length.
  TYPE lamb_data_X_type
     doubleprecision, dimension(:), pointer :: d
     integer(kind=dataptr_k) :: data_len
     logical :: mine
     INTEGER :: refcount
     INTEGER :: id
  END TYPE lamb_data_X_type

  TYPE lamb_data_X_type_p
     TYPE(lamb_data_X_type), POINTER :: p
  END TYPE lamb_data_X_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] data_X  Object to create
!> \param[in] presize  (optional) Preallocate this much data.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_create (data_X, presize, error)
    TYPE(lamb_data_X_type), INTENT(OUT):: data_X
    integer(kind=dataptr_k), intent(in), optional :: presize
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=dataptr_k) :: psize

    CALL lamb_error_set (error, error_handle, routineN)

    data_X%id = 0
    data_X%refcount = 0

    data_X%data_len = 0
    data_X%mine = .true.

    if (present (presize)) then
       psize = presize
    else
       psize = 1
    endif
    allocate (data_X%d (psize), stat=stat)
    call lamb_memory_check (stat, "data_X", psize, error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_data_X_create

! *****************************************************************************
!> \brief Copies a data_X object.
!>
!> \param[in] data_X       Object from which the copy is made.
!> \param[out] new_data_X  Object to which the copy is made.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_copy (data_X, new_data_X, error)
    TYPE(lamb_data_X_type), INTENT(IN):: data_X
    TYPE(lamb_data_X_type), INTENT(OUT):: new_data_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=dataptr_k) :: psize

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_data_X_create (new_data_X, presize=data_X%data_len, error=error)
    new_data_X%data_len = data_X%data_len
    call lamb_1d_X_copy (data_X%d, new_data_X%d, data_X%data_len)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_data_X_copy



! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] data_X  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_destroy (data_X, error)
    TYPE(lamb_data_X_type), INTENT(INOUT) :: data_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    if (data_X%mine) then
       deallocate (data_X%d, stat=stat)
       call lamb_memory_check (stat, "data_X%d", error)
    endif
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_data_X_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] data_X  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_new (data_X, error)
    TYPE(lamb_data_X_type), POINTEROUT :: data_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (data_X, stat=stat)
    CALL lamb_memory_check (stat, "data_X", -1, error)

    CALL lamb_data_X_create (data_X, error=error)
    data_X%refcount = 1
    data_X%id = data_X_id
    data_X_id = data_X_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_data_X_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] data_X  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_hold (data_X, error)
    TYPE(lamb_data_X_type), POINTERINOUT :: data_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    data_X%refcount = data_X%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_data_X_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] data_X  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_release (data_X, error)
    TYPE(lamb_data_X_type), POINTERINOUT :: data_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    data_X%refcount = data_X%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (data_X%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (data_X%refcount <= 0) THEN
       CALL lamb_data_X_destroy (data_X, error)
       DEALLOCATE (data_X, stat=stat)
       CALL lamb_memory_check (stat, "data_X", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_data_X_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] data_X  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_data_X_valid (data_X, error) RESULT (valid)
    TYPE(lamb_data_X_type), POINTERIN :: data_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (data_X)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_data_X_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] data_X  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_verify (data_X, error)
    TYPE(lamb_data_X_type), INTENT(IN) :: data_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_assert (associated(data_X%d),&
         lamb_warning_level, lamb_internal_error,&
         "Data area data pointer not associated.",&
         routineN, __LINE__, error)

    call lamb_assert (data_X%data_len, "LE", int(size(data_X%d), kind=dataptr_k), &
         lamb_warning_level, lamb_caller_error,&
         "Data length can not exceed length of its storage array.",&
         routineN, __LINE__, error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_data_X_verify


END MODULE lamb_data_X_types
