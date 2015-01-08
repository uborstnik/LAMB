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

!> \brief A class to hold sectors specific to internal multiplication.

!MAKE s d c z
MODULE lamb_mult_sector_X_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_data_X_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_mult_sector_X_type
  PUBLIC :: lamb_mult_sector_X_type_p

  PUBLIC :: lamb_mult_sector_X_create
  PUBLIC :: lamb_mult_sector_X_copy
  PUBLIC :: lamb_mult_sector_X_destroy

  PUBLIC :: lamb_mult_sector_X_new
  PUBLIC :: lamb_mult_sector_X_hold
  PUBLIC :: lamb_mult_sector_X_release

  PUBLIC :: lamb_mult_sector_X_valid

  PUBLIC :: lamb_mult_sector_X_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'mult_sector_X'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: mult_sector_X_id


  TYPE lamb_mult_sector_X_type
     INTEGER :: refcount
     INTEGER :: id
     integer(kind=blkn_k) :: n_idx_blks
     integer(kind=locatm_k) :: n_rows
     type(lamb_data_X_type) :: data
     integer(kind=blkn_k), dimension(:), pointer :: row_p
     integer(kind=blkn_k), dimension(:), pointer :: col_i
     integer(kind=dataptr_k), dimension(:), pointer :: extents
     integer(kind=int_1), dimension(:), pointer :: bits
  END TYPE lamb_mult_sector_X_type

  TYPE lamb_mult_sector_X_type_p
     TYPE(lamb_mult_sector_X_type), POINTER :: p
  END TYPE lamb_mult_sector_X_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] mult_sector_X  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mult_sector_X_create (mult_sector_X, error)
    TYPE(lamb_mult_sector_X_type), INTENT(OUT):: mult_sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mult_sector_X_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    mult_sector_X%id = mult_sector_X_id
    mult_sector_X_id = mult_sector_X_id + 1
    mult_sector_X%refcount = 0
    NULLIFY(mult_sector_X%row_p)
    NULLIFY(mult_sector_X%col_i)
    NULLIFY(mult_sector_X%extents)
    NULLIFY(mult_sector_X%bits)
    call lamb_data_X_create (mult_sector_X%data, error=error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mult_sector_X_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] old_mult_sector_X       Object to copy from
!> \param[out] mult_sector_X  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mult_sector_X_copy (old_mult_sector_X, mult_sector_X, error)
    TYPE(lamb_mult_sector_X_type), INTENT(IN):: old_mult_sector_X
    TYPE(lamb_mult_sector_X_type), INTENT(OUT):: mult_sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mult_sector_X_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    mult_sector_X = old_mult_sector_X
    mult_sector_X%refcount = 0
    mult_sector_X%id = 0
    STOP routineN//" The copy method is not implemented."

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mult_sector_X_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] mult_sector_X  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mult_sector_X_destroy (mult_sector_X, error)
    TYPE(lamb_mult_sector_X_type), INTENT(INOUT) :: mult_sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mult_sector_X_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_data_X_destroy (mult_sector_X%data, error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mult_sector_X_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] mult_sector_X  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mult_sector_X_new (mult_sector_X, error)
    TYPE(lamb_mult_sector_X_type), POINTEROUT :: mult_sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mult_sector_X_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (mult_sector_X, stat=stat)
    CALL lamb_memory_check (stat, "mult_sector_X", -1, error)

    CALL lamb_mult_sector_X_create (mult_sector_X, error)
    mult_sector_X%refcount = 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mult_sector_X_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] mult_sector_X  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mult_sector_X_hold (mult_sector_X, error)
    TYPE(lamb_mult_sector_X_type), POINTERINOUT :: mult_sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mult_sector_X_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    mult_sector_X%refcount = mult_sector_X%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mult_sector_X_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] mult_sector_X  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mult_sector_X_release (mult_sector_X, error)
    TYPE(lamb_mult_sector_X_type), POINTERINOUT :: mult_sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mult_sector_X_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    mult_sector_X%refcount = mult_sector_X%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (mult_sector_X%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            routineN, "Reference count below 0.", __LINE__, error)
    ENDIF
    IF (mult_sector_X%refcount <= 0) THEN
       CALL lamb_mult_sector_X_destroy (mult_sector_X, error)
       DEALLOCATE (mult_sector_X, stat=stat)
       CALL lamb_memory_check (stat, "mult_sector_X", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mult_sector_X_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] mult_sector_X  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_mult_sector_X_valid (mult_sector_X, error) RESULT (valid)
    TYPE(lamb_mult_sector_X_type), POINTERIN :: mult_sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mult_sector_X_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (mult_sector_X)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_mult_sector_X_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] mult_sector_X  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mult_sector_X_verify (mult_sector_X, error)
    TYPE(lamb_mult_sector_X_type), INTENT(IN) :: mult_sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mult_sector_X_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mult_sector_X_verify


END MODULE lamb_mult_sector_X_types
