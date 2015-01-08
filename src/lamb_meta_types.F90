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

!> \brief The class to hold LAMB matrix metadata.

MODULE lamb_meta_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_relation_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_meta_type
  PUBLIC :: lamb_meta_type_p

  PUBLIC :: lamb_meta_create
  PUBLIC :: lamb_meta_copy
  PUBLIC :: lamb_meta_destroy

  PUBLIC :: lamb_meta_new
  PUBLIC :: lamb_meta_hold
  PUBLIC :: lamb_meta_release

  PUBLIC :: lamb_meta_valid

  PUBLIC :: lamb_meta_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'meta_types'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: meta_id

  TYPE lamb_meta_type
     INTEGER :: refcount
     INTEGER :: id
     !> The metadata describes what is stored in the matrix.
     !> A name as a human-oriented identifier:
     character(len=def_str_len) :: name
     !> The relation type of the matrix
     type(lamb_relation_type), pointer :: relation
  END TYPE lamb_meta_type

  TYPE lamb_meta_type_p
     TYPE(lamb_meta_type), POINTER :: p
  END TYPE lamb_meta_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[out] meta     The created matrix.
!> \param[in] name      Name of the matrix.
!> \param[in] relation  The relation type of the matrix.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_meta_create (meta, name, relation, error)
    TYPE(lamb_meta_type), INTENT(OUT):: meta
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    character(len=*), intent(in) :: name
    type(lamb_relation_type), POINTERIN :: relation

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_meta_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    meta%id = 0
    meta%refcount = 0

    meta%name = TRIM(name)
    meta%relation => relation
    call lamb_relation_hold (meta%relation, error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_meta_create


! *****************************************************************************
!> \brief Copies an object.
!>
!> \param[in] meta      The existing object.
!> \param[out] name      The new object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_meta_copy (meta, new_meta, error)
    TYPE(lamb_meta_type), INTENT(IN) :: meta
    TYPE(lamb_meta_type), INTENT(OUT) :: new_meta
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_meta_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    new_meta%name = meta%name
    new_meta%relation => meta%relation
    call lamb_relation_hold(new_meta%relation, error)
    new_meta%id = 0
    new_meta%refcount = 0

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_meta_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] meta  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_meta_destroy (meta, error)
    TYPE(lamb_meta_type), INTENT(INOUT) :: meta
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_meta_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_relation_release (meta%relation, error)
    if (careful_mod) then
       meta%name = ''
       meta%id = 0
       meta%refcount = 0
    endif

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_meta_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[out] meta      Matrix object to allocate.
!> \param[in] name       Name of matrix.
!> \param[in] relation   Relation type of the matrix.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_meta_new (meta, name, relation, error)
    TYPE(lamb_meta_type), POINTEROUT :: meta
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    character(len=*), intent(in) :: name
    type(lamb_relation_type), POINTERIN :: relation

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_meta_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (meta, stat=stat)
    CALL lamb_memory_check (stat, "meta", -1, error)

    CALL lamb_meta_create (meta, name, relation, error)
    meta%refcount = 1
    meta%id = meta_id
    meta_id = meta_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_meta_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] meta  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_meta_hold (meta, error)
    TYPE(lamb_meta_type), POINTERINOUT :: meta
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_meta_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    meta%refcount = meta%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_meta_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] meta  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_meta_release (meta, error)
    TYPE(lamb_meta_type), POINTERINOUT :: meta
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_meta_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    meta%refcount = meta%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (meta%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            routineN, "Reference count below 0.", __LINE__, error)
    ENDIF
    IF (meta%refcount <= 0) THEN
       CALL lamb_meta_destroy (meta, error)
       DEALLOCATE (meta, stat=stat)
       CALL lamb_memory_check (stat, "meta", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_meta_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] meta  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_meta_valid (meta, error) RESULT (valid)
    TYPE(lamb_meta_type), POINTERIN :: meta
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_meta_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (meta)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_meta_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] meta  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_meta_verify (meta, error)
    TYPE(lamb_meta_type), INTENT(IN) :: meta
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_meta_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_meta_verify


END MODULE lamb_meta_types
