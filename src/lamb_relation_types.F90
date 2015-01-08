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

!> \brief A class to describe the relation between sets.

MODULE lamb_relation_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_set_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_relation_type
  PUBLIC :: lamb_relation_type_p

  PUBLIC :: lamb_relation_create
  PUBLIC :: lamb_relation_copy
  PUBLIC :: lamb_relation_destroy

  PUBLIC :: lamb_relation_new
  PUBLIC :: lamb_relation_hold
  PUBLIC :: lamb_relation_release

  PUBLIC :: lamb_relation_valid

  PUBLIC :: lamb_relation_verify

  CHARACTER, PARAMETER, PUBLIC :: lamb_rel_invalid = '0'
  CHARACTER, PARAMETER, PUBLIC :: lamb_rel_no_symmetry = 'N'
  CHARACTER, PARAMETER, PUBLIC :: lamb_rel_symmetric = 'S'
  CHARACTER, PARAMETER, PUBLIC :: lamb_rel_antisymmetric = 'A'
  CHARACTER, PARAMETER, PUBLIC :: lamb_rel_hermitian = 'H'
  CHARACTER, PARAMETER, PUBLIC :: lamb_rel_antihermitian = 'K'

  integer(kind=int_1), public :: lamb_dim_row = 1
  integer(kind=int_1), public :: lamb_dim_col = 2


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'relation'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: relation_id


  TYPE lamb_relation_type
     INTEGER :: refcount
     INTEGER :: id
     !> An object describes the relation between two sets:
     type(lamb_set_type_p), dimension(2) :: sets
     !> The type of relation between the two sets.
     character :: matrix_type = lamb_rel_invalid
  END TYPE lamb_relation_type

  TYPE lamb_relation_type_p
     TYPE(lamb_relation_type), POINTER :: p
  END TYPE lamb_relation_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[out] relation  Object to create
!> \param[in] sets       The sets this relation describes.
!> \param[in] relation_type  The type of relation (one of lamb_rel_*).
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_relation_create (relation, sets, relation_type, error)
    TYPE(lamb_relation_type), INTENT(OUT):: relation
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_set_type_p), dimension(2), intent(in) :: sets
    character, intent(in) :: relation_type

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_relation_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    relation%id = 0
    relation%refcount = 0
    relation%sets = sets
    call lamb_set_hold (relation%sets(1)%p, error)
    call lamb_set_hold (relation%sets(2)%p, error)
    relation%matrix_type = relation_type
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_relation_create


! *****************************************************************************
!> \brief Copies an object
!>
!> \param[out] relation      Object to copy from.
!. \param[out] new_relation  Object to copy to.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_relation_copy (relation, new_relation, error)
    TYPE(lamb_relation_type), INTENT(IN) :: relation
    TYPE(lamb_relation_type), INTENT(OUT):: new_relation
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_relation_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    new_relation = relation
    call lamb_set_hold (new_relation%sets(1)%p, error)
    call lamb_set_hold (new_relation%sets(2)%p, error)
    new_relation%id = 0
    new_relation%refcount = 0
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_relation_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] relation  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_relation_destroy (relation, error)
    TYPE(lamb_relation_type), INTENT(INOUT) :: relation
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_relation_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)
    
    call lamb_set_release (relation%sets(1)%p, error)
    call lamb_set_release (relation%sets(2)%p, error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_relation_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] relation  Object to allocate.
!> \param[in] sets       The sets this relation describes.
!> \param[in] relation_type  The type of relation (one of lamb_rel_*).
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_relation_new (relation, sets, relation_type, error)
    TYPE(lamb_relation_type), POINTEROUT :: relation
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_set_type_p), dimension(2), intent(in) :: sets
    character, intent(in) :: relation_type

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_relation_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (relation, stat=stat)
    CALL lamb_memory_check (stat, "relation", -1, error)

    CALL lamb_relation_create (relation, sets, relation_type, error)
    relation%refcount = 1
    relation%id = relation_id
    relation_id = relation_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_relation_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] relation  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_relation_hold (relation, error)
    TYPE(lamb_relation_type), POINTERINOUT :: relation
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_relation_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    relation%refcount = relation%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_relation_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] relation  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_relation_release (relation, error)
    TYPE(lamb_relation_type), POINTERINOUT :: relation
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_relation_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    relation%refcount = relation%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (relation%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (relation%refcount <= 0) THEN
       CALL lamb_relation_destroy (relation, error)
       DEALLOCATE (relation, stat=stat)
       CALL lamb_memory_check (stat, "relation", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_relation_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] relation  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_relation_valid (relation, error) RESULT (valid)
    TYPE(lamb_relation_type), POINTERIN :: relation
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_relation_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (relation)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_relation_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] relation  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_relation_verify (relation, error)
    TYPE(lamb_relation_type), INTENT(IN) :: relation
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_relation_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_relation_verify


END MODULE lamb_relation_types
