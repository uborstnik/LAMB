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

!> \brief A class to describe subsets.

MODULE lamb_subset_types

#include "lamb_defs.h"

  USE lamb_error
  USE lamb_kinds

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_subset_type
  PUBLIC :: lamb_subset_type_p

  PUBLIC :: lamb_subset_create
  PUBLIC :: lamb_subset_destroy

  PUBLIC :: lamb_subset_new
  PUBLIC :: lamb_subset_hold
  PUBLIC :: lamb_subset_release

  PUBLIC :: lamb_subset_valid

  PUBLIC :: lamb_subset_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'subset'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: subset_id


  TYPE lamb_subset_type
     INTEGER :: refcount
     INTEGER :: id
     integer(kind=subset_k) :: pos
     integer(kind=locatm_k) :: n
     integer(kind=blksz_k), dimension(:), allocatable :: blksizes
  END TYPE lamb_subset_type

  TYPE lamb_subset_type_p
     TYPE(lamb_subset_type), POINTER :: p
  END TYPE lamb_subset_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[out] subset  Object to create
!> \param[in] pos      Position (order) of subset.
!> \param[in] n        Number of atoms in subset.
!> \param[in] block_sizes  Block sizes of atoms in subset
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_subset_create (subset, pos, n, block_sizes, error)
    TYPE(lamb_subset_type), INTENT(OUT):: subset
    integer(kind=subset_k), intent(in) :: pos
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=blksz_k), dimension(:), intent(in) :: block_sizes
    integer(kind=locatm_k), intent(in) :: n

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_subset_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)
    if (careful_mod) then
       call lamb_assert(int(n,kind=int_big), "EQ", int(size(block_sizes), kind=int_big),&
            lamb_warning_level, lamb_wrong_args, &
            "Number of blocksizes passed does not matched declared subset size.",&
            routineN, __LINE__, error)
    endif

    subset%id = 0
    subset%refcount = 0

    subset%pos = pos
    subset%n = n

    allocate (subset%blksizes(n), stat=stat)
    call lamb_memory_check(stat, "subset%blksizes", int(n, kind=int_big), error)
    subset%blksizes(:) = block_sizes
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_subset_create


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] subset  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_subset_destroy (subset, error)
    TYPE(lamb_subset_type), INTENT(INOUT) :: subset
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_subset_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    deallocate (subset%blksizes, stat=stat)
    call lamb_memory_check(stat, "subset%blksizes", error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_subset_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] subset  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_subset_new (subset, pos, n, block_sizes, error)
    TYPE(lamb_subset_type), POINTEROUT :: subset
    integer(kind=subset_k), intent(in) :: pos
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=blksz_k), dimension(:), intent(in) :: block_sizes
    integer(kind=locatm_k), intent(in) :: n

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_subset_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (subset, stat=stat)
    CALL lamb_memory_check (stat, "subset", -1, error)

    CALL lamb_subset_create (subset, pos, n, block_sizes, error)
    subset%refcount = 1
    subset%id = subset_id
    subset_id = subset_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_subset_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] subset  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_subset_hold (subset, error)
    TYPE(lamb_subset_type), POINTERINOUT :: subset
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_subset_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    subset%refcount = subset%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_subset_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] subset  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_subset_release (subset, error)
    TYPE(lamb_subset_type), POINTERINOUT :: subset
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_subset_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    subset%refcount = subset%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (subset%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error, &
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (subset%refcount <= 0) THEN
       CALL lamb_subset_destroy (subset, error)
       DEALLOCATE (subset, stat=stat)
       CALL lamb_memory_check (stat, "subset", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_subset_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] subset  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_subset_valid (subset, error) RESULT (valid)
    TYPE(lamb_subset_type), POINTERIN :: subset
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_subset_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (subset)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_subset_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] subset  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_subset_verify (subset, error)
    TYPE(lamb_subset_type), INTENT(IN) :: subset
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_subset_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_subset_verify


END MODULE lamb_subset_types
