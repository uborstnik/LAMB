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

!> \brief The LAMB matrix type and basic lifecycle methods.

!MAKE s d c z
MODULE lamb_matrix_X_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_distribution_types
  use lamb_distribution_methods
  USE lamb_meta_types
  USE lamb_meta_methods
  USE lamb_sector_X_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_matrix_X_type
  PUBLIC :: lamb_matrix_X_type_p

  PUBLIC :: lamb_matrix_X_create
  PUBLIC :: lamb_matrix_X_copy
  PUBLIC :: lamb_matrix_X_destroy

  PUBLIC :: lamb_matrix_X_new
  PUBLIC :: lamb_matrix_X_hold
  PUBLIC :: lamb_matrix_X_release

  PUBLIC :: lamb_matrix_X_valid

  PUBLIC :: lamb_matrix_X_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'matrix_X'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: matrix_X_id


  TYPE lamb_matrix_X_type
     INTEGER :: refcount
     INTEGER :: id
     !> The metadata about the matrix.
     type(lamb_meta_type) :: meta
     !> These are the submatrices, each containing the index and the data.
     type(lamb_sector_X_type), dimension(:), allocatable :: sectors
     type(lamb_distribution_type) :: distribution
  END TYPE lamb_matrix_X_type

  TYPE lamb_matrix_X_type_p
     TYPE(lamb_matrix_X_type), POINTER :: p
  END TYPE lamb_matrix_X_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] matrix_X  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_matrix_X_create (matrix_X, meta, distribution, error)
    TYPE(lamb_matrix_X_type), INTENT(OUT):: matrix_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_meta_type), intent(in) :: meta
    type(lamb_distribution_type), intent(in) :: distribution

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    !integer(kind=cube_k) :: n_realms

    CALL lamb_error_set (error, error_handle, routineN)

    matrix_X%id = 0
    matrix_X%refcount = 0

    call lamb_meta_copy(meta, matrix_X%meta, error)

    !n_realms = lamb_dist_get_n_local_realms(distribution, error)
    !allocate (matrix_X%sectors(n_realms), stat=stat)
    !call lamb_memory_check (stat, "matrix%sectors", int(n_realms,kind=int_big), error)
    allocate (matrix_X%sectors(0), stat=stat)
    call lamb_memory_check (stat, "matrix%sectors", 0, error)

    call lamb_distribution_copy(distribution, matrix_X%distribution, error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_create


! *****************************************************************************
!> \brief Copies a matrix.
!>
!> The general structure is copied shallowly. The data is copied deeply.
!>
!> \param[in] matrix_X        Source matrix 
!> \param[out] new_matrix_X_  New matrix
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_matrix_X_copy (matrix_X, new_matrix_X, error)
    TYPE(lamb_matrix_X_type), INTENT(IN):: matrix_X
    TYPE(lamb_matrix_X_type), INTENT(OUT):: new_matrix_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=cube_k) :: n_sectors, sector_i
    character(len=def_str_len) :: new_name

    CALL lamb_error_set (error, error_handle, routineN)

    ! Create the matrix & change its name.
    call lamb_matrix_X_create (new_matrix_X,&
         matrix_X%meta, matrix_X%distribution, error)
    new_name = trim(lamb_meta_get_name(matrix_X%meta)) // " (copy)"
    call lamb_meta_set_name(new_matrix_X%meta, new_name)

    ! Now copy the data (including indexes).
    n_sectors = lamb_dist_get_n_local_sectors(matrix_X%distribution, error)
    if (careful_mod) then
       call lamb_assert(int(n_sectors), 'LE', size(matrix_X%sectors),&
            lamb_warning_level, lamb_internal_error, &
            "More sectors defined than present.",&
            routineN, __LINE__, error)
    endif
    call lamb_assert(size(new_matrix_X%sectors), "EQ", 0,&
         lamb_warning_level, lamb_internal_error,&
         "I expect the sectors to not have been allocated yet. "//&
         "Change this to a resize.",&
         routineN, __LINE__, error)
    deallocate (new_matrix_X%sectors, stat=stat)
    call lamb_memory_check (stat, "new_matrix%sectors", error)
    allocate (new_matrix_X%sectors(n_sectors), stat=stat)
    call lamb_memory_check (stat, "new_matrix%sectors", n_sectors, error)
    do sector_i = 1_cube_k, n_sectors
       call lamb_sector_X_copy(matrix_X%sectors(sector_i),&
            new_matrix_X%sectors(sector_i), error=error)
    enddo
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] matrix_X  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_matrix_X_destroy (matrix_X, error)
    TYPE(lamb_matrix_X_type), INTENT(INOUT) :: matrix_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=cube_k) :: si

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_meta_destroy(matrix_X%meta, error)
    do si = 1_cube_k, int(size(matrix_X%sectors),kind=cube_k)
       call lamb_sector_X_destroy (matrix_X%sectors(si), error)
    enddo
    deallocate (matrix_X%sectors, stat=stat)
    call lamb_memory_check (stat, "matrix%sectors", 0, error)

    call lamb_distribution_destroy (matrix_X%distribution, error)
    
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] matrix_X  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_matrix_X_new (matrix_X, meta, distribution, error)
    TYPE(lamb_matrix_X_type), POINTEROUT :: matrix_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_meta_type), intent(in) :: meta
    type(lamb_distribution_type), intent(in) :: distribution

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (matrix_X, stat=stat)
    CALL lamb_memory_check (stat, "matrix_X", -1, error)

    CALL lamb_matrix_X_create (matrix_X, meta, distribution, error)
    matrix_X%refcount = 1
    matrix_X%id = matrix_X_id
    matrix_X_id = matrix_X_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] matrix_X  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_matrix_X_hold (matrix_X, error)
    TYPE(lamb_matrix_X_type), POINTERINOUT :: matrix_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    matrix_X%refcount = matrix_X%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] matrix_X  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_matrix_X_release (matrix_X, error)
    TYPE(lamb_matrix_X_type), POINTERINOUT :: matrix_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    matrix_X%refcount = matrix_X%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (matrix_X%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            routineN, "Reference count below 0.", __LINE__, error)
    ENDIF
    IF (matrix_X%refcount <= 0) THEN
       CALL lamb_matrix_X_destroy (matrix_X, error)
       DEALLOCATE (matrix_X, stat=stat)
       CALL lamb_memory_check (stat, "matrix_X", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] matrix_X  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_matrix_X_valid (matrix_X, error) RESULT (valid)
    TYPE(lamb_matrix_X_type), POINTERIN :: matrix_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (matrix_X)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_matrix_X_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] matrix_X  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_matrix_X_verify (matrix_X, error)
    TYPE(lamb_matrix_X_type), INTENT(IN) :: matrix_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_verify


END MODULE lamb_matrix_X_types
