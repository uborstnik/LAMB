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

!> \brief Sector objects hold submatrices.

!MAKE s d c z
MODULE lamb_sector_X_types

#include "lamb_defs.h"

  use lamb_data_X_types
  use lamb_data_types
  USE lamb_error
  use lamb_index_types
  use lamb_kinds
  use lamb_relation_types
  use lamb_sector_util
  use lamb_subset_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_sector_X_type
  PUBLIC :: lamb_sector_X_type_p

  PUBLIC :: lamb_sector_X_create
  PUBLIC :: lamb_sector_X_copy
  PUBLIC :: lamb_sector_X_destroy

  PUBLIC :: lamb_sector_X_new
  PUBLIC :: lamb_sector_X_hold
  PUBLIC :: lamb_sector_X_release

  PUBLIC :: lamb_sector_X_valid

  PUBLIC :: lamb_sector_X_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'sector_X'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: sector_X_id


  TYPE lamb_sector_X_type
     INTEGER :: refcount
     INTEGER :: id
     type(lamb_index_type) :: index
     type(lamb_data_X_type), dimension(:), allocatable :: data_stores
     type(lamb_relation_type), pointer :: relation
     type(lamb_subset_type_p), dimension(2) :: subsets
     integer(kind=blkn_k) :: n_blks
  END TYPE lamb_sector_X_type

  TYPE lamb_sector_X_type_p
     TYPE(lamb_sector_X_type), POINTER :: p
  END TYPE lamb_sector_X_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[out] sector_X  Object to create
!> \param[in] subsets    The subsets forming this sector.
!> \param[in] relation   The relation governing this sector.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sector_X_create (sector_X, subsets, relation, error)
    TYPE(lamb_sector_X_type), INTENT(OUT):: sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_subset_type_p), dimension(2), intent(in) :: subsets
    type(lamb_relation_type), POINTERIN :: relation

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=locatm_k) :: n_rows

    CALL lamb_error_set (error, error_handle, routineN)

    sector_X%id = 0
    sector_X%refcount = 0

    allocate(sector_X%data_stores(1), stat=stat)
    call lamb_memory_check(stat, "sector_X%data_stores", 1, error)
    sector_X%relation => relation
    call lamb_relation_hold (sector_X%relation, error)
    sector_X%subsets = subsets
    call lamb_subset_hold (sector_X%subsets(1)%p, error)
    call lamb_subset_hold (sector_X%subsets(2)%p, error)
    sector_X%n_blks = 0
    n_rows = subsets(1)%p%n
    call lamb_index_create (sector_X%index, n_rows, error)

    call lamb_data_X_create(sector_X%data_stores(1), error=error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sector_X_create

! *****************************************************************************
!> \brief Copies a sector
!>
!> Shallow copy, except for data & index.
!> \param[in] sector_x       Sector from which the copy is made
!> \param[out] new_sector_X  New sector to which the copy is made
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sector_X_copy (sector_X, new_sector_X, error)
    TYPE(lamb_sector_X_type), INTENT(IN):: sector_X
    TYPE(lamb_sector_X_type), INTENT(OUT):: new_sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer :: n_data_stores, i

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_sector_X_create (new_sector_X,&
         sector_X%subsets, sector_X%relation, error)

    ! Currently the create_sector allocates a dummy data store. Get
    ! rid of it and make a fresh copy.
    call lamb_data_X_destroy(new_sector_X%data_stores(1), error)
    n_data_stores = size(sector_X%data_stores)
    if (n_data_stores > size(new_sector_X%data_stores)) then
       allocate(new_sector_X%data_stores(n_data_stores), stat=stat)
       call lamb_memory_check(stat, "new_sector_X%data_stores", n_data_stores, error)
    endif
    do i = 1, n_data_stores
       call lamb_data_X_copy(sector_X%data_stores(i), new_sector_X%data_stores(i),&
            error)
    enddo

    ! Copy the index
    call lamb_index_destroy(new_sector_X%index, error)
    call lamb_index_copy(sector_X%index, new_sector_X%index, error)

    new_sector_X%n_blks = sector_X%n_blks
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sector_X_copy



! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] sector_X  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sector_X_destroy (sector_X, error)
    TYPE(lamb_sector_X_type), INTENT(INOUT) :: sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer(kind=datastore_k) :: ds

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_index_destroy (sector_X%index, error)
    call lamb_subset_release (sector_X%subsets(1)%p, error)
    call lamb_subset_release (sector_X%subsets(2)%p, error)
    call lamb_relation_release (sector_X%relation, error)
    do ds = 1_datastore_k, int(size(sector_X%data_stores),kind=datastore_k)
       call lamb_data_X_destroy(sector_X%data_stores(ds), error)
    end do
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sector_X_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[out] sector_X  Object to allocate.
!> \param[in] subsets    The subsets forming this sector.
!> \param[in] relation   The relation governing this sector.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sector_X_new (sector_X, subsets, relation, error)
    TYPE(lamb_sector_X_type), POINTEROUT :: sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_subset_type_p), dimension(2), intent(in) :: subsets
    type(lamb_relation_type), POINTERIN :: relation

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (sector_X, stat=stat)
    CALL lamb_memory_check (stat, "sector_X", -1, error)

    CALL lamb_sector_X_create (sector_X, subsets, relation, error)
    sector_X%refcount = 1
    sector_X%id = sector_X_id
    sector_X_id = sector_X_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sector_X_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] sector_X  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sector_X_hold (sector_X, error)
    TYPE(lamb_sector_X_type), POINTERINOUT :: sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    sector_X%refcount = sector_X%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sector_X_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] sector_X  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sector_X_release (sector_X, error)
    TYPE(lamb_sector_X_type), POINTERINOUT :: sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    sector_X%refcount = sector_X%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (sector_X%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            routineN, "Reference count below 0.", __LINE__, error)
    ENDIF
    IF (sector_X%refcount <= 0) THEN
       CALL lamb_sector_X_destroy (sector_X, error)
       DEALLOCATE (sector_X, stat=stat)
       CALL lamb_memory_check (stat, "sector_X", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sector_X_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] sector_X  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_sector_X_valid (sector_X, error) RESULT (valid)
    TYPE(lamb_sector_X_type), POINTERIN :: sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (sector_X)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_sector_X_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] sector_X  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sector_X_verify (sector_X, error)
    TYPE(lamb_sector_X_type), INTENT(IN) :: sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer :: i

    CALL lamb_error_set (error, error_handle, routineN)

    do i = 1, size(sector_X%data_stores)
       call lamb_data_X_verify(sector_X%data_stores(i), error)
    enddo

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sector_X_verify


END MODULE lamb_sector_X_types
