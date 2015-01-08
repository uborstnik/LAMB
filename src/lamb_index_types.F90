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

!> \brief Objects and basic methods to define the index of sectors.

MODULE lamb_index_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_array_1d_types
  use lamb_alloc_1d_util

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_index_type, lamb_blk_info_type
  PUBLIC :: lamb_index_type_p

  PUBLIC :: lamb_index_create
  PUBLIC :: lamb_index_copy
  PUBLIC :: lamb_index_destroy

  PUBLIC :: lamb_index_new
  PUBLIC :: lamb_index_hold
  PUBLIC :: lamb_index_release

  PUBLIC :: lamb_index_valid

  PUBLIC :: lamb_index_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'index'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: index_id

  ! Used logical block metadata is coalesced into a single integer.
  integer, parameter, public :: bits_tr = 0
  integer, parameter, public :: bits_inv_real = 1
  integer, parameter, public :: bits_inv_im = 2


  !> \par Block descriptors
  !> Block metadata.  Storage location and possible block
  !> transformations of individual blocks.
  !>
  !> After testing reading rates, I have decided it is better to have
  !> a per-block structure rather than separate arrays for each type
  !> of block metadata.
  type lamb_blk_info_type
     integer(kind=dataptr_k), dimension(2) :: extent
     integer(kind=datastore_k) :: store
     !logical(kind=bool_k) :: tr, inv_real, inv_im
     integer(kind=int_1) :: bits
  end type lamb_blk_info_type

  !> \par CSR row description
  !> row_p(row) contains blocks row_p(row+1) to row_p(row+1).
  TYPE lamb_index_type
     INTEGER :: refcount
     INTEGER :: id
     logical :: csr
     integer(kind=blkn_k) :: n_idx_blks
     integer(kind=locatm_k) :: n_rows
     integer(kind=blkn_k), dimension(:), allocatable :: row_p
     integer(kind=locatm_k), dimension(:), allocatable :: row_i
     integer(kind=locatm_k), dimension(:), allocatable :: col_i
     type(lamb_blk_info_type), dimension(:), allocatable :: blk_info
  END TYPE lamb_index_type

  TYPE lamb_index_type_p
     TYPE(lamb_index_type), POINTER :: p
  END TYPE lamb_index_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[out] index  Object to create.
!> \param[in] n_rows  Number of rows to index.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_index_create (index, n_rows, error)
    TYPE(lamb_index_type), INTENT(OUT):: index
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=locatm_k), intent(in) :: n_rows

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(blkn_k) :: n

    CALL lamb_error_set (error, error_handle, routineN)

    index%id = 0
    index%refcount = 0

    index%n_rows = n_rows
    index%n_idx_blks = 0_blkn_k
    index%csr = .true.
    n = 1_blkn_k
    allocate(index%row_p(n_rows+1), stat=stat)
    call lamb_memory_check(stat, "index%row_p", n_rows+1, error)
    index%row_p(:) = 0
    allocate (index%col_i(n), stat=stat)
    call lamb_memory_check(stat, "index%col_i", n, error)
    allocate (index%blk_info(n), stat=stat)
    call lamb_memory_check(stat, "index%blk_info", n, error)
        
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_index_create


! *****************************************************************************
!> \brief Copies an index object.
!>
!> \param[in] index       The object from which the copy is made.
!> \param[out] new_index  The object to which the copy is made.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_index_copy (index, new_index, error)
    TYPE(lamb_index_type), INTENT(IN):: index
    TYPE(lamb_index_type), INTENT(OUT):: new_index
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(blkn_k) :: n, i

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_index_create(new_index, index%n_rows, error)

    new_index%n_idx_blks = index%n_idx_blks
    new_index%csr = index%csr

    !row_p, row_i, col_i, blk_info
    if (index%csr) then
       call lamb_alloc_1d_copy(index%row_p, new_index%row_p, error=error)
    else
       call lamb_assert(.false.,&
            lamb_warning_level, lamb_internal_error,&
            "Copy not-CSR index not supported.",&
            routineN, __LINE__, error=error)
       call lamb_alloc_1d_copy(index%row_i, new_index%row_i, error=error)
    endif
    call lamb_alloc_1d_copy(index%col_i, new_index%col_i, error=error)
    n = index%n_idx_blks
    if (n > size(new_index%blk_info,kind=blkn_k)) then
       deallocate (new_index%blk_info, stat=stat)
       call lamb_memory_check(stat, "index%blk_info", error)
       allocate (new_index%blk_info(n), stat=stat)
       call lamb_memory_check(stat, "index%blk_info", n, error)
    end if
    do i = 1, n
       new_index%blk_info(i) = index%blk_info(i)
    enddo
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_index_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] index  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_index_destroy (index, error)
    TYPE(lamb_index_type), INTENT(INOUT) :: index
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    if (index%csr) then
       deallocate(index%row_p, stat=stat)
       call lamb_memory_check(stat, "index%row_p", error)
    else
       deallocate(index%row_i, stat=stat)
       call lamb_memory_check(stat, "index%row_i", error)
    endif
    deallocate (index%col_i, stat=stat)
    call lamb_memory_check(stat, "index%col_i", error)
    deallocate (index%blk_info, stat=stat)
    call lamb_memory_check(stat, "index%blk_info", error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_index_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[out] index  Object to allocate.
!> \param[in] n_rows  Number of rows to index.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_index_new (index, n_rows, error)
    TYPE(lamb_index_type), POINTEROUT :: index
    integer(kind=locatm_k), intent(in) :: n_rows
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (index, stat=stat)
    CALL lamb_memory_check (stat, "index", -1, error)

    CALL lamb_index_create (index, n_rows, error)
    index%refcount = 1
    index%id = index_id
    index_id = index_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_index_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] index  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_index_hold (index, error)
    TYPE(lamb_index_type), POINTERINOUT :: index
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    index%refcount = index%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_index_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] index  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_index_release (index, error)
    TYPE(lamb_index_type), POINTERINOUT :: index
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    index%refcount = index%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (index%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (index%refcount <= 0) THEN
       CALL lamb_index_destroy (index, error)
       DEALLOCATE (index, stat=stat)
       CALL lamb_memory_check (stat, "index", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_index_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] index  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_index_valid (index, error) RESULT (valid)
    TYPE(lamb_index_type), POINTERIN :: index
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (index)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_index_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] index  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_index_verify (index, error)
    TYPE(lamb_index_type), INTENT(IN) :: index
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_index_verify


END MODULE lamb_index_types
