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


!> \brief Describes the partition of a set into subsets.
MODULE lamb_set_partition_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_partition_types
  use lamb_partition_methods
  use lamb_set_types
  use lamb_set_methods
  use lamb_subset_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_set_partition_type
  PUBLIC :: lamb_set_partition_type_p

  PUBLIC :: lamb_set_partition_create
  PUBLIC :: lamb_set_partition_destroy

  PUBLIC :: lamb_set_partition_new
  PUBLIC :: lamb_set_partition_hold
  PUBLIC :: lamb_set_partition_release

  PUBLIC :: lamb_set_partition_valid

  PUBLIC :: lamb_set_partition_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'set_partition'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: set_partition_id


  !> Mapping of all atoms to their subsets.
  TYPE lamb_set_partition_type
     INTEGER :: refcount
     INTEGER :: id
     type(lamb_partition_type), pointer :: partition
     type(lamb_set_type), pointer :: set
     integer(kind=cube_k) :: n_subsets
     !> Mapping atoms to subsets (subset,postition).
     integer(kind=subset_k), dimension(:), allocatable :: subset_map
     !> Mapping atoms to subsets (subset,postition).
     integer(kind=locatm_k), dimension(:), allocatable :: order_map
     !> Subset sizes.
     integer(kind=locatm_k), dimension(:), allocatable :: n_subset_elements
     !> All subsets. Subsets not in the local catalog may be absent here.
     type(lamb_subset_type_p), dimension(:), allocatable :: subsets
  END TYPE lamb_set_partition_type

  TYPE lamb_set_partition_type_p
     TYPE(lamb_set_partition_type), POINTER :: p
  END TYPE lamb_set_partition_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] set_partition  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_partition_create (set_partition, set, partition, error)
    TYPE(lamb_set_partition_type), INTENT(OUT):: set_partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_set_type), POINTERIN :: set
    type(lamb_partition_type), POINTERIN :: partition

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_partition_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat, i
    integer(kind=allatm_k) :: n_elements
    integer(kind=cube_k) :: n_cubes

    CALL lamb_error_set (error, error_handle, routineN)

    set_partition%id = set_partition_id
    set_partition_id = set_partition_id + 1
    set_partition%refcount = 0
    set_partition%partition => partition
    call lamb_partition_hold (set_partition%partition, error)
    set_partition%set => set
    call lamb_set_hold (set_partition%set, error)

    n_elements = lamb_set_get_n_elements(set)
    allocate(set_partition%subset_map(n_elements), stat=stat)
    call lamb_memory_check(stat, "set_partition%subset_map", n_elements, error)
    allocate(set_partition%order_map(n_elements), stat=stat)
    call lamb_memory_check(stat, "set_partition%order_map", n_elements, error)

    n_cubes = lamb_partition_get_n_cubes(partition)
    set_partition%n_subsets = n_cubes
    allocate(set_partition%n_subset_elements(n_cubes), stat=stat)
    call lamb_memory_check(stat, "set_partition%n_subset_elements", n_cubes, error)

    allocate(set_partition%subsets(n_cubes), stat=stat)
    call lamb_memory_check(stat, "set_partition%subsets", n_cubes, error)
    do i = 1, size(set_partition%subsets)
       nullify(set_partition%subsets(i)%p)
    enddo
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_partition_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] set_partition       Object to copy from
!> \param[out] new_set_partition  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_partition_copy (set_partition, new_set_partition, error)
    TYPE(lamb_set_partition_type), INTENT(IN):: set_partition
    TYPE(lamb_set_partition_type), INTENT(OUT):: new_set_partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_partition_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat, i
    integer(kind=allatm_k) :: n_elements
    integer(kind=cube_k) :: n_cubes

    CALL lamb_error_set (error, error_handle, routineN)

    new_set_partition = set_partition
    new_set_partition%refcount = 0
    new_set_partition%id = 0

    new_set_partition%partition => set_partition%partition
    call lamb_partition_hold(new_set_partition%partition, error)

    new_set_partition%set => set_partition%set
    call lamb_set_hold(new_set_partition%set, error)

    n_elements = lamb_set_get_n_elements(set_partition%set)
    allocate(new_set_partition%subset_map(n_elements), stat=stat)
    call lamb_memory_check(stat, "new_set_partition%subset_map", n_elements, error)
    allocate(new_set_partition%order_map(n_elements), stat=stat)
    call lamb_memory_check(stat, "new_set_partition%order_map", n_elements, error)
    new_set_partition%subset_map(:) = set_partition%subset_map(:)
    new_set_partition%order_map(:) = set_partition%order_map(:)

    n_cubes = set_partition%n_subsets
    allocate(new_set_partition%n_subset_elements(n_cubes), stat=stat)
    call lamb_memory_check(stat, "new_set_partition%n_subset_elements", n_cubes, error)
    new_set_partition%n_subset_elements(:) = set_partition%n_subset_elements(:)

    allocate(new_set_partition%subsets(n_cubes), stat=stat)
    call lamb_memory_check(stat, "new_set_partition%subsets", n_cubes, error)
    do i = 1, size(set_partition%subsets)
       new_set_partition%subsets(i)%p => set_partition%subsets(i)%p
    enddo

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_partition_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] set_partition  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_partition_destroy (set_partition, error)
    TYPE(lamb_set_partition_type), INTENT(INOUT) :: set_partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_partition_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer :: i

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_partition_release (set_partition%partition, error)
    call lamb_set_release (set_partition%set, error)

    deallocate(set_partition%subset_map, stat=stat)
    call lamb_memory_check(stat, "set_partition%subset_map", error)    
    deallocate(set_partition%order_map, stat=stat)
    call lamb_memory_check(stat, "set_partition%order_map", error)

    deallocate(set_partition%n_subset_elements, stat=stat)
    call lamb_memory_check(stat, "set_partition%n_subset_elements", error)    

    do i = 1, size(set_partition%subsets)
       if (associated (set_partition%subsets(i)%p)) then
          call lamb_subset_release (set_partition%subsets(i)%p, error)
       endif
    enddo
    deallocate(set_partition%subsets, stat=stat)
    call lamb_memory_check(stat, "set_partition%subsets", error)    

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_partition_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] set_partition  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_partition_new (set_partition, set, partition, error)
    TYPE(lamb_set_partition_type), POINTEROUT :: set_partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_partition_type), POINTERIN :: partition
    type(lamb_set_type), POINTERIN :: set

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_partition_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (set_partition, stat=stat)
    CALL lamb_memory_check (stat, "set_partition", -1, error)

    CALL lamb_set_partition_create (set_partition, set, partition, error)
    set_partition%refcount = 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_partition_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] set_partition  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_partition_hold (set_partition, error)
    TYPE(lamb_set_partition_type), POINTERINOUT :: set_partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_partition_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    set_partition%refcount = set_partition%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_partition_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] set_partition  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_partition_release (set_partition, error)
    TYPE(lamb_set_partition_type), POINTERINOUT :: set_partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_partition_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    set_partition%refcount = set_partition%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (set_partition%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error, &
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (set_partition%refcount <= 0) THEN
       CALL lamb_set_partition_destroy (set_partition, error)
       DEALLOCATE (set_partition, stat=stat)
       CALL lamb_memory_check (stat, "set_partition", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_partition_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] set_partition  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_set_partition_valid (set_partition, error) RESULT (valid)
    TYPE(lamb_set_partition_type), POINTERIN :: set_partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_partition_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (set_partition)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_set_partition_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] set_partition  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_set_partition_verify (set_partition, error)
    TYPE(lamb_set_partition_type), INTENT(IN) :: set_partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_partition_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_partition_verify


END MODULE lamb_set_partition_types
