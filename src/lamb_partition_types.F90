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

!> \brief A class to describe the partition of Cartesian space.

MODULE lamb_partition_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_space_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_partition_type
  PUBLIC :: lamb_partition_type_p

  PUBLIC :: lamb_partition_create
  PUBLIC :: lamb_partition_destroy

  PUBLIC :: lamb_partition_new
  PUBLIC :: lamb_partition_hold
  PUBLIC :: lamb_partition_release

  PUBLIC :: lamb_partition_valid
  PUBLIC :: lamb_partition_verify
  public :: lamb_partition_print


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'partition_types'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: partition_id


  TYPE lamb_partition_type
     INTEGER :: refcount
     INTEGER :: id
     type(lamb_space_type), pointer :: space
     integer(kind=dimcube_k), dimension(3) :: ncubes
     !> Local types to help conversions
     real(kind=space_k), dimension(3) :: multiplier
  END TYPE lamb_partition_type

  TYPE lamb_partition_type_p
     TYPE(lamb_partition_type), POINTER :: p
  END TYPE lamb_partition_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] partition  Object to create
!> \param[in] space      Space to partition.
!> \param[in] ncubes     Determines partition of space into cubes by the
!>                       thee dimensions.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_partition_create (partition, space, ncubes, error)
    TYPE(lamb_partition_type), INTENT(OUT) :: partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_space_type), pointerin :: space
    integer(kind=dimcube_k), dimension(3), intent(in) :: ncubes

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_partition_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, dim

    CALL lamb_error_set (error, error_handle, routineN)

    do dim = 1, 3
       call lamb_assert (int(ncubes(dim)), "GT", 0,&
            lamb_failure_level, lamb_wrong_args,&
            "At least one spatial cube is required in every dimension.",&
            routineN, __LINE__, error=error)
       call lamb_assert (int(ncubes(dim)), "LE", int(HUGE(ncubes(1))),&
            lamb_failure_level, lamb_wrong_args, &
            "Too many spatial cubes requested in one of the dimensions.",&
            routineN, __LINE__, error=error)
       if (lamb_error_not_ok (error)) then
          CALL lamb_error_stop (error, error_handle)
          return
       endif
    enddo
    partition%id = 0
    partition%refcount = 0

    partition%ncubes(:) = int(ncubes(:), kind=dimcube_k)
    if (lamb_space_valid (space, error)) then
       partition%space => space
       call lamb_space_hold (partition%space, error)
    else
       call lamb_assert (.false.,&
            lamb_failure_level, lamb_wrong_args,&
            "The space object is not associated.",&
            routineN, __LINE__, error=error)
    endif
    forall (dim = 1:3)
       partition%multiplier(dim) = real(partition%ncubes(dim),kind=space_k) / &
            partition%space%h(dim,dim)
       !!!CUBE This is only for orthogonal space.
    end forall
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_partition_create


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] partition  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_partition_destroy (partition, error)
    TYPE(lamb_partition_type), INTENT(INOUT) :: partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_partition_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_space_release (partition%space, error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_partition_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] partition  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_partition_new (partition, space, ncubes, error)
    TYPE(lamb_partition_type), POINTEROUT :: partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_space_type), pointerin :: space
    integer(kind=dimcube_k), dimension(3), intent(in) :: ncubes

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_partition_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (partition, stat=stat)
    CALL lamb_memory_check (stat, "partition", -1, error)

    CALL lamb_partition_create (partition, space, ncubes, error)
    partition%refcount = 1
    partition%id = partition_id
    partition_id = partition_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_partition_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] partition  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_partition_hold (partition, error)
    TYPE(lamb_partition_type), POINTERINOUT :: partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_partition_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    partition%refcount = partition%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_partition_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] partition  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_partition_release (partition, error)
    TYPE(lamb_partition_type), POINTERINOUT :: partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_partition_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    partition%refcount = partition%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (partition%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (partition%refcount <= 0) THEN
       CALL lamb_partition_destroy (partition, error)
       DEALLOCATE (partition, stat=stat)
       CALL lamb_memory_check (stat, "partition", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_partition_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] partition  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_partition_valid (partition, error) RESULT (valid)
    TYPE(lamb_partition_type), POINTERIN :: partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_partition_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (partition)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_partition_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] partition  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_partition_verify (partition, error)
    TYPE(lamb_partition_type), INTENT(IN) :: partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_partition_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_partition_verify

  SUBROUTINE lamb_partition_print (partition, error)
    TYPE(lamb_partition_type), INTENT(IN) :: partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_partition_print', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    write(*,*)"Partition:", partition%ncubes

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_partition_print


END MODULE lamb_partition_types
