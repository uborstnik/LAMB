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

!> \brief Distribution class type definition and basic methods.

MODULE lamb_distribution_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_plexus_types
  use lamb_set_partition_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_distribution_type
  PUBLIC :: lamb_distribution_type_p

  PUBLIC :: lamb_distribution_create
  PUBLIC :: lamb_distribution_copy
  PUBLIC :: lamb_distribution_destroy

  PUBLIC :: lamb_distribution_new
  PUBLIC :: lamb_distribution_hold
  PUBLIC :: lamb_distribution_release

  PUBLIC :: lamb_distribution_valid

  PUBLIC :: lamb_distribution_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'distribution'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: distribution_id


  TYPE lamb_distribution_type
     INTEGER :: refcount
     INTEGER :: id
     type(lamb_plexus_type), pointer :: plexus
     type(lamb_set_partition_type_p), dimension(2) :: set_partitions
  END TYPE lamb_distribution_type

  TYPE lamb_distribution_type_p
     TYPE(lamb_distribution_type), POINTER :: p
  END TYPE lamb_distribution_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] distribution  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_distribution_create (distribution, plexus, set_partitions, error)
    TYPE(lamb_distribution_type), INTENT(OUT):: distribution
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_plexus_type), pointerin :: plexus
    type(lamb_set_partition_type_p), dimension(2), intent(in) :: set_partitions

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_distribution_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    distribution%id = 0
    distribution%refcount = 0

    distribution%plexus => plexus
    call lamb_plexus_hold(distribution%plexus, error)
    distribution%set_partitions = set_partitions
    call lamb_set_partition_hold(distribution%set_partitions(1)%p, error)
    call lamb_set_partition_hold(distribution%set_partitions(2)%p, error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_distribution_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] distribution       Object to copy from
!> \param[out] new_distribution  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_distribution_copy (distribution, new_distribution, error)
    TYPE(lamb_distribution_type), INTENT(IN):: distribution
    TYPE(lamb_distribution_type), INTENT(OUT):: new_distribution
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_distribution_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    new_distribution = distribution
    new_distribution%refcount = 0
    new_distribution%id = 0

    new_distribution%plexus => distribution%plexus
    call lamb_plexus_hold(new_distribution%plexus, error)
    new_distribution%set_partitions = distribution%set_partitions
    call lamb_set_partition_hold(new_distribution%set_partitions(1)%p, error)
    call lamb_set_partition_hold(new_distribution%set_partitions(2)%p, error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_distribution_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] distribution  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_distribution_destroy (distribution, error)
    TYPE(lamb_distribution_type), INTENT(INOUT) :: distribution
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_distribution_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_plexus_release(distribution%plexus, error)
    call lamb_set_partition_release(distribution%set_partitions(1)%p, error)
    call lamb_set_partition_release(distribution%set_partitions(2)%p, error)

    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_distribution_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] distribution  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_distribution_new (distribution, plexus, set_partitions, error)
    TYPE(lamb_distribution_type), POINTEROUT :: distribution
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_plexus_type), pointerin :: plexus
    type(lamb_set_partition_type_p), dimension(2), intent(in) :: set_partitions

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_distribution_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (distribution, stat=stat)
    CALL lamb_memory_check (stat, "distribution", -1, error)

    CALL lamb_distribution_create (distribution, plexus, set_partitions, error)
    distribution%refcount = 1
    distribution%id = distribution_id
    distribution_id = distribution_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_distribution_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] distribution  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_distribution_hold (distribution, error)
    TYPE(lamb_distribution_type), POINTERINOUT :: distribution
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_distribution_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    distribution%refcount = distribution%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_distribution_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] distribution  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_distribution_release (distribution, error)
    TYPE(lamb_distribution_type), POINTERINOUT :: distribution
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_distribution_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    distribution%refcount = distribution%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (distribution%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (distribution%refcount <= 0) THEN
       CALL lamb_distribution_destroy (distribution, error)
       DEALLOCATE (distribution, stat=stat)
       CALL lamb_memory_check (stat, "distribution", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_distribution_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] distribution  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_distribution_valid (distribution, error) RESULT (valid)
    TYPE(lamb_distribution_type), POINTERIN :: distribution
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_distribution_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (distribution)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_distribution_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] distribution  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_distribution_verify (distribution, error)
    TYPE(lamb_distribution_type), INTENT(IN) :: distribution
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_distribution_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_distribution_verify


END MODULE lamb_distribution_types
