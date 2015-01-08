!******************************************************************************
!
! This source code is part of the LAMB library.
!
! Written by Urban Borstnik.
!
! Inspired by CP2K and DBCSR.
!
! Copyright (C) 2014 Urban Borstnik.
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

MODULE [package]_[module]_types

#include "[package]_defs.h"

  USE [package]_error

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: [package]_[module]_type
  PUBLIC :: [package]_[module]_type_p

  PUBLIC :: [package]_[module]_create
  PUBLIC :: [package]_[module]_copy
  PUBLIC :: [package]_[module]_destroy

  PUBLIC :: [package]_[module]_new
  PUBLIC :: [package]_[module]_hold
  PUBLIC :: [package]_[module]_release

  PUBLIC :: [package]_[module]_valid

  PUBLIC :: [package]_[module]_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = '[module]'

  LOGICAL, PARAMETER :: careful_mod = careful_[package]
  LOGICAL, PARAMETER :: debug_mod = debug_[package]

  INTEGER, PRIVATE, SAVE :: [module]_id


  TYPE [package]_[module]_type
     INTEGER :: refcount
     INTEGER :: id
  END TYPE [package]_[module]_type

  TYPE [package]_[module]_type_p
     TYPE([package]_[module]_type), POINTER :: p
  END TYPE [package]_[module]_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] [module]  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE [package]_[module]_create ([module], error)
    TYPE([package]_[module]_type), INTENT(OUT):: [module]
    TYPE([package]_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = '[package]_[module]_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL [package]_error_set (error, error_handle, routineN)

    [module]%id = [module]_id
    [module]_id = [module]_id + 1
    [module]%refcount = 0

    STOP routineN//" The create method is not implemented."
    
    CALL [package]_error_stop (error, error_handle)
  END SUBROUTINE [package]_[module]_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] old_[module]       Object to copy from
!> \param[out] [module]  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE [package]_[module]_copy (old_[module], [module], error)
    TYPE([package]_[module]_type), INTENT(IN):: old_[module]
    TYPE([package]_[module]_type), INTENT(OUT):: [module]
    TYPE([package]_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = '[package]_[module]_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL [package]_error_set (error, error_handle, routineN)

    [module] = old_[module]
    [module]%refcount = 0
    [module]%id = 0
    STOP routineN//" The copy method is not implemented."

    CALL [package]_error_stop (error, error_handle)
  END SUBROUTINE [package]_[module]_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] [module]  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE [package]_[module]_destroy ([module], error)
    TYPE([package]_[module]_type), INTENT(INOUT) :: [module]
    TYPE([package]_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = '[package]_[module]_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL [package]_error_set (error, error_handle, routineN)

    STOP routineN//" The destroy method is not implemented."
    
    CALL [package]_error_stop (error, error_handle)
  END SUBROUTINE [package]_[module]_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] [module]  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE [package]_[module]_new ([module], error)
    TYPE([package]_[module]_type), POINTEROUT :: [module]
    TYPE([package]_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = '[package]_[module]_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL [package]_error_set (error, error_handle, routineN)

    ALLOCATE ([module], stat=stat)
    CALL [package]_memory_check (stat, "[module]", -1, error)

    CALL [package]_[module]_create ([module], error)
    [module]%refcount = 1

    CALL [package]_error_stop (error, error_handle)
  END SUBROUTINE [package]_[module]_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] [module]  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE [package]_[module]_hold ([module], error)
    TYPE([package]_[module]_type), POINTERINOUT :: [module]
    TYPE([package]_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = '[package]_[module]_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL [package]_error_set (error, error_handle, routineN)

    [module]%refcount = [module]%refcount + 1

    CALL [package]_error_stop (error, error_handle)
  END SUBROUTINE [package]_[module]_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] [module]  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE [package]_[module]_release ([module], error)
    TYPE([package]_[module]_type), POINTERINOUT :: [module]
    TYPE([package]_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = '[package]_[module]_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL [package]_error_set (error, error_handle, routineN)

    [module]%refcount = [module]%refcount - 1
    IF (careful_mod) THEN
       CALL [package]_assert ([module]%refcount, "GE", 0,&
            [package]_fatal_level, [package]_caller_error,&
            routineN, "Reference count below 0.", __LINE__, error)
    ENDIF
    IF ([module]%refcount <= 0) THEN
       CALL [package]_[module]_destroy ([module], error)
       DEALLOCATE ([module], stat=stat)
       CALL [package]_memory_check (stat, "[module]", error)
    ENDIF

    CALL [package]_error_stop (error, error_handle)
  END SUBROUTINE [package]_[module]_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] [module]  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION [package]_[module]_valid ([module], error) RESULT (valid)
    TYPE([package]_[module]_type), POINTERIN :: [module]
    TYPE([package]_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = '[package]_[module]_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL [package]_error_set (error, error_handle, routineN)

    valid = ASSOCIATED ([module])

    CALL [package]_error_stop (error, error_handle)
  END FUNCTION [package]_[module]_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] [module]  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE [package]_[module]_verify ([module], error)
    TYPE([package]_[module]_type), INTENT(IN) :: [module]
    TYPE([package]_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = '[package]_[module]_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL [package]_error_set (error, error_handle, routineN)

    CALL [package]_error_stop (error, error_handle)
  END SUBROUTINE [package]_[module]_verify


END MODULE [package]_[module]_types
