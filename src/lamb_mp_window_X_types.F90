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

!> \brief The class representing MPI windows.

!MAKE s d c z i l b w
MODULE lamb_mp_window_X_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_mp_env_types
  use lamb_mpi_kinds
  use lamb_mpi_util

  IMPLICIT NONE

#include "mpif.h"

  PRIVATE

  PUBLIC :: lamb_mp_window_X_type
  PUBLIC :: lamb_mp_window_X_type_p

  PUBLIC :: lamb_mp_window_X_create
  PUBLIC :: lamb_mp_window_X_copy
  PUBLIC :: lamb_mp_window_X_destroy

  PUBLIC :: lamb_mp_window_X_new
  PUBLIC :: lamb_mp_window_X_hold
  PUBLIC :: lamb_mp_window_X_release

  PUBLIC :: lamb_mp_window_X_valid

  PUBLIC :: lamb_mp_window_X_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'mp_window_X'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: mp_window_X_id


  TYPE lamb_mp_window_X_type
     INTEGER :: refcount
     INTEGER :: id
     integer(kind=the_mpi_integer_kind) :: win_id
     doubleprecision, dimension(:), pointer :: range
  END TYPE lamb_mp_window_X_type

  TYPE lamb_mp_window_X_type_p
     TYPE(lamb_mp_window_X_type), POINTER :: p
  END TYPE lamb_mp_window_X_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes a window object.
!>
!> Sets up an object for use.
!> \param[out] mp_window_X  Window object to create
!> \param[in] range         Creates window from this range. If it is NULL, then
!>                          no data is exposed.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_window_X_create (mp_window_X, mp_env, range, error)
    TYPE(lamb_mp_window_X_type), INTENT(OUT):: mp_window_X
    type(lamb_mp_env_type), intent(in) :: mp_env
    doubleprecision, dimension(:), POINTER :: range
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer(kind=the_mpi_addr_kind) :: window_size
    integer(the_mpi_integer_kind) :: data_type_size, ierr

    CALL lamb_error_set (error, error_handle, routineN)

    mp_window_X%id = mp_window_X_id
    mp_window_X_id = mp_window_X_id + 1
    mp_window_X%refcount = 0

    call MPI_Type_size (mpi_datatype_X, data_type_size, ierr)
    call lamb_mpi_check_error (ierr,&
         lamb_here(moduleN, routineN, __LINE__),&
         lamb_failure_level, error,&
         "Could not get the type size.")
    if (ASSOCIATED (range)) then
       window_size = size(range, KIND=the_mpi_addr_kind) * int(data_type_size,kind=the_mpi_addr_kind)
    else
       window_size = 0_the_mpi_addr_kind
    endif

    call MPI_Win_create(range, window_size, data_type_size,&
         MPI_INFO_NULL, mp_env%comm_id, mp_window_X%win_id, ierr)
    call lamb_mpi_check_error (ierr,&
         lamb_here(moduleN, routineN, __LINE__),&
         lamb_failure_level, error,&
         "Could not create an MPI window.")
    mp_window_X%range => range
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_window_X_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] old_mp_window_X       Object to copy from
!> \param[out] mp_window_X  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_window_X_copy (old_mp_window_X, mp_window_X, error)
    TYPE(lamb_mp_window_X_type), INTENT(IN):: old_mp_window_X
    TYPE(lamb_mp_window_X_type), INTENT(OUT):: mp_window_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    mp_window_X = old_mp_window_X
    mp_window_X%refcount = 0
    mp_window_X%id = 0

    call lamb_error_throw(error, lamb_warning_level, lamb_internal_error,&
         "An MPI window can not be copied.",&
         HERE)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_window_X_copy


! *****************************************************************************
!> \brief Destroys a window object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] mp_window_X  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_window_X_destroy (mp_window_X, error)
    TYPE(lamb_mp_window_X_type), INTENT(INOUT) :: mp_window_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer(the_mpi_integer_kind) :: ierr

    CALL lamb_error_set (error, error_handle, routineN)

    call MPI_Win_free (mp_window_X%win_id, ierr)
    call lamb_mpi_check_error (ierr,&
         HERE,&
         lamb_failure_level, error,&
         "Error destroying an MPI window.")

    NULLIFY(mp_window_X%range)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_window_X_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] mp_window_X  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_window_X_new (mp_window_X, mp_env, range,error)
    TYPE(lamb_mp_window_X_type), POINTEROUT :: mp_window_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_mp_env_type), intent(in) :: mp_env
    doubleprecision, dimension(:), POINTER :: range

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (mp_window_X, stat=stat)
    CALL lamb_memory_check (stat, "mp_window_X", -1, error)

    CALL lamb_mp_window_X_create (mp_window_X, mp_env, range, error)
    mp_window_X%refcount = 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_window_X_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] mp_window_X  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_window_X_hold (mp_window_X, error)
    TYPE(lamb_mp_window_X_type), POINTERINOUT :: mp_window_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    mp_window_X%refcount = mp_window_X%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_window_X_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] mp_window_X  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_window_X_release (mp_window_X, error)
    TYPE(lamb_mp_window_X_type), POINTERINOUT :: mp_window_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    mp_window_X%refcount = mp_window_X%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (mp_window_X%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            routineN, "Reference count below 0.", __LINE__, error)
    ENDIF
    IF (mp_window_X%refcount <= 0) THEN
       CALL lamb_mp_window_X_destroy (mp_window_X, error)
       DEALLOCATE (mp_window_X, stat=stat)
       CALL lamb_memory_check (stat, "mp_window_X", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_window_X_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] mp_window_X  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_mp_window_X_valid (mp_window_X, error) RESULT (valid)
    TYPE(lamb_mp_window_X_type), POINTERIN :: mp_window_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (mp_window_X)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_mp_window_X_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] mp_window_X  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_window_X_verify (mp_window_X, error)
    TYPE(lamb_mp_window_X_type), INTENT(IN) :: mp_window_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_window_X_verify


END MODULE lamb_mp_window_X_types
