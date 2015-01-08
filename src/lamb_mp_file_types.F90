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

!> \brief A class for multi-processor file I/O.

MODULE lamb_mp_file_types

  USE lamb_error
  use lamb_mp_env_types
  use lamb_mpi_kinds
  use lamb_mpi_util

  IMPLICIT NONE

#include "lamb_defs.h"
#include "mpif.h"


  PRIVATE

  PUBLIC :: lamb_mp_file_type
  PUBLIC :: lamb_mp_file_type_p

  PUBLIC :: lamb_mp_file_create
  PUBLIC :: lamb_mp_file_copy
  PUBLIC :: lamb_mp_file_destroy

  PUBLIC :: lamb_mp_file_new
  PUBLIC :: lamb_mp_file_hold
  PUBLIC :: lamb_mp_file_release

  PUBLIC :: lamb_mp_file_valid

  PUBLIC :: lamb_mp_file_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'mp_file'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: mp_file_id

  integer(kind=the_mpi_integer_kind), parameter, private :: bad_handle = -134


  TYPE lamb_mp_file_type
     INTEGER :: refcount
     INTEGER :: id
     type(lamb_mp_env_type), pointer :: mp_env
     integer(kind=the_mpi_integer_kind) :: handle
  END TYPE lamb_mp_file_type

  TYPE lamb_mp_file_type_p
     TYPE(lamb_mp_file_type), POINTER :: p
  END TYPE lamb_mp_file_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] mp_file  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_file_create (mp_file, mp_env, filename, reading, writing, error)
    TYPE(lamb_mp_file_type), INTENT(OUT):: mp_file
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_mp_env_type), pointerin :: mp_env
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: reading, writing

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_file_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, prev_e_level
    integer(kind=the_mpi_integer_kind) :: mode, info, ierr, file_handle
    logical :: i_reading, i_writing

    CALL lamb_error_set (error, error_handle, routineN)

    mp_file%id = 0
    mp_file%refcount = 0

    mp_file%handle = bad_handle
    mp_file%mp_env => mp_env
    call lamb_mp_env_hold (mp_file%mp_env, error)

    if (present(reading)) then
       i_reading = reading
    else
       i_reading = .false.
    endif
    if (present (writing)) then
       i_writing = writing
    else
       i_writing = .false.
    endif
    call lamb_assert (i_writing, "OR", i_reading,&
         lamb_warning_level, lamb_caller_error,&
         "File to be opened is not for reading nor writing.",&
         routineN, __LINE__, error)
    mode = 0_the_mpi_integer_kind
    mode = MPI_INFO_NULL
    if (i_reading .and. i_writing) then
       mode = MPI_MODE_RDWR
    elseif (i_reading) then
       mode = MPI_MODE_RDONLY
    elseif (i_writing) then
       mode = MPI_MODE_WRONLY
    end if

    info = MPI_INFO_NULL
    call MPI_File_open(mp_env%comm_id, filename, mode, &
         info, file_handle, ierr)
    call lamb_error_make_ok(error, prev_e_level)
    call lamb_error_set_die_level(error, lamb_failure_level+1)
    call lamb_mpi_check_error (ierr, HERE, lamb_failure_level, error=error)
    if (lamb_error_not_ok (error)) then
       call lamb_error_set_message (error, "Could not open file "//filename)
    else
       mp_file%handle = file_handle
    endif
    call lamb_error_update_level(error, prev_e_level)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_file_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] mp_file       Object to copy from
!> \param[out] new_mp_file  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_file_copy (mp_file, new_mp_file, error)
    TYPE(lamb_mp_file_type), INTENT(IN):: mp_file
    TYPE(lamb_mp_file_type), INTENT(OUT):: new_mp_file
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_file_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    new_mp_file = mp_file
    new_mp_file%refcount = 0
    new_mp_file%id = 0

    stop 'not copyable'

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_file_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] mp_file  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_file_destroy (mp_file, error)
    TYPE(lamb_mp_file_type), INTENT(INOUT) :: mp_file
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_file_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer(kind=the_mpi_integer_kind) :: ierr

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_assert (mp_file%handle, "NE", bad_handle, &
         lamb_warning_level, lamb_caller_error,&
         "Attempting to close file with an invalid handle.",&
         routineN, __LINE__, error)
    if (mp_file%handle .ne. bad_handle) then
       call MPI_File_close(mp_file%handle, ierr)
       call lamb_mpi_check_error (ierr, HERE, lamb_warning_level, error=error)
    endif
    call lamb_mp_env_release (mp_file%mp_env, error)
    mp_file%handle = 0

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_file_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] mp_file  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_file_new (mp_file, mp_env, filename,&
       reading, writing, error)
    TYPE(lamb_mp_file_type), POINTEROUT :: mp_file
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_mp_env_type), pointerin :: mp_env
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: reading, writing

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_file_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (mp_file, stat=stat)
    CALL lamb_memory_check (stat, "mp_file", -1, error)

    CALL lamb_mp_file_create (mp_file, mp_env, filename,&
         reading=reading, writing=writing,&
         error=error)
    mp_file%refcount = 1
    mp_file%id = mp_file_id
    mp_file_id = mp_file_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_file_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] mp_file  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_file_hold (mp_file, error)
    TYPE(lamb_mp_file_type), POINTERINOUT :: mp_file
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_file_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    mp_file%refcount = mp_file%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_file_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] mp_file  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_file_release (mp_file, error)
    TYPE(lamb_mp_file_type), POINTERINOUT :: mp_file
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_file_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    mp_file%refcount = mp_file%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (mp_file%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            routineN, "Reference count below 0.", __LINE__, error)
    ENDIF
    IF (mp_file%refcount <= 0) THEN
       CALL lamb_mp_file_destroy (mp_file, error)
       DEALLOCATE (mp_file, stat=stat)
       CALL lamb_memory_check (stat, "mp_file", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_file_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] mp_file  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_mp_file_valid (mp_file, error) RESULT (valid)
    TYPE(lamb_mp_file_type), POINTERIN :: mp_file
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_file_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (mp_file)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_mp_file_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] mp_file  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_file_verify (mp_file, error)
    TYPE(lamb_mp_file_type), INTENT(IN) :: mp_file
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_file_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_file_verify


END MODULE lamb_mp_file_types
