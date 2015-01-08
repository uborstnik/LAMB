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

!> \brief Objects to hold information about the multi-processor environment.

MODULE lamb_mp_env_types

  USE lamb_error
  use lamb_kinds
  use lamb_mpi_kinds
  use lamb_mpi_util

  IMPLICIT NONE

  PRIVATE

#include "lamb_defs.h"
#include "mpif.h"


  PUBLIC :: lamb_mp_env_type
  PUBLIC :: lamb_mp_env_type_p

  PUBLIC :: lamb_mp_env_create
  PUBLIC :: lamb_mp_env_destroy

  PUBLIC :: lamb_mp_env_new
  PUBLIC :: lamb_mp_env_hold
  PUBLIC :: lamb_mp_env_release

  PUBLIC :: lamb_mp_env_valid

  PUBLIC :: lamb_mp_env_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'mp_env'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: mp_env_id


  TYPE lamb_mp_env_type
     INTEGER :: refcount
     INTEGER :: id
     logical :: i_did_init
     integer(kind=the_mpi_integer_kind) :: comm_id
     integer(kind=proc_k) :: n_procs, my_proc
  END TYPE lamb_mp_env_type

  TYPE lamb_mp_env_type_p
     TYPE(lamb_mp_env_type), POINTER :: p
  END TYPE lamb_mp_env_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] mp_env  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_env_create (mp_env, comm, error)
    TYPE(lamb_mp_env_type), INTENT(OUT):: mp_env
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=the_mpi_integer_kind), intent(in), optional :: comm

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_env_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer(kind=the_mpi_integer_kind) :: ierr, the_mpi_rank, the_mpi_size,&
         is_inited

    CALL lamb_error_set (error, error_handle, routineN)

    mp_env%id = 0
    mp_env%refcount = 0

    if (present (comm)) then
       mp_env%i_did_init = .false.
       call MPI_Comm_dup (comm, mp_env%comm_id, ierr)
       call lamb_mpi_check_error (ierr, HERE, lamb_fatal_level, error=error)
    else
       call MPI_Initialized (is_inited, ierr)
       call lamb_mpi_check_error (ierr, HERE, lamb_fatal_level, error=error)
       call lamb_assert (is_inited .EQ. 0,&
         lamb_warning_level, lamb_caller_error,&
         "MPI is already initialized.",&
         routineN, __LINE__, error=error)
       mp_env%i_did_init = .true.
       call MPI_Init (ierr)
       call lamb_mpi_check_error (ierr, HERE, lamb_fatal_level, error=error)
       mp_env%comm_id = MPI_COMM_WORLD

       CALL MPI_Errhandler_set (MPI_COMM_WORLD, MPI_ERRORS_RETURN, ierr)
       call lamb_mpi_check_error (ierr, HERE, lamb_fatal_level, error=error)
    endif

    call MPI_Comm_size (mp_env%comm_id, the_mpi_size, ierr)
    call lamb_mpi_check_error (ierr, HERE, lamb_failure_level, error=error)
    call lamb_assert (int(the_mpi_size, kind=int_8), "LE",&
         int(HUGE(0_proc_k), kind=int_8),&
         lamb_failure_level, lamb_internal_error,&
         "Process kind too small to hold number of actual MPI ranks.",&
         routineN, __LINE__, error=error)
    mp_env%n_procs = int(the_mpi_size, kind=proc_k)

    call MPI_Comm_rank (mp_env%comm_id, the_mpi_rank, ierr)
    call lamb_mpi_check_error (ierr, HERE, lamb_failure_level, error=error)
    mp_env%my_proc = int(the_mpi_rank, kind=proc_k)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_env_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] mp_env       Object to copy from
!> \param[out] new_mp_env  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_env_copy (mp_env, new_mp_env, error)
    TYPE(lamb_mp_env_type), INTENT(IN):: mp_env
    TYPE(lamb_mp_env_type), INTENT(OUT):: new_mp_env
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_env_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    new_mp_env = mp_env
    new_mp_env%refcount = 0
    new_mp_env%id = 0

    stop 'not implemented'

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_env_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] mp_env  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_env_destroy (mp_env, error)
    TYPE(lamb_mp_env_type), INTENT(INOUT) :: mp_env
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_env_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer(kind=the_mpi_integer_kind) :: ierr

    CALL lamb_error_set (error, error_handle, routineN)

    if (mp_env%i_did_init) then
       call MPI_Finalize (ierr)
       call lamb_mpi_check_error (ierr, HERE, lamb_failure_level, error=error)
    else
       call MPI_Comm_free (mp_env%comm_id, ierr)
       call lamb_mpi_check_error (ierr, HERE, lamb_failure_level, error=error)
    endif
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_env_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] mp_env  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_env_new (mp_env, comm, error)
    TYPE(lamb_mp_env_type), POINTEROUT :: mp_env
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=the_mpi_integer_kind), intent(in), optional :: comm

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_env_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (mp_env, stat=stat)
    CALL lamb_memory_check (stat, "mp_env", -1, error)

    CALL lamb_mp_env_create (mp_env, comm=comm, error=error)
    mp_env%refcount = 1
    mp_env%id = mp_env_id
    mp_env_id = mp_env_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_env_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] mp_env  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_env_hold (mp_env, error)
    TYPE(lamb_mp_env_type), POINTERINOUT :: mp_env
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_env_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    mp_env%refcount = mp_env%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_env_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] mp_env  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_env_release (mp_env, error)
    TYPE(lamb_mp_env_type), POINTERINOUT :: mp_env
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_env_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    mp_env%refcount = mp_env%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (mp_env%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error, &
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (mp_env%refcount <= 0) THEN
       CALL lamb_mp_env_destroy (mp_env, error)
       DEALLOCATE (mp_env, stat=stat)
       CALL lamb_memory_check (stat, "mp_env", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_env_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] mp_env  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_mp_env_valid (mp_env, error) RESULT (valid)
    TYPE(lamb_mp_env_type), POINTERIN :: mp_env
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_env_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (mp_env)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_mp_env_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] mp_env  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_env_verify (mp_env, error)
    TYPE(lamb_mp_env_type), INTENT(IN) :: mp_env
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_env_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_env_verify


END MODULE lamb_mp_env_types
