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

!> \brief Methods for the multiprocessing environment.

MODULE lamb_mp_env_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_mpi_kinds
  use lamb_mpi_util
  use lamb_mp_env_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_mp_env_get_n_procs
  PUBLIC :: lamb_mp_env_get_my_proc
  public :: lamb_mp_env_am_master
  public :: lamb_mp_env_barrier

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'mp_env_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb


CONTAINS

  pure function lamb_mp_env_get_n_procs(mp_env) result (n_procs)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=proc_k) :: n_procs
    n_procs = mp_env%n_procs
  end function lamb_mp_env_get_n_procs
  pure function lamb_mp_env_get_my_proc(mp_env) result (my_proc)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=proc_k) :: my_proc
    my_proc = mp_env%my_proc
  end function lamb_mp_env_get_my_proc
  pure function lamb_mp_env_am_master(mp_env) result (am_master)
    type(lamb_mp_env_type), intent(in) :: mp_env
    logical :: am_master
    am_master = (mp_env%my_proc == 0_proc_k)
  end function lamb_mp_env_am_master

! Like MPI cancel, but checks that the request is not NULL.
  subroutine lamb_mp_env_barrier(mp_env, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_env_barrier', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    call MPI_Barrier (mp_env%comm_id, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error syncinc the communicator.")

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_env_barrier
  
END MODULE lamb_mp_env_methods
