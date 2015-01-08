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
module lamb_mp_methods

#include "lamb_defs.h"

  use lamb_kinds
  use lamb_error
  use lamb_mp_env_types
  use lamb_mp_types
  use lamb_mpi_kinds
  use lamb_mpi_util
  use lamb_timing_util

#include "lamb_mp_methods.uses"

  implicit none

  private

#include "mpif.h"

  character(len=*), parameter, private :: moduleN = 'lamb_mp_methods'
  logical, parameter :: careful_mod = .FALSE.

  public :: lamb_mp_test, lamb_mp_wait, lamb_mp_cancel
  public :: lamb_mp_nullify_req
  public :: lamb_mp_req_exists
  public :: lamb_mp_req_get_bw

#include "lamb_mp_methods.publics"

#include "lamb_mp_methods.interfaces"

  contains

  ! int MPI_Test(MPI_Request *request, int *flag, MPI_Status *status)
  subroutine lamb_mp_test (mp_env, request, flag, status, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    type(lamb_mp_op_req_type), intent(inout) :: request
    logical, intent(out) :: flag
    type(lamb_mp_op_status_type), intent(out) :: status
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_test', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr, mpi_flag

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    flag = .false.
    if (request%req_handle /= MPI_REQUEST_NULL) then
       call MPI_Test (&
            request, mpi_flag, status, ierr)
       call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
            "Error performing the Test operation.")
       status%status(MPI_ERROR) = ierr
       request%status = status
       flag = (mpi_flag > 0)
       request%finished = flag
    endif
    if (flag) then
       ! newly finished
       call lamb_timestamp(request%t_req_end)
       request%t_req_focus = request%t_req_end
       request%finished = .true.
    endif
    flag = request%finished

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_test

  ! int MPI_Test(MPI_Request *request, int *flag, MPI_Status *status)
  subroutine lamb_mp_wait (mp_env, request, status, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    type(lamb_mp_op_req_type), intent(inout) :: request
    type(lamb_mp_op_status_type), intent(out) :: status
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_wait', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    if (request%req_handle /= MPI_REQUEST_NULL) then
       call lamb_timestamp(request%t_req_focus)
       call MPI_Wait (request, status, ierr)
       call lamb_timestamp(request%t_req_end)
       call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
            "Error performing the Wait operation.")
       status%status(MPI_ERROR) = ierr
       request%finished = .true.
       request%status = status
    endif

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_wait


! Like MPI cancel, but checks that the request is not NULL.
  subroutine lamb_mp_cancel(request, error)
    type(lamb_mp_op_req_type), intent(inout) :: request
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_cancel', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    if (request%req_handle /= MPI_REQUEST_NULL) then
       call lamb_timestamp(request%t_req_focus)
       request%t_req_end = request%t_req_focus
       call MPI_Cancel (request, ierr)
       call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
            "Error cancelling a background operation.")
    endif

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_cancel

  pure subroutine lamb_mp_nullify_req(req)
    type(lamb_mp_op_req_type), intent(inout) :: req
    req%req_handle = MPI_REQUEST_NULL
    req%finished = .false.
    req%valid = .false.
  end subroutine lamb_mp_nullify_req


  pure function lamb_mp_req_exists(req) result (exists)
    type(lamb_mp_op_req_type), intent(in) :: req
    logical :: exists
    exists = req%req_handle /= MPI_REQUEST_NULL
  end function lamb_mp_req_exists

  pure function lamb_mp_req_get_bw(req) result (bw)
    type(lamb_mp_op_req_type), intent(in) :: req
    real(kind=dp) :: bw
    bw = real(req%volume, kind=dp) / lamb_timing_ceil(req%t_req_end, req%t_req_started)
  end function lamb_mp_req_get_bw

#include "lamb_mp_methods.contains"

end module lamb_mp_methods
