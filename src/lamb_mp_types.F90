!******************************************************************************
!
! This source code is part of the LAMB library.
!
! Written by Urban Borstnik.
!
! Inspired by CP2K and DBCSR.
!
! Copyright (C) 2012, 2013 Urban Borstnik.
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
! find a list in the README file.
!
!******************************************************************************

! *****************************************************************************
!> \brief Various multi-processor types and functions.
!>
!> Mostly for handling background communication (MPI request, status).
! *****************************************************************************
MODULE lamb_mp_types

  use lamb_error
  use lamb_kinds
  use lamb_mpi_kinds

  IMPLICIT NONE

#include "mpif.h"

  public :: lamb_mp_op_status_type
  public :: lamb_mp_op_req_type

  public :: lamb_mp_interpret_status
  
  PRIVATE

  type lamb_mp_op_status_type
     integer(kind=the_mpi_integer_kind), dimension(MPI_STATUS_SIZE) :: status
  end type lamb_mp_op_status_type

  !> \brief Structure to hold MPI requests and timing statistics.
  !> \var req_handle      MPI request handle
  !> \var t_req_started   Time when the request is made.
  !> \var t_req_focus     Time when the application starts to wait for request completion.
  !> \var t_req_end       Time when the request is actually completed.
  !> \var volume          Amount of data transferred in this request.
  type lamb_mp_op_req_type
     integer(kind=the_mpi_integer_kind) :: req_handle
     logical :: valid, finished
     type(lamb_mp_op_status_type) :: status
     real(kind=dp) :: t_req_started, t_req_focus, t_req_end
     integer(kind=int_big) :: volume
  end type lamb_mp_op_req_type

contains

  subroutine lamb_mp_interpret_status(status, src, tag, ierr, error)
    type(lamb_mp_op_status_type), intent(in) :: status
    integer(kind=proc_k), intent(out) :: src
    integer(kind=the_mpi_integer_kind), intent(out) :: tag, ierr
    type(lamb_error_type), intent(inout) :: error
    src = int(status%status(MPI_SOURCE), kind=proc_k)
    tag = status%status(MPI_TAG)
    ierr = status%status(MPI_ERROR)
  end subroutine lamb_mp_interpret_status

END MODULE lamb_mp_types
