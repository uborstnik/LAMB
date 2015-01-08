!******************************************************************************
!
! This source code is part of the LAMB library.
!
! Written by Urban Borstnik.
!
! Inspired by CP2K and DBCSR.
!
! Copyright (C) 2013 Urban Borstnik.
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

!MAKE s d c z i l b w u
MODULE lamb_mp_X_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_mp_env_types
  use lamb_mp_types
  use lamb_mpi_kinds
  use lamb_mpi_util
  use lamb_timing_util

  IMPLICIT NONE

#include <mpif.h>


  PRIVATE

  public :: lamb_mp_X_sum_v

  public :: lamb_mp_X_max_s
  public :: lamb_mp_X_max_v

  public :: lamb_mp_X_bcast_s
  public :: lamb_mp_X_bcast_v

  public :: lamb_mp_X_alltoall
  public :: lamb_mp_X_alltoall_v

  public :: lamb_mp_X_allgather

  public :: lamb_mp_X_send, lamb_mp_X_isend

  public :: lamb_mp_X_irecv

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'mp_X_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  subroutine lamb_mp_X_sum_v (mp_env, n, send_buffer, recv_buffer, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=int_big), intent(in) :: n
    doubleprecision, dimension(*), intent(in) :: send_buffer
    doubleprecision, dimension(*), intent(out) :: recv_buffer
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_sum_v', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    call MPI_Allreduce (send_buffer, recv_buffer,&
         int(n, kind=the_mpi_integer_kind),&
         mpi_datatype_X, MPI_SUM,&
         mp_env%comm_id, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error performing sum operation.")

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_sum_v

  subroutine lamb_mp_X_max_s (mp_env, send_buffer, recv_buffer, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    doubleprecision, intent(in) :: send_buffer
    doubleprecision, intent(out) :: recv_buffer
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_max_s', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    call MPI_Allreduce (send_buffer, recv_buffer,&
         int(1, kind=the_mpi_integer_kind),&
         mpi_datatype_X, MPI_MAX,&
         mp_env%comm_id, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error performing reduction operation.")

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_max_s

  subroutine lamb_mp_X_max_v (mp_env, n, send_buffer, recv_buffer, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=int_big), intent(in) :: n
    doubleprecision, dimension(n), intent(in) :: send_buffer
    doubleprecision, dimension(n), intent(out) :: recv_buffer
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_max_v', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    call MPI_Allreduce (send_buffer, recv_buffer,&
         int(n, kind=the_mpi_integer_kind),&
         mpi_datatype_X, MPI_MAX,&
         mp_env%comm_id, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error performing reduction operation.")

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_max_v


  subroutine lamb_mp_X_bcast_s (mp_env, buffer, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    doubleprecision, intent(inout) :: buffer
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_bcast_s', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    call MPI_Bcast (buffer, 1_the_mpi_integer_kind, &
         mpi_datatype_X, int(0, kind=the_mpi_integer_kind),&
         mp_env%comm_id, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error performing broadcast operation.")

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_bcast_s


  subroutine lamb_mp_X_bcast_v (mp_env, n, buffer, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=int_big), intent(in) :: n
    doubleprecision, dimension(n), intent(inout) :: buffer
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_bcast_v', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    call MPI_Bcast (buffer, int(n, kind=the_mpi_integer_kind), &
         mpi_datatype_X, int(0, kind=the_mpi_integer_kind),&
         mp_env%comm_id, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error performing broadcast operation.")

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_bcast_v


  !> Sends n data elements to each process and recevies n data elements from
  !> each process.
  subroutine lamb_mp_X_alltoall (mp_env, n, send_buffer, recv_buffer, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=int_big), intent(in) :: n
    doubleprecision, dimension(*), intent(in) :: send_buffer
    doubleprecision, dimension(*), intent(out) :: recv_buffer
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_alltoall', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    call MPI_Alltoall (&
         send_buffer, int(n, kind=the_mpi_integer_kind), mpi_datatype_X,&
         recv_buffer, int(n, kind=the_mpi_integer_kind), mpi_datatype_X,&
         mp_env%comm_id, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error performing Alltoall operation.")

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_alltoall

  !> Sends n_send(p) data elements to each process p and recevies
  !> n_recv(p) data elements from each process p.
  subroutine lamb_mp_X_alltoall_v (mp_env, n_send, send_displ, send_buffer,&
       n_recv, recv_displ, recv_buffer, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=int_big), dimension(0:), intent(in) :: n_send, n_recv, send_displ, recv_displ
    doubleprecision, dimension(*), intent(in) :: send_buffer
    doubleprecision, dimension(*), intent(out) :: recv_buffer
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_alltoall_v', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    call MPI_Alltoallv (&
         send_buffer, int(n_send, kind=the_mpi_integer_kind), &
         int(send_displ, kind=the_mpi_integer_kind), mpi_datatype_X,&
         recv_buffer, int(n_recv, kind=the_mpi_integer_kind), &
         int(recv_displ, kind=the_mpi_integer_kind), mpi_datatype_X,&
         mp_env%comm_id, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error performing Alltoall operation.")

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_alltoall_v


  !> Broadcasts the same n data elements to all process. Recevies n
  !> data elements from each process.
  subroutine lamb_mp_X_allgather (mp_env, n, send_buffer, recv_buffer, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=int_big), intent(in) :: n
    doubleprecision, dimension(*), intent(in) :: send_buffer
    doubleprecision, dimension(*), intent(out) :: recv_buffer
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_allgather', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    call MPI_Allgather (&
         send_buffer, int(n, kind=the_mpi_integer_kind), mpi_datatype_X,&
         recv_buffer, int(n, kind=the_mpi_integer_kind), mpi_datatype_X,&
         mp_env%comm_id, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error performing Allgather operation.")

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_allgather

  subroutine lamb_mp_X_send (mp_env, dest, n, send_buffer, tag, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=proc_k), intent(in) :: dest
    integer(kind=int_big), intent(in) :: n
    doubleprecision, dimension(*) :: send_buffer
    integer(kind=the_mpi_integer_kind), intent(in) :: tag
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_send', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    !int MPI_Send(void *buf, int count, MPI_Datatype datatype, int dest, int tag,
    !MPI_Comm comm)
    call MPI_Send (&
         send_buffer, int(n, kind=the_mpi_integer_kind), mpi_datatype_X,&
         int(dest, kind=the_mpi_integer_kind), tag, mp_env%comm_id, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error performing Send operation.")

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_send


  subroutine lamb_mp_X_irecv (mp_env, source, n, recv_buffer, tag, req, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=proc_k), intent(in) :: source
    integer(kind=int_big), intent(in) :: n
    doubleprecision, dimension(:), pointer :: recv_buffer
    integer(kind=the_mpi_integer_kind), intent(in) :: tag
    type(lamb_mp_op_req_type), intent(out) :: req
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_irecv', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr, type_size

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    call lamb_timestamp(req%t_req_started)
    call MPI_Irecv (&
         recv_buffer, int(n, kind=the_mpi_integer_kind), mpi_datatype_X,&
         int(source, kind=the_mpi_integer_kind), tag,&
         mp_env%comm_id, req%req_handle, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error issuing Irecv operation.")

    call MPI_Type_size(mpi_datatype_X, type_size, ierr)
    call lamb_mpi_check_error (ierr,&
         lamb_here(moduleN, routineN, __LINE__),&
         lamb_failure_level, error,&
         "Could not get the type size.")
    req%volume = n * int(type_size,kind=int_big)
    req%finished = .false.

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_irecv

  subroutine lamb_mp_X_isend (mp_env, dest, n, send_buffer, tag, req, error)
    type(lamb_mp_env_type), intent(in) :: mp_env
    integer(kind=proc_k), intent(in) :: dest
    integer(kind=int_big), intent(in) :: n
    doubleprecision, dimension(:), pointer :: send_buffer
    integer(kind=the_mpi_integer_kind), intent(in) :: tag
    type(lamb_mp_op_req_type), intent(out) :: req
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_X_isend', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=the_mpi_integer_kind) :: ierr, type_size

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    !int MPI_Isend(void *buf, int count, MPI_Datatype datatype, int dest, int tag,
    !MPI_Comm comm, MPI_Request *request)
    call lamb_timestamp(req%t_req_started)
    call MPI_Isend (&
         send_buffer, int(n, kind=the_mpi_integer_kind), mpi_datatype_X,&
         int(dest, kind=the_mpi_integer_kind), tag,&
         mp_env%comm_id, req%req_handle, ierr)
    call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
         "Error issuing Isend operation.")

    call MPI_Type_size(mpi_datatype_X, type_size, ierr)
    call lamb_mpi_check_error (ierr,&
         lamb_here(moduleN, routineN, __LINE__),&
         lamb_failure_level, error,&
         "Could not get the type size.")
    req%volume = n * int(type_size,kind=int_big)
    req%finished = .false.

    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_X_isend


END MODULE lamb_mp_X_methods
