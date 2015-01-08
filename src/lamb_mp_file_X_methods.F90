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

!> \brief Methods for type-specific multi-processor file I/O.

!MAKE s d c z i l b w u
MODULE lamb_mp_file_X_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_mp_env_types
  use lamb_mp_file_types
  use lamb_mpi_kinds
  use lamb_mpi_util

  IMPLICIT NONE

#include <mpif.h>


  PRIVATE

  public :: lamb_mp_file_X_read

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'mp_file_X_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  subroutine lamb_mp_file_X_read (mp_file, n, buffer, error)
    type(lamb_mp_file_type), intent(inout) :: mp_file
    integer(kind=int_big) :: n
    doubleprecision, dimension(*), intent(out) :: buffer
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_file_X_read', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level, loop_count
    integer, parameter :: retry_max = 10000
    integer(kind=the_mpi_integer_kind) :: ierr, numread, total_read,&
         all_to_read
    integer(kind=the_mpi_integer_kind), dimension(MPI_STATUS_SIZE) :: status
    logical :: finished, first

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok(error, prev_level)

    all_to_read = int(n, kind=the_mpi_integer_kind)
    total_read = 0_the_mpi_integer_kind
    first = .true.
    loop_count = 0
    do while(lamb_error_ok(error) .and. total_read < all_to_read)
       call MPI_File_read (mp_file%handle,&
            buffer, all_to_read, mpi_datatype_X, status, ierr)
       call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
            "Error reading from file.")
       if (lamb_error_ok(error)) then
          first = .false.
          call MPI_GET_COUNT(status, mpi_datatype_X, numread, ierr)
          call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
               "Error getting read count.")
          call lamb_assert (numread, "NE", MPI_UNDEFINED,&
               lamb_failure_level, lamb_external_error,&
               "Undefined count read", routineN, __LINE__, error)
          if (lamb_error_ok(error)) then
             total_read = total_read + numread
          endif
          call lamb_assert (total_read, "EQ", all_to_read,&
               lamb_note_level, lamb_external_error,&
               "Read must be resumed...", routineN, __LINE__, error)
       end if
       loop_count = loop_count+1
       call lamb_assert (loop_count, "LE", retry_max,&
            lamb_warning_level, lamb_external_error,&
            "Too many read retries.",&
            routineN, __LINE__, error=error)
    end do
    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_file_X_read

END MODULE lamb_mp_file_X_methods
