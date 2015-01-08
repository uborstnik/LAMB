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

!> \brief Utility functions for MPI.

MODULE lamb_mpi_util

  use lamb_error
  use lamb_kinds
  use lamb_mpi_kinds


  IMPLICIT NONE

  PRIVATE

#include "lamb_defs.h"
#include "mpif.h"

  PUBLIC :: lamb_mpi_check_error


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'mpi_util'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  subroutine lamb_mpi_check_error (ierr, where, error_level, error, message)
    integer(kind=the_mpi_integer_kind), intent(in) :: ierr
    type(lamb_error_location), intent(in) :: where
    integer, intent(in) :: error_level
    type(lamb_error_type), intent(inout) :: error
    character(len=*), intent(in), optional :: message

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mpi_check_error', &
         routineP = moduleN//':'//routineN

    character(len=def_str_len) :: the_mpi_error_string
    integer(kind=the_mpi_integer_kind) :: my_ierr, str_len

    if (ierr /= MPI_SUCCESS) then
       str_len = def_str_len
       call MPI_Error_string(ierr, the_mpi_error_string, str_len, my_ierr)
       if (.false.) then
          call lamb_assert(my_ierr, "EQ", MPI_SUCCESS,&
               lamb_warning_level, lamb_external_error,&
               "Error getting MPI Error string.", routineN, __LINE__, error)
       endif
       if (my_ierr == MPI_SUCCESS) then
          call lamb_assert (ierr, "EQ", MPI_SUCCESS,&
               error_level, lamb_external_error,&
               "MPI Error: "//the_mpi_error_string, where%scope, where%line, error)
       endif
       if (present (message)) then
          call lamb_error_set_message (error, message=message)
       endif
    endif
  end subroutine lamb_mpi_check_error

END MODULE lamb_mpi_util
