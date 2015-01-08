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

!> \brief Utility function for memory management using MPI.

!MAKE: d s c z i l b w
MODULE lamb_mp_mem_X_util

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_mpi_kinds
  use lamb_mpi_util

  USE ISO_C_BINDING

  IMPLICIT NONE

  PRIVATE

#include "mpif.h"

  public :: lamb_mp_mem_X_allocate
  public :: lamb_mp_mem_X_deallocate

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = '1d_X_util'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  subroutine lamb_mp_mem_X_allocate(mem, length, error)
    doubleprecision, dimension(:), POINTEROUT :: mem
    integer(kind=int_big) :: length
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_mem_X_allocate', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat
    integer(kind=the_mpi_integer_kind) :: ierr

    type(c_ptr) :: mp_baseptr
    integer(kind=the_mpi_addr_kind) :: mp_size
    integer(kind=the_mpi_integer_kind) :: mp_info, mp_res, data_type_size

    call lamb_error_set (error, error_handle, routineN)

    call MPI_Type_size (mpi_datatype_X, data_type_size, ierr)
    call lamb_mpi_check_error (ierr,&
         lamb_here(moduleN, routineN, __LINE__),&
         lamb_failure_level, error,&
         "Could not get the type size.")

    mp_size = length * data_type_size
    mp_info = MPI_INFO_NULL

    ! This is a C function.
    call MPI_Alloc_mem(mp_size, mp_info, mp_baseptr, mp_res)
    call lamb_mpi_check_error (mp_res,&
         lamb_here(moduleN, routineN, __LINE__),&
         lamb_failure_level, error,&
         "Could not allocate memory.")

    call C_F_POINTER (mp_baseptr, mem, (/length/))
    
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_mem_X_allocate


  subroutine lamb_mp_mem_X_deallocate(mem, error)
    doubleprecision, dimension(:), POINTEROUT :: mem
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_mem_X_deallocate', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat
    integer(kind=the_mpi_integer_kind) :: mp_res

    call lamb_error_set (error, error_handle, routineN)

    call MPI_Free_mem(mem, mp_res)
    call lamb_mpi_check_error (mp_res,&
         lamb_here(moduleN, routineN, __LINE__),&
         lamb_failure_level, error,&
         "Could not free memory.")

    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_mem_X_deallocate

END MODULE lamb_mp_mem_X_util
