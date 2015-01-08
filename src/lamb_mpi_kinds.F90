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
!> \brief Defines the basic variable types
! *****************************************************************************
MODULE lamb_mpi_kinds

  use lamb_kinds

  IMPLICIT NONE

#include "mpif.h"
  
  PRIVATE

  integer, parameter, public :: the_mpi_integer_kind = int_4
  integer, parameter, public :: the_mpi_addr_kind    = MPI_ADDRESS_KIND
  integer(kind=the_mpi_integer_kind), parameter, public :: lamb_mp_any_source = MPI_ANY_SOURCE
  integer(kind=the_mpi_integer_kind), parameter, public :: lamb_mp_any_tag = MPI_ANY_TAG
  integer, parameter, public :: the_mpi_status_size = MPI_STATUS_SIZE

  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_s = MPI_REAL4
  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_d = MPI_REAL8
  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_c = MPI_COMPLEX8
  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_z = MPI_COMPLEX16

  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_i = MPI_INTEGER4
  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_l = MPI_INTEGER8
  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_b = MPI_INTEGER1
  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_w = MPI_INTEGER2

  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_h = MPI_INTEGER2 ! half-precision 16-bit
  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_q = MPI_REAL16

  integer(kind=the_mpi_integer_kind), parameter, public :: &
       mpi_datatype_u = MPI_CHARACTER

END MODULE lamb_mpi_kinds
