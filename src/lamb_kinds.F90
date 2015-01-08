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
! find a list in the README file in the main directory.
!
!******************************************************************************

! *****************************************************************************
!> \brief Defines the basic variable types and kinds.
!>
!> All common integer and real type kinds are defined in this file.
!> Meaningful aliases are defined that map onto the static kinds.
!>
!> \par Type definitions
!> There are standard single-character abbreviations for types:
!> <ul>
!> <li> s real single precision
!> <li> d real double precision
!> <li> c complex single precision
!> <li> z complex double precision
!> <li> h half precision (16-bit)
!> <li> e extended precision (80 bit)
!> <li> q quad precision (128 bit)
!> <li> i integer (32-bit)
!> <li> l long integer (64-bit)
!> <li> b byte (8-bit)
!> <li> (n nibble) (4-bit)
!> <li> w short int (16-bit)
!> <li> (a boolean)
!> <li> u character (8-bit)
!> </ul>
! *****************************************************************************
MODULE lamb_kinds

#ifdef FORTRAN_SPECIFIC_TYPES
  use iso_fortran_env, only: int8, int16, int32, int64, real32, real64
#endif


  IMPLICIT NONE
  
  PRIVATE

  public :: int_1,  int_1_size
  public :: int_2,  int_2_size
  public :: int_4,  int_4_size
  public :: int_8,  int_8_size
  public :: real_4, real_4_size
  public :: real_8, real_8_size


  PUBLIC :: sp, dp
  PUBLIC :: def_str_len
  PUBLIC :: bool_k
  PUBLIC :: int_big


!>@{\name Basic Kinds
!> Basic kinds for integer and real types.
#ifdef FORTRAN_SPECIFIC_TYPES
  integer, parameter :: int_1   = int8
  integer, parameter :: int_2   = int16
  integer, parameter :: int_4   = int32
  integer, parameter :: int_8   = int64
  integer, parameter :: int_16  = selected_int_kind(38)
  integer, parameter :: real_1  = selected_real_kind(0,0)
  integer, parameter :: real_2  = selected_real_kind(3,4)
  integer, parameter :: real_4  = real32
  integer, parameter :: real_8  = real64
  integer, parameter :: real_16 = real128
#else
  integer, parameter :: int_1   = selected_int_kind(2)
  integer, parameter :: int_2   = selected_int_kind(4)
  integer, parameter :: int_4   = selected_int_kind(9)
  integer, parameter :: int_8   = selected_int_kind(18)
  integer, parameter :: int_16  = selected_int_kind(38)
  integer, parameter :: real_1  = selected_real_kind(0,0)
  integer, parameter :: real_2  = selected_real_kind(2,4)
  integer, parameter :: real_4  = selected_real_kind(6,37)
  integer, parameter :: real_8  = selected_real_kind(15,307)
  integer, parameter :: real_16 = selected_real_kind(15,307)
#endif
!>@}

!>@{\name Kind Mnemonics
!> Mnemonics for real types
  integer, parameter :: hp = real_2
  integer, parameter :: sp = real_4
  integer, parameter :: dp = real_8
  integer, parameter :: ep = selected_real_kind(16,307)
  integer, parameter :: qp = real_16
!>@}

!>@{\name Byte Size Definitions
!> Byte sizes for the basic types.
  integer, parameter :: int_1_size   = 1
  integer, parameter :: int_2_size   = 2
  integer, parameter :: int_4_size   = 4
  integer, parameter :: int_8_size   = 8
  integer, parameter :: int_16_size  = 16
  integer, parameter :: real_1_size  = 1
  integer, parameter :: real_2_size  = 2
  integer, parameter :: real_4_size  = 4
  integer, parameter :: real_8_size  = 8
  integer, parameter :: real_16_size = 16
!>@}


!> The default string length.
  INTEGER, PARAMETER :: def_str_len = 80

!> The target is to have the KIND with the smallest storage
!> requirement. Compiler dependent, so defined here.
  INTEGER, PARAMETER :: bool_k = KIND(.TRUE.)

!> The biggest expected integer kind. Useful for generic allocation
!> routines.
  INTEGER, PARAMETER :: int_big = int_8

!> \namespace lamb_kinds
!> \par Kind Aliases
!> A number of integer kinds are used to make the program more
!> flexible (but also tricky).
!>


!> For referencing atoms in the entire, global, set.
  INTEGER, PARAMETER, PUBLIC :: allatm_k = int_8
!> For referencing atoms in a single spatial cube or subset.
  INTEGER, PARAMETER, PUBLIC :: locatm_k = int_2
!> For referencing spatial cubes and regions.
  INTEGER, PARAMETER, PUBLIC :: cube_k = int_2
!> For referencing spatial cubes by dimension.
  INTEGER, PARAMETER, PUBLIC :: dimcube_k = int_1
!> For referencing subsets.
  INTEGER, PARAMETER, PUBLIC :: subset_k = int_4
!!> For referencing sectors (subset x subset).
!  INTEGER, PARAMETER, PUBLIC :: sector_k = int_8
!> For referencing processes.  Should be mindful of MPI restrictions.
  INTEGER, PARAMETER, PUBLIC :: proc_k = int_4
!> For referencing threads.
  INTEGER, PARAMETER, PUBLIC :: thread_k = int_1
!> For referencing block sizes.
  INTEGER, PARAMETER, PUBLIC :: blksz_k = int_1
!> For referencing data stores.
  INTEGER, PARAMETER, PUBLIC :: datastore_k = int_1
!> For referencing block descriptors.
  INTEGER, PARAMETER, PUBLIC :: blkn_k = int_8
!> For referencing data elements.
  INTEGER, PARAMETER, PUBLIC :: dataptr_k = int_8
!> For referencing spatial coordinates.
  INTEGER, PARAMETER, PUBLIC :: space_k = real_4
END MODULE lamb_kinds
