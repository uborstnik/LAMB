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

!> \brief Methods for the LAMB matrix metadata class.

MODULE lamb_meta_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_meta_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_meta_set_name, lamb_meta_get_name

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'meta_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  pure subroutine lamb_meta_set_name(meta, name)
    type(lamb_meta_type), intent(inout) :: meta
    character(len=*), intent(in) :: name
    meta%name = trim(name)
  end subroutine lamb_meta_set_name

  pure function lamb_meta_get_name(meta) result (name)
    type(lamb_meta_type), intent(in) :: meta
    character(len=def_str_len) :: name
    name = TRIM(meta%name)
  end function lamb_meta_get_name

END MODULE lamb_meta_methods
