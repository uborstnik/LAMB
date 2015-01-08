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

!> \brief Utilities needed by the index methods.

MODULE lamb_index_util

#include "lamb_defs.h"

  USE lamb_error
  use lamb_index_types
  use lamb_kinds
  use lamb_array_1d_types

  IMPLICIT NONE

  PRIVATE

  public :: lamb_index_expand

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'index_util'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  ! Convert from compressed list to list.
  subroutine lamb_index_expand(n_compressed, compressed, expanded)
    integer(kind=locatm_k), intent(in) :: n_compressed
    integer(kind=blkn_k), dimension(:), intent(in) :: compressed
    integer(kind=locatm_k), dimension(:), intent(out) :: expanded
    integer(kind=blkn_k) :: i
    integer(kind=locatm_k) :: bin
    do bin = 1_locatm_k, n_compressed
       expanded(compressed(bin)+1_blkn_k : compressed(bin+1_locatm_k)) &
            = bin
    enddo
  end subroutine lamb_index_expand

END MODULE lamb_index_util
