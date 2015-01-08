!******************************************************************************
!
! This source code is part of the LAMB library.
!
! Written by Urban Borstnik.
!
! Inspired by CP2K and DBCSR.
!
! Copyright (C) 2012 Urban Borstnik.
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
!> \author Urban Borstnik
! *****************************************************************************


MODULE lamb_constants

  IMPLICIT NONE
  
  PRIVATE

  public :: imeta, idata
  public :: isrc, idst
  public :: irow, icol

!> For referring to size-2 array indices.
  integer, parameter :: imeta = 1, idata = 2

!> For referring to source vs. destination.
  integer, parameter :: isrc = 1, idst = 2

!> For referring to size-2 array indices
  integer, parameter :: irow = 1, icol = 2

  real, parameter, public :: scale_factor = 1.2

!> Copied from DBCSR
!> Type definitions:
!> * bit 0: always 1
!> * bit 1: single (0: 4) vs. double (1: 8)
!> * bit 2: real (0) vs. complex (1)
!> * bit 3: dimension (0: 1, 1: 2) [not used]
!> * bit 4: floating point (0) or integer type (1) [not used]
  INTEGER, PARAMETER, PUBLIC   :: lamb_type_real_4 = 1         !001
  INTEGER, PARAMETER, PUBLIC   :: lamb_type_real_8 = 3         !011
  INTEGER, PARAMETER, PUBLIC   :: lamb_type_complex_4 = 5      !101
  INTEGER, PARAMETER, PUBLIC   :: lamb_type_complex_8 = 7      !111

END MODULE lamb_constants
