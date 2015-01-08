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
module lamb_alloc_1d_util

#include "lamb_defs.h"

#include "lamb_alloc_1d_util.uses"

  implicit none

  private

#include "lamb_alloc_1d_util.publics"
  public :: lamb_alloc_1d_ensure_size

#include "lamb_alloc_1d_util.interfaces"

     interface lamb_alloc_1d_ensure_size
          module procedure&
              lamb_alloc_1d_d_ensure_size_i8,&
              lamb_alloc_1d_s_ensure_size_i8,&
              lamb_alloc_1d_c_ensure_size_i8,&
              lamb_alloc_1d_z_ensure_size_i8,&
              lamb_alloc_1d_i_ensure_size_i8,&
              lamb_alloc_1d_l_ensure_size_i8,&
              lamb_alloc_1d_b_ensure_size_i8,&
              lamb_alloc_1d_w_ensure_size_i8,&

              lamb_alloc_1d_d_ensure_size_i4,&
              lamb_alloc_1d_s_ensure_size_i4,&
              lamb_alloc_1d_c_ensure_size_i4,&
              lamb_alloc_1d_z_ensure_size_i4,&
              lamb_alloc_1d_i_ensure_size_i4,&
              lamb_alloc_1d_l_ensure_size_i4,&
              lamb_alloc_1d_b_ensure_size_i4,&
              lamb_alloc_1d_w_ensure_size_i4
       end interface lamb_alloc_1d_ensure_size


  character(len=*), parameter, private :: moduleN = 'lamb_alloc_1d_util'

  logical, parameter :: careful_mod = .FALSE.

  contains

#include "lamb_alloc_1d_util.contains"

end module lamb_alloc_1d_util
