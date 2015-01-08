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

!> \brief Timing utilities.

MODULE lamb_timing_util

  use lamb_kinds

  !$ USE OMP_LIB

  IMPLICIT NONE

  PRIVATE

#include "lamb_defs.h"

  PUBLIC :: lamb_timestamp
  public :: lamb_timing_ceil


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'mpi_timing_util'

  LOGICAL, PARAMETER :: careful_mod = .FALSE.
  LOGICAL, PARAMETER :: debug_mod = .FALSE.

  real(kind=dp), save :: timing_eps

CONTAINS

  subroutine lamb_timestamp(place)
    real(kind=dp), intent(out) :: place
    logical, save :: first = .TRUE.
    integer, save :: epoch, last, tick_rate, max_ticks
    integer :: ticks
    real(kind=dp), save :: inv_rate, n_overflows, max_ticks_f
    !$ if (.true.) then
    !$   place = OMP_GET_WTIME()
    !$ else
    if (first) then
       call system_clock(epoch, tick_rate, max_ticks)
       place = 0.0_dp
       last = epoch
       inv_rate = 1.0_dp / real(tick_rate, kind=dp)
       n_overflows = 0.0_dp
       max_ticks_f = real(max_ticks, kind=dp)
       timing_eps = inv_rate
    else
       call system_clock(ticks)
       place = real(ticks - epoch, kind=dp) * inv_rate
       if (ticks <= last) then
          n_overflows = n_overflows + 1.0_dp
          place = place + n_overflows * max_ticks_f * inv_rate
       endif
    endif
    !$ endif
  end subroutine lamb_timestamp

  pure function lamb_timing_ceil(t2, t1) result (cv)
    real(kind=dp), intent(in) :: t2, t1
    real(kind=dp) :: cv
    real(kind=dp) :: d
    d = t2-t1
    if (d < timing_eps) then
       cv = timing_eps
    else
       cv = d
    endif
  end function lamb_timing_ceil
END MODULE lamb_timing_util
