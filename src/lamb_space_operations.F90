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

! *****************************************************************************
!> \brief LAMB operations for spatial calculations
!> \par History
!> \author Urban Borstnik
!> \history
!> - 2010-10-13 [UB] Created for SD DBCSR
!> - 2012-10-23 [UB] Assimilated for LAMB
! *****************************************************************************
MODULE lamb_space_operations

#include "lamb_defs.h"

  use lamb_error
  use lamb_kinds
  use lamb_space_types
  use lamb_array_2d_types


  IMPLICIT NONE

  PRIVATE

! *** Global parameters (in this module) ***

  CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'space_operations'

  logical, parameter :: careful_mod = careful_lamb
  logical, parameter :: debug_mod = debug_lamb

! *** Public type ***

  public :: lamb_space_make_random_points
  public :: lamb_space_fill_points

CONTAINS

! *****************************************************************************
!> \brief Creates random points in space
! *****************************************************************************
  subroutine lamb_space_make_random_points (space, n_points, points, error)
    TYPE(lamb_space_type), intent(IN)              :: space
    integer(kind=allatm_k), intent(IN) :: n_points
    !type(lamb_array_2d_s_type), intent(OUT) :: points
    real(kind=space_k), dimension(:,:), intent(out) :: points
    type(lamb_error_type), intent(INOUT) :: error
    CHARACTER(len=*), PARAMETER :: routineN = 'lamb_space_make_random_points', &
      routineP = moduleN//':'//routineN
    integer(kind=allatm_k) :: i
    integer :: error_handle
    if (trace_lamb) call lamb_error_set (error, error_handle, routineN)
    !allocate (ps(3,n_points))
    !call random_number(ps)
    !forall (i = 1 : n_points)
    !   ps(:,i) = MATMUL (space%h(:,:), ps(:,i))
    !end forall
    !call lamb_array_2d_create (points, ps, error)
    call random_number(points)
    forall (i = 1_allatm_k : n_points)
       points(:,i) = MATMUL (space%h(:,:), points(:,i))
    end forall
    if (trace_lamb) call lamb_error_stop (error, error_handle)
  end subroutine lamb_space_make_random_points

! *****************************************************************************
!> \brief Creates random points in space
! *****************************************************************************
  subroutine lamb_space_fill_points (space, n_points, points, error)
    TYPE(lamb_space_type), intent(IN)              :: space
    integer(kind=allatm_k), intent(IN) :: n_points
    !type(lamb_array_2d_s_type), intent(OUT) :: points
    real(kind=space_k), dimension(:,:), intent(out) :: points
    type(lamb_error_type), intent(INOUT) :: error
    CHARACTER(len=*), PARAMETER :: routineN = 'lamb_space_fill_points', &
      routineP = moduleN//':'//routineN
    integer(kind=allatm_k) :: i
    integer :: d
    integer :: error_handle
    real(kind=space_k), dimension(2,3) :: box
    logical, parameter :: debug = debug_mod
    call lamb_error_set (error, error_handle, routineN)
    forall (d = 1 : 3)
       box(1,d) = 0.0_space_k
       box(2,d) = 1.0_space_k
    end forall
    call fill_curve(points, 1_allatm_k, n_points, box)
    if (debug) then
       do i = 1_allatm_k, n_points
          write(*,*)i,points(:,i)
       enddo
    endif
    forall (i = 1_allatm_k : n_points)
       points(:,i) = MATMUL (space%h(:,:), points(:,i))
    end forall
    call lamb_error_stop (error, error_handle)
  contains
    recursive subroutine fill_curve(points, p_start, p_end, box)
      real(kind=space_k), dimension(:,:), intent(inout) :: points
      integer(kind=allatm_k), intent(in) :: p_start, p_end
      real(kind=space_k), dimension(2,3), intent(in) :: box
      integer :: d
      integer(kind=allatm_k), dimension(9) :: split_bounds
      integer(kind=allatm_k) :: diff
      real(kind=space_k), dimension(2,3) :: new_box
      real(kind=space_k), dimension(3) :: box_split
      if (p_start > p_end) then
         return
      end if
      forall (d = 1:3)
         box_split(d) = (box(1,d) + box(2,d)) / 2.0_space_k
      end forall
      if (p_start == p_end) then
         points(:,p_start) = box_split(:)
      elseif (p_start < p_end) then
         diff = p_end - p_start + 1_allatm_k
         forall (d = 1:8)
            split_bounds(d) = p_start + ((d-1_allatm_k)*diff)/8_allatm_k
         end forall
         split_bounds(9) = p_end+1_allatm_k
         do d = 1, 8
            call cut(box, new_box, box_split, d)
            call fill_curve(points, split_bounds(d), split_bounds(d+1)-1_allatm_k, new_box)
         enddo
      end if
    end subroutine fill_curve
    subroutine cut(box, new_box, box_split, d)
      real(kind=space_k), dimension(2,3), intent(in) :: box
      real(kind=space_k), dimension(2,3), intent(out) :: new_box
      real(kind=space_k), dimension(3), intent(in) :: box_split
      integer, intent(in) :: d
      new_box(:,:) = box(:,:)
      !1-4: below z
      !	1-2: below y
      !		1: below x
      !		2: above x
      !	3:4: above y
      !		3: below x
      !		4: above x
      !5-8: above z
      !	5-6: below y
      !		5: below x
      !		6: above x
      !	7:8: above y
      !		7: below x
      !		8: above x
      select case (d)
      case (1)
         new_box(2,1) = box_split(1)
         new_box(2,2) = box_split(2)
         new_box(2,3) = box_split(3)
      case (2) ! dx
         new_box(1,1) = box_split(1)
         new_box(2,2) = box_split(2)
         new_box(2,3) = box_split(3)
      case (3) ! dy
         new_box(1,1) = box_split(1)
         new_box(1,2) = box_split(2)
         new_box(2,3) = box_split(3)
      case (4) ! dx
         new_box(2,1) = box_split(1)
         new_box(1,2) = box_split(2)
         new_box(2,3) = box_split(3)
      case (5) ! dz
         new_box(2,1) = box_split(1)
         new_box(1,2) = box_split(2)
         new_box(1,3) = box_split(3)
      case (6) ! dx
         new_box(1,1) = box_split(1)
         new_box(1,2) = box_split(2)
         new_box(1,3) = box_split(3)
      case (7) ! dy
         new_box(1,1) = box_split(1)
         new_box(2,2) = box_split(2)
         new_box(1,3) = box_split(3)
      case (8) !dx
         new_box(2,1) = box_split(1)
         new_box(2,2) = box_split(2)
         new_box(1,3) = box_split(3)
      end select
    end subroutine cut
  end subroutine lamb_space_fill_points

END MODULE lamb_space_operations
