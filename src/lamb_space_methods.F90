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
MODULE lamb_space_methods

#include "lamb_defs.h"

  use lamb_error
  use lamb_kinds
  use lamb_space_types


  IMPLICIT NONE

  PRIVATE

! *** Global parameters (in this module) ***

  CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'space_methods'

  logical, parameter :: careful_mod = careful_lamb
  logical, parameter :: debug_mod = debug_lamb

! *** Public type ***

  public :: lamb_point_originate, lamb_point_delta
  public :: lamb_point_reimage, lamb_point_get_image

  public :: lamb_min_vector
  public :: lamb_distance, lamb_sq_distance, lamb_vector_len

  public :: lamb_space_new_unit_box


  interface lamb_point_originate
     module procedure lamb_point_originate_v
  end interface

  interface lamb_point_delta
     module procedure lamb_point_delta_v
  end interface

  interface lamb_min_vector
     module procedure lamb_min_vector_v
  end interface

  interface lamb_point_reimage
     module procedure lamb_point_reimage_v, lamb_point_reimage_s
  end interface

  interface lamb_point_get_image
     module procedure lamb_point_get_image_v
  end interface

  interface lamb_distance
     module procedure lamb_distance_v
  end interface
  interface lamb_sq_distance
     module procedure lamb_sq_distance_v
  end interface

  interface lamb_vector_len
     module procedure lamb_vector_len_v
  end interface

CONTAINS

! *****************************************************************************
!> \brief Folds a point into the base cell with one corner in origin
!> \param[in] space   Space
!> \param[in] point   Point to fold
!> \return base_point Point image in the base cell
!> \author UB (Much code obtained from CP2K cell_types.)
! *****************************************************************************
  pure function lamb_point_originate_v(space, point) RESULT (base_point)
    TYPE(lamb_space_type), INTENT(IN)              :: space
    real(kind=space_k), dimension(3), intent(in)    :: point
    real(kind=space_k), dimension(3)                :: base_point

    CHARACTER(len=*), PARAMETER :: routineN = 'lamb_point_originate_v', &
      routineP = moduleN//':'//routineN

    integer :: d
    real(kind=space_k), dimension(3) :: s
    real(kind=space_k), parameter :: half = 0.5_space_k

    IF (space%orthorhombic) THEN
       forall (d = 1:3)
          base_point(d) = point(d)&
               - space%h(d,d) * space%s(d) &
               &  * ANINT(space%h_inv(d,d) * point(d) - half)
       end forall
    ELSE
       s(:) = MATMUL (space%h_inv(:,:), point(:))
       forall (d = 1:3)
          s(d) = s(d) - real(space%s(d) * int(ANINT (s(d) - 0.5), kind=space_k))
       end forall
       base_point(:) = MATMUL (space%h(:,:), s(:))
    ENDIF
  end function lamb_point_originate_v

! *****************************************************************************
!> \brief Folds a vector into a base cell centered at the origin
!> \param[in] space   Space
!> \param[in] point   Point to fold
!> \return base_point Point image in the base cell
!> \author UB (Much code obtained from CP2K cell_types.)
! *****************************************************************************
  pure function lamb_point_delta_v(space, point) RESULT (base_point)
    TYPE(lamb_space_type), INTENT(IN)              :: space
    real(kind=space_k), dimension(3), intent(in)    :: point
    real(kind=space_k), dimension(3)                :: base_point

    CHARACTER(len=*), PARAMETER :: routineN = 'lamb_point_delta_v', &
      routineP = moduleN//':'//routineN

    integer :: d
    real(kind=space_k), dimension(3) :: s

    IF (space%orthorhombic) THEN
       forall (d = 1:3)
          base_point(d) = point(d)&
               - space%h(d,d) * space%s(d) * ANINT(space%h_inv(d,d) * point(d))
       end forall
    ELSE
       s(:) = MATMUL (space%h_inv(:,:), point(:))
       forall (d = 1:3)
          s(d) = s(d) - real(space%s(d) * int(ANINT (s(d))), kind=space_k)
       end forall
       base_point(:) = MATMUL (space%h(:,:), s(:))
    ENDIF
  end function lamb_point_delta_v


! *****************************************************************************
!> \brief Moves a point to another spatial image
!> \param[in] space   Space
!> \param[in] point   Point to move.  Should be pre-folded to origin.
!> \param[in] image   Image multiplier
!> \return imaged_point Point image in the base cell
!> \author UB (Much code obtained from CP2K cell_types.)
! *****************************************************************************
  pure function lamb_point_reimage_v(space, point, image) RESULT (imaged_point)
    TYPE(lamb_space_type), INTENT(IN)              :: space
    real(kind=space_k), dimension(3), intent(in)    :: point
    integer, dimension(3), intent(in)              :: image
    real(kind=space_k), dimension(3)                :: imaged_point

    CHARACTER(len=*), PARAMETER :: routineN = 'lamb_point_originate_v', &
      routineP = moduleN//':'//routineN

    integer :: d

    imaged_point(:) = lamb_point_originate (space, point)
    IF (space%orthorhombic) THEN
       forall (d = 1:3)
          imaged_point(d) = imaged_point(d) + space%h(d,d) * image(d)
       end forall
    ELSE
       imaged_point(:) = imaged_point(:) + &
            MATMUL (space%h(:,:), REAL(image(:),kind=space_k))
    ENDIF
  end function lamb_point_reimage_v

! *****************************************************************************
!> \brief Moves a point to another spatial image
!> \param[in] space   Space
!> \param[in] point   Point to move.  Should be pre-folded to origin.
!> \param[in] image   Image multiplier
!> \return imaged_point Point image in the base cell
!> \author UB (Much code obtained from CP2K cell_types.)
! *****************************************************************************
  pure function lamb_point_reimage_s(space, point, ix, iy, iz) RESULT (imaged_point)
    TYPE(lamb_space_type), INTENT(IN)              :: space
    real(kind=space_k), dimension(3), intent(in)    :: point
    integer, intent(in)              :: ix, iy, iz
    real(kind=space_k), dimension(3)                :: imaged_point

    CHARACTER(len=*), PARAMETER :: routineN = 'lamb_point_originate_v', &
      routineP = moduleN//':'//routineN

    integer :: d
    real(kind=space_k), dimension(3) :: image

    imaged_point(:) = lamb_point_originate (space, point)
    IF (space%orthorhombic) THEN
       imaged_point(1) = imaged_point(1) + space%h(1,1) * ix
       imaged_point(2) = imaged_point(2) + space%h(2,2) * iy
       imaged_point(3) = imaged_point(3) + space%h(3,3) * iz
    ELSE
       image(1) = REAL(ix,kind=space_k)
       image(2) = REAL(iy,kind=space_k)
       image(3) = REAL(iz,kind=space_k)
       imaged_point(:) = imaged_point(:) + MATMUL (space%h(:,:), image(:))
    ENDIF
  end function lamb_point_reimage_s

! *****************************************************************************
!> \brief Determines the image in which a point lies
!> \param[in] space   Space
!> \param[in] point   Determine image of this point
!> \return image      Image of point
!> \author UB (Much code obtained from CP2K cell_types.)
! *****************************************************************************
  pure function lamb_point_get_image_v(space, point) RESULT (image)
    TYPE(lamb_space_type), INTENT(IN)              :: space
    real(kind=space_k), dimension(3), intent(in)    :: point
    integer, dimension(3)                          :: image

    CHARACTER(len=*), PARAMETER :: routineN = 'lamb_point_get_image_v', &
      routineP = moduleN//':'//routineN

    real(kind=space_k), parameter :: half = 0.5_space_k
    integer :: d

    IF (space%orthorhombic) THEN
       forall (d = 1:3)
          image(d) = INT (space%h_inv(d,d) * point(d) - half)
       end forall
    ELSE
       image = INT( MATMUL (space%h_inv(:,:), point(:)))
    ENDIF
  end function lamb_point_get_image_v

! *****************************************************************************
!> \brief Folds a vector between two points into the base cell centered at
!>        the origin (i.e., find minimum distance vector between two points).
!> \param[in] space   Space
!> \param[in] point1  First point
!> \param[in] point2  Second point 
!> \return base_vector Minimum vector between points point1 and point2
!> \author UB (Much code obtained from CP2K cell_types.)
! *****************************************************************************
  pure function lamb_min_vector_v(space, point1, point2) RESULT (min_vector)
    TYPE(lamb_space_type), INTENT(IN)              :: space
    real(kind=space_k), dimension(3), intent(in)    :: point1, point2
    real(kind=space_k), dimension(3)                :: min_vector

    CHARACTER(len=*), PARAMETER :: routineN = 'lamb_min_vector_v', &
      routineP = moduleN//':'//routineN

    real(kind=space_k), dimension(3) :: p1, p2, p2i
    integer :: xd, yd, zd
    !real(kind=space_k), dimension(-1:1,-1:1,-1:1) :: distances
    real(kind=space_k) :: mind, dist
    real(kind=space_k) :: minv

    p1 = lamb_point_originate(space, point1)
    p2 = lamb_point_originate(space, point1)
    mind = HUGE(1.0_space_k)
    do xd = -space%s(1), space%s(1)
       do yd = -space%s(2), space%s(2)
          do zd = -space%s(3), space%s(3)
             p2i = lamb_point_reimage(space, p2, xd,yd,zd)
             dist = (p1(1)-p2i(1))**2 + (p1(2)-p2i(2))**2 + (p1(3)-p2i(3))**2
             if (dist < mind) then
                mind = dist
                min_vector(:) = p2i(:)-p1(:)
             endif
          enddo
       enddo
    enddo
  end function lamb_min_vector_v

  PURE FUNCTION lamb_distance_v(point1, point2) result (distance)
    real(kind=space_k), dimension(3), intent(in) :: point1, point2
    real(kind=space_k) :: distance
    real(kind=space_k), dimension(3)             :: p1, p2
    distance = sqrt (SUM ((point1(:) - point2(:))**2))
  END FUNCTION lamb_distance_v
  PURE FUNCTION lamb_sq_distance_v(point1, point2) result (distance)
    real(kind=space_k), dimension(3), intent(in) :: point1, point2
    real(kind=space_k) :: distance
    distance = SUM((point1(:) - point2(:))**2)
  END FUNCTION lamb_sq_distance_v

  pure function lamb_vector_len_v(v) result (len)
    real(kind=space_k), dimension(3), intent(in) :: v
    real(kind=space_k) :: len
    len = sqrt (SUM(v(:)**2))
  end FUNCTION lamb_vector_len_v
  pure function lamb_sq_vector_len_v(v) result (len)
    real(kind=space_k), dimension(3), intent(in) :: v
    real(kind=space_k) :: len
    len = SUM(v(:)**2)
  end FUNCTION lamb_sq_vector_len_v

  subroutine lamb_space_new_unit_box (space, error)
    type(lamb_space_type), pointerout :: space
    type(lamb_error_type), intent(inout) :: error

    real(kind=space_k), dimension(3,3) :: h
    integer :: i
    h = 0.0_space_k
    forall (i = 1:3)
       h(i,i) = real(1, kind=space_k)
    end forall
    call lamb_space_new (space, h, error)
  end subroutine lamb_space_new_unit_box


END MODULE lamb_space_methods
