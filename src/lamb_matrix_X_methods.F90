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

!> \brief Methods for the LAMB matrix class.

!MAKE s d c z
MODULE lamb_matrix_X_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_distribution_types
  use lamb_distribution_methods
  use lamb_matrix_X_types
  use lamb_meta_methods
  use lamb_relation_types
  USE lamb_sector_X_types
  use lamb_set_partition_types
  use lamb_subset_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_matrix_X_rename
  PUBLIC :: lamb_matrix_X_update
  PUBLIC :: lamb_matrix_X_get_distribution
  public :: lamb_matrix_X_get_n_sectors

  !!! Only for building
  public :: sectors_X_init

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'matrix_X_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  function lamb_matrix_X_get_n_sectors (matrix_X, error) result (n_sectors)
    type(lamb_matrix_X_type), intent(in) :: matrix_X
    type(lamb_error_type), intent(inout) :: error
    integer :: n_sectors
  
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_get_n_sectors', &
         routineP = moduleN//':'//routineN
  
    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)
    n_sectors = int(lamb_dist_get_n_local_sectors(matrix_X%distribution, error))
    CALL lamb_error_stop (error, error_handle)
  END function lamb_matrix_X_get_n_sectors

  pure subroutine lamb_matrix_X_rename (matrix_X, name)
    type(lamb_matrix_X_type), intent(inout) :: matrix_X
    character(len=*), intent(in) :: name
    call lamb_meta_set_name(matrix_X%meta, name)
  end subroutine lamb_matrix_X_rename

  function lamb_matrix_X_get_distribution (matrix_X) result (distribution)
    type(lamb_matrix_X_type), intent(in) :: matrix_X
    type(lamb_distribution_type) :: distribution
    distribution = matrix_X%distribution
  end function lamb_matrix_X_get_distribution

  SUBROUTINE lamb_matrix_X_update (matrix_X, error)
    TYPE(lamb_matrix_X_type), INTENT(INOUT), TARGET :: matrix_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_update', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=cube_k) :: n_sectors
    integer(kind=cube_k), dimension(:,:), allocatable :: my_sectors
    type(lamb_matrix_X_type), pointer :: matrix_p
    type(lamb_distribution_type), pointer :: distribution_p

    CALL lamb_error_set (error, error_handle, routineN)

    n_sectors = lamb_dist_get_n_local_sectors(matrix_X%distribution, error)
    if (size(matrix_X%sectors) == 0) then
       deallocate(matrix_X%sectors, stat=stat)
       call lamb_memory_check (stat, "matrix%sectors", error)
       allocate(matrix_X%sectors(n_sectors), stat=stat)
       call lamb_memory_check (stat, "matrix%sectors", int(n_sectors,kind=int_big), error)
    else
       stop routineP//'nyi'
    endif
    matrix_p => matrix_X
    distribution_p => matrix_p%distribution
    call sectors_X_init(matrix_X%sectors,&
         n_sectors, &
         lamb_dist_get_my_sectors_p(distribution_p, error),&
         distribution_p%set_partitions, matrix_X%meta%relation, &
         error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_update

  subroutine sectors_X_init (sectors, n_sectors, my_sectors, set_parts, relation, error)
    type(lamb_sector_X_type), dimension(:), intent(out) :: sectors
    integer(kind=cube_k), intent(in) :: n_sectors
    integer(kind=subset_k), dimension(:,:), intent(in) :: my_sectors
    type(lamb_set_partition_type_p), dimension(2), intent(in) :: set_parts
    type(lamb_relation_type), POINTERIN :: relation
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_update', &
         routineP = moduleN//':'//routineN
    integer :: error_handle
    integer(kind=cube_k) :: sector_i
    integer :: sel
    type(lamb_subset_type_p), dimension(2) :: subsets
    logical, parameter :: debug = debug_mod
    CALL lamb_error_set (error, error_handle, routineN)
    if (careful_mod) then
       call lamb_assert (int(n_sectors), "EQ", size(my_sectors,2),&
            lamb_warning_level, lamb_wrong_args, &
            "My sector count does not match array size.",&
            routineP, __LINE__, error)
    endif
    do sector_i = 1_cube_k, n_sectors
       if (debug) &
            write(*,*)routineN//" Creating sector", my_sectors(:,sector_i)
       do sel = 1, 2
          subsets(sel)%p => set_parts(sel)%p%subsets(my_sectors(sel,sector_i))%p
       enddo
       call lamb_sector_X_create (sectors(sector_i), &
            subsets, relation, error)
    end do
    call lamb_error_stop (error, error_handle)
  end subroutine sectors_X_init


END MODULE lamb_matrix_X_methods
