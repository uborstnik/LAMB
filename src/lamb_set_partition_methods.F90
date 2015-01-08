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

!> \brief Methods for the partition set class.

MODULE lamb_set_partition_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_partition_types
  use lamb_partition_methods
  use lamb_set_types
  use lamb_set_methods
  use lamb_set_partition_types
  use lamb_subset_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_set_partition_update
  public :: lamb_set_part_find_subsets

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'set_partition_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb


CONTAINS

!> \brief Repartitions a set into subsets.
!>
!> The set of particles is repartitioned according to the spatial
!> partition.
  SUBROUTINE lamb_set_partition_update (set_partition, error)
    TYPE(lamb_set_partition_type), INTENT(INOUT):: set_partition
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_partition_update', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=allatm_k) :: g_element, n_elements
    integer(kind=subset_k) :: subset_i, n_subsets
    integer :: i
    real(kind=space_k), dimension(3) :: coordinates

    integer(kind=blksz_k), dimension(:,:), allocatable :: subset_blk_sizes
    integer(kind=locatm_k) :: max_subset_size
    integer(kind=locatm_k), dimension(:), allocatable :: sizes_p
    logical, parameter :: debug = debug_mod

    CALL lamb_error_set (error, error_handle, routineN)

    n_elements = lamb_set_get_n_elements(set_partition%set)
    n_subsets = int(lamb_partition_get_n_cubes(set_partition%partition), kind=subset_k)
    if (debug) then
       write(*,*)routineN//" n_elements, n_subsets", n_elements, n_subsets
       write(*,*)routineN//' part n', set_partition%partition%ncubes
       write(*,*)routineN//' part mult', set_partition%partition%multiplier
       write(*,*)routineN//' h', set_partition%partition%space%h
    endif
    set_partition%n_subset_elements(:) = 0_subset_k
    do g_element = 1_allatm_k, n_elements
       coordinates = lamb_set_get_coordinates(set_partition%set, g_element, error)
       subset_i = int(lamb_partition_which_cube(set_partition%partition, coordinates),kind=subset_k)
       if (debug) write(*,*)routineN//" subset_i=", subset_i
       set_partition%subset_map(g_element) = subset_i
       set_partition%n_subset_elements(subset_i) = &
            set_partition%n_subset_elements(subset_i) + 1_locatm_k
       set_partition%order_map(g_element) = set_partition%n_subset_elements(subset_i)
       if (careful_mod) then
          call lamb_assert (int(set_partition%n_subset_elements(subset_i),kind=int_big) + 1_int_big, "LE",&
               int(huge(0_locatm_k),kind=int_big),&
               lamb_failure_level, lamb_overflow_error,&
               "Too many subset atoms for subset type.",&
               routineN, __LINE__, error=error)
          call lamb_assert(int(subset_i,kind=int_big), "LE", int(n_subsets,kind=int_big),&
               lamb_failure_level, lamb_internal_error,&
               "Cube out of range.",&
               routineN, __LINE__, error)
       endif
       if (debug) then
          write(*,*)routineN,g_element,'->',subset_i,set_partition%order_map(g_element)
       endif
    enddo
    max_subset_size = maxval(set_partition%n_subset_elements(1:set_partition%n_subsets))
    allocate(subset_blk_sizes(max_subset_size, n_subsets), stat=stat)
    call lamb_memory_check(stat, "subset_blk_sizes",&
         int(max_subset_size, kind=int_big)*int(n_subsets,kind=int_big), error)
    allocate(sizes_p(n_subsets), stat=stat)
    call lamb_memory_check(stat, "sizes_p", n_subsets, error)

    !!! Only the subsets in the processes catalog should be
    !!! initialized.  TODO: At the very least, release the subsets not
    !!! in my catalog and nullify the pointer.
    sizes_p(:) = 0_locatm_k
    do g_element = int(1,kind=kind(g_element)), n_elements
       subset_i = set_partition%subset_map(g_element)
       sizes_p(subset_i) = sizes_p(subset_i) + 1_locatm_k
       subset_blk_sizes(sizes_p(subset_i),subset_i) = set_partition%set%sizes(g_element)
    enddo
    do subset_i = 1, n_subsets
       call lamb_subset_new(set_partition%subsets(subset_i)%p, subset_i, sizes_p(subset_i),&
            subset_blk_sizes(1:sizes_p(subset_i), subset_i), error)
       if (debug) &
            write(*,*)routineN//' subset',subset_i,'size',sizes_p(subset_i)
    end do
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_partition_update

  !subroutine init_subset(subset, subset_size, set_size, all_blk_sizes, error)
  !  type(lamb_subset_type), pointerout :: subset
  !  integer(kind=locatm_k), intent(in) :: subset_size
  !  integer(kind=allatm_k), intent(in) :: set_size
  !  integer(kind=blksz_k), dimension(1:set_size), intent(in) :: all_blk_sizes
  !  type(lamb_error_type), intent(inout) :: error
  !
  !  integer(kind=blksz_k), dimension(subset_size) :: blk_sizes
  !  integer :: error_handle
  !  integer(kind=allatm_k) :: e
  !  integer(kind=locatm_k) :: le
  !
  !  CHARACTER(LEN=*), PARAMETER :: routineN = 'init_subset', &
  !       routineP = moduleN//':'//routineN
  !
  !  CALL lamb_error_set (error, error_handle, routineP)
  !  le = 1_locatm_k
  !  do e = 1_allatm_k, set_size
  !     
  !  end do
  !  call lamb_assert(le, 'EQ', subset_size,&
  !       lamb_failure_level, lamb_internal_error, routineP,&
  !       "Mismatch in the subset size.", __LINE__, error)
  !  call lamb_subset_new(subset, subset_size, blk_sizes, error)
  !  call lamb_error_stop (error, error_handle)
  !end subroutine init_subset

  !> \brief Find the subset to which the elements of the input list belong.
  !> 
  !> A list of global elements is given. For each list member
  !> global_elements(i), the subset it belongs to is saved in
  !> subsets(i) and the rank in that subset is saved in
  !> local_elements(i).
  !> \param[in] set_partition    The set partition
  !> \param[in] global_elements  The list of elements for which subset membership is sought.
  !> \param[out] subsets         The subsets that the given list of elements belong to.
  !> \param[out] local_elements  The rank in the individual subsets of the given list of elements.
  !> \param[in,out] error        LAMB error.
  SUBROUTINE lamb_set_part_find_subsets (set_partition, global_elements,&
       subsets, local_elements, error)
    TYPE(lamb_set_partition_type), INTENT(IN):: set_partition
    integer(kind=allatm_k), dimension(:), intent(in) :: global_elements
    integer(kind=subset_k), dimension(:), intent(out) :: subsets
    integer(kind=locatm_k), dimension(:), intent(out) :: local_elements
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_part_find_subsets', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer(kind=allatm_k) :: i

    if (careful_mod) &
         CALL lamb_error_set (error, error_handle, routineN)
    subsets = set_partition%subset_map(global_elements)
    local_elements = set_partition%order_map(global_elements)
    if (careful_mod) then
       do i = 1_allatm_k, int(size(local_elements),kind=allatm_k)
          call lamb_assert (int(local_elements(i)), "LE",&
               int(set_partition%n_subset_elements(subsets(i))),&
               lamb_warning_level, lamb_internal_error, &
               "Indexed atom too big.",&
               routineN, __LINE__, error=error)
       enddo
    endif
    if (careful_mod) &
         CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_part_find_subsets


END MODULE lamb_set_partition_methods
