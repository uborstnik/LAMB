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

!> \brief Methods for sector objects.

!MAKE s d c z
MODULE lamb_sector_X_methods

#include "lamb_defs.h"

  use lamb_data_types
  use lamb_data_X_types
  use lamb_data_X_methods
  use lamb_data_types
  USE lamb_error
  use lamb_index_types
  use lamb_index_methods
  use lamb_kinds
  use lamb_relation_types
  use lamb_sector_X_types
  use lamb_subset_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_sector_X_add_new_block
  PUBLIC :: lamb_sector_X_put_block
  public :: lamb_sector_X_shove

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'sector_X_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  subroutine lamb_sector_X_add_new_block (sector_X, coor, block_info,&
       data_len, block_data, error)
    TYPE(lamb_sector_X_type), INTENT(INOUT):: sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=locatm_k), dimension(:), intent(in) :: coor
    type(lamb_blk_info_type) :: block_info
    integer(kind=dataptr_k), intent(in) :: data_len
    doubleprecision, dimension(*), intent(in) :: block_data

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_add_block', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=blkn_k) :: blk_n
    logical :: found
    integer(kind=dataptr_k) :: extent_start, extent_end
    logical :: tr, inv_real, inv_im
    type(lamb_blk_info_type) :: blk_info

    CALL lamb_error_set (error, error_handle, routineN)

    ! add data to data_store (#1)
     call lamb_data_X_add_data(sector_X%data_stores(1), data_len, block_data,&
         extent_start, error)
    extent_end = extent_start + data_len - 1_dataptr_k
    ! change the blk_info
    blk_info = block_info
    call lamb_blk_info_update_extent(blk_info, extent_start, extent_end)
    ! update the index
    call lamb_index_coor_add_blk(sector_X%index, coor(:), blk_info, error)
    sector_X%n_blks = sector_X%n_blks + 1_blkn_k
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_sector_X_add_new_block

! *****************************************************************************
!> \brief Adds a data block to the sector.
!>
!> \param[in,out] sector_X   Adds data block to this sector.
!> \param[in] row            Adds data block to this blocked row.
!> \param[in] col            Adds data block to this blocked column.
!> \param[in] block          The data block to add.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sector_X_put_block (sector_X, row, col, block_data, error)
    TYPE(lamb_sector_X_type), INTENT(OUT):: sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=locatm_k), intent(in) :: row, col
    doubleprecision, dimension(:,:), intent(in) :: block_data

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_add_block', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=blkn_k) :: blk_n
    logical :: found
    integer(kind=datastore_k) :: data_store
    integer(kind=dataptr_k), dimension(2) :: extent
    logical :: tr, inv_real, inv_im

    CALL lamb_error_set (error, error_handle, routineN)

    blk_n = lamb_index_get_blk_n(sector_X%index, row, col, found)
    if (found) then
       call lamb_index_blk_get_info(sector_X%index, blk_n, data_store, extent,&
            tr, inv_real, inv_im)
       STOP "Not implemented."
       ! Replaces the existing block data with the new data.
    else
       ! Adds the block to the sector.
       STOP "Not implemented."
    endif

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sector_X_put_block

  SUBROUTINE lamb_sector_X_shove (sector_X, nblks, block_list, extents, block_data, error)
    TYPE(lamb_sector_X_type), INTENT(INOUT):: sector_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=blkn_k), intent(in) :: nblks
    integer(kind=locatm_k), dimension(:,:), intent(in) :: block_list
    integer(kind=dataptr_k), dimension(:,:), intent(in) :: extents
    type(lamb_data_X_type), intent(in) :: block_data

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sector_X_shove', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    if (careful_mod) then
       call lamb_assert (size(sector_X%data_stores), "GE", 1,&
            lamb_fatal_level, lamb_internal_error, &
            "Number of data stores not equal to 1.",&
            routineN, __LINE__, error=error)
    endif
    call lamb_assert (size(sector_X%data_stores), "EQ", 1,&
         lamb_warning_level, lamb_internal_error,&
         "Number of data stores not equal to 1.",&
         routineN, __LINE__, error=error)
    call lamb_data_destroy (sector_X%data_stores(1), error)
    sector_X%data_stores(1) = block_data

    sector_X%n_blks = nblks
    call lamb_index_build_from_list(sector_X%index, nblks, block_list, extents, error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sector_X_shove

END MODULE lamb_sector_X_methods
