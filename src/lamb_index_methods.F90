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

!> \brief Methods for the basic index.

MODULE lamb_index_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_index_types
  use lamb_kinds
  use lamb_alloc_1d_util

  IMPLICIT NONE

  PRIVATE

  public :: lamb_index_get_blk_n
  PUBLIC :: lamb_index_blk_get_tr
  public :: lamb_index_blk_get_info

  public :: lamb_index_build_from_list
  public :: lamb_index_convert_to_csr
  public :: lamb_index_set_fmt_coor
  public :: lamb_index_coor_add_blk

  public :: lamb_index_blk_info_pack, lamb_index_blk_info_pack_f
  public :: lamb_blk_info_update_extent

  public :: lamb_index_ensure_size

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'index_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: index_id

CONTAINS

  function lamb_index_get_blk_n (index, row, col, found) result (blk_n)
    type(lamb_index_type), intent(in) :: index
    integer(kind=locatm_k), intent(in) :: row, col
    integer(kind=blkn_k) :: blk_n
    logical, intent(out) :: found
    blk_n = get_blk_n(index%row_p, index%col_i, row, col, found)
  end function lamb_index_get_blk_n

  function get_blk_n (row_p, col_i, row, col, found) result (blk_n)
    integer(kind=blkn_k), dimension(*), intent(in) :: row_p
    integer(kind=locatm_k), dimension(*), intent(in) :: col_i
    integer(kind=locatm_k), intent(in) :: row, col
    logical, intent(out) :: found
    integer(kind=blkn_k) :: blk_n

    integer(kind=blkn_k) :: first, last
    integer(kind=locatm_k) :: c

    first = row_p(row)+1
    last = row_p(row+1)
    found = .false.
    blk_n = (first + last) / int(2,kind=blkn_k)
    do while (blk_n .ge. first .and. blk_n .le. last)
       c = col_i(blk_n)
       if (c .eq. col) then
          found = .true.
          exit
       elseif (c .lt. col) then
          first = blk_n + 1
       else
          last = blk_n -1
       endif
       blk_n = (first + last) / int(2,kind=blkn_k)
    enddo
  end function get_blk_n

  PURE FUNCTION lamb_index_blk_get_tr (blk_info, blk_n) result (tr)
    TYPE(lamb_blk_info_type), dimension(:), INTENT(IN) :: blk_info
    integer(kind=blkn_k), intent(in) :: blk_n
    logical :: tr
    tr = BTEST (int(blk_info(blk_n)%bits), bits_tr)
  END FUNCTION lamb_index_blk_get_tr

  subroutine lamb_index_blk_get_info (index, blk_n, data_store, extent, tr, inv_real, inv_im)
    type(lamb_index_type), intent(in) :: index
    integer(kind=blkn_k), intent(in) :: blk_n
    integer(kind=datastore_k), intent(out) :: data_store
    integer(kind=dataptr_k), dimension(2), intent(out) :: extent
    logical, intent(out) :: tr, inv_real, inv_im
    call blk_info_expand(index%blk_info(blk_n), data_store, extent, tr, inv_real, inv_im)
  end subroutine lamb_index_blk_get_info
  pure subroutine blk_info_expand(blk_info, data_store, extent, tr, inv_real, inv_im)
    type(lamb_blk_info_type), intent(in) :: blk_info
    integer(kind=datastore_k), intent(out) :: data_store
    integer(kind=dataptr_k), dimension(2), intent(out) :: extent
    logical, intent(out) :: tr, inv_real, inv_im
    data_store = blk_info%store
    extent =     blk_info%extent
    tr = BTEST (int(blk_info%bits), bits_tr)
    inv_real = BTEST (int(blk_info%bits), bits_inv_real)
    inv_im = BTEST (int(blk_info%bits), bits_inv_im)
  end subroutine blk_info_expand
  pure subroutine lamb_index_blk_info_pack(blk_info, data_store, extent, tr, inv_real, inv_im)
    type(lamb_blk_info_type), intent(out) :: blk_info
    integer(kind=datastore_k), intent(in) :: data_store
    integer(kind=dataptr_k), dimension(2), intent(in) :: extent
    logical, intent(in) :: tr, inv_real, inv_im
    integer(kind=int_1) :: bits
    blk_info%store = data_store
    blk_info%extent = extent
    bits = blk_info%bits
    if (tr) bits = IBSET (bits, bits_tr)
    if (inv_real) bits = IBSET (bits, bits_inv_real)
    if (inv_im) bits = IBSET (bits, bits_inv_im)
    blk_info%bits = bits
  end subroutine lamb_index_blk_info_pack
  pure function lamb_index_blk_info_pack_f(data_store, extent, tr, inv_real, inv_im) RESULT (blk_info)
    integer(kind=datastore_k), intent(in) :: data_store
    integer(kind=dataptr_k), dimension(2), intent(in) :: extent
    logical, intent(in) :: tr, inv_real, inv_im
    type(lamb_blk_info_type) :: blk_info
    call lamb_index_blk_info_pack(blk_info, data_store, extent, tr, inv_real, inv_im)
  end function lamb_index_blk_info_pack_f

  subroutine lamb_blk_info_update_extent(blk_info, extent_start, extent_end)
    type(lamb_blk_info_type), intent(inout) :: blk_info
    integer(kind=dataptr_k), intent(in) :: extent_start, extent_end
    blk_info%extent = (/ extent_start, extent_end /)
  end subroutine lamb_blk_info_update_extent

  subroutine lamb_index_ensure_size (index, nblks, error)
    type(lamb_index_type), intent(inout) :: index
    integer(kind=blkn_k), intent(in) :: nblks
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_ensure_size', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    type(lamb_blk_info_type), dimension(:), allocatable :: new_blk_info
    integer(kind=blkn_k) :: new_size, old_size

    if (careful_mod) &
         CALL lamb_error_set (error, error_handle, routineN)
    call lamb_alloc_1d_ensure_size (index%col_i, int(nblks,kind=int_big), error)
    old_size = int(size (index%blk_info), kind=blkn_k)
    if (old_size < nblks) then
       new_size = max (int(real(old_size)*1.4,kind=blkn_k), nblks)
       allocate (new_blk_info(old_size), stat=stat)
       call lamb_memory_check(stat, "new_blk_info", old_size, error)
       new_blk_info(1:old_size) = index%blk_info(1:old_size)
       deallocate (index%blk_info, stat=stat)
       call lamb_memory_check(stat, "index%blk_info", error)
       allocate (index%blk_info(new_size), stat=stat)
       call lamb_memory_check(stat, "index%blk_info", error)
       index%blk_info(1:old_size) = new_blk_info(1:old_size)
       deallocate (new_blk_info, stat=stat)
       call lamb_memory_check(stat, "new_blk_info", error)
    end if
    if (.not. index%csr) then
       call lamb_alloc_1d_ensure_size (index%row_i, int(nblks,kind=int_big), error)
    endif
    if (careful_mod) &
         call lamb_error_stop (error, error_handle)
  end subroutine lamb_index_ensure_size

  subroutine lamb_index_build_from_list(index, nblks, block_list,&
       extents, error)
    type(lamb_index_type), intent(inout) :: index
    integer(kind=blkn_k), intent(in) :: nblks
    integer(kind=locatm_k), dimension(:,:), intent(in) :: block_list
    integer(kind=dataptr_k), dimension(:,:), intent(in) :: extents
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_build_from_list', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=blkn_k) :: blk

    CALL lamb_error_set (error, error_handle, routineN)
    call lamb_index_ensure_size(index, nblks, error)
    call index_compress(nblks, block_list(1,:), index%row_p)
    index%col_i(1:nblks) = block_list(2,1:nblks)
    do blk = 1_blkn_k, nblks
       index%blk_info(blk)%extent = extents(1:2,blk)
       index%blk_info(blk)%store = 1
       index%blk_info(blk)%bits = 0_int_1
    enddo
    index%n_idx_blks = nblks
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_index_build_from_list

  subroutine lamb_index_convert_to_csr(index, error)
    type(lamb_index_type), intent(inout) :: index
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_convert_to_csr', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)
    call lamb_assert("NOT", index%csr,&
         lamb_warning_level, lamb_internal_error,&
         "Index is already in CSR format.",&
         routineN, __LINE__, error)
    if (.not. index%csr) then
       call lamb_alloc_1d_ensure_size(index%row_p,&
            int(index%n_rows+1_locatm_k,kind=int_8), error)
       call index_compress(index%n_idx_blks, index%row_i, index%row_p)
       index%csr = .true.
       deallocate(index%row_i, stat=stat)
       call lamb_memory_check(stat, "index%row_i", error)
    endif
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_index_convert_to_csr

  subroutine index_compress(n_expanded, expanded, compressed)
    integer(kind=blkn_k), intent(in) :: n_expanded
    integer(kind=locatm_k), dimension(:), intent(in) :: expanded
    integer(kind=blkn_k), dimension(:), intent(out) :: compressed
    integer(kind=blkn_k) :: i
    integer(kind=locatm_k) :: bin
    ! First the values are counted. Next they are converted to
    ! offsets.
    compressed(:) = 0_blkn_k
    do i = 1_blkn_k, n_expanded
       bin = expanded(i)
       compressed(bin) = compressed(bin) + 1_blkn_k
    enddo
    call index_count_to_offset (compressed)
  end subroutine index_compress

  subroutine index_count_to_offset(array)
    integer(kind=blkn_k), dimension(:), intent(inout) :: array
    integer(kind=blkn_k) :: i, c, prev
    prev = array(1)
    array(1) = 0_blkn_k
    do i = 2_blkn_k, int(size(array),kind=blkn_k)
       c = array(i)
       array(i) = array(i-1_blkn_k) + prev
       prev = c
    enddo
  end subroutine index_count_to_offset

  subroutine lamb_index_set_fmt_coor(index, error)
    type(lamb_index_type), intent(inout) :: index
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_set_fmt_coor', &
         routineP = moduleN//':'//routineN
    integer :: stat, error_handle
    call lamb_error_set(error, error_handle, routineN)
    call lamb_assert (index%csr,&
         lamb_failure_level, lamb_caller_error,&
         "Index is not in CSR format.",&
         routineN, __LINE__, error)
    index%csr = .false.
    index%n_idx_blks = 0_blkn_k
    call lamb_assert("NOT", allocated(index%row_i),&
         lamb_failure_level, lamb_caller_error,&
         "Coordinate index is already allocated.",&
         routineN, __LINE__, error)
    allocate (index%row_i(1), stat=stat)
    call lamb_memory_check(stat, "index%row_i", 1, error)
    deallocate (index%row_p, stat=stat)
    call lamb_memory_check(stat, "index%row_p", error)
    call lamb_error_stop(error, error_handle)
  end subroutine lamb_index_set_fmt_coor

  subroutine lamb_index_coor_add_blk(index, coor, blk_info, error)
    type(lamb_index_type), intent(inout) :: index
    integer(kind=locatm_k), dimension(:), intent(in) :: coor
    type(lamb_blk_info_type), intent(in) :: blk_info
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_index_coor_add_blk', &
         routineP = moduleN//':'//routineN
    integer :: stat, error_handle
    integer(kind=blkn_k) :: new_nblks
    if (careful_mod) then
       call lamb_error_set(error, error_handle, routineN)
       call lamb_assert ("NOT", index%csr,&
            lamb_failure_level, lamb_caller_error,&
            "Can not use this routine on a CSR index.",&
            routineN, __LINE__, error=error)
    endif
    index%n_idx_blks = index%n_idx_blks + 1_blkn_k
    new_nblks = index%n_idx_blks
    call lamb_index_ensure_size(index, new_nblks, error)
    index%row_i(new_nblks) = coor(1)
    index%col_i(new_nblks) = coor(2)
    index%blk_info(new_nblks) = blk_info
    if (careful_mod) &
         call lamb_error_stop(error, error_handle)
  end subroutine lamb_index_coor_add_blk

END MODULE lamb_index_methods
