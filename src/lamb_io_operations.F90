!******************************************************************************
!
! This source code is part of the LAMB library.
!
! Written by Urban Borstnik.
!
! Inspired by CP2K and DBCSR.
!
! Copyright (C) 2012, 2013 Urban Borstnik.
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

!> \brief LAMB matrix input/output.
!>
!> Routines for reading matrices from disk.
MODULE lamb_io_operations

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_alloc_1d_util
  use lamb_alloc_2d_util
  use lamb_array_1d_types
  use lamb_array_1d_d_types
  use lamb_data_types
  use lamb_data_d_types
  use lamb_data_methods
  use lamb_distribution_types
  use lamb_distribution_methods
  use lamb_matrix_d_types
  use lamb_matrix_operations
  use lamb_meta_types
  use lamb_mp_file_types
  use lamb_mp_file_methods
  use lamb_mp_file_s_methods
  use lamb_mp_file_i_methods
  use lamb_mp_file_l_methods
  use lamb_mp_file_u_methods
  use lamb_mp_env_types
  use lamb_mp_env_methods
  use lamb_mp_methods
  use lamb_mp_u_methods
  use lamb_mp_s_methods
  use lamb_mp_i_methods
  use lamb_partition_types
  use lamb_plexus_types
  use lamb_plexus_methods
  use lamb_plexus_operations
  use lamb_relation_types
  use lamb_relation_methods
  use lamb_set_types
  use lamb_set_methods
  use lamb_set_partition_types
  use lamb_space_types
  use lamb_space_methods
  use lamb_space_operations

  IMPLICIT NONE

  PRIVATE

  public :: lamb_load_generic
  public :: lamb_load_separate
  public :: lamb_load_dbcsr

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'io_operations'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

!> \brief Read simple matrix data.
!>
!> Loads a basic data dump from disk into memory.
!> \note Currently does not save data to a matrix.
  subroutine lamb_load_generic(matrix, filename, error)
    type(lamb_matrix_d_type), intent(inout) :: matrix
    character(len=*), intent(in) :: filename
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_load_generic', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    type(lamb_mp_file_type) :: mp_file
    type(lamb_mp_env_type), pointer :: mp_env
    integer(kind=int_8), dimension(2) :: sizes
    integer(kind=int_8) :: nentries, data_size
    integer(kind=int_8), dimension(:,:), allocatable :: idx
    real(kind=dp), dimension(:), allocatable :: data_dump

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok (error, prev_level)
    mp_env => lamb_distribution_get_mp_env (matrix%distribution)
    call lamb_mp_file_create (mp_file, mp_env, filename, reading=.TRUE.,&
         error=error)
    call lamb_error_catch (error, location=HERE)

    ! File format: integer: NE = number of entries, DS = data_size
    ! NE * (row,col)
    ! DATA (DS entries)

    nentries = 0
    call lamb_mp_file_read (mp_file, 2_int_big, sizes, error)
    nentries = sizes(1)
    data_size = sizes(2)
    allocate (idx(2,nentries), stat=stat)
    call lamb_memory_check (stat, "idx", 2*nentries, error)
    allocate (data_dump(data_size), stat=stat)
    call lamb_memory_check (stat, "data_dump", data_size, error)

    call lamb_mp_file_l_read (mp_file, 2*int(nentries,kind=int_big),&
         idx(:,:), error)
    !write(*,*)'index'
    !write(*,'(2(1X,I8))')idx
    call lamb_mp_file_read (mp_file, int(data_size,kind=int_big),&
         data_dump, error=error)
    call lamb_mp_file_destroy (mp_file, error)

    !call internal_redistribute (matrix, int(nentries,kind=allatm_k),&
    !     idx(1,:), idx(2,:), data_dump, error)

    deallocate (idx, stat=stat)
    call lamb_memory_check (stat, "idx", error)
    deallocate (data_dump, stat=stat)
    call lamb_memory_check (stat, "data_dump", error)

    call lamb_error_stop (error, error_handle)
  end subroutine lamb_load_generic

!> \brief Read simple matrix data.
!>
!> Loads a data dump, stored as separate files, from disk.
!> \note Currently does not save data to a matrix. Because of changes
!> to the internal_redistribute methods.
  subroutine lamb_load_separate(matrix, filename, mp_env, error)
    type(lamb_matrix_d_type), pointerout :: matrix
    character(len=*), intent(in) :: filename
    type(lamb_mp_env_type), pointerin :: mp_env
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_load_separate', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    type(lamb_mp_file_type) :: meta_file
    integer(kind=int_8), dimension(4) :: sizes
    integer(kind=int_8) :: n_data_files, ss1, ss2
    integer(kind=int_4), dimension(:), allocatable :: block_sizes
    real(kind=real_4), dimension(:,:), allocatable :: coordinates

    type(lamb_set_type_p), dimension(2) :: atom_sets
    type(lamb_relation_type), pointer :: matrix_relation
    type(lamb_meta_type) :: meta
    type(lamb_plexus_type), pointer :: plexus
    type(lamb_set_partition_type_p), dimension(2) :: set_partitions
    type(lamb_partition_type), pointer :: partition
    type(lamb_distribution_type), pointer :: distribution

    integer(kind=int_8), dimension(:,:), allocatable :: idx
    real(kind=dp), dimension(:), allocatable :: data_dump

    !call lamb_error_trace_on(error)
    call lamb_error_set (error, error_handle, routineN)

    call lamb_error_make_ok (error, prev_level)

    call lamb_mp_file_create (meta_file, mp_env, trim(filename)//".meta",&
         reading=.TRUE., error=error)
    call lamb_error_catch (error, location=HERE)

    ! Meta File format:
    ! 1x int8 : data files included
    ! 1x int8 : type ; (==0=>0-based coordinates)
    ! 2x int8 : set sizes N1, N2
    ! (N1 + N2)x int4 block_sizes for sets 1 and 2
    ! (N1 + N2)x 3*real4 coors in range 0 to 1 for sets 1 and 2
    
    ! data files format
    ! NE = number of entries, DS = data_size
    ! NE * (row,col)
    ! DATA (DS entries)

    call lamb_mp_file_l_read (meta_file, 4_int_big, sizes, error)
    n_data_files = sizes(1)
    ss1 = sizes(2)
    ss2 = sizes(3)

    allocate (block_sizes(max(ss1, ss2)), stat=stat)
    call lamb_memory_check(stat, "block_sizes", int(max(ss1,ss2),kind=int_big),&
         error)

    call lamb_mp_file_i_read(meta_file, int(ss1,kind=int_big), block_sizes, error)
    call lamb_set_new(atom_sets(1)%p, int(ss1,kind=allatm_k), int(block_sizes(1:ss1),kind=blksz_k), error)
    call lamb_mp_file_i_read(meta_file, int(ss2,kind=int_big), block_sizes, error)
    call lamb_set_new(atom_sets(2)%p, int(ss2,kind=allatm_k), int(block_sizes(1:ss2),kind=blksz_k), error)
    deallocate (block_sizes, stat=stat)
    call lamb_memory_check(stat, "block_sizes", error)

    call lamb_relation_new (matrix_relation, atom_sets, lamb_rel_no_symmetry, error)

    allocate (coordinates(3,max(ss1, ss2)), stat=stat)
    call lamb_memory_check(stat, "coordinates", 3*int(max(ss1,ss2),kind=int_big),&
         error)
    call lamb_mp_file_s_read(meta_file, int(ss1,kind=int_big), coordinates, error)
    call lamb_set_update_coordinates(atom_sets(1)%p,&
         real(coordinates(:,1:ss1),kind=space_k), error)
    call lamb_mp_file_s_read(meta_file, int(ss2,kind=int_big), coordinates, error)
    call lamb_set_update_coordinates(atom_sets(2)%p,&
         real(coordinates(:,1:ss2),kind=space_k), error)
    call lamb_mp_file_destroy(meta_file, error)
    deallocate (coordinates, stat=stat)
    call lamb_memory_check(stat, "coordinates", error)

    call lamb_meta_create(meta, "read-in matrix", matrix_relation, error)
    call lamb_plexus_create_simple (plexus, mp_env, error)
    partition => lamb_plexus_get_partition (plexus, error)
    call lamb_set_partition_new (set_partitions(1)%p, atom_sets(1)%p, partition, error)
    call lamb_set_partition_new (set_partitions(2)%p, atom_sets(1)%p, partition, error)
    call lamb_distribution_new(distribution, plexus, set_partitions, error)
    call lamb_plexus_release (plexus, error)

    call lamb_distribution_update (distribution, error)

    call lamb_matrix_d_new (matrix, meta, distribution, error)

    call lamb_distribution_release (distribution, error)
    call lamb_set_partition_release (set_partitions(1)%p, error)
    call lamb_set_partition_release (set_partitions(2)%p, error)
    call lamb_set_release(atom_sets(1)%p, error)
    call lamb_set_release(atom_sets(2)%p, error)

    call lamb_meta_destroy(meta, error)
    call lamb_relation_release (matrix_relation, error)

    call lamb_matrix_d_release (matrix, error)

    call lamb_error_stop (error, error_handle)
    !call lamb_error_trace_off(error)
  end subroutine lamb_load_separate

!> \brief Loads a DBCSR matrix from disk into a LAMB matrix.
!>
!> Loads a DBCSR matrix as saved by the appropriate DBCSR routine into
!> a newly created LAMB matrix.
!> The DBCSR file format is described on a separate page, \ref dbcsr_file_format.
  subroutine lamb_load_dbcsr(matrix, filename, mp_env, error)
    type(lamb_matrix_d_type), pointerout :: matrix
    character(len=*), intent(in) :: filename
    type(lamb_mp_env_type), pointerin :: mp_env
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_load_dbcsr', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level, i
    type(lamb_mp_file_type) :: meta_file, coor_file
    integer(kind=int_8), dimension(4) :: sizes
    integer(kind=int_8) :: n_data_files, ss1, ss2
    integer(kind=int_4), dimension(:), allocatable :: block_sizes
    real(kind=real_4), dimension(:,:), allocatable :: coordinates

    type(lamb_set_type_p), dimension(2) :: atom_sets
    type(lamb_relation_type), pointer :: matrix_relation
    type(lamb_meta_type) :: meta
    type(lamb_plexus_type), pointer :: plexus
    type(lamb_set_partition_type_p), dimension(2) :: set_partitions
    type(lamb_partition_type), pointer :: partition
    type(lamb_distribution_type), pointer :: distribution
    type(lamb_space_type), pointer :: space
    character :: rel_type
    real(kind=space_k), dimension(3,3) :: h
    logical, parameter :: debug = debug_mod

    integer(kind=int_8), dimension(:,:), allocatable :: idx
    real(kind=dp), dimension(:), allocatable :: data_dump

!> \page dbcsr_file_format DBCSR File Format
!> The DBCSR file format is dump of the processes' distributed
!> data. As such it is composed of four types of segments:
!> - sub-header1, which contains the basic matrix information
!> - sub-header2, which defines the size of a DBCSR tile (index and data)
!> - sub-header3, which contains the DBCSR index
!> - data
!> The data format for n chunks (processes) can be defined as
!> 
!> sub-header1
!> [sub-header2_1 sub-header2_2 ... sub-header2_n]
!> [sub-header3_1 sub-header3_2 ... sub-header3_n]
!> [data_1 data_2 ... data_n]
!>
!> There is a sub-header2, sub-header3, and data segment for each of
!> the processes; as a whole they are described in the format as
!> chunks. In the file, first all sub-header1s are written, then all
!> sub-header2s, followed by sub-header3s and finally followed by the
!> matrix data.
!> <pre>
!>   sub-header1 contains: 
!>     1 string: (of length version_len) the current version of this routine, 
!>     1 string: (of length default_string_length) matrix_name, 
!>     1 character: matrix_type, 
!>     4 integers: numnodes, data_type, nblkrows_total, nblkcols_total, 
!>     2 vectors:  row_blk_size (length = nblkrows_total), 
!>                 col_blk_size (length = nblkcols_total), 
!>   sub-header2 contains:
!>     2 integers: nblks, data_area_size, 
!>   sub-header3 contains: 
!>     3 vectors:  row_p (length = nblkrows_total+1),  
!>                 col_i (length = nblks), 
!>                 blk_p (length = nblks); 
!>   and the file's body contains the block data
!> </pre>

    integer, parameter :: dbcsr_version_len = 10
    integer, parameter :: dbcsr_default_string_length = 80
    character(len=dbcsr_version_len) :: dbcsr_version
    character(len=dbcsr_default_string_length) :: dbcsr_name
    character, dimension(dbcsr_version_len) :: dbcsr_version_v
    character, dimension(dbcsr_default_string_length) :: dbcsr_name_v
    character :: dbcsr_matrix_type
    integer, dimension(4) :: dbcsr_subh1meta
    integer :: dbcsr_nblkrows, dbcsr_nblkcols
    integer, dimension(:), allocatable :: dbcsr_row_blk_size, dbcsr_col_blk_size

    !call lamb_error_trace_on(error)
    call lamb_error_set (error, error_handle, routineN)

    call lamb_error_make_ok (error, prev_level)

    call lamb_mp_file_create (meta_file, mp_env, trim(filename),&
         reading=.TRUE., error=error)
    call lamb_error_catch (error, location=HERE)


    ! Reads in DBCSR subheader 1.
    ! Only the master processes reads the file. It then broadcasts
    ! it to the other ranks.
    if (lamb_mp_env_am_master(mp_env)) then
       call lamb_mp_file_u_read (meta_file, int(dbcsr_version_len, kind=int_big), dbcsr_version, error)
       call lamb_mp_file_u_read (meta_file, int(dbcsr_default_string_length, kind=int_big), dbcsr_name, error)
       call lamb_mp_file_u_read (meta_file, 1_int_big, dbcsr_matrix_type, error)
       call lamb_mp_file_i_read (meta_file, 4_int_big, dbcsr_subh1meta, error)
    endif

    call lamb_mp_u_bcast_v (mp_env, int(dbcsr_version_len,kind=int_big), dbcsr_version, error)
    call lamb_mp_u_bcast_v (mp_env, int(dbcsr_default_string_length,kind=int_big),&
         dbcsr_name, error)
    call lamb_mp_bcast_s (mp_env, dbcsr_matrix_type, error)
    call lamb_mp_bcast_v (mp_env, 4_int_big, dbcsr_subh1meta, error)

    dbcsr_nblkrows = dbcsr_subh1meta(3)
    dbcsr_nblkcols = dbcsr_subh1meta(4)
    if (debug) then
       write(*,*)'DBCSR matrix blocked size',dbcsr_nblkrows,dbcsr_nblkcols
    endif

    allocate (dbcsr_row_blk_size (dbcsr_nblkrows), stat=stat)
    call lamb_memory_check (stat, "dbcsr_row_blk_size", dbcsr_nblkrows, error)
    allocate (dbcsr_col_blk_size (dbcsr_nblkcols), stat=stat)
    call lamb_memory_check (stat, "dbcsr_col_blk_size", dbcsr_nblkcols, error)
    if (lamb_mp_env_am_master(mp_env)) then
       call lamb_mp_file_i_read (meta_file, int(dbcsr_nblkrows, kind=int_big), dbcsr_row_blk_size, error)
       call lamb_mp_file_i_read (meta_file, int(dbcsr_nblkcols, kind=int_big), dbcsr_col_blk_size, error)
    endif

    call lamb_mp_bcast_v (mp_env, int(dbcsr_nblkrows, kind=int_big), dbcsr_row_blk_size, error)
    call lamb_mp_bcast_v (mp_env, int(dbcsr_nblkcols, kind=int_big), dbcsr_col_blk_size, error)

    if (debug) then
       write(*,*)dbcsr_row_blk_size
       write(*,*)dbcsr_col_blk_size
    endif

    ! Verify that row/column block sizes fit the data type (kind).
    call lamb_assert(all(dbcsr_row_blk_size < huge(int(0,kind=blksz_k))),&
         lamb_failure_level, lamb_external_error, &
         "Row block size too big for blksz_k data type.",&
         routineN, __LINE__, error)
    call lamb_assert(all(dbcsr_col_blk_size < huge(int(0,kind=blksz_k))),&
         lamb_failure_level, lamb_external_error, &
         "Colmun block size too big for blksz_k data type.",&
         routineN, __LINE__, error)

    ! Build up the LAMB matrix.
    call lamb_set_new(atom_sets(1)%p,&
         int(dbcsr_nblkrows,kind=allatm_k),&
         int(dbcsr_row_blk_size(1:dbcsr_nblkrows),kind=blksz_k), error)
    if (dbcsr_matrix_type == 'N') then
       call lamb_set_new(atom_sets(2)%p,&
            int(dbcsr_nblkcols,kind=allatm_k),&
            int(dbcsr_col_blk_size(1:dbcsr_nblkcols),kind=blksz_k), error)
       call lamb_relation_new (matrix_relation, atom_sets,&
            lamb_rel_no_symmetry, error)
    else
       call lamb_assert (dbcsr_nblkrows, "EQ", dbcsr_nblkcols,&
            lamb_failure_level, lamb_external_error,&
            "Number of rows and columns differ for symmetric-type matrix.",&
            routineN, __LINE__, error)
       call lamb_assert (all(dbcsr_row_blk_size .eq. dbcsr_col_blk_size),&
            lamb_failure_level, lamb_external_error,&
            "Row and column sizes differ for symmetric-type matrix.",&
            routineN, __LINE__, error)
       call lamb_assert (dbcsr_matrix_type, "EQ", "N",&
            lamb_failure_level, lamb_unimplemented,&
            "Only non-symmetric matrices currently supported.",&
            routineN, __LINE__, error)
       atom_sets(2)%p => atom_sets(1)%p
       call lamb_set_hold (atom_sets(2)%p, error)
       select case (dbcsr_matrix_type)
       case('S')
          rel_type = lamb_rel_no_symmetry
       case('A')
          rel_type = lamb_rel_antisymmetric
       case('H')
          rel_type = lamb_rel_hermitian
       case('K')
          rel_type = lamb_rel_antihermitian
       case default
          rel_type = lamb_rel_invalid
       end select
       call lamb_relation_new (matrix_relation, atom_sets, rel_type, error)
    endif
    allocate (coordinates(3,max(dbcsr_nblkrows,dbcsr_nblkcols)), stat=stat)
    call lamb_memory_check(stat, "coordinates",&
         3*int(max(dbcsr_nblkrows,dbcsr_nblkcols),kind=int_big),&
         error)
    ! Try to read in coordinate file; if failure, generate random points.
    call lamb_error_make_ok(error, prev_level)
    call lamb_error_set_die_level (error, lamb_failure_level+1)
    call lamb_mp_file_create(coor_file, mp_env, 'small.coor', reading=.true., error=error)
    if (.not. lamb_error_not_ok(error)) then
       if (lamb_mp_env_am_master(mp_env)) then
          call lamb_mp_file_s_read (coor_file, int(3*dbcsr_nblkrows, kind=int_big), coordinates, error)
       endif
       call lamb_mp_file_destroy (coor_file, error)
       call lamb_mp_s_bcast_v (mp_env, int(3*dbcsr_nblkrows, kind=int_big), coordinates, error)
       h(:,:) = 0.0_space_k
       do i = 1, 3
          h(i,i) = maxval(coordinates(i,1:dbcsr_nblkrows))
       enddo
       call lamb_space_new(space, h, error)
    endif
    if (lamb_error_not_ok(error)) then
       call lamb_space_new_unit_box(space, error)
       if (debug) then
          write(*,*)routineN//' Problem reading coordinate file.'
       endif
       call lamb_space_make_random_points(space,&
            int(dbcsr_nblkrows,kind=allatm_k), coordinates, error)
       call lamb_space_fill_points(space,&
            int(dbcsr_nblkrows,kind=allatm_k), coordinates, error)
    endif
    call lamb_error_update_level(error, prev_level)
    ! End of coordinate file reading.
    call lamb_set_update_coordinates(atom_sets(1)%p,&
         real(coordinates(:,1:dbcsr_nblkrows),kind=space_k), error)
    if (.not.lamb_relation_has_symmetry(matrix_relation)) then
       if (dbcsr_nblkcols .NE. dbcsr_nblkrows) then
          call lamb_space_fill_points(space,&
               int(dbcsr_nblkcols,kind=allatm_k), coordinates, error)
       endif
       call lamb_set_update_coordinates(atom_sets(2)%p,&
            real(coordinates(:,1:dbcsr_nblkcols),kind=space_k), error)
    endif
    deallocate (coordinates, stat=stat)
    call lamb_memory_check(stat, "coordinates", error)


    call lamb_meta_create(meta, trim(dbcsr_name), matrix_relation, error)

    call lamb_plexus_calculate (plexus, space, mp_env, error)
    call lamb_space_release (space, error)

    partition => lamb_plexus_get_partition (plexus, error)
    call lamb_set_partition_new (set_partitions(1)%p, atom_sets(1)%p, partition, error)
    if (lamb_relation_has_symmetry(matrix_relation)) then
       set_partitions(2)%p => set_partitions(1)%p
       call lamb_set_partition_hold (set_partitions(2)%p, error)
    else
       call lamb_set_partition_new (set_partitions(2)%p, atom_sets(2)%p, partition, error)
    end if
    call lamb_distribution_new(distribution, plexus, set_partitions, error)
    call lamb_plexus_release (plexus, error)

    call lamb_distribution_update (distribution, error)

    call lamb_matrix_d_new (matrix, meta, distribution, error)

    call lamb_distribution_release (distribution, error)
    call lamb_set_partition_release (set_partitions(1)%p, error)
    call lamb_set_partition_release (set_partitions(2)%p, error)
    call lamb_set_release(atom_sets(1)%p, error)
    call lamb_set_release(atom_sets(2)%p, error)

    call lamb_meta_destroy(meta, error)
    call lamb_relation_release (matrix_relation, error)

    call load_dbcsr_chunks(meta_file, matrix%distribution, mp_env,&
         dbcsr_subh1meta(1), error)

    call lamb_mp_file_destroy(meta_file, error)

    call lamb_error_stop (error, error_handle)
    !call lamb_error_trace_off(error)

  contains
    
    subroutine load_dbcsr_chunks (dbcsr_file, distribution, mp_env, n_chunks, error)
      type(lamb_mp_file_type), intent(inout) :: dbcsr_file
      type(lamb_distribution_type), intent(in) :: distribution
      type(lamb_mp_env_type), intent(in) :: mp_env
      integer, intent(in) :: n_chunks
      type(lamb_error_type), intent(inout) :: error

      CHARACTER(LEN=*), PARAMETER :: routineN = 'load_dbcsr_chunks', &
           routineP = moduleN//':'//routineN
      INTEGER :: error_handle, stat, prev_level

      integer, dimension(2) :: dbcsr_subh2
      integer :: dbcsr_nblks, dbcsr_data_area_size
      integer, dimension(:), allocatable :: dbcsr_row_p, dbcsr_blk_p, dbcsr_col_i
      integer(kind=allatm_k), dimension(:,:), allocatable :: global_idx
      integer(kind=subset_k), dimension(:,:), allocatable :: subset_idx
      integer(kind=locatm_k), dimension(:,:), allocatable :: local_idx
      integer(kind=datastore_k), dimension(:), allocatable :: datastore_i
      integer(kind=dataptr_k), dimension(:,:), allocatable :: blkdata_extents

      integer, dimension(2,n_chunks) :: headers2, headers2_tmp
      integer(kind=int_big), dimension(n_chunks,3) :: offsets
      integer(kind=int_big) :: h1_size, h2_size

      integer(kind=proc_k) :: n_proc, i_proc
      integer :: chunk_i, bin, chunk_counter
      type(lamb_data_d_type), dimension(n_chunks) :: data_areas
      integer(kind=allatm_k) :: all_nblks, lb, ub, blk
      integer(kind=dataptr_k) :: to_read_count

      call lamb_error_set (error, error_handle, routineN)

      n_proc = lamb_mp_env_get_n_procs(mp_env)
      i_proc = lamb_mp_env_get_my_proc(mp_env)

      h1_size = int(dbcsr_version_len + dbcsr_default_string_length + 1&
           + 4*4 + dbcsr_nblkrows*4 + dbcsr_nblkcols*4, kind=int_big)
      h2_size = 2_int_big * 4_int_big
      ! offsets(:,1) are the offsets in the file of the chunks' header2 data.
      ! offsets(:,2) are the offsets in the file of the chunks' header3 data
      ! offsets(:,3) are the offsets in the file of the chunks' data-data
      offsets = 0
      offsets(1,1) = h1_size
      do chunk_i = 2, n_chunks
         offsets(chunk_i,1) = offsets(chunk_i-1,1) + h2_size
      enddo

      headers2_tmp = 0
      do chunk_i = 1+i_proc, n_chunks, n_proc
         ! Reads in DBCSR subheader 2.
         call lamb_mp_file_seek (dbcsr_file, offsets(chunk_i,1),&
              absolute=.true., error=error)
         call lamb_mp_file_i_read (dbcsr_file,&
              2_int_big, headers2_tmp(:,chunk_i), error)
         dbcsr_nblks = headers2_tmp(1,chunk_i)
         dbcsr_data_area_size = headers2_tmp(2,chunk_i)
         if (debug) then
            write(*,*)'nblks, data_area_size:', dbcsr_nblks, dbcsr_data_area_size
         endif
      enddo
      call lamb_mp_i_sum_v(mp_env, int(2*n_chunks,kind=int_big),&
           headers2_tmp, headers2, error)
      if (debug) then
         write(*,*)'headers2',headers2
      endif

      offsets(1,2) = offsets(n_chunks,1) + h2_size
      do chunk_i = 2, n_chunks
         offsets(chunk_i,2) = offsets(chunk_i-1,2) + &
              4*(dbcsr_nblkrows + 1) + &
              4 * (2*headers2(1,chunk_i-1))
              !4 * (2 * headers2(1,chunk_i-1))
      enddo

      offsets(1,3) = offsets(n_chunks,2) + 4* (dbcsr_nblkrows + 1) +&
           4*(2 * headers2(1,n_chunks))
      do chunk_i = 2, n_chunks
         offsets(chunk_i,3) = offsets(chunk_i-1,3) + &
              8*headers2(2,chunk_i-1)
      enddo
      if (debug) then
         write(*,*)'offsets'
         write(*,*)offsets(:,1)
         write(*,*)offsets(:,2)
         write(*,*)offsets(:,3)
      endif

      ! Calculate the sum of the extents we need for the index and data
      ! for all our chunks.
      all_nblks = 0_allatm_k
      ! These loop indices must match the "chunk_loop" DO loop.
      do chunk_i = 1+i_proc, n_chunks, n_proc
         all_nblks = all_nblks + int(headers2(1,chunk_i),kind=allatm_k)
      enddo

      allocate (global_idx(2,all_nblks), stat=stat)
      call lamb_memory_check(stat, "global_idx", 2*all_nblks, error)
      allocate (datastore_i(all_nblks), stat=stat)
      call lamb_memory_check(stat, "datastore_i", all_nblks, error)
      allocate (blkdata_extents(2,all_nblks), stat=stat)
      call lamb_memory_check(stat, "blkdata_extents", 2*all_nblks, error)

      ! ub and lb point into the local arrays for atoms
      chunk_counter = 0
      ub = 0_allatm_k
      chunk_loop: do chunk_i = 1+i_proc, n_chunks, n_proc
         chunk_counter = chunk_counter + 1
         call lamb_data_create(data_areas(chunk_counter),&
              int(headers2(2,chunk_i),kind=dataptr_k),&
              error)
         lb = ub + 1_allatm_k
         ub = lb + int(headers2(1,chunk_i),kind=allatm_k) - 1_allatm_k
         call lamb_assert(int(ub,kind=int_big), 'LE', int(all_nblks,kind=int_big),&
              lamb_failure_level, lamb_internal_error,&
              "Mismatch in calculated chunk extents.",&
              routineN, __LINE__, error)
         ! Hopefully there aren't too many chunks.
         ! Reads in DBCSR subheader 3 for my chunks
         ! Knowingly inefficient due to the repeated de/re-allocating.
         dbcsr_nblks = headers2(1,chunk_i)
         dbcsr_data_area_size = headers2(2,chunk_i)
         allocate (dbcsr_row_p(dbcsr_nblkrows+1), stat=stat)
         call lamb_memory_check(stat, "dbcsr_row_p", dbcsr_nblkrows+1, error)
         allocate (dbcsr_col_i(dbcsr_nblks), stat=stat)
         call lamb_memory_check(stat, "dbcsr_col_i", dbcsr_nblks, error)
         allocate (dbcsr_blk_p(dbcsr_nblks), stat=stat)
         call lamb_memory_check(stat, "dbcsr_blk_p", dbcsr_nblks, error)

         !call lamb_alloc_2d_ensure_size (local_idx, 2, dbcsr_nblks, error)

         call lamb_mp_file_seek (dbcsr_file, offsets(chunk_i,2),&
              absolute=.true., error=error)
         call lamb_mp_file_i_read (dbcsr_file, int(dbcsr_nblkrows+1, kind=int_big),&
              dbcsr_row_p, error)
         call lamb_mp_file_i_read (dbcsr_file, int(dbcsr_nblks, kind=int_big),&
              dbcsr_col_i, error)
         call lamb_mp_file_i_read (dbcsr_file, int(dbcsr_nblks, kind=int_big),&
              dbcsr_blk_p, error)

         ! Build a coordinate index from col_i and
         global_idx(2,lb:ub) = int(dbcsr_col_i, kind=allatm_k)
         ! row_p.
         do bin = 1, dbcsr_nblkrows
            global_idx(1, lb-1+dbcsr_row_p(bin)+1 : lb-1+dbcsr_row_p(bin+1))&
                 = int(bin, kind=allatm_k)
         enddo
         blkdata_extents(1,lb:ub) = int(dbcsr_blk_p, kind=dataptr_k)
         deallocate (dbcsr_row_p, stat=stat)
         call lamb_memory_check(stat, "dbcsr_row_p", error)
         deallocate (dbcsr_col_i, stat=stat)
         call lamb_memory_check(stat, "dbcsr_col_i", error)
         deallocate (dbcsr_blk_p, stat=stat)
         call lamb_memory_check(stat, "dbcsr_blk_p", error)

         blkdata_extents(2,lb:ub) = blkdata_extents(1,lb:ub) - 1_dataptr_k &
              + int(dbcsr_row_blk_size(global_idx(1,lb:ub)),kind=dataptr_k) &
              * int(dbcsr_col_blk_size(global_idx(2,lb:ub)),kind=dataptr_k)

         to_read_count = maxval(blkdata_extents(2,lb:ub))
         call lamb_assert (int(to_read_count,kind=int_big), 'LE', &
              int(dbcsr_data_area_size,kind=int_big),&
              lamb_failure_level, lamb_external_error, &
              'Data block extent exceeds data area.',&
              routineN, __LINE__, error)
         call lamb_data_set_len(data_areas(chunk_counter), int(to_read_count,kind=int_big), error)

         ! Reads in matrix data.
         call lamb_mp_file_seek (dbcsr_file, offsets(chunk_i,3),&
              absolute=.true., error=error)
         call lamb_mp_file_read (dbcsr_file,&
              int(to_read_count, kind=int_big),&
              data_areas(chunk_counter)%d, error)

         call lamb_assert(chunk_counter <= huge(int(0,kind=datastore_k)),&
              lamb_fatal_level, lamb_internal_error,&
              "Too many chunks to handle with one process.",&
              routineN, __LINE__, error=error)
         datastore_i(lb:ub) = int(chunk_counter, datastore_k)

      enddo chunk_loop
      allocate (subset_idx(2,all_nblks), stat=stat)
      call lamb_memory_check(stat, "subset_idx", 2*all_nblks, error)
      allocate (local_idx(2,all_nblks), stat=stat)
      call lamb_memory_check(stat, "local_idx", 2*all_nblks, error)
      ! Re-calculate index to be local.
      call lamb_dist_localize_index (distribution,&
           int(all_nblks,kind=allatm_k), global_idx,&
           subset_idx, local_idx, error)
      if (debug) then
         write(*,*)'(global) -> (subset)(local_idx)/(max,max)'
         do lb = 1, all_nblks
            write(*,'("(",2(I7),") -> (",2(I4),")(",2(I4),")",I7,1X,2(I7),"/",2(I7))')&
                 global_idx(:,lb),subset_idx(:,lb),local_idx(:,lb),&
                 datastore_i(lb), blkdata_extents(:,lb),&
                 matrix%distribution%set_partitions(1)%p%n_subset_elements(&
                 subset_idx(1,lb)),&
                 matrix%distribution%set_partitions(2)%p%n_subset_elements(&
                 subset_idx(2,lb))
            if (local_idx(1,lb) > matrix%distribution%set_partitions(1)%p%n_subset_elements(&
                 subset_idx(1,lb))) write(*,*)1
            if (local_idx(2,lb) > matrix%distribution%set_partitions(2)%p%n_subset_elements(&
                 subset_idx(2,lb))) write(*,*)2
         end do
      endif

      deallocate (global_idx, stat=stat)
      call lamb_memory_check(stat, "global_idx", error)


      call internal_distribute(matrix, all_nblks,&
           subset_idx, local_idx, datastore_i, blkdata_extents, data_areas, error)

      chunk_counter = 0
      do chunk_i = 1+i_proc, n_chunks, n_proc
         chunk_counter = chunk_counter + 1
         call lamb_data_destroy(data_areas(chunk_counter), error)
      enddo


      deallocate(datastore_i, stat=stat)
      call lamb_memory_check(stat, "datastore_i", error)
      deallocate (subset_idx, stat=stat)
      call lamb_memory_check(stat, "subset_idx", error)
      deallocate (local_idx, stat=stat)
      call lamb_memory_check(stat, "local_idx", error)
      deallocate (blkdata_extents, stat=stat)
      call lamb_memory_check(stat, "blkdata_extents", error)
      call lamb_error_stop (error, error_handle)
    end subroutine load_dbcsr_chunks

  end subroutine lamb_load_dbcsr


END MODULE lamb_io_operations
