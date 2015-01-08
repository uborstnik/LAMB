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

!> \brief Type definitions for the plexus of space, particles, and processes.
!>
!> Defines the storage used for data structures used by the plexus.

MODULE lamb_plexus_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_mp_env_types
  use lamb_mp_env_methods
  use lamb_partition_methods
  use lamb_partition_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_plexus_type
  PUBLIC :: lamb_plexus_type_p

  PUBLIC :: lamb_plexus_create
  PUBLIC :: lamb_plexus_destroy

  PUBLIC :: lamb_plexus_new
  PUBLIC :: lamb_plexus_hold
  PUBLIC :: lamb_plexus_release

  PUBLIC :: lamb_plexus_valid

  PUBLIC :: lamb_plexus_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'plexus'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: plexus_id  !< Unique identifier for intantiated types.

  !> \brief Hold plexus-related data.
  !>
  !> Several of these data structures are not scalable. In the extreme
  !> case a processes will only need the data related to it, such as
  !> only the cube pools of the cubes in its catalog. The "map" is
  !> temporary. So are the process catalogs for all processes.
  TYPE lamb_plexus_type
     INTEGER :: refcount
     INTEGER :: id
     !> The partition thix plexus uses.
     type(lamb_partition_type), pointer :: partition
     !> The number of regions.
     integer :: n_regions
     !> The number of subsets.
     integer :: n_subsets
     type(lamb_mp_env_type), pointer :: mp_env
     !> \brief A mapping that maps sectors as (subset,subset) to processes.
     !> \details This data structure is not scalable for large numbers of
     !> cubes; hence the map should only be accessed via a method.
     integer(kind=proc_k), dimension(:,:), allocatable :: realm_map
     !> A region pool region_pools(:,region) lists the processes in
     !> its pool.
     integer(kind=proc_k), dimension(:,:), allocatable :: region_pools
     !> The sizes of region pools.
     integer(kind=proc_k), dimension(:), allocatable :: region_pools_sizes
     !> A process catalog my_catalog(:) lists the regions
     !> in which the process lies (maps P->R).
     integer(kind=cube_k), dimension(:), allocatable   :: my_catalog
     integer(kind=cube_k) :: my_catalog_size
     !> \brief A process orbit my_orbit(:) lists the processes
     !> that lie in the pools that a process catalogs.
     !> \details A process
     !> should normally only need to communicate or otherwise deal
     !> with the processes listed here. This is the key to limiting
     !> communication.
     integer(kind=proc_k), dimension(:), allocatable :: my_orbit
     integer(kind=proc_k) :: my_orbit_size
     !> \var A mapping that maps processes to sectors as (subset,subset).
     integer(kind=subset_k), dimension(:,:), allocatable :: my_sectors
     integer(kind=cube_k) :: my_sectors_size
  END TYPE lamb_plexus_type

  TYPE lamb_plexus_type_p
     TYPE(lamb_plexus_type), POINTER :: p
  END TYPE lamb_plexus_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] plexus  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_plexus_create (plexus, partition, mp_env, error)
    TYPE(lamb_plexus_type), INTENT(OUT):: plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_partition_type), POINTERIN :: partition
    type(lamb_mp_env_type), POINTERIN :: mp_env

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=cube_k) :: n_cubes
    integer(kind=subset_k) :: n_subsets
    integer(kind=proc_k) :: n_procs

    CALL lamb_error_set (error, error_handle, routineN)

    plexus%id = 0
    plexus%refcount = 0

    plexus%partition => partition
    call lamb_partition_hold (plexus%partition, error)
    plexus%mp_env => mp_env
    call lamb_mp_env_hold (mp_env, error)

    n_cubes = lamb_partition_get_n_cubes(partition)
    plexus%n_regions = int(n_cubes)
    n_subsets = int(n_cubes, kind=subset_k)
    plexus%n_subsets = int(n_cubes)
    !map allocate(plexus%map(n_subsets, n_subsets), stat=stat)
    !map call lamb_memory_check(stat, "plexus%map", int(n_subsets,kind=int_big)**2, error)
    allocate(plexus%realm_map(n_cubes, n_cubes), stat=stat)
    call lamb_memory_check(stat, "plexus%realm_map", int(n_cubes,kind=int_big)**2, error)

    n_procs = lamb_mp_env_get_n_procs(mp_env)
    !allocate(plexus%realm_map(2,n_cubes,0:n_procs-1), stat=stat)
    !call lamb_memory_check(stat, "plexus%realm_map",&
    !     2*int(n_cubes,kind=int_big)*int(n_procs,kind=int_big), error)
    !allocate(plexus%realm_map_sizes(0:n_procs-1), stat=stat)
    !call lamb_memory_check(stat, "plexus%realm_map_sizes",&
    !     int(n_procs,kind=int_big), error)
    allocate(plexus%my_sectors(2,int(n_cubes,kind=int_big)**2_int_big), stat=stat)
    call lamb_memory_check(stat, "plexus%my_sectors",&
         int(n_cubes,kind=int_big)**2_int_big, error)

    allocate (plexus%region_pools(n_procs, n_cubes), stat=stat)
    call lamb_memory_check(stat, "plexus%region_pools", &
         int(n_procs*n_cubes,kind=int_big), error)
    allocate (plexus%region_pools_sizes(n_cubes), stat=stat)
    call lamb_memory_check(stat, "plexus%region_pools", &
         int(n_cubes,kind=int_big), error)

    !allocate (plexus%process_catalogs(n_cubes,0:n_procs-1), stat=stat)
    !call lamb_memory_check(stat, "plexus%process_catalogs", &
    !     int(n_procs*n_cubes,kind=int_big), error)
    !allocate (plexus%process_catalogs_sizes(0:n_procs-1), stat=stat)
    !call lamb_memory_check(stat, "plexus%process_catalogs_sizes", &
    !     int(n_procs,kind=int_big), error)
    allocate (plexus%my_catalog(n_cubes), stat=stat)
    call lamb_memory_check(stat, "plexus%my_catalog", &
         int(n_cubes,kind=int_big), error)

    !allocate (plexus%process_orbits(n_procs, 0:n_procs-1), stat=stat)
    !call lamb_memory_check(stat, "plexus%process_orbits", &
    !     int(n_procs*n_procs,kind=int_big), error)
    !allocate (plexus%process_orbits_sizes(0:n_procs-1), stat=stat)
    !call lamb_memory_check(stat, "plexus%process_orbits_sizes", &
    !     int(n_procs,kind=int_big), error)
    allocate (plexus%my_orbit(n_procs), stat=stat)
    call lamb_memory_check(stat, "plexus%my_orbit", &
         int(n_procs,kind=int_big), error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_plexus_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] plexus       Object to copy from
!> \param[out] new_plexus  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_plexus_copy (plexus, new_plexus, error)
    TYPE(lamb_plexus_type), INTENT(IN):: plexus
    TYPE(lamb_plexus_type), INTENT(OUT):: new_plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    new_plexus = plexus
    new_plexus%refcount = 0
    new_plexus%id = 0

    stop 'not implemented'

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_plexus_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] plexus  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_plexus_destroy (plexus, error)
    TYPE(lamb_plexus_type), INTENT(INOUT) :: plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    !map deallocate(plexus%map, stat=stat)
    !map call lamb_memory_check(stat, "plexus%map", error)
    deallocate(plexus%realm_map, stat=stat)
    call lamb_memory_check(stat, "plexus%realm_map", error)
    !deallocate(plexus%realm_map, stat=stat)
    !call lamb_memory_check(stat, "plexus%realm_map", error)
    !deallocate(plexus%realm_map_sizes, stat=stat)
    !call lamb_memory_check(stat, "plexus%realm_map_sizes", error)
    deallocate(plexus%my_sectors, stat=stat)
    call lamb_memory_check(stat, "plexus%my_sectors", error)

    deallocate (plexus%region_pools, stat=stat)
    call lamb_memory_check(stat, "plexus%region_pools", error)
    deallocate (plexus%region_pools_sizes, stat=stat)
    call lamb_memory_check(stat, "plexus%region_pools_sizes", error)

    !deallocate (plexus%process_catalogs, stat=stat)
    !call lamb_memory_check(stat, "plexus%process_catalogs", error)
    !deallocate (plexus%process_catalogs_sizes, stat=stat)
    !call lamb_memory_check(stat, "plexus%process_catalogs_sizes", error)
    deallocate (plexus%my_catalog, stat=stat)
    call lamb_memory_check(stat, "plexus%my_catalog", error)

    !deallocate (plexus%process_orbits, stat=stat)
    !call lamb_memory_check(stat, "plexus%process_orbits", error)
    !deallocate (plexus%process_orbits_sizes, stat=stat)
    !call lamb_memory_check(stat, "plexus%process_orbits_sizes", error)
    deallocate (plexus%my_orbit, stat=stat)
    call lamb_memory_check(stat, "plexus%my_orbit", error)

    call lamb_partition_release (plexus%partition, error)
    call lamb_mp_env_release (plexus%mp_env, error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_plexus_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] plexus  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_plexus_new (plexus, partition, mp_env, error)
    TYPE(lamb_plexus_type), POINTEROUT :: plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_partition_type), POINTERIN :: partition
    type(lamb_mp_env_type), POINTERIN :: mp_env

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (plexus, stat=stat)
    CALL lamb_memory_check (stat, "plexus", -1, error)

    CALL lamb_plexus_create (plexus, partition, mp_env, error)
    plexus%refcount = 1
    plexus%id = plexus_id
    plexus_id = plexus_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_plexus_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] plexus  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_plexus_hold (plexus, error)
    TYPE(lamb_plexus_type), POINTERINOUT :: plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    plexus%refcount = plexus%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_plexus_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] plexus  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_plexus_release (plexus, error)
    TYPE(lamb_plexus_type), POINTERINOUT :: plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    plexus%refcount = plexus%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (plexus%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error, &
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (plexus%refcount <= 0) THEN
       CALL lamb_plexus_destroy (plexus, error)
       DEALLOCATE (plexus, stat=stat)
       CALL lamb_memory_check (stat, "plexus", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_plexus_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] plexus  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_plexus_valid (plexus, error) RESULT (valid)
    TYPE(lamb_plexus_type), POINTERIN :: plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (plexus)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_plexus_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] plexus  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_plexus_verify (plexus, error)
    TYPE(lamb_plexus_type), INTENT(IN) :: plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_plexus_verify


END MODULE lamb_plexus_types
