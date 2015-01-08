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

!> \brief Plexus of space, particles, and processes.
!>
!> The plexus defines the relationship between space, particles, and processes.
!> The partitions of these three entities and their tuples are mapped.

MODULE lamb_plexus_operations

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_mp_env_types
  use lamb_mp_env_methods
  use lamb_partition_methods
  use lamb_partition_operations
  use lamb_partition_types
  use lamb_plexus_types
  use lamb_sched_types
  use lamb_sched_methods
  use lamb_space_types
  use lamb_space_methods

  IMPLICIT NONE

  PRIVATE

  public :: lamb_plexus_calculate
  public :: lamb_plexus_create_sched
  public :: lamb_plexus_create_simple

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'plexus_operations'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  !> \brief Generates a plexus.
  !> \details Generates the needed mappings between the particle
  !> system, space, and processes. Also generates a partition of space.
  !> \param[out] plexus   The generated plexus.
  !> \param[in] space     The space on which the generated plexus is based.
  !> \param[in] mp_env    The multiprocessor environment used.
  !> \param[in,out] error Error structure.
  !> \param[in] force_n_procs  (Optional) Make the plexus use this number
  !>                            of processes (for testing).
  !>                          \details The plexus is generated for the
  !>                            given number of processes instead of the actual number.
  !> \param[in] force_n_cubes  (Optional) Make the plexus use this number
  !>                            of cubes (for testing).
  !>                          \details The plexus is generated for the
  !>                            given number of cubes instead of the actual number.
  subroutine lamb_plexus_calculate(plexus, space, mp_env, error,&
       force_n_procs, force_n_cubes)
    TYPE(lamb_plexus_type), pointerout :: plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_space_type), POINTERIN :: space
    type(lamb_mp_env_type), POINTERIN :: mp_env
    integer(kind=proc_k), intent(in), optional :: force_n_procs
    integer(kind=cube_k), intent(in), optional :: force_n_cubes

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_calculate', &
         routineP = moduleN//':'//routineN

    type(lamb_partition_type), pointer :: partition
    INTEGER :: error_handle, stat, error_level, prev_level
    integer(kind=cube_k) :: n_cubes, i, j, cube, cube_i
    integer(kind=proc_k) :: n_procs, my_proc, p, q, src_p, p_i, o_i, o
    real :: root
    logical :: found
    logical, parameter :: verbose=.true., debug = .true.

    CALL lamb_error_set (error, error_handle, routineN)

    n_procs = lamb_mp_env_get_n_procs(mp_env)
    if (present(force_n_procs)) then
       n_procs = force_n_procs
       mp_env%n_procs = n_procs
    endif

    root = (sqrt(real(1+8*n_procs))-1.0)/2.0 * 2.0
    call lamb_error_make_ok (error, prev_level)
    call lamb_error_set_die_level(error, lamb_warning_level+1)
    call lamb_assert (int(root), "LE", int(huge(0_cube_k)),&
         lamb_warning_level, lamb_internal_error, &
         "Cube type too small to hold required number of cubes.",&
         routineN, __LINE__, error)
    if (lamb_error_ok (error)) then
       n_cubes = int(real(0.0,kind(root))+root, kind=cube_k)
    else
       n_cubes = huge(0_cube_k)
    endif
    call lamb_error_set_die_level(error)
    if (present (force_n_cubes)) then
       n_cubes = force_n_cubes
    endif
    call lamb_partition_calculate (partition, space, n_cubes, error)
    call lamb_partition_print (partition, error)
    call lamb_plexus_new (plexus, partition, mp_env, error)
    call lamb_partition_release (partition, error)

    if (verbose) then
       write(*,*)'Creating plexus with',n_procs,'processes and',n_cubes,'regions.'
    endif

    my_proc = lamb_mp_env_get_my_proc(mp_env)

    ! The first step is to determine the region pools. In effect it
    ! limits the regions (~cubes or ~subsets) that a processes needs
    ! to be aware aware of.
    call fill_region_pools(plexus%region_pools, plexus%region_pools_sizes,&
         plexus%my_catalog, plexus%my_catalog_size,&
         plexus%my_orbit, plexus%my_orbit_size,&
         n_cubes, n_procs, my_proc)

    if (debug) then
       write(*,*)'Region Pools^T'
       do i = 1_cube_k, n_cubes
          write(*,*)i,':',plexus%region_pools(1:plexus%region_pools_sizes(i),i)
       enddo
       write(*,*)'My Catalog',my_proc
       write(*,*)plexus%my_catalog(1:plexus%my_catalog_size)
       write(*,*)'My Orbit',my_proc,'size',plexus%my_orbit_size
       write(*,*)plexus%my_orbit(1:plexus%my_orbit_size)
    endif

    call fill_realm_map(plexus%realm_map,&
         plexus%region_pools, plexus%region_pools_sizes,&
         n_cubes, n_procs, my_proc, error)

    if (debug) then
       write(*,*)'Realm Map^T'
       do i = 1_cube_k, n_cubes
          write(*,*)plexus%realm_map(:,i)
       enddo
    endif

    plexus%my_sectors_size = 0_cube_k
    call fill_my_sectors(plexus%my_sectors, plexus%my_sectors_size,&
         plexus%my_catalog, plexus%my_catalog_size,&
         plexus%realm_map,&
         n_cubes, n_procs, my_proc, error)

    write(*,*)'my sectors'
    write(*,*)plexus%my_sectors(1,:plexus%my_sectors_size)
    write(*,*)plexus%my_sectors(2,:plexus%my_sectors_size)

    call lamb_error_update_level(error, prev_level)
    CALL lamb_error_stop (error, error_handle)

  end subroutine lamb_plexus_calculate

  !> \brief Defines the \ref Region_pool "region pools".
  !>
  !> This calcules the \ref Region_pool "region pools" (the processes
  !> that lie in a \ref Region "region"). Also calculates the
  !> processes's \ref Catalog "catalog", which is straightforward given
  !> the \ref Region_pool "region pools". Since the \ref Region_pool "region pools"
  !> are duplicated on all processes, it is also possible to calculate
  !> this processes's \ref Orbit "orbit". (While it would be possible to calculate
  !> all processes' \ref Orbit "orbits", that is not needed nor necessary.)
  !> \note This subroutine must return the same region pools on all processes.
  !> It is currently not calculated in a distrubuted manner.
  subroutine fill_region_pools (pools, pools_sizes,&
       my_catalog, my_catalog_size,&
       my_orbit, my_orbit_size,&
       n_regions, n_proc, my_proc)
    integer(kind=proc_k), dimension(:,:), intent(out) :: pools
    integer(kind=proc_k), dimension(:), intent(out) :: pools_sizes
    integer(kind=cube_k), dimension(:), intent(out) :: my_catalog
    integer(kind=cube_k), intent(out) :: my_catalog_size
    integer(kind=proc_k), dimension(:), intent(out) :: my_orbit
    integer(kind=proc_k), intent(out) :: my_orbit_size
    integer(kind=cube_k), intent(in) :: n_regions
    integer(kind=proc_k), intent(in) :: n_proc, my_proc

    integer(kind=proc_k) :: p, pi
    integer(kind=cube_k) :: r1, r2, r1a, r1b, r2a, r2b, r, ri
    integer(kind=cube_k) :: level1, level2
    real :: p_per_r, p_per_r2, r2_per_p, p_per_realms
    real :: p_in_r2, nr, nrminus, sqp_per_r
    logical :: my_turn
    integer(kind=int_big) :: n_realms, realm
    logical, parameter :: debug = .true.

    pools = 0_proc_k
    pools_sizes = 0_proc_k
    my_catalog = 0
    my_catalog_size = 0

    nr = real(n_regions)
    nrminus = real(n_regions-1_cube_k)/nr
    p_per_r = real(n_proc) / nr
    p_per_r2 = real(n_proc) / (nr*nr/2.0)
    sqp_per_r = sqrt(p_per_r2)
    r2_per_p = 1.0 / p_per_r2
    p_per_realms = real(n_proc) / (real(nr)*(real(nr)+1.0)/2.0)
    !
    n_realms = (int(n_regions,kind=int_big) * (int(n_regions,kind=int_big)+1_int_big)) / 2_int_big
    if (debug) then
       write(*,*)"Regions, realms, procs", n_regions, n_realms, n_proc
       write(*,*)"p/realms",p_per_r2, p_per_realms, 1.0/p_per_realms
    endif
    realm = 1_int_big
    !
    ! X X   1 2
    !   X     3
    !
    ! XX XX  1-2 4-5
    ! XX XX  . 3/7-6
    !    XX      8-9
    !    XX      . A
    call trr_tr(1_cube_k, n_regions)
    my_orbit = -1
    my_orbit_size = 0
    do ri = 1, my_catalog_size
       r = my_catalog(ri)
       do pi = 1_proc_k, pools_sizes(r)
          p = pools(pi,r)
          call add_to_pool(my_orbit, my_orbit_size, p)
       enddo
    enddo
  contains
    function which_p(realm) result (p)
      integer(kind=int_big), intent(in) :: realm
      integer(kind=proc_k) :: p
      p = mod(int(real(realm-1_int_big) * p_per_realms), n_proc)
    end function which_p
    subroutine trr_register1(r)
      integer(kind=cube_k), intent(in) :: r
      p = which_p(realm)
      call add_to_pool(pools(:,r), pools_sizes(r), p)
      if (p == my_proc) then
         call add_to_catalog(my_catalog, my_catalog_size, r)
      endif
      realm = realm + 1_int_big
    end subroutine trr_register1
    subroutine trr_register2(r, s)
      integer(kind=cube_k), intent(in) :: r, s
      logical :: my_turn
      p = which_p(realm)
      my_turn = p == my_proc
      call add_to_pool(pools(:,r), pools_sizes(r), p)
      if (r /= s) then
         call add_to_pool(pools(:,s), pools_sizes(s), p)
      endif
      if (my_turn) then
         call add_to_catalog(my_catalog, my_catalog_size, r)
         if (r /= s) then
            call add_to_catalog(my_catalog, my_catalog_size, s)
         endif
      endif
      realm = realm + 1_int_big
    end subroutine trr_register2

    recursive subroutine trr_tr(r_first, r_last)
      integer(kind=cube_k), intent(in) :: r_first, r_last
      integer(kind=cube_k) :: r_mid
      if (r_last < r_first) return
      if (r_last == r_first) then
         call trr_register1(r_first)
      else
         r_mid = (r_first + r_last-1_cube_k) / 2_cube_k
         call trr_tr(r_first,r_mid)
         call trr_sq(r_first,r_mid, r_mid+1_cube_k,r_last)
         call trr_tr(r_mid+1_cube_k,r_last)
      endif
    end subroutine trr_tr

    recursive subroutine trr_sq(r_first, r_last, s_first, s_last)
      integer(kind=cube_k), intent(in) :: r_first, r_last, s_first, s_last
      integer(kind=cube_k) :: r_mid, s_mid
      if (r_last < r_first) return
      if (s_last < s_first) return
      if (r_first == r_last .and. s_first == s_last) then
         call trr_register2(r_first, s_first)
      else
         r_mid = (r_first + r_last-1_cube_k) / 2_cube_k
         s_mid = (s_first + s_last-1_cube_k) / 2_cube_k
         call trr_sq(r_first, r_mid, s_first, s_mid)
         call trr_sq(r_first, r_mid, s_mid+1_cube_k, s_last)
         call trr_sq(r_mid+1_cube_k, r_last, s_mid+1_cube_k, s_last)
         call trr_sq(r_mid+1_cube_k, r_last, s_first, s_mid)
      endif
    end subroutine trr_sq
  end subroutine fill_region_pools

  subroutine add_to_pool(pool, size, val)
    integer(kind=proc_k), dimension(:), intent(inout) :: pool
    integer(kind=proc_k), intent(inout) :: size
    integer(kind=proc_k), intent(in) :: val
    integer(kind=proc_k) :: p, a, b
    if (size == 0) then
       pool(1) = val
       size = 1
    else
       do p = size, 1_proc_k, -1_proc_k
          if (val >= pool(p)) then
             exit
          endif
       end do
       if (p == 0) then
          pool(2:size+1) = pool(1:size)
          pool(1) = val
          size = size+1
       elseif (val > pool(p)) then
          pool(p+2:size+1) = pool(p+1:size)
          pool(p+1) = val
          size = size + 1
       endif
    endif
  end subroutine add_to_pool
  subroutine add_to_catalog(catalog, size, val)
    integer(kind=cube_k), dimension(:), intent(inout) :: catalog
    integer(kind=cube_k), intent(inout) :: size
    integer(kind=cube_k), intent(in) :: val
    integer(kind=cube_k) :: p
    write(*,*)'adding to catalag',val,';',catalog(1:size)
    if (size == 0) then
       catalog(1) = val
       size = 1
    else
       do p = size, 1_proc_k, -1_proc_k
          if (val >= catalog(p)) then
             exit
          endif
       end do
       if (p == 0) p = 1
       if (val == catalog(p)) return
       ! Either p == 1 or val > catalog(p)
       if (val > catalog(p)) then
          catalog(p+2:size+1) = catalog(p+1:size)
          catalog(p+1) = val
          size = size + 1_cube_k
       else
          catalog(2:size+1) = catalog(1:size)
          catalog(1) = val
          size = size + 1_cube_k
       endif
    endif
  end subroutine add_to_catalog

  !> \brief Generates a mapping of \ref Realm "realms" to processes.
  subroutine fill_realm_map (realm_map, pools, pools_sizes,&
       n_regions, n_proc, my_proc, error)
    integer(kind=proc_k), dimension(:,:), intent(out) :: realm_map
    integer(kind=proc_k), dimension(:,:), intent(in) :: pools
    integer(kind=proc_k), dimension(:), intent(in) :: pools_sizes
    integer(kind=cube_k), intent(in) :: n_regions
    integer(kind=proc_k), intent(in) :: n_proc, my_proc
    type(lamb_error_type), intent(inout) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'fill_realm_map', &
         routineP = moduleN//':'//routineN

    integer :: error_handle
    integer(kind=cube_k), dimension(0:n_proc-1) :: proc_burdens
    integer(kind=cube_k) :: r1, r2
    integer(kind=proc_k) :: pool1_p, pool2_p, conj_p, least_burdened, pi
    integer, dimension(n_proc) :: conj_list
    integer(kind=cube_k) :: min_burden

    logical, parameter :: debug = .false.

    call lamb_error_set(error, error_handle, routineN)

    proc_burdens = 0_cube_k

    do r1 = 1_cube_k, n_regions
       do r2 = r1, n_regions
          if (debug) then
             write(*,*)'finding best proc for realm (', r1, "," ,r2, ').'
             write(*,*)pools(1:pools_sizes(r1),r1)
             write(*,*)pools(1:pools_sizes(r2),r2)
          endif
          conj_p = 0_proc_k
          pool1_p = 1
          pool2_p = 1
          do while (pool1_p <= pools_sizes(r1) .and. pool2_p <= pools_sizes(r2))
             if (pools(pool1_p,r1) == pools(pool2_p,r2)) then
                conj_p = conj_p + 1_proc_k
                conj_list(conj_p) = pools(pool1_p,r1)
                pool1_p = pool1_p + 1
                pool2_p = pool2_p + 1
             elseif (pools(pool1_p,r1) > pools(pool2_p,r2)) then
                pool2_p = pool2_p + 1
             else
                pool1_p = pool1_p + 1
             endif
          enddo
          call lamb_assert(int(conj_p,kind=int_big), "GE", int(1,int_big),&
               lamb_failure_level, lamb_internal_error,&
               "Can not find common processor of two regions.",&
               routineN, __LINE__, error=error)
          if (debug) then
             write(*,*)'common ',conj_list(1:conj_p)
             write(*,*)'burdens',proc_burdens(conj_list(1:conj_p))
          end if
          least_burdened = conj_list(conj_p)
          min_burden = proc_burdens(least_burdened)
          do pi = conj_p - 1_proc_k, 1, -1
             if (proc_burdens(conj_list(pi)) < min_burden) then
                min_burden = proc_burdens(conj_list(pi))
                least_burdened = conj_list(pi)
             end if
          enddo
          if (debug) &
               write(*,*)'chosen','?','->',least_burdened
          realm_map(r1, r2) = least_burdened
          realm_map(r2, r1) = least_burdened
          proc_burdens(least_burdened) = proc_burdens(least_burdened)+2_cube_k
       enddo
    enddo
    call lamb_error_stop(error, error_handle)
  end subroutine fill_realm_map

  !> \brief Generates a list of sectors local to this process.
  !>
  !> Given the \ref Realm "realm map", calculate this processes's sectors.
  subroutine fill_my_sectors (my_sectors, my_sectors_size,&
       my_catalog, my_catalog_size,&
       realm_map,&
       n_regions, n_proc, my_proc, error)
    integer(kind=subset_k), dimension(:,:), intent(out) :: my_sectors
    integer(kind=cube_k), intent(out) :: my_sectors_size
    integer(kind=proc_k), dimension(:,:), intent(in) :: realm_map
    integer(kind=cube_k), dimension(:), intent(in) :: my_catalog
    integer(kind=cube_k), intent(in) :: my_catalog_size
    integer(kind=cube_k), intent(in) :: n_regions
    integer(kind=proc_k), intent(in) :: n_proc, my_proc
    type(lamb_error_type), intent(inout) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'fill_my_sectors', &
         routineP = moduleN//':'//routineN

    integer :: error_handle
    integer(kind=cube_k) :: r, s
    integer(kind=cube_k) :: r_i, s_i

    CALL lamb_error_set (error, error_handle, routineN)
    do r_i = 1_cube_k, my_catalog_size
       r = my_catalog(r_i)
       do s_i = 1_cube_k, my_catalog_size
          s = my_catalog(s_i)
          if (realm_map(r, s) == my_proc) then
             my_sectors_size = my_sectors_size + 1_cube_k
             ! We have a 1:1 map between subsets and regions.
             my_sectors(1, my_sectors_size) = int(r,kind=subset_k)
             my_sectors(2, my_sectors_size) = int(s,kind=subset_k)
          endif
       enddo
    enddo
    call lamb_error_stop (error, error_handle)
  end subroutine fill_my_sectors

  !> \brief Generates a transfer schedule for matrix multiplication of
  !> two matrices sharing a plexus.
  subroutine lamb_plexus_create_sched(plexus, sched, error)
    TYPE(lamb_plexus_type), intent(in) :: plexus
    type(lamb_sched_type), intent(out) :: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_create_sched', &
         routineP = moduleN//':'//routineN

    integer :: error_handle
    logical :: debug = .false.

    integer :: sector_i
    integer :: k_region, m_region, n_region, k_region_i, m_region_i, n_region_i
    integer :: m_subset, k_subset, n_subset, m_subset_i, n_subset_i
    integer :: src_realm_a, src_realm_b
    integer, dimension(2) :: pair, sector_to_fetch
    integer, dimension(3) :: what
    integer :: era, action
    integer :: me

    CALL lamb_error_set (error, error_handle, routineN)
    if (debug) then
       write(*,*)routineN//' Calculating comm schedule'
    endif

    call lamb_sched_create(sched, 3, error)

    call lamb_assert(huge(int(0,kind=subset_k)) <= huge(int(0)),&
         lamb_warning_level, lamb_internal_error,&
         "Subset kind is larger than the default int size. Expect trouble!",&
         routineN, __LINE__, error)

    ! Find out what I have to receive. Go by regions. For each region
    ! region pair (m,k) and also (k,n), get the sectors (via subsets).

    ! Implies a 1:1 mapping between subsets and regions.
    call lamb_assert(plexus%n_regions, "EQ", plexus%n_subsets,&
         lamb_warning_level, lamb_internal_error, &
         "Number of regions must match number of subsets.",&
         routineN, __LINE__, error)
    me = lamb_mp_env_get_my_proc(plexus%mp_env)

    era = 0

    ! Straightforward but inefficient way of creating the schedule (no
    ! re-use, ...).

    do sector_i = 1, int(plexus%my_sectors_size)
       if (debug) &
            write(*,*)routineN//' sector_i', sector_i
       m_subset = plexus%my_sectors(1,sector_i)
       m_region = m_subset
       n_subset = plexus%my_sectors(2,sector_i)
       n_region = n_subset
       do k_subset = 1, plexus%n_subsets
          if (debug) &
               write(*,*)routineN//' k_subset',k_subset
          !k_region = int(plexus%my_catalog(k_region_i))
          ! Find realm, then process, for (m, k).
          k_region = k_subset
          src_realm_a = plexus%realm_map(m_region, k_region)
          src_realm_b = plexus%realm_map(k_region, n_region)
          era = era+1
          action = lamb_sched_action_get
          pair = (/ src_realm_a, me /)
          sector_to_fetch = (/ m_region, k_region /)
          what(2:3) = sector_to_fetch(:)
          what(1) = 1
          call lamb_sched_add_entry(sched, pair, action, what, era, error)
          pair = (/ src_realm_b, me /)
          sector_to_fetch = (/ k_region, n_region /)
          what(2:3) = sector_to_fetch(:)
          what(1) = 2
          call lamb_sched_add_entry(sched, pair, action, what, era, error)
       enddo
    enddo
    call lamb_sched_print(sched, error)

    CALL lamb_error_stop (error, error_handle)
  end subroutine lamb_plexus_create_sched
    
  !> \brief Generates a basic space and plexus (for testing).
  subroutine lamb_plexus_create_simple(plexus, mp_env, error)
    TYPE(lamb_plexus_type), pointerout :: plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_mp_env_type), pointerin :: mp_env

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_create_simple', &
         routineP = moduleN//':'//routineN

    type(lamb_space_type), pointer :: space
    type(lamb_partition_type) :: partition
    integer :: error_handle, i

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_space_new_unit_box (space, error)

    call lamb_plexus_calculate (plexus, space, mp_env, error)

    call lamb_space_release (space, error)

    CALL lamb_error_stop (error, error_handle)
  end subroutine lamb_plexus_create_simple


END MODULE lamb_plexus_operations
