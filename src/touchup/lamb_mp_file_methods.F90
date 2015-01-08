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
module lamb_mp_file_methods

#include "lamb_defs.h"

#include "lamb_mp_file_methods.uses"

  use lamb_mp_file_types
  use lamb_kinds
  use lamb_error
  use lamb_mpi_kinds
  use lamb_mpi_util

  implicit none

#include "mpif.h"

  private

  public :: lamb_mp_file_seek

#include "lamb_mp_file_methods.publics"

#include "lamb_mp_file_methods.interfaces"

  character(len=*), parameter, private :: moduleN = 'lamb_mp_file_methods'

  logical, parameter :: careful_mod = .FALSE.

  contains

    subroutine lamb_mp_file_seek (mp_file, position, absolute, from_end, error)
      type(lamb_mp_file_type), intent(inout) :: mp_file
      integer(kind=int_big) :: position
      logical, intent(in), optional :: absolute, from_end
      type(lamb_error_type), intent(inout) :: error
      CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_file_seek', &
           routineP = moduleN//':'//routineN
      INTEGER :: error_handle, stat, prev_level
      integer(kind=the_mpi_integer_kind) :: ierr, whence
      logical :: my_a, my_fe

      call lamb_error_set (error, error_handle, routineN)
      call lamb_error_make_ok(error, prev_level)

      if (present (absolute)) then
         my_a = absolute
      else
         my_a = .true.
      endif
      if (present (from_end)) then
         my_fe = from_end
      else
         my_fe = .false.
      endif
      if (my_a) then ! absolute
         if (.not. my_fe) then ! from beginning
            whence = MPI_SEEK_SET
         else
            whence = MPI_SEEK_END
         endif
      else ! relative
         whence = MPI_SEEK_CUR
      endif

      call MPI_File_seek (mp_file%handle, int(position, kind=MPI_OFFSET_KIND),&
           whence, ierr)
      call lamb_mpi_check_error(ierr, HERE, lamb_failure_level, error,&
           "Error seeking in file.")
    call lamb_error_update_level (error, prev_level)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_mp_file_seek


#include "lamb_mp_file_methods.contains"

end module lamb_mp_file_methods
