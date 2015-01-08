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

!> \brief Methods for the relation class.

MODULE lamb_relation_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_relation_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_relation_has_symmetry

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'relation_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  PURE FUNCTION lamb_relation_has_symmetry (relation) result (has_symmetry)
    TYPE(lamb_relation_type), INTENT(IN):: relation
    logical :: has_symmetry
    has_symmetry = relation%matrix_type == lamb_rel_symmetric .or. &
         relation%matrix_type == lamb_rel_antisymmetric .or. &
         relation%matrix_type == lamb_rel_hermitian .or. &
         relation%matrix_type == lamb_rel_antihermitian
  END FUNCTION lamb_relation_has_symmetry

END MODULE lamb_relation_methods
