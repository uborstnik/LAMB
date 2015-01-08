#!/bin/bash
#******************************************************************************
#
# This source code is part of the LAMB library.
#
# Written by Urban Borstnik.
#
# Inspired by CP2K and DBCSR.
#
# Copyright (C) 2012, 2013, 2014 Urban Borstnik.
#
# The LAMB library is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 2 of the
# License, or (at your option) any later version.
#
# LAMB is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LAMB.  If not, see <http://www.gnu.org/licenses/>.
#
# If you want to redistribute modifications, please note that derived
# work must not be called LAMB, Lamb, lamb, libLAMB, libLamb, liblamb
# nor any other case variation.  Details are found in the README &
# COPYING files.  If they are missing, get the official version at the
# http://lamb.borstnik.net/ website.
#
# We ask you to cite the published articles on this software.  You can
# find a list in the README file in the main directory.
#
#******************************************************************************

#set -e
#set -o pipefail

files="${1:-*.F90 autogen/*.F90 autogen/*.uses touchup/*.F90}"

finddep () {
    deps=""
    ds=` grep -i "USE " $1 | cut -d , -f 1`
    for d in $ds; do
        if [[ -e "autogen/${d}.F90" ]]; then
            deps="$deps ${d}.F90"
        elif [[ -e "touchup/${d}.F90" ]]; then
            deps="$deps ${d}.F90"
        elif [[ "$d" =~ "lamb" || -e "${d}.F90" ]]; then
            deps="$deps ${d}.F90"
        fi
    done
    ds=` grep '^#include "lamb' $1 | cut -d \" -f 2`
    for d in $ds; do
	deps="$deps ${d}"
    done
};

normname () {
	name=`basename ${1} | sed -e s/uses/F90/g`
}

for f in $files; do
    [ -e "$f" ] || { echo File "$f" does not exist. >&2; exit 1; }
    finddep $f
    normname ${f}
    newtgts="`echo $name | sed -e 's/\.F90/.o/g'` `echo $name | sed -e 's/\.F90/.mod/g'`"
    newdeps="`echo $deps | sed -e 's/\.F90/.mod/g'`"
    echo ${newtgts}: ${newdeps}
done
