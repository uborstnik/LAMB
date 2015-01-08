#!/usr/bin/env python

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


import sys
import re
import string
import os
import os.path

addAnyType = False
autodir = "autogen"
halfdir = "touchup"
filesep = "/" # fix this

try:
    templateFileName = sys.argv[1]
    templateFile = open(templateFileName, "r")
except Exception, e:
    print "Error parsing command line arguments" + str(e)
    print "Usage:\n\t"+sys.argv[0]+" template_file"
    sys.exit(1)

procedures = []
subroutines = []
functions = []
uses = []
types = []
kinds = None
names = {}

templateLines = []

#match_name = re.compile("^\s*(?!end)\s*(subroutine|function|type)\s+([a-z0-9_]+).*(?!ign)", re.IGNORECASE)
match_name = re.compile("^\s*(?!end)\s*(subroutine|function|type)\s+([a-z0-9_]+)[^!]*(?!!ignore)$", re.IGNORECASE)
#match_subroutine = re.compile("(use|subroutine|function|type)\s+([a-z_]+)", re.IGNORECASE)
#match_function = re.compile("function\s+([a-z_]+)", re.IGNORECASE)
#match_type = re.compile("type\s+([a-z_]+)", re.IGNORECASE)

for line in templateFile:
    templateLines += [line]
    if line[:5] == "!MAKE":
        kinds = line[6:].strip().split()
        print kinds
    # Now find subroutines, functions, and types.
    m = match_name.search(line)
    if (m):
        token = m.group(1).lower()
        print token, m.group(1), m.group(2)
        if token == "use":
            uses += [m.group(2)]
        if token == "type":
            types += [m.group(2)]
        if token == "subroutine" or token == "function":
            procedures += [m.group(2)]
        if token == "subroutine":
            subroutines += [m.group(2)]
        if token == "function":
            functions += [m.group(2)]
        names[m.group(2)] = m.group(1)
templateFile.close()

if not kinds:
    print "No type instantiators specified."
    sys.exit(1)

state = 0

templateFileBase=os.path.splitext(templateFileName)[0]
baseFileName = templateFileBase.replace("_X", "")
useFile = open(autodir + filesep + baseFileName + "." + "uses", "w")
for k in kinds:
    useFile.write("    use "+templateFileBase.replace("_X","_"+k)+"\n")
useFile.close()

interfaceFile = open(autodir + filesep + baseFileName + "." + "interfaces", "w")
publicFile = open(autodir + filesep + baseFileName + "." + "publics", "w")
#if addAnyType:
containsFile = open(autodir + filesep + baseFileName + "." + "contains", "w")
for proc in procedures:
    interfaceFile.write("\n      interface "+proc.replace("_X","")+"\n")
    interfaceFile.write("          module procedure")
    for k in kinds[0]:
        interfaceFile.write("&\n              "+proc.replace("_X", "_"+k))
    for k in kinds[1:]:
        interfaceFile.write(",&\n              "+proc.replace("_X", "_"+k))
    if addAnyType:
        interfaceFile.write("&\n              "+proc.replace("_X", "_"+"a"))
    interfaceFile.write("\n      end interface "+proc.replace("_X","")+"\n")
    publicFile.write("      PUBLIC :: " + proc.replace("_X","")+"\n")
for struct in types:
    for k in kinds:
        publicFile.write("      PUBLIC :: " + struct.replace("_X","_"+k)+"\n")
interfaceFile.close()
publicFile.close()
#if addAnyType:
containsFile.close()


#common_decl = "  character(len=*), parameter, private :: moduleN = '%s'\n"
#common_declFile = open(halfdir + filesep + baseFileName + "." + "common_decl.F90", "w")
#common_declFile.write(common_decl.replace("%s", baseFileName))
#common_declFile.write("  logical, parameter :: careful_mod = .FALSE.\n")
#common_declFile.close()

#common_contains = "  character(len=*), parameter, private :: moduleN = '%s'\n"
#common_containsFile = open(halfdir + filesep + baseFileName + "." + "common_contains.F90", "w")
#common_containsFile.write("! Code with no discernable types goes here.")
#common_containsFile.close()


mainFileTemplate = """module %s

#include "lamb_defs.h"

#include "%s.uses"

  implicit none

  private

  character(len=*), parameter, private :: moduleN = '%s'\n

#include "%s.publics"

#include "%s.interfaces"


  contains

#include "%s.contains"

end module %s
"""

# If separate files: Included
##      "#include "%s.common_decl.F90""


if os.path.isfile(halfdir + filesep + baseFileName + ".F90"):
	mainFile = open(halfdir + filesep + baseFileName + "." + "main.auto", "w")
else:
	mainFile = open(halfdir + filesep + baseFileName + ".F90", "w")
mainFile.write(mainFileTemplate.replace("%s", baseFileName))
mainFile.close()
