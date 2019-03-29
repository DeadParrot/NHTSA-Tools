#!/usr/bin/env python

# Dependency Generator for C/C++ and Fortran
#
# Language: Python (2.7 or 3.x)
#
# Version: 1.0.0
#
# Copyright (c) 2000-2018 Objexx Engineering, Inc. All Rights Reserved.

# Notes:
# . Dependency files for use with GNU make
# . Only processes files with supported source C/C++ extensions
# . Leaves specified paths on prerequisites but does not add path where found
# . Adds a line with all prerequisites as targets to avoid make error if one is removed or renamed
# . Include search path defaults to environment variables:
#   . C/C++: CPATH then CPLUS_INCLUDE_PATH then INCLUDE
#   . Fortran: CPATH then CPLUS_INCLUDE_PATH then INCLUDE then FINCLUDE
# . Use --no-mod for compilers like GFortran that don't regenerate unchanged .mod files

# Imports
import argparse, os, re, sys

# Globals
C_ext = re.compile( '(c|cc|cpp|cxx|c\+\+|h|hh|hpp|hxx|h\+\+|ii|ipp|ixx|i\+\+)', re.I )
C_inc = re.compile( '\s*#\s*include +<([^<>]+\.[^<>]+)>' ) # Exclude <name> C++ Standard Library headers
C_inq = re.compile( '\s*#\s*include +"([^"]+)"' )
C_sys = re.compile( 'sys/' ) # Exclude <sys/header> C/C++ system headers
C_path = None
F_ext = re.compile( '(f|for|f90|f9x|f95|f03|f08|fh|fi|inc)', re.I )
F_inc = re.compile( "\s*INCLUDE +'([^']+)'", re.I )
F_mod = re.compile( "\s*MODULE +([^ \t\n]+)", re.I )
F_use = re.compile( "\s*USE +([^ ,\t\n]+)", re.I )
F_sys = [ 'ISO_C_BINDING', 'ISO_FORTRAN_ENV', 'IEEE_EXCEPTIONS', 'IEEE_ARITHMETIC', 'IEEE_FEATURES', 'OMP_LIB', 'OMP_LIB_KINDS', 'OPENACC', 'HDF5', 'H5GLOBAL' ]
F_path = None

# Main
def main():

    # Get options and arguments
    parser = argparse.ArgumentParser( description = 'Generates C/C++ and Fortran dependency files' )
    parser.add_argument( '--inc', help = 'include path or env var [CPATH|CPLUS_INCLUDE_PATH|INCLUDE|FINCLUDE]' )
    parser.add_argument( '--ext', help = 'object file extension [obj|o]', default = ( 'obj' if os.name == 'nt' else 'o' ) )
    parser.add_argument( '--no-mod', dest = 'no_mod', help = 'Suppress mod file targets', action = 'store_true', default = False )
    parser.add_argument( 'source', help = 'Source file' )
    arg = parser.parse_args()

    # Set up include search paths
    global C_path
    if not arg.inc:
        if 'CPATH' in os.environ:
            C_path = os.environ[ 'CPATH' ]
        elif 'CPLUS_INCLUDE_PATH' in os.environ:
            C_path = os.environ[ 'CPLUS_INCLUDE_PATH' ]
        elif 'INCLUDE' in os.environ:
            C_path = os.environ[ 'INCLUDE' ]
    elif arg.inc in os.environ: # Treat arg as env var name
        C_path = os.environ[ arg.inc ]
    else: # Treat arg as path
        C_path = arg.inc
    C_path = C_path.split( os.pathsep ) if C_path else None
    global F_path
    if not arg.inc:
        if 'CPATH' in os.environ:
            F_path = os.environ[ 'CPATH' ]
        elif 'CPLUS_INCLUDE_PATH' in os.environ:
            F_path = os.environ[ 'CPLUS_INCLUDE_PATH' ]
        elif 'INCLUDE' in os.environ:
            F_path = os.environ[ 'INCLUDE' ]
        elif 'FINCLUDE' in os.environ:
            F_path = os.environ[ 'FINCLUDE' ]
    elif arg.inc in os.environ: # Treat arg as env var name
        F_path = os.environ[ arg.inc ]
    else: # Treat arg as path
        F_path = arg.inc
    F_path = F_path.split( os.pathsep ) if F_path else None

    # Parse source file name
    if not os.path.isfile( arg.source ):
        raise IOError( 'Source file not found: ' + str( arg.source ) )
    src_name = os.path.basename( arg.source )
    src_base, src_ext = os.path.splitext( src_name )
    dep_name = src_base + '.d'
    obj_name = src_base + '.' + arg.ext
    if src_ext: src_ext = src_ext[ 1: ]
    if not ( src_base and src_ext and ( C_ext.match( src_ext ) or F_ext.match( src_ext ) ) ):
        raise ValueError( 'Not a recognized C/C++ or Fortran file name extension: ' + str( src_ext ) )

    # Build dependencies
    tar = obj_name + ' ' + dep_name
    dep = [ src_name ]
    if C_ext.match( src_ext ):
        dep_list = list( C_deps( os.path.abspath( arg.source ), add = False ) )
        mod_list = None
        use_list = None
    elif F_ext.match( src_ext ):
        dep_list, mod_list, use_list = F_deps( os.path.abspath( arg.source ), add = False )
        dep_list = list( dep_list )
        mod_list = sorted( mod_list )
        use_list = list( use_list )
        use_list.sort()
    else:
        assert False, 'Not a recognized C/C++ or Fortran file name extension: ' + str( src_ext )
    dep_list.sort()
    dep.extend( dep_list )
    dep_str = ' '.join( dep )
    if mod_list and ( not arg.no_mod ):
        for mod in mod_list:
            tar += ' ' + mod + '.mod' # Assumes lowercase module naming
    if use_list:
        for use in use_list:
            dep_str += ' ' + use + '.mod' # Assumes lowercase module naming

    # Write dependency file
    dep_file = open( dep_name, mode = 'w' )
    dep_file.write( tar + ' : ' + dep_str + '\n' )
    dep_file.write( dep_str + ' :\n' )
    if mod_list: # Add rules for building .mod files: Implicit rules don't work because .mod file names != source file names
        if arg.no_mod:
            for mod in mod_list:
                dep_file.write( mod + '.mod : ' + obj_name + '\n' )
                dep_file.write( '\t@true\n' )
        else:
            for mod in mod_list:
                dep_file.write( mod + '.mod : ' + src_name + '\n' )
                if os.name == 'nt':
                    dep_file.write( '\t$(FC) $(FFLAGS) -c $(subst /,\,$<)\n' )
                else:
                    dep_file.write( '\t$(FC) $(FFLAGS) -c $<\n' )
    dep_file.close()

def C_deps( fname, fdeps = None, par_dir = None, quoted = False, add = True ):
    '''C/C++ dependencies of a file: Recursive'''
    if fdeps is None: fdeps = set()
    if par_dir is None: par_dir = []
    try:
        gname = None
        if ( not os.path.isabs( fname ) ) and ( not os.path.isfile( fname ) ): # Search include path list
            if quoted: # Check relative to parent directories
                for adir in par_dir:
                    tname = os.path.join( adir, fname )
                    if os.path.isfile( tname ):
                        if not ' ' in tname: gname = tname # Skip dependencies in paths with spaces for GNU Make compatibility
                        break
            if not gname: # Use include search path
                for adir in C_path:
                    tname = os.path.join( adir, fname )
                    if os.path.isfile( tname ):
                        if not ' ' in tname: gname = tname # Skip dependencies in paths with spaces for GNU Make compatibility
                        break
        if gname is None: gname = fname
        if sys.version_info >= ( 3, 0 ):
            dfile = open( gname, mode = 'r', newline = None )
        else:
            dfile = open( gname, mode = 'rU' )
        if add: fdeps.add( fname ) # Only add dependency if it is found
        par_new = os.path.dirname( os.path.abspath( gname ) )
        if ( not par_dir ) or ( par_dir[ 0 ] != par_new ): # Push parent dir onto front of list
            par_dir.insert( 0, par_new )
        else: # Flag not to pop since didn't push
            par_new = None
        for line in dfile:
            m = C_inc.match( line ) # #include <header> form
            if m :
                dep = m.group( 1 )
                if dep and ( dep not in fdeps ) and ( not C_sys.match( dep ) ):
                    C_deps( dep, fdeps, par_dir ) # Recurse
            else:
                m = C_inq.match( line ) # #include "header" form
                if m:
                    dep = m.group( 1 )
                    if dep and ( dep not in fdeps ) and ( not C_sys.match( dep ) ):
                        C_deps( dep, fdeps, par_dir, quoted = True ) # Recurse
        dfile.close()
        if par_new: del par_dir[ 0 ] # Pop parent dir from front of list
    except:
        pass # Skip missing file
    return fdeps

def F_deps( fname, fdeps = None, fmods = None, fuses = None, par_dir = None, quoted = False, add = True ):
    '''Fortran dependencies of a file: Recursive'''
    if fdeps is None: fdeps = set()
    if fmods is None: fmods = set()
    if fuses is None: fuses = set()
    if par_dir is None: par_dir = []
    try:
        gname = None
        if ( not os.path.isabs( fname ) ) and ( not os.path.isfile( fname ) ): # Search include path list
            if quoted: # Check relative to parent directories
                for adir in par_dir:
                    tname = os.path.join( adir, fname )
                    if os.path.isfile( tname ):
                        if not ' ' in tname: gname = tname # Skip dependencies in paths with spaces for GNU Make compatibility
                        break
            if not gname: # Use include search path
                for adir in F_path:
                    tname = os.path.join( adir, fname )
                    if os.path.isfile( tname ):
                        if not ' ' in tname: gname = tname # Skip dependencies in paths with spaces for GNU Make compatibility
                        break
        if gname is None: gname = fname
        if sys.version_info >= ( 3, 0 ):
            dfile = open( gname, mode = 'r', newline = None )
        else:
            dfile = open( gname, mode = 'rU' )
        if add: fdeps.add( fname ) # Only add dependency if it is found
        par_new = os.path.dirname( os.path.abspath( gname ) )
        if ( not par_dir ) or ( par_dir[ 0 ] != par_new ): # Push parent dir onto front of list
            par_dir.insert( 0, par_new )
        else: # Flag not to pop since didn't push
            par_new = None
        for line in dfile:
            m = F_inc.match( line ) # INCLUDE 'header'
            if m:
                dep = m.group( 1 )
                if dep and ( dep not in fdeps ):
                    F_deps( dep, fdeps, fmods, fuses, par_dir ) # Recurse
            else:
                m = F_mod.match( line ) # MODULE modname
                if m:
                    mod = m.group( 1 ).lower()
                    if mod and ( mod not in fmods ) and ( mod != 'procedure' ):
                        fmods.add( mod )
                else:
                    m = F_use.match( line ) # USE modname
                    if m:
                        mod = m.group( 1 ).lower()
                        if mod.upper() not in F_sys:
                            if mod and ( mod not in fuses ):
                                fuses.add( mod )
        dfile.close()
        if par_new: del par_dir[ 0 ] # Pop parent dir from front of list
    except:
        pass # Skip missing file
    return fdeps, fmods, fuses

# Runner
if __name__ == '__main__':
    main()
