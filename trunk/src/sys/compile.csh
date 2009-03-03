#!/bin/csh
##set echo
#+
#
#  compile.csh
#     CMV 930525  Created
#     CMV 931104  Added update of hypertext for psc/pef/pin
#     CMV 931104  Added Dec WS patch
#     CMV 931116  .x?? and .a?? may now be compressed (not for .?vx)
#     CMV 931123  Make n_ulib current dir when searching for local objects
#     CMV 931201  Use different name for temporary main routine and delete
#     CMV 931223  Different handling of libraries
#     CMV 940221  Include version numbers in executables
#     CMV 940228  Work around module multiply.o in test for multiply defined
#     HjV 940314  Use environment ARR (=ar crv  or  ar crlv)
#     CMV 940323  Better tests for moving files in $n_exe
#     HjV 940516  Typo
#     WNB 940531  Make vx dependencies
#     WNB 940624  Bypass .cun, .fun on VAX
#     CMV 940705  Add types .pls, .tbl, .cap, .fig (all ignore)
#     CMV 940705  Copy .exe/.ppd to various NFRA machines
#     CMV 940719  Handle new documentation stuff (depending on n_doc/n_hlp)
#     HjV 940803  Make sure we have rsh
#     CMV 940804  Correct copy command for NFRA HP's
#     CMV 940805  Add "getarg" for VAX
#     CMV 940812  Change docaid to genaid
#     HjV 940902  Move 'old' .exe/.ppd to .old on various NFRA machines
#     HjV 941020  Better test for .exe/.ppd update on various NFRA machines
#     CMV 941102  Bitmaps have extension .bbm in source tree
#     HjV 941122  Fix problem with Ptolemeus (RUG)
#     CMV 950116  Option not to keep old executables and ppd-files
#     JPH 950124  Remove comment-only lines in .pin file before compiling
#		   (bldppd does not eliminate them from HELP texts)
#     HjV 950130  Also copy .exe files (build from .c files) to other NFRA HP's
#     HjV 950424  Add rzmws4
#     HjV 960423  Bitmaps have now extension .xbm in source tree
#     HjV 960618  Add some stuff for Solaris; rzmws6 removed, add daw16
#     JPH 960726  Add possibility of selecting $n_lib/.o files through an 
#		  object list
#     HjV 970424  Remove rzmws4
#     HjV 970728  Remove rzmws7
#     HjV 970728  Remove rzmws5
#     JPH 981113  Bug fix in setting of L_Obj  
#     AXC 040127  Removes IGETARG iso GETARG exception for HP
#     WNB 070831  Replace termcap ncurses
#     WNB 090303  Change 'tail +<num>' into 'tail -n +<num>'
#
#  compile.csh  Process all files specified in Input_file
#
#  compile will be called by both update and shadow and assumes
#  that initcompile.csh has been sourced by one of these.
#
#  compile will use at least some of the following:
#
#    General setup and things defined in initcompile.csh:
#
#      n_root n_src ...   The Newstar directory structure
#      FC FFLAGS ...      The variables defined in $n_arch.def
#      C_Date, C_Time     Current date and time
#
#    Parameters and aliases passed by update or shadow:
#
#      Input_file         List of fully defined files to be compiled
#      _Objectlib         Name of object library for storage
#      _Textlib           Name of text library for storage
#      _....              Various options set in switches.csh
#
#      log "String"       Alias to write string to screen and/or log-file
#
#  
#  The files in Input_file should not contain any wildcards.
#
#  Objectlib is required for work in the Master system and optional
#   in a shadow system. Object modules will be made in $n_work. For the
#   master system, they will at some stage be moved into the library.
#   For the shadow system, they are moved into $n_ulib
#
#  Text_lib is optional. If it is defined, source modules will be moved
#    into a text library.
#  
#
#  For a list of valid options, refer to switches.csh.
#
#  The following variables are modified by compile (using set):
#
#       Errors         Total number of errors
#
#  All error-files, listings, object-files etc are made in $n_work
#
#      Object_file     An object module produced by some compiler
#                       Common treatment includes checks for errors,
#                       inclusion in object libraries etc.
#      List_file       Some listing output, may be printed (-Print)
#                       or deleted (-NList).
#      Error_file      Any error output, may be shown (-Errors)
#
#  To move to an archive, just call with Input_file set to the archivename.
#
#  
#-
#

unset Abort_flag
onintr Abort_exit

#
#  Check some rather important settings
#
if (! $?n_root || ! $?Input_file || ! $?FC || ! $?_Debug ) then
   echo " "
   echo "$0 invoked in an illegitimate way, use either shadow or update"
   echo " "
   exit
endif

if (! $?RSH ) then                             # Make sure we have rsh
   setenv RSH \rsh
   if ($n_arch == hp) setenv RSH \remsh
endif

set Local_HP=""			# Local HP's 

set Home=$cwd; 
if (! $?Errors) @ Errors = 0

#
# Process all files in Input_file
#
while ( "$Input_file" != "")

   set File=$Input_file[1]
   set Input_file[1]=""
   set Type=ignore
   log " "

#
# If not selected, easy job
#
   if (! $_Select) then
      log "---- File $File not selected..."
#
# We want to handle only files of type name.ext 
#
   else if ( $File:t !~ *.* ) then
     log "----- Warning: Invalid filename, ignored $File"
#
# Exe files are a bit special in that they do not yet exist
#
   else if ($File:e == exe) then
     set Type=exe
#
# Object libraries and text libraries are also a bit special
#
   else if ($File:e == olb || $File:e == tlb) then
     set Flag=$n_work/${File:t}.list
     if (-e $Flag && ! -z $Flag) then
        log "Updating archive $File..." 
        if (! $?ARR) setenv ARR "ar crv"
#
# Make sure we pass at most 500 characters to the archive command
#
        set c_nline = `cat $Flag | wc -l`   # Count lines
        @ c_iline = 0
        set Object_file=""
        while ($c_iline < $c_nline)              # Lines left?
          @ c_iline = $c_iline + 1
          set Object_file=($Object_file `tail -n +$c_iline $Flag | head -1`)
          if (`echo $Object_file | wc -c ` > 500) then
             log `$ARR $File $Object_file`
             'rm' -f $Object_file
             set Object_file=""
          endif
        end
        if ("$Object_file" != "") then
           log `$ARR $File $Object_file`
           'rm' -f $Object_file
           set Object_file=""
        endif

        'rm' -f $Flag
        ranlib $File
     endif
#
# Error if file does not exist
#
   else if (! -e $File) then
     log "***** Error: $File does not exist..."
     @ Errors = $Errors + 1
#
# Skip softlinks unless -Softlink set
#
   else if (! $_Softlink && "`ls -F $File `" =~ *@ ) then
     log "Ignoring softlink $File"
   else 
#
#  Give the file a full path specification 
#
     set noglob
     if ($File:h != $File) cd $File:h
     set File=$cwd/$File:t
     cd $Home
     unset noglob
     set Type=$File:e
   endif

   if ($Type != ignore) then
     set Tail=$File:t        # Name+Extension without directory
     set Name=$Tail:r        # Name only

     set Object_file = $n_work/$Name.o       
     set List_file   = $n_work/$Name.lis
     set Error_file  = $n_work/$Name.err

#     if (-e $Object_file) 'rm' -f $Object_file   # Uncomment when .fvx etc gone?
     if (-e $List_file) then
        'rm' -f $List_file
     endif
     if (-e $Error_file) then
         'rm' -f $Error_file
     endif
     log "----- Working on $File "
   endif

#
#  Next file if any of the above errors occured
#
   if ($Type == ignore) then

#
# GRP   Group files: no special action
# IDX   Index file/database
# TEX   Tex files: translate using ndoc full
# CAP   Figure captions: idem
# TBL   Tables: idem
# FIG   Figures: idem
#
   else if ("$Type" == "grp" || "$Type" == "idx" || \
            "$Type" == "kwa" || "$Type" == "tex" || \
            "$Type" == "cap" || "$Type" == "tbl" || \
            "$Type" == "fig"  )  then
     if ("$Tail" == "version.idx") then
        set tmp=(`head -1 $File`)
        if ($#tmp > 2) then
           if ("$tmp[3]" != "") set C_Version=$tmp[3]
        endif
        echo "%%%%% Version: $C_Version"
     else
        log "No action on $Tail"
     endif
#
# GIF GFS PS Images
#
   else if ("$Type" == "gif" || "$Type" == "gfs" || \
            "$Type" == "xbm" || "$Type" == "ps") then
     if ($_Update) then
       if ($File =~ *icons/$Tail) then
          if (! -d $n_hlp/icons) mkdir $n_hlp/icons
          if ("$Type" == "xbm") then
            cp $File $n_hlp/icons/$Name.xbm
          else
            cp $File $n_hlp/icons/$Tail
          endif
       else
          cp $File $n_hlp/$Tail
       endif
       echo "Moved into "\$n_hlp
     endif
#
# TXT HLP HTML Help text etc
#
   else if ("$Type" == "txt" || "$Type" == "hlp" || "$Type" == "html") then

     if ("$Type" == "hlp") then               # xmosaic doesnot know .hlp
        set Flag=$Name.txt
     else
        set Flag=$Tail
     endif

     if ($_Update) then
       if ("$Tail" == "homepage.html") then      # encode site name     
          sed -e s/N_SITE/$n_site/ $File >$n_hlp/$Flag
       else
          cp $File $n_hlp/$Flag
       endif
       echo "Moved into "\$n_hlp
     endif
#
# HUN  Library with html files
#
   else if ("$Type" == "hun") then
     if ($n_site != nfra) then
       if (! ("$n_site" == "rug" && "$n_arch" == "sw")) then
	 rm -fR $n_hlp
	 mkdir $n_hlp
         cp $File $n_hlp/$Name.hlb.Z
         cd $n_hlp
         uncompress $Name.hlb
         if (! -e $Name.hlb) mv $Name.hlb.Z $Name.hlb
         tar xvf $Name.hlb | grep -v "^x"
         rm $Name.hlb
         echo "Library unpacked."
       endif
     endif
#
# SCN  Scanfiles
# WMP  Mapfiles
# MDL  Modelfiles
#
   else if ("$Type" == "scn" || "$Type" == "wmp" || "$Type" == "mdl" || \
            "$Type" == "ngf" || "$Type" == "flf" ) then
     chmod a+r $File
     log "Datafile: $Tail"
#
# SSC   Combined command files/shell scripts: split out
#
   else if ("$Type" == "ssc") then
     foreach out ( sun com )
       log "Creating $Name.$out from $Tail"
       if ("$n_arch" == "vx") then
	 set Flag="wn_vax__"
       else
         set Flag="wn_un__"
       endif
       if ($out == com) set Flag="wn_vax__"
       set Flag="($Flag)|(wn_${n_arch}__)|(wn_${n_site}__)"
       if (-e ${File:r}.$out) then
         'rm' -f ${File:r}.$out
       endif
       cat $File | awk '\
BEGIN { FS = " "; lvl=1; on[1]=1; } \
/^.*# *ifdef/ { \
  lvl++; \
  if ($2 ~ /'$Flag'/) {on[lvl]=1;} else {on[lvl]=0;} \
  next } \
/^.*# *ifndef/ { \
  lvl++; \
  if ($2 ~ /'$Flag'/) {on[lvl]=0;} else {on[lvl]=1;} \
  next } \
/^.*# *else/   { on[lvl]=1-on[lvl]; next } \
/^.*# *endif/  { lvl--;             next } \
  { ok=1; for (i=1; i<=lvl; i++) { if (on[i]==0) ok=0; }; \
    if ("'$out'" == "com" && lvl == 1) printf "$\!";      \
    if (ok != 0) print }'  >${File:r}.$out

       if (! -e ${File:r}.$out) then
          @ Error = $Error + 1
          log "Error creating ${File:r}.$out..."
       else
          chmod a+x ${File:r}.$out
       endif
     end
#
# CSH PLS   Shell scripts: make them executable
#
   else if ("$Type" == "csh" || "$Type" == "pls") then
     if ( -o $File) then
        log "chmod a+x $Tail"
        chmod a+x $File
     else
        log "Ignore: not owner of $File"
     endif
#
# COM   Command files: no special action
#
   else if ("$Type" == "com") then
     log "No action on $Tail"

#
# INC   Include files (c)
# DEF   Include files (fortran)
#
  else if ("$Type" == "inc" || "$Type" == "def" || "$Type" == "dsf") then

     if ("$Type" == "def" || "$Type" == "dsf") then
        set Flag=`echo $Tail | tr '[a-z].' '[A-Z]_'`
     else
        set Flag=${Name}_$Type
     endif

     if (-e $n_uinc/$Flag ) then
        'rm' -f $n_uinc/$Flag
     endif

#     cp $File $n_uinc/$Flag
     awk -f $n_src/sys/data_splitter.kwa $File >& $n_uinc/$Flag

     if (! -e $n_uinc/$Flag ) then
        log "***** Error: Could not copy $Tail into $n_uinc"
        @ Errors = $Errors + 1
     else
        log "Copied $Tail into $n_uinc"
     endif

#
# A??   Special object libraries: copy to $n_lib and uncompress
#
  else if ("$Type" =~ a?? ) then
     if ("$Type" != "a$n_arch") then
        log "Ignoring archive $Tail for $n_arch"
     else
        cp $File $n_lib/$Name.olb
        mv $n_lib/$Name.olb $n_lib/$Name.olb.Z
        if (-e $n_lib/$Name.olb.Z) then
           log `uncompress $n_lib/$Name.olb`
           if (-e $n_lib/$Name.olb.Z) then
              mv $n_lib/$Name.olb.Z $n_lib/$Name.olb
           endif
        endif
        if (-e $n_lib/$Name.olb) then
           ranlib $n_lib/$Name.olb
           log "Installed library "\$n_lib/$Name.olb
        else
           log "***** Error: could not copy $Tail into "\$n_lib
           @ Errors = $Errors + 1
        endif
     endif

#
# X??   Special executable: copy to $n_exe (no use to have them in $n_uexe)
#
  else if ("$Type" =~ x??) then
     if ("$Type" != "x$n_arch") then
        log "Ignoring executable $Tail for $n_arch"
     else
        cp $File $n_exe/$Name.exe
        mv $n_exe/$Name.exe $n_exe/$Name.exe.Z
        if (-e $n_exe/$Name.exe.Z) then
           log `uncompress $n_exe/$Name.exe`
           if (-e $n_exe/$Name.exe.Z) then
              mv $n_exe/$Name.exe.Z $n_exe/$Name.exe
           endif
        endif
        if (-e $n_exe/$Name.exe) then
           chmod a+x $n_exe/$Name.exe
           log "Installed executable "\$n_exe"/$Name.exe (chmod a+x)"
#
# NFRA has local executables on several HP machines
#
           if ($n_site == "nfra" && $n_arch == "hp") then
             cp $n_exe/$Name.exe $n_root/exe/hp
             echo "Copied to master exe-directory"
             foreach tmphost ( $Local_HP )
               if ($HOST != $tmphost) then
		 set Remok=`$RSH $tmphost echo \$n_exe`
		 if ($Remok != "" && $Remok != $n_root/exe/hp) then
		   $RSH $tmphost ' mv '\$n_exe/$Name.exe \$n_exe/$Name.exe.old' ; \
		   cp '$n_root/exe/hp/$Name.exe \$n_exe' '
		   echo "Copied to "\$n_exe" on $tmphost"
		 else
		   if ($Remok == "") then
		     echo "$tmphost heeft probleem: is waarschijnlijk down"
		   else
		     echo "$tmphost heeft probleem: n_exe is $Remok (Bestaat execute.exe wel ??)"
		   endif
		 endif
               endif
             end
           endif           
        else
           log "***** Error: could not copy $Tail into "\$n_exe
           @ Errors = $Errors + 1
        endif
     endif

#
# PEF   PIN-Include files: make a link in $n_uinc and check it
#
  else if ("$Type" == "pef") then

     if (-e $n_uinc/$Tail ) then
        'rm' -f $n_uinc/$Tail
     endif
     cp $File $n_uinc
     if (! -e $n_uinc/$Tail ) then
        log "***** Error: Could not link $Tail into "\$n_uinc
        @ Errors = $Errors + 1
     else
        log "Copied $Tail into "\$n_uinc
        if ($_Update && -o $n_hlp && -o $n_src) $n_exe/genaid.exe keys $File
     endif
#
# PIN files: compile to ppd
#
#  It would be better to update dwarf to include pef-files and make listings 
#  and other things in current dir, for the while we work around
#
   else if ("$Type" == "pin" || "$Type" == "psc") then
#
# Check if we have a compiler
#
     if (! -e $n_uexe/sys_bldppd.exe && ! -e $n_exe/sys_bldppd.exe) then
        log "***** Error: ppd-compiler (sys_bldppd) does not exist"
        @ Errors = $Errors + 1

     else
#
# Do all the work in $n_work
#
        cd $n_work
#
# Does the file contain include commands?
#
        onintr ppd_done                     # To remove temp. copy
        set Flag=`grep '^INCLUDE=' $File`
        if ("$Flag" != "") then
           $n_exe/genaid.exe psc $File >$Name.tmp
        else
           cp $File $Name.tmp               # Make temp copy in $n_uinc
        endif
	sed -e '/^\!/d' < $Name.tmp >! $Name.pin
	rm $Name.tmp

        if (! -e global.ppd) touch global.ppd
        if (! -e gen.ppd)    touch gen.ppd
        if (! -e ngen.ppd)   touch ngen.ppd

        log "sys_bldppd.exe $Name /list"
        if (-e $n_uexe/sys_bldppd.exe) then
           $n_uexe/sys_bldppd.exe $Name /list  >! $Error_file
           @ Flag = $status
        else
           $n_exe/sys_bldppd.exe  $Name /list  >! $Error_file
           @ Flag = $status
        endif

ppd_done:                                          # Interrupt handler
        'rm' -f $Name.pin                          # Remove temp. copy

        if (-e $Name.lis && $n_work/$Name.lis != $List_file) mv $Name.lis $List_file
        if (-z global.ppd) then
           'rm' -f global.ppd
        endif
        if (-z gen.ppd) then
           'rm' -f gen.ppd
        endif
        if (-z ngen.ppd) then
           'rm' -f ngen.ppd
        endif
        if (-e ppd.ref) then
           'rm' -f ppd.ref
        endif

        onintr Abort_exit                    # Original interrupt handler

#        if ( ($Flag != 9 && $Flag != 97 && $Flag != 1) || \
        if ( \
             ! -e $Name.ppd || ! -e $List_file) then
           if (-e $Name.ppd) then
              'rm' -f $Name.ppd
           endif
           echo "***** Error producing ppd-file: $Flag" >> $Error_file 
           @ Errors = $Errors + 1
        else
           cat $Error_file >>$List_file
          'rm' -f $Error_file

           if ($n_uexe != $n_work && -e $n_uexe/$Name.ppd) then
              mv  $n_uexe/$Name.ppd $n_uexe/$Name.ppd.old
              if (! $_Keep) then
                 'rm' -f $n_uexe/$Name.ppd.old
              endif 
           endif

           if ($n_uexe == $n_exe || $_Update) then
              if ($n_exe != $n_work) then
                 if (-e $n_exe/$Name.ppd) then
                    mv $n_exe/$Name.ppd $n_exe/$Name.ppd.old
                    if (! $_Keep) then
                      'rm' -f $n_exe/$Name.ppd.old
                    endif 
                 endif
                 mv $n_work/$Name.ppd $n_exe
              endif
              if (-e $n_exe/$Name.ppd) then
                 log "----- "\$n_exe"/$Name.ppd properly installed"
#
# NFRA has local executables on several HP machines
#
                 if ($n_site == "nfra" && $n_arch == "hp") then
                    cp $n_exe/$Name.ppd $n_root/exe/hp
                    echo "Copied to master exe-directory"
                    foreach tmphost ( $Local_HP )
                      if ($HOST != $tmphost) then
			 set Remok=`$RSH $tmphost echo \$n_exe`
			 if ($Remok != "" && $Remok != $n_root/exe/hp) then
			   $RSH $tmphost ' mv '\$n_exe/$Name.ppd \$n_exe/$Name.ppd.old' ; \
			   cp '$n_root/exe/hp/$Name.ppd \$n_exe' '
			   echo "Copied to "\$n_exe" on $tmphost"
			 else
			   if ($Remok == "") then
			     echo "$tmphost heeft probleem: is waarschijnlijk down"
			   else
			     echo "$tmphost heeft probleem: n_exe is $Remok (Bestaat execute.exe wel ??)"
			   endif
			 endif
                      endif
                    end
                 endif           

                 if (-o $n_hlp && -o $n_src) then
                    $n_exe/genaid.exe keys $File
                 else
                    log "***** Error: could not update documentation"
                 endif
              else
                 log "***** Error: could not move ppd file to "\$n_exe
                 @ Errors = $Errors + 1
              endif
           else
              if ($n_uexe != $n_work) mv $Name.ppd $n_uexe
              if (! -e $n_uexe/$Name.ppd ) then
                log "***** Error: could not move ppd file to "\$n_uexe
                @ Errors = $Errors + 1
              endif
           endif
        endif
     endif
#
#
# DSC files: use WNTINC
#          
  else if ("$Type" == "dsc") then
#
# Do all the work in $n_uinc
#
     cd $n_uinc

     if (! -e $n_uexe/wntinc.exe && ! -e $n_exe/wntinc.exe) then
        log "***** Error: dsc-compiler (wntinc) does not exist"
        @ Errors = $Errors + 1
     
     else
#
# Remove tricky links and all possible output files
#
        set Flag=`echo $Name | tr '[a-z]' '[A-Z]'`
        if (-e ${Flag}_DEF)    then
           'rm' -f ${Flag}_DEF
        endif
        if (-e ${Flag}_O_DEF)  then
           'rm' -f ${Flag}_O_DEF
        endif
        if (-e ${Flag}_T_DEF)  then
           'rm' -f ${Flag}_T_DEF
        endif
        if (-e ${Flag}_E_DEF)  then
           'rm' -f ${Flag}_E_DEF
        endif
        if (-e ${Name}_inc)    then
           'rm' -f ${Name}_inc
        endif
        if (-e ${Name}_o_inc)  then
           'rm' -f ${Name}_o_inc
        endif
        if (-e ${Name}_t_inc)  then
           'rm' -f ${Name}_t_inc
        endif
        if (-e ${Name}_e_inc)  then
           'rm' -f ${Name}_e_inc
        endif

        if (-e ${Name}.def)    then
           'rm' -f ${Name}.def
        endif
        if (-e ${Name}.inc)    then
           'rm' -f ${Name}.inc
        endif
        if (-e ${Name}_o.inc)  then
           'rm' -f ${Name}_o.inc
        endif
        if (-e ${Name}_t.inc)  then
           'rm' -f ${Name}_t.inc
        endif
        if (-e ${Name}_e.inc)  then
           'rm' -f ${Name}_e.inc
        endif
        if (-e ${Name}_bd.for) then
           'rm' -f ${Name}_bd.for
        endif
        if (-e ${Name}_o.def)  then
           'rm' -f ${Name}_o.def
        endif
        if (-e ${Name}_t.def)  then
           'rm' -f ${Name}_t.def
        endif
        if (-e ${Name}_e.def)  then
           'rm' -f ${Name}_e.def
        endif
#
# Invoke WNTINC to compile new output files in $n_uinc
#
        onintr dsc_done                   # To remove temp. copy
        cp $File $Tail                    # Temporary copy
        log "wntinc.exe $Name ($cwd $Tail)"
        if (-e $n_uexe/wntinc.exe) then
           $n_uexe/wntinc.exe $Name
           @ Flag = $status
        else
           $n_exe/wntinc.exe $Name
           @ Flag = $status
        endif
dsc_done:                                 # Interrupt handler
        'rm' -f $Tail                     # Remove temp. copy
        if (-e $Name.lis) then            # Move logfile
           mv $Name.lis $List_file
        endif
        onintr Abort_exit                 # Original interrupt handler
        if ( $Flag != 1 ) then
           if (-e $List_file) mv $List_file $Error_file
           echo "***** Errors compiling $Name.dsc ($Flag)" >>$Error_file
           @ Errors = $Errors + 1
        endif
#
# Link fortran includes to uppercase with underscore
#
        set Flag=`echo $Name | tr '[a-z]' '[A-Z]'`
        if (-e ${Name}.def)   then
	  mv ${Name}.def ${Name}.def.tmp
          awk -f $n_src/sys/data_splitter.kwa ${Name}.def.tmp |\
            sed -e 's%^	&%	1%' > ${Name}.def
	  ln -s ${Name}.def ${Flag}_DEF
	endif
        if (-e ${Name}_o.def) then
	  mv ${Name}_o.def ${Name}_o.def.tmp
          awk -f $n_src/sys/data_splitter.kwa ${Name}_o.def.tmp|\
            sed -e 's%^	&%	1%' >& ${Name}_o.def
          ln -s ${Name}_o.def ${Flag}_O_DEF
        endif
        if (-e ${Name}_t.def) then
	  mv ${Name}_t.def ${Name}_t.def.tmp
          awk -f $n_src/sys/data_splitter.kwa ${Name}_t.def.tmp|\
            sed -e 's%^	&%	1%' >& ${Name}_t.def
          ln -s ${Name}_t.def ${Flag}_T_DEF
        endif
        if (-e ${Name}_e.def) then
	  mv ${Name}_e.def ${Name}_e.def.tmp
          awk -f $n_src/sys/data_splitter.kwa ${Name}_e.def.tmp|\
            sed -e 's%^	&%	1%' >& ${Name}_e.def
          ln -s ${Name}_e.def ${Flag}_E_DEF
	endif
        if (-e ${Name}.inc) then
	  mv ${Name}.inc ${Name}.inc.tmp
          awk -f $n_src/sys/data_splitter.kwa ${Name}.inc.tmp >& ${Name}.inc
          ln -s ${Name}.inc ${Name}_inc
        endif
        if (-e ${Name}_o.inc) then
	  mv ${Name}_o.inc ${Name}_o.inc.tmp
          awk -f $n_src/sys/data_splitter.kwa ${Name}_o.inc.tmp >& ${Name}_o.inc
          ln -s ${Name}_o.inc ${Name}_o_inc
        endif
        if (-e ${Name}_t.inc) then
	  mv ${Name}_t.inc ${Name}_t.inc.tmp
          awk -f $n_src/sys/data_splitter.kwa ${Name}_t.inc.tmp >& ${Name}_t.inc
          ln -s ${Name}_t.inc ${Name}_t_inc
        endif
        if (-e ${Name}_e.inc) then
	  mv ${Name}_e.inc ${Name}_e.inc.tmp
          awk -f $n_src/sys/data_splitter.kwa ${Name}_e.inc.tmp >& ${Name}_e.inc
          ln -s ${Name}_e.inc ${Name}_e_inc
        endif

#
# If these files are needed for building WNTINC, copy them back
# into the source tree
#
#        if ($n_usrc == $n_src) then
#           set Flag=`grep -i $Name $n_src/wng/wnt_boot.grp`
#           if ("$Flag" != "") then
#              log "copy back into "\$n_src/wng" (kip/ei): ${Name}*.??? "
#              cp ${Name}*.??? $n_src/wng
#           endif
#        endif
     endif
#
# Prepare for compilation of _bd.for if it exists
#
     if (-e ${Name}_bd.for) set Input_file[1]=$n_uinc/${Name}_bd.for

#
#
# Macro: compile it
#
  else if ("$Type" == "m??") then
     log "Ignoring macro files on $n_arch"
     set Type="ignore"
  else if ("$Type" == "s") then
     if (-e $Object_file) then
        'rm' -f $Object_file
     endif
     awk '{printf("%4.4d   %s\n",NR,$0)}' $File > $List_file   # Make listing
     cd $n_uinc                                               # See includes
     log "$AS -o $Object_file $ASFLAGS $Tail"
          $AS -o $Object_file $ASFLAGS $File >&! $Error_file
     if ( $status || ! -e $Object_file ) then
          echo "Compilation errors..." >>$Error_file
          @ Errors = $Errors + 1
     else
          'rm' -f $Error_file
     endif
#
# C source (utility programs): compile to executable
#
  else if ("$Type" == c ) then 
     if (-e $Object_file) then
        'rm' -f $Object_file
     endif
     set Flag=""
     if ($_Debug)    set Flag=($Flag $CFLAGS_D)
     if ($_Optimise) set Flag=($Flag $CFLAGS_O)
     set L_Lib=""
     if ($?LD_USER) then                # Add user supplied libraries
         set L_Lib=( $L_Lib $LD_USER )
     endif
#
     log "$CC -o $Name.exe $Flag -I$n_inc -I$n_uinc $Tail"
          $CC -o $n_uexe/$Name.exe $Flag -I$n_inc -I$n_uinc $L_Lib $File >&! $Error_file
     if ( $status || ! -e $n_uexe/$Name.exe ) then
        echo "Compilation errors..." >>$Error_file
        @ Errors = $Errors + 1
     else
        if ($_Update) then
	  mv $n_uexe/$Name.exe $n_exe/$Name.exe
          log "----- "\$n_exe"/$Name.exe properly installed"
#
# NFRA has local executables on several HP machines
#
	  if ($n_site == "nfra" && $n_arch == "hp") then
	    cp $n_exe/$Name.exe $n_root/exe/hp
	    echo "Copied to master exe-directory"
	    foreach tmphost ( $Local_HP )
              if ($HOST != $tmphost) then
	        set Remok=`$RSH $tmphost echo \$n_exe`
	        if ($Remok != "" && $Remok != $n_root/exe/hp) then
	          $RSH $tmphost ' mv '\$n_exe/$Name.exe \$n_exe/$Name.exe.old' ; \
	          cp '$n_root/exe/hp/$Name.exe \$n_exe' '
	          echo "Copied to "\$n_exe" on $tmphost"
	        else
	          if ($Remok == "") then
		    echo "$tmphost heeft probleem: is waarschijnlijk down"
	          else
		    echo "$tmphost heeft probleem: n_exe is $Remok (Bestaat execute.exe wel ??)"
	          endif
	        endif
              endif
            end
	  endif
        endif           
        'rm' -f $Error_file
     endif
     if (-e $Object_file) then
        'rm' -f $Object_file
     endif
#
# C source: compile it (NB: .com and .csh already matched earlier!)
#
  else if ("$Type" =~ c?? ) then
     if ("$Type" != "cee" && "$Type" != "cun" && "$Type" != "c$n_arch" || \
		("$Type" == "cun" && "$n_arch" == "vx")) then
        log "Ignoring $Tail for $n_arch"
        set Type="ignore"
     else
        if (-e $n_work/$Name.c) then
           'rm' -f $n_work/$Name.c
        endif
        cp  $File $n_work/$Name.c
        set File=$n_work/$Name.c
        log "Linked $Tail to "\$n_work/$Name.c

        if (-e $Object_file) then
           'rm' -f $Object_file
        endif
        if ("$n_arch" == "vx") then
          set Flag=( $CFLAGS -Dwn_vax__ -Dwn_${n_arch}__ -Dwn_${n_site}__ )
	else
          set Flag=( $CFLAGS -Dwn_un__ -Dwn_${n_arch}__ -Dwn_${n_site}__ )
	  if ("$n_arch" == "so") then
             set Flag=( $Flag -Dwn_sw__ )
	  endif
	endif
        if (-e $n_lib/pgplot.olb ) set Flag=( $Flag -Dwn_pgplot__ )
        if (-e $n_lib/giplib.olb || \
            -e $n_lib/libgdi.olb) set Flag=( $Flag -Dwn_gipsy__ )
        if ($_Debug)    set Flag=($Flag $CFLAGS_D)
        if ($_Optimise) set Flag=($Flag $CFLAGS_O)

        awk '{printf("%4.4d   %s\n",NR,$0)}' $File > $List_file   # Make listing
        log "$CC -o $Object_file $Flag -I$n_inc -I$n_uinc $Tail"
             $CC -o $Object_file $Flag -I$n_inc -I$n_uinc $File >&! $Error_file
        if ( $status || ! -e $Object_file ) then
             echo "Compilation errors..." >>$Error_file
             @ Errors = $Errors + 1
        else
             'rm' -f $Error_file
             if (-e $n_work/$Name.c && ! $_Debug) then
                'rm' -f $n_work/$Name.c
                log "Removed temporary file "\$n_work/$Name.c
             endif
        endif
     endif

#
#
# Fortran source: precompile and compile
#
  else if ("$Type" =~ f?? ) then

    if ("$Type" != "for" && "$Type" != "fsc" && \
        "$Type" != "fun" && "$Type" != "f$n_arch" || \
	("$Type" == "fun" && "$n_arch" == "vx")) then
        log "Ignoring $Tail on $n_arch"
        set Type="ignore"
    else
#
# Does the file contain precompiler commands? If so, process them
#
      set Flag=`grep '^.*#endif' $File`
      if ("$Flag" != "") then
         log "Precompiling $Tail -> "\$n_work/$Name.f
	 if ("$n_arch" == "vx") then
           set Flag="(wn_vax__)|(wn_${n_arch}__)|(wn_${n_site}__)"
	 else
           set Flag="(wn_un__)|(wn_${n_arch}__)|(wn_${n_site}__)"
	 endif
         if (-e $n_lib/pgplot.olb ) set Flag="$Flag|(wn_pgplot__)"
         if (-e $n_lib/giplib.olb || \
             -e $n_lib/libgdi.olb)   set Flag="$Flag|(wn_gipsy__)"

         if (-e $n_work/$Name.f) then
            'rm' -f $n_work/$Name.f
         endif
#         cat $File | awk '\
         cat $File | sed -e 's%# *%#%' | awk '\
BEGIN { FS = " "; lvl=1; on[1]=1; } \
/^.*# *ifdef/ { \
  print "c" $0; lvl++; \
  if ($2 ~ /'$Flag'/) {on[lvl]=1;} else {on[lvl]=0;} \
  next } \
/^.*# *ifndef/ { \
  print "c" $0; lvl++; \
  if ($2 ~ /'$Flag'/) {on[lvl]=0;} else {on[lvl]=1;} \
  next } \
/^.*# *else/   { print "c" $0; on[lvl]=1-on[lvl]; next } \
/^.*# *endif/  { print "c" $0; lvl--;             next } \
  { ok=1; for (i=1; i<=lvl; i++) { if (on[i]==0) ok=0; }; \
    if (ok == 0) {printf "c:";} print }'  >$n_work/$Name.f.1st

         set File=$n_work/$Name.f
         awk -f $n_src/sys/data_splitter.kwa $File.1st   | \
           sed -e 's%PARAMETER (%PARAMETER ( %' >& $File
         rm $File.1st

      else 
         if (-e $n_work/$Name.f ) then
            'rm' -f $n_work/$Name.f
         endif
#        cp  $File $n_work/$Name.f
         awk -f $n_src/sys/data_splitter.kwa $File  | \
	    sed -e 's%PARAMETER (%PARAMETER ( %'  \
                -e 's%^	&%	1%'>& $n_work/$Name.f
         set File=$n_work/$Name.f
         log "Linked $Tail to $n_work/$Name.f"
      endif

      if (! -e $File ) then
         log "Could not produce file $File"
         @ Errors = $Errors + 1
      else
         if (-e $Object_file) then
            'rm' -f $Object_file
         endif
         set Flag=( $FFLAGS )
         if ($_Debug)    set Flag=($Flag $FFLAGS_D)
         if ($_Optimise) set Flag=($Flag $FFLAGS_O)
         if ($_Xref)     set Flag=($Flag $FFLAGS_X)

         cd $n_uinc
         log "$FC -o $Object_file $Flag $File"
#
# DEC Looks for include files in the directory where the source file is
#
         if ("$n_arch" == "dw" || "$n_arch" == "da" || \
		"$n_arch" == "vx") then
            cp $File $n_uinc
            $FC -o $Object_file $Flag $File:t >&! $Error_file
            if (-e $n_uinc/$Name.l) mv $n_uinc/$Name.l $List_file
            'rm' -f $n_uinc/$File:t
         else
            awk '{printf("%4.4d   %s\n",NR,$0)}' $File > $List_file  # Make listing
            $FC -o $Object_file $Flag $File >&! $Error_file
         endif
         if ( $status || ! -e $Object_file ) then
              echo "Compilation errors..." >>$Error_file
              @ Errors = $Errors + 1
         else
             'rm' -f $Error_file
             if (-e $n_work/$Name.f && ! $_Debug) then
                'rm' -f $n_work/$Name.f
                log "Removed temporary file "\$n_work/$Name.f
             endif
         endif
      endif     
    endif     
     
#
#
# Executable: make dummy file, compile and link
#
  else if ("$Type" == "exe") then

#
# Work in $n_ulib for local objects (length of string too long if we
# have to include the full path
#
     if ($?n_ulib) then
        cd $n_ulib
     endif

     set List_file=$n_work/$Name.map
#
#  Set up initialisation routines (better make dummy routine for hp)
#
     if ($n_arch == vx) then 
        set Getarg="CALL LIB$GET_FOREIGN(CLSTR)"
        set Tstarg=".TRUE."
     else
        set Getarg="CALL GETARG(1,CLSTR)"
        set Tstarg="IARGC().GT.0"
     endif
     if ($_Alternate) then
        set Flag="1"
     else
        set Flag=""
     endif
     if (! $?C_Date)    set C_Date="today"
     if (! $?C_Time)    set C_Time=""
     if (! $?C_Version) set C_Version=$C_Date
     set NAME=`echo $Name | tr '[a-z]' '[A-Z]'`
#
# Create a dummy fortran file
#
     cat << _EOD_ > $n_work/main_${Name}$$.f
        PROGRAM ${Name}_EXE
        CHARACTER*80 CLSTR	!COMMAND LINE ARGUMENT
        CHARACTER*25 VRS        !Version passed to WNGIN
        CHARACTER*80 WHAT       !Version found by what
        IF ($Tstarg) THEN
          $Getarg
        ELSE
          CLSTR=' '
        END IF
        WHAT='@(#)%NST%$NAME $C_Version $C_Date/$C_Time'
        VRS='$C_Version '                  !Set version number        
        VRS(8:)='$C_Date/$C_Time'          !Append date/time
        CALL WNGIN$Flag('$NAME',VRS,$Dattyp)
        CALL $Name(CLSTR)                   !CALL PROGRAM
        CALL WNGEX                          !FINISH OFF
        END
_EOD_
     unset Getarg
#
# Compile the dummy file
#
     set L_Lib=""
     set L_Obj=""

     set nonomatch

     touch main_.o                     # Make sure at least one is present
     set Flag=( main_*.o )             # Remove any old main object files
     if (-e $Flag[1] ) then            # At least one exists
        'rm' -f main_*.o               # Delete them all
     endif

     if ($?n_ulib) then                # Add any object files (shadow)
        set L_Obj=( *.o )
	if (-e $Name.objlist) then     # We did a cd to $n_ulib, mind you
	   set L_Obj = `sed -e 's: .*::' < $Name.objlist`
	   (ls $L_Obj >! $Name.tmp) >& /dev/null   # discard 'Not found' errors
	   set L_Obj = `cat $Name.tmp`
	   rm $Name.tmp
  	endif
        if ("$L_Obj" != "") then
	   if ("$L_Obj[1]" == '*.o') set L_Obj=""
	endif
        if ("$L_Obj[1]" != "") then
	   echo "Local objects:"
	   ls -l $L_Obj
##	   echo "$L_Obj" | tr ' ' '\012' | sed -e 's:^:  :'
	endif
     endif
     unset nonomatch

     if ($?LD_USER) then                # Add user supplied libraries
         set L_Lib=( $L_Lib $LD_USER )
     endif

     if (-e $n_lib/wnglib.olb) set L_Lib=( $L_Lib $n_lib/wnglib.olb )
     if (-e $n_lib/nstlib.olb) set L_Lib=( $L_Lib $n_lib/nstlib.olb )

     if (-e $n_lib/libgdi.olb) set L_Lib=( $L_Lib $n_lib/libgdi.olb )
     if (-e $n_lib/giplib.olb) set L_Lib=( $L_Lib $n_lib/giplib.olb )

     
##     set L_Lib=( $L_Lib -lm -ltermcap )
##     set L_Lib=( $L_Lib -L/usr/lib -lm -lncursesw )

     if (-e $n_lib/wnglib.olb) set L_Lib=( $L_Lib $n_lib/wnglib.olb )
     if (-e $n_lib/nstlib.olb) set L_Lib=( $L_Lib $n_lib/nstlib.olb )


     if ($?LD_X11) then                  # Special path to X11 libs
        set L_Lib=( $L_Lib $LD_X11 )
     else
        set L_Lib=( $L_Lib -lX11 )
     endif

     if (-e $n_lib/dwarflib.olb) set L_Lib=( $L_Lib $n_lib/dwarflib.olb )
     if (-e $n_lib/wnglib.olb)   set L_Lib=( $L_Lib $n_lib/wnglib.olb )
     if (-e $n_lib/giplib.olb) set L_Lib=( $L_Lib $n_lib/giplib.olb )

     if ($?LD_USER) then                # Add user supplied libraries
         set L_Lib=( $L_Lib $LD_USER )
     endif

##     set L_Lib=( $L_Lib  -lm -ltermcap )
     if ($?LD_USER) then                # Add user supplied libraries
	set L_Lib=( $L_Lib  $LD_USER -lm  )
     else
 ##      	set L_Lib=( $L_Lib -lncursesw -lm  )
      	set L_Lib=( $L_Lib -lm -lncursesw )
     endif

     set Flag=( $FFLAGS_L )
     if ($_Optimise) set Flag=( $Flag $FFLAGS_O )
     if ($_Debug)    set Flag=( $Flag $FFLAGS_D )
     if ($_Xref)     set Flag=( $Flag $FFLAGS_X )

     if (-e $n_work/$Name.exe) then
        'rm' -f $n_work/$Name.exe
     endif

     log "$FC -o $Name.exe $Flag main_${Name}$$.f "
     log "  Libraries: $L_Lib     "

###
#echo L_Lib $L_Lib
     $FC -o $n_work/$Name.exe $Flag  $n_work/main_${Name}$$.f  $L_Obj \
	$L_Lib >&! $List_file

     'rm' -f $n_work/main_${Name}$$.f 
     if (-e main_${Name}$$.o ) then
        'rm' -f main_${Name}$$.o
     endif
#
# Check result of compilation
#
     if ($status || ! -e $n_work/$Name.exe || ! -e $List_file) then
        if (-e $List_file)         mv $List_file $Error_file
        if (-e $n_work/$Name.exe) then
           'rm' -f $n_work/$Name.exe
        endif
        log "**** Error: could not build $Name.f"
        echo "**** Error during compilation ****" >>$Error_file
        @ Errors = $Errors + 1
     else 
        set Flag=`grep ndefine $List_file`
        if ("$Flag" == "") set Flag=`grep nresolv     $List_file`
        if ("$Flag" == "") set Flag=`grep nsatisf     $List_file`
        if ("$Flag" == "") set Flag=`grep duplicate   $List_file`
        if ("$Flag" == "") set Flag=`grep "multiply " $List_file`
        if ("$Flag" == "" && ! -x $n_work/$Name.exe) set Flag="Not executable"
        if ("$Flag" != "") then
           log "**** Error: $Flag"
           mv $List_file $Error_file
           if (-e $n_work/$Name.exe) then
              'rm' -f $n_work/$Name.exe
           endif
           @ Errors = $Errors + 1
        else
#
# If correct, move the executable to $n_uexe
#
           if ($n_uexe != $n_work && -e $n_uexe/$Name.exe) then
              mv  $n_uexe/$Name.exe $n_uexe/$Name.exe.old
              if (! $_Keep) then
                 'rm' -f $n_uexe/$Name.exe.old
              endif 
           endif
#
# For update: strip debugging info for speed and space, keep old exe
#
           if ($n_uexe == $n_exe || $_Update) then
              strip $n_work/$Name.exe 
              if ($n_exe != $n_work) then
                 if (-e $n_exe/$Name.exe) then
                    mv  $n_exe/$Name.exe $n_exe/$Name.exe.old
                    if (! $_Keep) then
                       'rm' -f $n_exe/$Name.exe.old
                    endif 
                 endif
                 mv $n_work/$Name.exe $n_exe
              endif
              if (-e $n_exe/$Name.exe) then
                 log "----- "\$n_exe"/$Name.exe properly installed"
#
# NFRA has local executables on several HP machines
#
                 if ($n_site == "nfra" && $n_arch == "hp") then
                    cp $n_exe/$Name.exe $n_root/exe/hp
                    echo "Copied to master exe-directory"
                    foreach tmphost ( $Local_HP )
                      if ($HOST != $tmphost) then
			 set Remok=`$RSH $tmphost echo \$n_exe`
			 if ($Remok != "" && $Remok != $n_root/exe/hp) then
			   $RSH $tmphost ' mv '\$n_exe/$Name.exe \$n_exe/$Name.exe.old' ; \
			   cp '$n_root/exe/hp/$Name.exe \$n_exe' '
			   echo "Copied to "\$n_exe" on $tmphost"
			 else
			   if ($Remok == "") then
			     echo "$tmphost heeft probleem: is waarschijnlijk down"
			   else
			     echo "$tmphost heeft probleem: n_exe is $Remok (Bestaat execute.exe wel ??)"
			   endif
			 endif
                      endif
                    end
                 endif           
#
# Update version number in the database entry
#
                 cp $n_src/sys/database.idx $n_work/database.old
                 grep -v $Name.exe $n_work/database.old      >$n_src/sys/database.idx
                 echo $Name.exe  $C_Version $C_Time $C_Date >>$n_src/sys/database.idx
              else
                 log "***** Error: could not move $Name.exe to "\$n_exe
                 @ Errors = $Errors + 1
              endif
#
# We do not keep old executables in $n_uexe
#
           else
              if ($n_uexe != $n_work && -e $n_uexe/$Name.exe) then
                 mv  $n_uexe/$Name.exe $n_uexe/$Name.exe.old
                'rm' -f $n_uexe/$Name.exe.old
              endif
              if ($n_uexe != $n_work) mv $n_work/$Name.exe $n_uexe
              if (-e $n_uexe/$Name.exe) then
                 log "----- "\$n_uexe"/$Name.exe properly installed"
              else
                log "***** Error: could not move $Name.exe to "\$n_uexe
                @ Errors = $Errors + 1
              endif
           endif

        endif
     endif        
#
#
#  No recognised filetype, issue a warning
#
  else
    log "**** Unrecognised filetype: $Tail, ignored"
  endif  

#
# We may have changed directory for compilation
#
  cd $Home
#
# Handle object files
#
 if ($Type != ignore) then
  if (-e $Object_file) then
     if (-e $Error_file || -e $List_file) then
        echo " " >>$List_file
        echo "Symbol table of module ${Object_file}:" >>$List_file
        nm $Object_file >>$List_file
     endif
#
#  Error occured, remove object file
#
     if (-e $Error_file) then
        'rm' -f $Object_file
#
#  Objectlib defined: put object file on list for library
#
     else if ("$_Objectlib" != "") then
       echo $Object_file >>$n_work/${_Objectlib:t}.list
     else
        if ($n_ulib != $n_work) mv $Object_file $n_ulib/$Object_file:t
        if (! -e $n_ulib/$Object_file:t ) then
           log "***** Error: could not move $Object_file into "\$n_ulib
           @ Errors = $Errors + 1
        endif
     endif
  endif

#
# If Textlibrary should be used, and no errors detected: push on the list
#
  if ("$_Textlib" != "") then
     if (! -e $Error_file || -z $Error_file) then
       log "Put $Name in textlibrary."
       echo $File >>$n_work/${_Textlib:t}.list
     endif
  endif

#
# Handle error files
#
  unset Flag  # Force keep List_file in case of errors
  if (-e $Error_file) then
     if (! -z $Error_file ) then
        echo 
        if ($_Errors) head -25 $Error_file
        echo " " >>$List_file
        echo "***** Error messages: " >>$List_file
        cat $Error_file >>$List_file
        log "***** Errors appended to $List_file"
        set Flag
     endif
    'rm' -f $Error_file
  endif
#
# Handle list files
#
  if (-e $List_file) then
     mv $List_file $n_work/$Name.tmp 
     if (-e $n_work/$Name.tmp) \
        pr -f -l60 -h $File  $n_work/$Name.tmp > $List_file 
     if ($_Print && -e $n_src/sys/wngfex.csh) then
        $n_src/sys/wngfex.csh sp $List_file $File
        log "----- Listing in $List_file printed"
     else if ("$List_file:e" == "map") then
        log "----- Mapfile in $List_file"
     else if ($_List || $?Flag) then
        log "----- Listing in $List_file"
     else
        'rm' -f $List_file
     endif
  endif
  unset Flag
 endif

#
# Ready for the next file...
# Input_file[1] may have been overwritten to cause a freshly produced file
# to be processed (pin from psc etc.)
#
  if ($Input_file[1] == ""  && $#Input_file > 1) shift Input_file
end    

goto Normal_exit

Abort_exit: 
  set Abort_flag

Normal_exit:

cd $Home

if ($?Name) then
  if (-e $n_work/$Name.tmp ) then
      'rm' -f $n_work/$Name.tmp; 
  endif
  if (-e $n_work/main_${Name}$$.f ) then
      'rm' -f $n_work/main_${Name}$$.f; 
  endif
  if (-e $n_ulib/main_${Name}$$.o ) then
     'rm' -f $n_ulib/main_${Name}$$.o
  endif
endif

#
# Remove all local definitions (not necessary, but doesnot really harm)
#
unset Type Tail Name Object_file List_file Error_file
unset Getarg Flag L_Lib L_Obj

exit
