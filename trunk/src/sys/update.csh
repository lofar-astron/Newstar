#! /bin/csh -f
#+ update.csh
#
#   CMV 930524  Created
#   CMV 931013	Improved Clean option, added Save, added Group
#   CMV 931018	Correct mailer in save option
#   CMV 931018	Implemeted -List for retrieve
#   CMV 931019	Added library checks
#   CMV 931020	Changed call to switches
#   CMV 931020  Build -D  puts objects in $n_ulib
#   CMV 931025  Better text in Save
#   CMV 931102	Switch -[N]Check added for dependencies
#   CMV 931104  Added automatic mailing for NFRA updates
#   CMV 931107  Less output during backup (t in stead of tv)
#   CMV 931111  Removed retrieve -l option, added diff command
#   CMV 931116  Changed updating log for httpd 1.0
#   CMV 931124  Force editing of nnews after nup b -u at NFRA, 
#               no longer mail to NFRA after a retrieve,
#               moved get and put to shadow.csh
#   CMV 931202  Add locking for retrieve option
#   CMV 931220  Create subdirectory for checkin
#   CMV 931221  Multiple groupfile for new-to-old
#   CMV 931221  No .old files in pack
#   CMV 931223	Change handling of libraries
#   CMV 931223  Some more prevention against "word too long"
#   CMV 940214  Revision numbers and change in retrieve procedure
#   CMV 940216  Check l spawns rebuild of library
#   CMV 940216  New command: update (retrieve revision)
#   CMV 940218  Include revision number in executable
#   CMV 940304  Add argument all for Pack command
#   HjV 940314  Use environment ARD (=ar dv  or  ar dlv)
#   CMV 940323  remsh for HP's
#   HjV 940328  FTP info about Newstar use to NFRA
#   HjV 940331  PACK: not in background, causes diskquota problems
#   CMV 940419  Made test on version in check exe more robust
#   CMV 940420  Compile abp executables only if n_doabp set
#   HjV 940503	Remove old version of newstar.use
#   CMV 940506	Better format in log to revision history
#   HjV 940516  CLEAR: Do not print message for other machines binaries
#   CMV 950517  Option to delay notification to friends of Newstar
#   HjV 940624	Do not get newstar.use when NFRA does the retrieve
#   CMV 940705  Add notify command
#   CMV 940718  Typo 
#   HjV 940802  More typo's
#   CMV 940804  Notify does all releases or nothing
#   CMV 940821  Move grp files to n_import/old after files have been updated
#   CMV 940915  Change reference to $n_exe/html to $n_hlp
#   HjV 940922	Copy files with ftp (i.s.o. rsh) to DAW10 
#   HjV 941019	Copy source files and hlp-directory in .tar-files with
#		ftp to DAW10. On DAW10 there will be a program which 
#		check the tar-files every hour.
#   HjV 941031	Change for better updating
#   CMV 941101	If any file from $n_src/sys retrieved, update right away
#   CMV 941102	Add ranlib for nup check l
#   CMV 941102  Implement retrieve -Import and build -NMerge
#   CMV 941102  nup notify is now nup release
#   CMV 941111  Proper handling of version numbers in case of nup -t:none
#   HjV 941114  Mail message when something wrong with NSERVER
#   HjV 950112	Build SYS_BLDPPD.EXE (if necessary) before handling PIN/PSC files
#   HjV 950530  Check for (and set) PWD, not known on all systems
#   HjV 960102	Change option FULL into ALL for document.csh
#   HjV 960423  Bitmaps have now extension .xbm in source tree
#   HjV 960522  Change files to exclude for PACK option (different for hlp)
#   HjV 000309  Change elm -s into nsmail
#
#
# This is the update script for the Newstar programs
#
# Note for programmers: 
#   Search for %Blurp gets you to start of code for command Blurp etc.
#
#
# For maintenance of the master system, file information is 
# taken from the following databases:
#
#    $n_src/sys/version.idx      Release and revision number
#    $n_src/sys/database.idx     Full filesystem database
#    $n_src/sys/locks.idx        Database with locked files
#    $n_src[/*]*.grp             Groupfiles with compilation instructions
#
#---------------------------------------------------------------------
#
#
#
#  Check wether we can use update.
#
if (! -o $n_src) then
   echo "***** You can only run update as the Newstar manager..."
   exit
endif

if ($cwd != $n_src && $cwd !~ $n_src/* && $cwd != $n_import) then
   echo "***** You are not allowed to run update from $cwd..."
   echo "You should be either in "\$n_src" or one of it's subdirectories,"
   echo "or in directory "\$n_import", not in $cwd..."
   if (-d $n_import) then
      cd $n_import
      echo "Now in $n_import"
   else
      exit
   endif
endif

onintr Abort_exit

#
#  Setup logging, initialise, check filesystem etc.
#
alias log 'echo \!* | tee -a $Logfile'
source $n_src/sys/initcompile.csh
if (! $?PWD ) then                             # Make sure we have PWD
   setenv PWD `pwd`
endif

#
#  Decode switches, get command, or set menu mode if none given.
#
set noglob; set Command=( $argv ); unset noglob
set Options=""; source $n_src/sys/switches.csh

set Files=""
if ("$Command" != "") then
   set Mode="Command"
   if ($#Command > 1) then
      set noglob; set Files=( $Command[2-] ); unset noglob
      set Command=$Command[1]
   endif
else
   set Mode="Menu"
   set Command=""
endif


#
#
# Check the file system and try to make any missing directories
#
if (! -d $n_lib ) then
   set n_tmp=$n_lib
   if (! -d ${n_tmp:h} ) then
      log ">>>>>>>> Creating root of library tree... ${n_tmp:h}"
      mkdir ${n_tmp:h}
   endif
   log ">>>>>>>> Creating library directory for $n_arch...  $n_lib"
   mkdir $n_lib
   if (! -d $n_lib ) then
      log " "
      log "Could not create library directory $n_lib..."
      goto Abort_exit
   endif
endif

if (! -d $n_inc ) then
   log ">>>>>>>> Creating include directory $n_inc"
   mkdir $n_inc
   if (! -d $n_inc ) then
      log " "
      log "Could not create include directory $n_inc..."
      goto Abort_exit
   endif
endif   

if (! -d $n_exe ) then
   set n_tmp=$n_exe
   if (! -d ${n_tmp:h} ) then
      log ">>>>>>>> Creating root of binary tree... ${n_tmp:h}"
      mkdir $n_tmp:h
   endif
   log ">>>>>>>> Creating binary directory for $n_arch...  $n_exe"
   mkdir $n_exe
   if (! -d $n_exe ) then
      log " "
      log "Could not create binary directory $n_exe..."
      goto Abort_exit
   endif
endif

if (! -d $n_hlp ) then
   log ">>>>>>>> Creating hypertext directory $n_hlp"
   mkdir $n_hlp
   if (! -d $n_hlp ) then
      log " "
      log "Could not create include directory $n_hlp..."
      goto Abort_exit
   endif
endif   

if (! -d $n_tst ) then
   set n_tmp=$n_tst
   if (! -d ${n_tmp:h} ) then
      log ">>>>>>>> Creating root of test binary tree... ${n_tmp:h}"
      mkdir $n_tmp:h
   endif
   log ">>>>>>>> Creating test binary directory for $n_arch...  $n_tst"
   mkdir $n_tst
   if (! -d $n_tst ) then
      log " "
      log "Could not create test binary directory $n_tst..."
      goto Abort_exit
   endif
endif

if (! -d $n_work) then
   set n_tmp=$n_work
   if (! -d ${n_tmp:h} ) then
      log ">>>>>>>> Creating root of work directory tree... ${n_tmp:h}"
      mkdir ${n_tmp:h}
   endif
   log ">>>>>>>> Creating work directory for $n_arch... $n_work"
   mkdir $n_work
   if (! -d $n_work ) then
      log " "
      log "Could not create work directory $n_work..."
      goto Abort_exit
   endif
endif

if (! -d $n_import) then
   log ">>>>>>>> Creating directory for import...  $n_import"
   mkdir $n_import
   chmod a+rwx $n_import
   if (! -d $n_import ) then
      log " "
      log "Could not create import directory $n_import..."
      goto Abort_exit
   endif
endif

#
#
# Remove any existing locks and create a new one
#
if (-e $n_work/update.lock ) then
   echo "Cannot update on ${n_arch}: "`cat $n_work/update.lock`
   echo -n "Remove "; 'rm' -i $n_work/update.lock
   if (-e $n_work/update.lock) goto Abort_exit
endif
echo "Locked by $USER at $C_Date $C_Time" >$n_work/update.lock

#
# Check wether the various precompilers exist
#
if (! -e $n_exe/genaid.exe ) then
   log "Building utility program genaid ($n_exe/genaid.exe)"
   $CC -o $n_exe/genaid.exe $n_src/sys/genaid.c
   if (! -e $n_exe/genaid.exe) goto Abort_exit
endif

if (! -e $n_exe/wntinc.exe ) then
   log "Missing dsc-compiler ($n_exe/wntinc.exe)"
   log "Run  update build wntinc  to build it first"
#   if ("$Mode" != "Menu") goto Abort_exit
endif

if (! -e $n_exe/sys_bldppd.exe ) then
   log "Missing ppd-compiler ($n_exe/sys_bldppd.exe)"
#   if ("$Mode" != "Menu") goto Abort_exit
endif


#
#
# If in Menu mode, repeatedly ask commands, else just one command
#
while ( "$Mode" != "Quit")
   
   if ( "$Mode" == "Menu" ) then
      if ("$n_site" == nfra) then
         echo "Commands are: update, build, cont,  check,  retrieve, clean, "
         echo "              diff,   pack,  group, release, save, help,  quit"
      else
         echo \
    "Commands are: update, build, cont, check, retrieve, clean, help, quit"
      endif
      echo -n "Enter a command: "
      set Command=($<)
      set Files=""
      set noglob; set Command=( $Command ); unset noglob
      set Options=""; source $n_src/sys/switches.csh
      if ($#Command > 1) then
         set noglob; set Files=( $Command[2-] ); unset noglob
         set Command=$Command[1]
      endif
   else if ( "$Mode" == "Update" ) then
      set Command=$Upd_list[1]                 # Get next command      
      if ($#Upd_list > 1) then
         shift Upd_list                        # Get rid of command
         set Files=( $Upd_list[1] )            # And it's argument
         if ($#Upd_list > 1) shift Upd_list    # Get rid of argument
      else
         set Files=""
      endif
   else
      set Mode="Quit"
   endif

   if ("$Command" == "" || "$Command" =~ [Qq]*) then
      set Mode="Quit"
      
#
#  %Help command:
#
   else if ("$Command" =~ [Hh]* ) then
       cat <<_EOD_
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

 Update is used for maintenance of the Newstar master system. 
 You are only allowed to run update when you are the owner of \$n_root.
 
 Update can only be run from the root of the master source tree (\$n_src), 
 from one of it's subdirectories (\$n_src/*) or from the directory for
 import of new files (\$n_import). Update leaves a full transaction log
 in \$n_src, with name 'updyymmdd[i].log' (where yymmdd is the current 
 date and i is an integer 1,2,...).

 For program development and debugging, please use the "shadow" command,
 in particular shadow build.

 Update can be called in one of the following ways:


     nup 

        Enter a menu mode where all options listed below can be 
        chosen. Additional arguments will be prompted for.

     nup update

        This will check your current implementation against the
        master copy at NFRA, retrieve any new files, build them,
        clean up your local copy and libraries and rebuild any
        out-of-date executables.

     nup build  [switches | groupfile | 'all' ] ...

        Accepts one or more groupfiles which should be either 
        in the current dir or in a subdirectory of the master source 
        tree (the extension .grp may be ommitted). Normally no switches 
        will be given (this is updating the master, so you should in 
        principle hardcode things in files i_$n_arch$n_site.csh).

        If you specify the -Update switch, executable files will end up 
        in \$n_exe, if you specify -NUpdate they land in \$n_tst.

        The groupfiles are scanned and all files contained therein
        that are relevant to this architecture are compiled.

        Processing takes place in a pre-defined order of filetypes,
        you may select some types only by using the -Types switch.
        You may select to compile only a subset of the filetypes by
        setting -Types:ask, which will cause the update to prompt 
        you (in advance) for the requested passes.

        If the groupfile was in \$n_import and no errors occurred then
        all files in the groupfiles (except .exe) are copied into their 
        appropriate directory in the Master source tree. 
        Once successfully copied, each file is unlocked in the locking 
        database (lock.idx). The master database (database.idx) is
        updated.

        If there are dependent files (that is: files that need to be 
        rebuilt because an include file has been updated), their names
        are stored in \$n_work/depend.grp.This file is processed at the
        end of each pass. This ensures that any dependencies introduced
        by the dependent files are teated properly (eg. an update of 
        a DSF file causes some dependent DSC files to be rebuilt, and
        these DSC files cause some Fortan and C sources to be rebuilt etc.
        Checking of dependencies can be disabled with the -NCheck switch.
 
        The file depend.grp is kept in \$n_work for inspection

        The groupfile should explicitly mention the .exe files that 
        need to be rebuilt!  

    nup continue  [switches | groupfile | 'all' ] ...

        Resumes a build that has crashed somewhere. To rebuild some
        files, please edit \$n_work/continue.idx
        
        Use this option with care.
        
    nup retrieve ['all' | groupfile [inet-address [user [directory]]] ]

        Default internet specifications are 'all' and the NFRA Master node.

        If the literal 'all' is specified, the Master database will
        be retrieved from the remote node. If no local database 
        exists, it will be updated (this will take some time but is 
        the only safe way to proceed). The master database will be 
        checked against the local database. From this comparison,
        a groupfile is constructed.

        If a groupfile is specified in stead of 'all', it will be
        retrieved from the remote node if it does not already exist.

        All files in the (constructed or retrieved) groupfile 
        will be retrieved over the network.
        To update the files, use the "nup build retrieved" command.
        All files are received in \$n_import, which should be empty.

     nup clean [directory | 'all']

        Verify the specified directory in the source tree against 
        the groupfiles in that directory. 
        If no directory is specified, check them all. 
        If the literal 'all' is specified, check against the
        the master database (\$n_src/sys/database.idx).
        Remove (with confirm) any files that exist but are not 
        mentioned in a groupfile,
        Report any files that do not exist but are in a groupfile.

     nup check ['all' | fdhle ]

        Default for the argument is all

        If the argument is all or contains an f:
          Verify the master source tree against the master database.
          Checksums, sizes and dates are compared. A groupfile is 
          created in \$n_import with entries for defect and missing 
          files, this groupfile can be processed with "nup retrieve".

        If the argument is all or contains an h:
          The command "ndoc all" is executed to make a fresh version
          of all documentation.

        If the argument is all or contains a d or contains an h:
          A new master database is created which reflects the current 
          situation. We need to do this after check h, because the
          documentation archive newstar.hun will have been updated

        If the argument is all or contains an l:
          The object libraries are checked for double entries and
          out-of-date files.

        If the argument is all or contains an e:
          The executables are checked and out-of-date files are rebuilt.

     nup system update

        This command is called by nup update. It checks wether any 
        system files from \$n_src/sys have been retrieved. If so, these
        files are installed immediately, and update invokes itself 
        using the new system files.

_EOD_

       if ("$n_site" == "nfra") then
          cat <<_EOD_

     nup diff [groupfile] ...

        Compare files in \$n_import (listed in the groupfile) with the
        versions in the master tree.

     nup save

        Start a full backup of the master system (in the background) 
        All files below \$n_root will be written to tape. A log is
        kept in \$n_root/backups.txt. A roulating tape pool can be 
        used. The actual command for the backup can be specified by
        the user. Default is the command previously used.


     nup pack [name_of_directory... | 'all']
      
        Put all files below the directories in a tar file (name 
        defaults to nstar_name.tgz, where name is the last item 
        in the directory specification or, if the directory roots in
        \$n_src, \$n_exe or \$n_lib: nstar_yyy_name.tgz, where
        yyy is either src, exe or lib. 

        If the literal 'all' is given, archives will be made for the
        source tree, \$n_inc, \$n_hlp and the executable trees for hp 
        and sw.
        

     The following command is an interface between old and new style
     groupfiles for update and retrieval:

     nup group groupfile(s)

          if a single input groupfile is specified and if it contains
          lines starting with +, it is split out in a series of files
          for the corresponding directories
 
          otherwise the groupfiles are transformed in a single groupfile
          as expected by retrieve.
_EOD_
       endif    

       echo \
"#-------------------------------------------------------------------------#"

#
#
#   %Update command:
#
    else if ("$Command" =~ [Uu]*) then
#
# First time, mode will not be Update
#
       if ("$Mode" != "Update") then
#
#  Is it a valid host?
#
          set Hosts=(`echo $n_hosts | tr ',' ' ' `)
          set Flag=""
          foreach name ( $Hosts )
            if ("$name" == $HOST) set Flag=$name
          end
          if ("$Flag" == "") then
             log "Invalid host $HOST, you should run on one of $n_hosts"
#
#  Yes, we run a couple of commands, which one depends on the argument
#
          else if ("$Files" == "" || "$Files" == "nosys") then
             cd $n_import
             echo "Now in $n_import"
             set _Update=1              # For Build
             set _Confirm=0             # For Clear
             set Mode=Update            # Process multiple commands
             set Upd_list=( retrieve all )
             if ("$Files" != "nosys") then
                set Upd_list=( $Upd_list \
                               system   update )
             endif
             set Upd_list=( $Upd_list \
                            build    retrieved \
                            clear    all       \
                            check    l         \
                            check    e         \
                            update   ""        )
          else if ("$Files" == "rsh") then
             cd $n_import
             echo "Now in $n_import"
             set _Update=1              # For Build
             set _Confirm=0             # For Clear
             set Mode=Update            # Process multiple commands
             set Upd_list=( build    retrieved \
                            check    l         \
                            check    e         \
                            quit     ""        )
          else
             log "Invalid argument for Update command"
          endif
#
#  Not first time, we spawn a series of remote commands
#
       else
         if (! $?RSH ) then                             # Make sure we have rsh
            setenv RSH \rsh
            if ($n_arch == hp) setenv RSH \remsh
         endif

         foreach name ( $Hosts )
           if ("$name" != $HOST) then
              echo "Now trying to update on $HOST with command"
              echo "$RSH $name "
              echo "( source \$n_src/sys/newstar_$n_site.csh; nup update rsh)"
              $RSH $name \
               '( source '$n_src/sys/newstar_$n_site.csh'; nup update rsh)' 
           endif
         end
         set Mode="Quit"
       endif 
#
#
#   %System command:
#
    else if ("$Command" =~ [Ss][Yy][Ss]*) then

       @ Errors = 0                               # No errors yet

       if (-e $n_import/retrieved.grp) then       # Groupfile should exist
#
#  Rebuild any utility programs
#
          log "Installing system files right away"
          set nonomatch
          set Input_file=( $n_import/*.c )
          if (-e $Input_file[1]) then
             source $n_src/sys/compile.csh
          endif
          unset nonomatch
#
#  Find list of shell-scripts, copy into system
#       
          grep +sys/.\*\.csh $n_import/retrieved.grp >$Tmpfile
          set Input_file=( `$n_exe/genaid.exe files $Tmpfile` )
          'rm' -f $Tmpfile
          if ("$Input_file" != "") then
             foreach wfile ($Input_file) 
               set Input_file=( $n_import/$wfile:t )
               source $n_src/sys/compile.csh
               if (-e $n_src/$wfile) mv $n_src/$wfile $n_src/$wfile.old
               mv $n_import/$wfile:t $n_src/$wfile
               set Flag=(`cmp $n_import/$wfile:t $n_src/$wfile`)
               if ("$Flag" != "") then
                  log "Error moving ${wfile:t}: $Flag"
                  @ Errors = $Errors + 1
                  if (-e $n_src/$wfile.old) mv $n_src/$wfile.old $n_src/$wfile
               else
#
# Update the Master database
#
                  cp $n_src/sys/database.idx $n_work/database.old
                  grep -v $wfile $n_work/database.old      \
                                  >$n_src/sys/database.idx
                  $n_exe/genaid.exe fstat -c $n_src/$wfile \
                                 >>$n_src/sys/database.idx 
                  log "${wfile:t} updated in master tree and database."
               endif
             end

#
#  If shell-scripts updated successfully, remove lock and restart
#
             if ($Errors == 0) then
                log "System files changed: restarting update..."
                if ("$n_site" != nfra) then
                   echo "System files updated on $n_site ($n_arch) at ${C_Date}" |\
                      nsmail "Newstar_update_on_$n_site/$n_arch" $n_master
                endif
                mv $n_work/update.lock $n_work/update.lock.$$
                $n_src/sys/update.csh $Files nosys
                mv $n_work/update.lock.$$ $n_work/update.lock
                set Mode="Quit"
             endif

          endif          
       endif 
#
# Send message if errors occurred
#
       if ($Errors != 0 ) then
          set Flag=(`cat $n_src/sys/version.idx`)
          cat <<_EOD_ | tee -a $Logfile

*************** Installation errors occured **********************

Errors during update of system-files. 
The log-file will be mailed to  $n_master.

Please inform this account of additional information that might be
connected with the errors (recent change of operation system, disk
space problems etc). The Newstar group will contact you about the 
problems as soon as possible.

Your present installtion is intact and has not been updated.
You seem to have $Flag

*****************************************************************

_EOD_
          cat $Logfile | \
            nsmail "Newstar_crash_on_$n_site/$n_arch" $n_master 
          set Mode="Quit"
       endif
#
#
#   %Build command:
#
    else if ("$Command" =~ [Bb]* || "$Command" =~ [Cc][Oo][Nn][Tt]*) then

       if ("$Command" =~ [Bb]*) then
          set Cmd="build"
          if (-e $n_work/continue.idx) then      # Files already done
             'rm' -f $n_work/continue.idx
          endif
          if (-e $n_work/depend.grp) then        # Remaining dependencies
             'rm' -f $n_work/depend.grp
          endif
          set nonomatch
          set file=( $n_work/*.?lb.list )        # Files to be archived
          if (-e $file[1]) then
             'rm' -f $n_work/*.?lb.list
          endif
          unset nonomatch
       else
          set Cmd="cont"
       endif
       if ($_Update) then
          set Cmd="$Cmd -U  "
       else 
          set Cmd="$Cmd -NU "
          setenv n_uexe $n_tst              # Executables/ppd files in $n_tst
       endif

       if (! -e $n_work/continue.idx) touch $n_work/continue.idx
       if (! -e $n_work/depend.grp)   touch $n_work/depend.grp
#
# Save the current version. We need to increase the version if we update 
# from $n_import, and we need to do that only once.
#
       set O_Version=$C_Version
       if ($n_site == nfra) then
          set N_Version=` echo $C_Version | awk -F. '{ printf "%s.%d",$1,$2+1 }' `
       else
          set N_Version=` echo $C_Version | awk -F. '{ printf "%s.%d.%d",$1,$2,$3+1 }' `
       endif
#
# Get input files, check defaults, prepare for general log
#
       if ("$Mode" == "Update") set _Types="^exe"

       if ("$Files" == "") then
          echo -n "Enter name of groupfile(s) or all: "
          set noglob             # Don't expand wildcards right now
          set Files=( $< )       # Read from stdin
          set Files=( $Files )   # Split in multiple words
          unset noglob
       endif

       if ("$Files" == "" || "$Files" =~ [Aa][Ll][Ll] ) then
          set Cmd="$Cmd -NC all"
          set Files=$n_src'/*/???.grp'
          set _Check=0
       else if ("$Files" =~ [Ww][Nn][Tt][Ii][Nn][Cc]) then
          set Cmd="$Cmd -NC wntinc"
          set Files=( $n_src/wng/wnt_boot           \
                      $n_src/wng/wng $n_src/wng/wnc \
                      $n_src/wng/wnf $n_src/wng/wnt )
          set _Check=0
       else if ($_Check) then
          set Cmd="$Cmd -C  $Files"
       else
          set Cmd="$Cmd -NC $Files"
       endif
#
#  To process files just retrieved, move to $n_import
#
       if ("$Files" == "retrieved" && $PWD != $n_import) then
          cd $n_import
          echo "Now in $n_import"
       endif
#
#  Expand wildcards, check existence of all files in advance.
#  Only files in $n_src/* and $n_import are allowed.
#  The current directory will be $n_src, $n_src/* or $n_import.
#
       set Input_file=""
       set noglob; set nonomatch
       foreach File ( $Files )
          if (-d $File) then
             unset noglob; set File=( $File/???.grp ); set noglob
          else if ("$File:e" == "") then
             set File=$File.grp 
          endif
          set Input_file=($Input_file $File)
       end
       unset noglob
       set Input_file=( $Input_file )
       set Files=""
       foreach File ( $Input_file )
          if (-e $File) then

             if ( $File:h == $File ) then         # Only name given
                set File=$cwd/$File
                if ($cwd == $n_import) then
                   log "%%%%%%% Updating $File:t from "\$n_import...
                endif
             else if ( $File !~ */*/$File:t && $cwd == $n_src ) then # wrt n_src
                set File=$n_src/$File
             endif

             if ( $File =~ $n_src/*/$File:t   || \
                  $File == $n_import/$File:t  ) then  
                set Files=( $Files $File )
             else
                log "Specify files in Master source tree or "\$n_import
                log "$File ignored."
             endif

          else
             log "Cannot find groupfile $File, ignored."
          endif
       end
       unset nonomatch
#
# Select the types to be processed and do global log
#
       set typelist=( "grp/idx/kwa"                                        \
                      "tex/txt/hlp/html/xbm/gif/gfs/cap/tbl/fig/hun/ps"\$  \
                      "scn/wmp/mdl/ngf/flf"                      \
                      "csh/com/ssc/pls/c"\$                      \
                      "x$n_arch/a$n_arch"                        \
                      "inc/dsf/pef/def"                          \
                      "dsc"                                      \
                      "for/fsc/fun/f$n_arch/f"\$                 \
                      "cee/csc/cun/c$n_arch/s"\$                 \
                      "exe" "pin/psc" )
       if ("$_Types" != "." && "$_Types" != "") then
          if ("$_Types" =~ \^[Ee][Xx][Ee]) then
             set typelist[10]=""
             set typelist=( $typelist )
          else if ("$_Types" !~ [Aa][Ss][Kk]) then
             set typelist="$_Types"
          else 
             echo "Selecting file-types to process: "

             set tmp=( "groupfiles"                                  \
                       "help files"                                  \
                       "data files"                                  \
                       "scripts"                                     \
                       "special binaries"                            \
                       "include files"                               \
                       "dsc definition files"                        \
                       "fortran sources"                             \
                       "c sources and macros"                        \
                       "executables"      "pin-files")
             @ ii = 1
             while ( $ii <= $#typelist )
               echo -n "Do $tmp[$ii] ($typelist[$ii])? (y,n) [Y] "
               set Flag=($<)
               if ($Flag =~ [Nn]* ) set typelist[$ii]=""
               @ ii = $ii + 1
             end

             unset tmp
             set typelist=( $typelist )
          endif
          echo "$C_Date $C_Time - $n_arch - $Cmd ($typelist) " >>$n_root/updates.log
       else
          echo "$C_Date $C_Time - $n_arch - $Cmd (all) " >>$n_root/updates.log
       endif

       @ Errors = 0
       log "Logging information in $Logfile"
       log "%%%%%%% Errors so far: $Errors ("`date`")"  
#
#  The check flag determines wether we want to do dependecy checking
#
       if ($_Check) then
          set Depend=$n_work/depend.grp
       else
          set Depend=""
          log "%%%%%% No dependency checks"
       endif

       foreach ftype ( $typelist )

        if ( ("$ftype" =~ *exe* || \
              "$ftype" =~ *pin* || \
              "$ftype" =~ *psc* )  && $Errors != 0) then
          log "======== Errors found, skip executables and ppd files ======="
        else

          log " "
          log "======== Pass for filetypes $ftype ========"


	  if ("$n_site" != nfra) then
	   if ("$ftype" =~ *pin* || "$ftype" =~ *psc* ) then
#
#  Check if we have to rebuild SYS_BLDPPD.EXE
#
            set Filestmp=(dwarf/sys_bldppd.exe)
            foreach File ( $Filestmp )
              set v_exe=("" "")
              set v_idx=("" "")
              set File=`echo $File:t | tr '[A-Z]' '[a-z]'`
              set Flag="${File}: No executable"
              if (-e $n_exe/$File) then
                 set Flag =( `grep $File $n_src/sys/database.idx` "" "")
                 set v_idx=( `echo $Flag[2] | awk -F. '{ print $1,$2}' ` "" "" )
                 set Flag =( `what $n_exe/$File | grep %NST% ` "" "" )
                 set v_exe=( `echo $Flag[2] | awk -F. '{ print $1,$2}' ` "" "" )
              endif
              set Input_file=""
              if ("$v_exe" == " " || "$v_idx" == " ") then
                 set Input_file=($File)
	      else if ("$v_exe" == "" || "$v_idx" == "") then
                 set Input_file=($File)  
  	      else 
	         if ("$v_exe[1]" < "$v_idx[1]" || \
                     "$v_exe[2]" < "$v_idx[2]") then
                   set Input_file=($File)
	         endif
	      endif
              if ("$Input_file" != "") then
                 if (`grep -c -i $Input_file $n_src/dwarf/src.grp` != 0) set _Alternate=1 
                 echo "Building new SYS_BLDPPD.EXE for handling pin/psc files"
                 source $n_src/sys/compile.csh
                 set _Alternate=0
                 set Flag =( `what $n_exe/$File | grep %NST% ` '(updated)' )
              endif
              echo $Flag | sed -e s/%NST%// >>$Tmpfile
            end
	   endif				# pin/psc
	  endif					# not nfra


          foreach grpfile ( $Files $Depend )

             if ($grpfile == $n_work/depend.grp) then
                echo "Checking dependencies..."
                sort -u -o $n_work/depend.grp $n_work/depend.grp
             endif

             set Newlib=${grpfile:h}
             if ($Newlib == $grpfile) set Newlib=${cwd:h}
             set Newlib=${Newlib:t}
             if ("$Newlib" =~ n*) set Newlib=nst

             $n_exe/genaid.exe expand -t:$ftype $grpfile >$Tmpfile
             if (! -z $Tmpfile ) then
                log " "
                log "=== Groupfile $grpfile "
             endif
        
             set nline = `cat $Tmpfile | wc -l`   # Count lines
             @ iline = -9         # Means 1 after first increment by 10
             while ($iline < $nline)              # Lines left?
              @ iline = $iline + 10               # Next series of 10
              foreach wfile ( `tail +$iline $Tmpfile | head -10` )

               if ("$wfile" =~ -* || "$wfile" =~ +* ) then
                  set Options=( local  $wfile ); 
                  source $n_src/sys/switches.csh
               else 
                set Flag=`grep '^'$wfile'$' $n_work/continue.idx`
                if ("$Flag" != "") then
                  log "Skipping $wfile (already done)"
                else if ($wfile =~ abpx_*.exe && ! $?n_doabp ) then
                  log "Not compiling abp-executables"
                else
                  set Input_file=$wfile
                  if ($grpfile == $n_import/$grpfile:t || \
                      $grpfile == $n_work/depend.grp ) then
                     set Newlib=$wfile:h
                     set Newlib=$Newlib:t
                     if ("$Newlib" =~ n*) set Newlib=nst
#
# If we take a file from $n_import, we have to increase the version number
#
                     if ($cwd == $n_import && -e $n_import/$wfile:t) then
                        set Input_file=$n_import/$wfile:t
                        if ("$O_Version" == "$C_Version") set C_Version=$N_Version
                     endif
                  endif
#
# If this file is in different library, handle pending library operations
#
                  if ($n_lib/${Newlib}lib.olb != $_Objectlib ) then
                     log "Library: $_Objectlib:t -> ${Newlib}lib.olb"
                     if ("$_Objectlib" != "") \
                        set Input_file=( $_Objectlib $Input_file )
                     set _Objectlib=$n_lib/${Newlib}lib.olb
                  endif
                  if ($n_src/${Newlib}lib.olb != $_Textlib ) then
                     if ("$_Textlib" != "") then
                        set Input_file=( $n_src/$_Textlib   $Input_file )
                        set _Textlib=$n_src/${Newlib}lib.tlb
                     endif
                  endif
#
#  Compile, check dependencies and restore original switches
#
                  source $n_src/sys/compile.csh  
                  if ($?Abort_flag) goto Abort_exit
                  echo $wfile >>$n_work/continue.idx

                  set wfile=$wfile:t
                  if ("$wfile:e" == "dsc") then
                     grep "@$wfile:r" $n_src/sys/database.idx  >> $n_work/depend.grp
                  else
                     grep "@$wfile"   $n_src/sys/database.idx  >> $n_work/depend.grp
                  endif

                  if ("$Save_switch" != "") set $Save_switch; 

                endif
               endif
              end
             end

             if (-e $Tmpfile) then
                'rm' -f $Tmpfile
             endif
          end
          log "Error so far: $Errors ("`date`")"
#
# Handle any pending library actions
#
          if ("$_Objectlib" != "") then
             set Input_file=$_Objectlib
             source $n_src/sys/compile.csh
          endif
          if ("$_Textlib" != "") then
             set Input_file=$_Textlib
             source $n_src/sys/compile.csh
          endif

         endif        # If exe and errors
       end
#
# Check any pending dependencies
#
       echo "Checking pending dependencies..."
       sort -u -o $n_work/depend.grp $n_work/depend.grp
       $n_exe/genaid.exe expand  $grpfile >$Tmpfile
       set nline = `cat $Tmpfile | wc -l`   # Count lines
       @ iline = -9         # Means 1 after first increment by 10
       while ($iline < $nline)              # Lines left?
        @ iline = $iline + 10               # Next series of 10
        foreach wfile ( `tail +$iline $Tmpfile | head -10` )
          if ("$wfile" !~ -* && "$wfile" !~ +* ) then
             set Flag=`grep '^'$wfile'$' $n_work/continue.idx`
             if ("$Flag" == "") echo "Error: remaining dependency $wfile"
          endif
        end
       end

       log "Total number of errors: $Errors"
       log "Logging information in $Logfile"

#
#
#  Compilation errors
#
       if ($Errors != 0 ) then
          log "Compilation errors...."
          if ($cwd == $n_import && $_Update && $_Merge) then
             log "Update cannot move files into \$n_src..."
          endif
#
#  Not in import, no need to modify source tree
#
       else if ($cwd != $n_import) then
          log "No compilation errors (compilation in source tree)."
#
#  -NUpdate or -NMerge was used, do not update files in the master tree
#
       else if (! $_Update) then
         log "You used -NUpdate, the master source tree remains intact"
       else if (! $_Merge) then
         log "You used -NMerge, the master source tree remains intact"
#
#  If it was an update, no errors occurred, and we were working in $n_import,
#  then files can be moved in the master source tree. 
# 
       else
#
#  Remove references to groupfiles not in $n_import
#
         set Flag=""
         foreach grpfile ( $Files )
           if ( -e $n_import/$grpfile:t) set Flag=( $Flag $grpfile )
         end
         set Files=( $Flag )
#
#  Move files in the master (if that is still necessary)
#
         @ files_moved = 0
         foreach grpfile ( $Files )
            $n_exe/genaid.exe files -t:^exe $grpfile >$Tmpfile
            set nline = `cat $Tmpfile | wc -l`   # Count lines
            @ iline = -9         # Means 1 after first increment by 10
            while ($iline < $nline)              # Lines left?
              @ iline = $iline + 10               # Next series of 10
              foreach file ( `tail +$iline $Tmpfile | head -10` )
#
#  If the file does no longer exist in $n_import is has been moved in 
#  $n_src earlier, so no need to worry.
#
                 if (-e $n_import/$file:t ) then
                    @ files_moved = $files_moved + 1
                    if ("$file:h" != "" && ! -d "$n_src/$file:h") then
                       mkdir $n_src/$file:h
                       echo "Created subdirectory $file:h"
                    endif
                    set Flag=$file:t
                    if (-e $n_ulib/${Flag:r}.o) then
                       'rm' -f $n_ulib/${Flag:r}.o
                    endif
                    if (-e $n_lib/${Flag:r}.o) then
                       'rm' -f $n_lib/${Flag:r}.o
                    endif
                    if (-e $n_src/$file) then
                       mv $n_src/$file $n_src/$file.old
                    endif
                    cp $n_import/$file:t $n_src/$file
                    set Flag=(`cmp $n_import/$file:t $n_src/$file`)
                    if ("$Flag" != "") then
                       log "Error moving ${file:t}: $Flag"
                       @ Errors = $Errors + 1
                       if (-e $n_src/$file.old) then
                          mv $n_src/$file.old $n_src/$file
                       endif
                    else
#
# Update database.idx: for NSTAR_DIR check dependencies, else just checksum
#
                       cp $n_src/sys/database.idx $n_work/database.old
                       grep -v $file $n_work/database.old      \
                                     >$n_src/sys/database.idx
                       set dir=$file:h
                       if ("$NSTAR_DIR" =~ *$dir:t* ) then
                          $n_exe/genaid.exe fstat -i -c $n_src/$file \
                                    >>$n_src/sys/database.idx 
                       else
                          $n_exe/genaid.exe fstat -c $n_src/$file \
                                     >>$n_src/sys/database.idx 
                       endif
                       if (-e $n_src/sys/lock.idx) then
                         set Lock=(`grep $file $n_src/sys/lock.idx`)
                         if ("$Lock" =~ *imported*) then
                            cp $n_src/sys/lock.idx $n_work/lock.old
                            grep -v $file $n_work/lock.old >$n_src/sys/lock.idx
                            'rm' -f $n_work/lock.old
                         endif
                       endif
                       'rm' -f $n_import/$file:t
                       log "$file updated in master tree and database."
                    endif
                 endif
              end
            end
            'rm' -f $Tmpfile
         end
#
#
# Errors moving files into the master, indicate with revision number
#
         if ($Errors != 0 ) then
            log "Errors moving files into the master..."
            echo "*** Incomplete revision *** " >>$n_src/sys/version.idx
#
# We moved files into the master, so we have a new revision. 
#
         else if ($files_moved != 0 ) then
            if ("$O_Version" == "$C_Version") set C_Version=$N_Version
            mv $n_src/sys/version.idx          $n_src/sys/version.idx.old
            echo "Newstar Release $C_Version" >$n_src/sys/version.idx
#
# Flag a local revision
#
            if ($n_site != nfra) then
               if (`cat $Files | grep -c +sys/version.idx ` == 0) \
                   echo "*** Local revision ***" >>$n_src/sys/version.idx
#
#
# If this is the NFRA master, update revision history and nnews.hlp
#
            else
               cat >>$n_root/updates.html <<_EOD_

<DT>$C_Date <STRONG>`cat $n_src/sys/version.idx`</STRONG>
_EOD_
               echo "1 NNews"  >$Tmpfile


#
# Clear the file for commands to be handled by bugreport
#
               if (-e ~/server/bugreport.in) then
                  'rm' -f ~/server/bugreport.in
               endif

               foreach grpfile ( $Files ) 
                 set Flag="File $grpfile:t updated"
                 if (`grep -c "$Flag" $n_root/updates.log` == 0) then
                    echo "$C_Date $C_Time - $n_arch - $Flag" >>$n_root/updates.log
#
# Add it to the index of updates
#
                    echo \
"<DT>$C_Date <A HREF=/nsbin/nview/import/$grpfile:t>$grpfile:t</A>" \
                        >>$n_root/updates.html 

                    grep 'Subject:' $grpfile | sed -e 's/\!.*Subject:/<DD>/' \
                        >>$n_root/updates.html 
#
# Update nnews and add it to the grpfile
#
                    echo -n " $C_Date"  >>$Tmpfile
                    grep Subject: $grpfile | sed -e 's/\!.*Subject://' >>$Tmpfile
                    echo "+doc/nnews.hlp" >>$grpfile
#
#  Check wether it solved a bug, if so: make input file and save number
#
                    if (`grep -c -e '- bug ' $grpfile` == 1) then
                       set Bug=( `grep -e '- bug ' $grpfile` )
                       if ("$Bug[$#Bug]" !~ [0-9]*) then
                          log "Invalid bug-id in $Bug"
                       else 
                          cat >> ~/server/bugreport.in <<_EOD_
Release $Bug[$#Bug]
Update revision $C_Version
import/$grpfile:t
n
_EOD_
                       endif
                    endif

                 endif      # if (first update)
               end          # foreach grpfile ()
#
#  Wait for the server to release the bugs, if any
#
               if (-e ~/server/bugreport.in) then
                  echo "quit"      >> ~/server/bugreport.in
                  echo "y"         >> ~/server/bugreport.in
                  echo "bugreport"  > ~/server/server.cmd 
                  log "Waiting for Export-Server to release bugs"
                  while (-e ~/server/server.cmd)
                    sleep 30
                  end
                  'rm' -f ~/server/bugreport.in
                  if (-e ~/server/server.err) then	# something wrong ?
                    if (! -z ~/server/server.err) then	# yes
                      log "Something went wrong with NSERVER"
	  	      cat ~/server/server.err | \
			nsmail "NSERVER error" $n_master
		    endif
		    'rm' -f ~/server/server.err
		  endif
               endif
#
# Append the remainder of nnews.hlp and put the new version in the system
#
               tail +2l $n_src/doc/nnews.hlp >>$Tmpfile
               mv $n_src/doc/nnews.hlp $n_src/doc/nnews.hlp.old
               mv $Tmpfile $n_src/doc/nnews.hlp
               'rm' -f $Tmpfile
#
#   We changed files in the master, so update the database
#
               set Input_file=( $n_src/sys/version.idx $n_src/doc/nnews.hlp )
               source $n_src/sys/compile.csh
               cp $n_src/sys/database.idx $n_work/database.old
               grep -v +doc/nnews.hlp $n_work/database.old | \
                 grep -v +sys/version.idx >$n_src/sys/database.idx
               $n_exe/genaid.exe fstat  \
                    $n_src/sys/version.idx $n_src/doc/nnews.hlp \
                                         >>$n_src/sys/database.idx 
#
# Compose a mail message about this fresh release. 
#
               set Flag="Newstar Release $C_Version"
               cat <<_EOD_ >message.$C_Version

From: The Newstar Master account
To:   All Friends of Newstar

Concern: $Flag


                                                       Dwingeloo, $C_Date

Dear Friends of Newstar,

A new Newstar revision has been installed in the Master system at NFRA:

_EOD_
               cat $Files | grep Subject: | sed -e 's/\!.*Subject:/=== /' >>message.$C_Version
               cat <<_EOD_ >>message.$C_Version

To upgrade your installation, login as the Newstar manager, initialise
the Newstar environment (e.g. source ~newstar/src/sys/newstar_????.csh) 
and enter:

       nup update

and follow the instructions given by that command.

Please direct any problems or questions to  $n_master


Your sincerely,

The Newstar Project Team.
_EOD_


#   Message made, so move groupfiles out of the way
#
               mv $Files             $n_import/old
#
#   Give an oppurtunity to edit the message and the subjects
#     (some people give those weird subjects in their groupfiles...)
#
               emacs $n_src/doc/nnews.hlp message.$C_Version

#
#   We changed files in the master, so update the database
#
               set Input_file=( $n_src/doc/nnews.hlp )
               source $n_src/sys/compile.csh
               cp $n_src/sys/database.idx $n_work/database.old
               grep -v +doc/nnews.hlp $n_work/database.old  \
                  >$n_src/sys/database.idx
               $n_exe/genaid.exe fstat  \
                    $n_src/doc/nnews.hlp  >>$n_src/sys/database.idx 

               clear
               cat <<_EOD_ | tee $Tmpfile

Newstar Revision $C_Version has been succefully merged in the 
NFRA-Master at $C_Date $C_Time.

Warning: no notification has been sent to the Friends of Newstar
Warning: the ftp-area has not been updated

You should give command   nup release   later to send out this release

_EOD_
               cat $Tmpfile | \
                 nsmail "Pending release $C_Version from $C_Date" $n_master
               'rm' -f $Tmpfile

            endif          # if (nfra)
         endif             # if (files_moved)
       endif               # if (in import)
#
#
# Errors occurred, give a message (different for nfra and elsewhere)
#
       if ($Errors != 0 ) then
          if ("$n_site" == nfra) then
             cat <<_EOD_ | tee -a $Logfile

Errors during execution of $Cmd

Libraries and \$n_inc have most probably been cluttered. 
Either correct the errors and try same command again,
or reconstruct the libraries etc. (only the potentially damaged files):
    cd \$n_src; nup build \$n_import/$grpfile:t

_EOD_
          else
             set Flag=(`cat $n_src/sys/version.idx`)
             cat <<_EOD_ | tee -a $Logfile

*************** Installation errors occured **********************

The log-file will be mailed to  $n_master.

Please inform this account of additional information that might be
connected with the errors (recent change of operation system, disk
space problems etc). The Newstar group will contact you about the 
problems as soon as possible.

Your present executables are still correct. 
You seem to have $Flag

*****************************************************************

_EOD_
            cat $Logfile | \
              nsmail "Newstar_crash_on_$n_site/$n_arch" $n_master 
            set Mode="Quit"
          endif
#
#  No errors: inform NFRA if run on remote site
#
       else if ("$n_site" != nfra) then
          nsmail "Newstar_update_on_$n_site/$n_arch" $n_master <<_EOD_

Newstar has been updated on $n_site ($n_arch) at ${C_Date}:

    $Cmd

The current version at $n_site is now:

`cat $n_src/sys/version.idx`

Yours truly,

update.csh
_EOD_

       endif
#
#
#  %Release updates the Export-Master and informs the rest of the world
#
    else if ("$Command" =~ [Rr][Ee][Ll]* ) then
       if (! $?n_ftp || "$n_site" != nfra) then
         log "Error: can only release things from NFRA Master"
       else
#
#  Enforce working in $n_import
#
         if ($PWD != $n_import) then
            cd $n_import
            echo "--> Now in directory $n_import"
         endif
#
#  Make sure we have a proper database and documentation archive
#
         mv $n_work/update.lock $n_work/update.lock.$$
         $n_src/sys/update.csh check h,d
         mv $n_work/update.lock.$$ $n_work/update.lock
#
#  Pack fresh archives
#
         cd $n_work                 # Safe place to make archives
         log "Making fresh archives..."
         mv $n_work/update.lock $n_work/update.lock.$$
         $n_src/sys/update.csh pack server
         mv $n_work/update.lock.$$ $n_work/update.lock 
#
#  Tell nserver what files to unpack 
#
         set Files=( nstar_hlp.tgz nstar_src.tgz )
         set Flag=( `echo $n_install | tr ',/:' '   ' ` )
         foreach aa ( $Flag )
            set Files=( $Files nstar_src_$aa.tgz )
         end
         echo unpack $Files >~/server/server.cmd
         log "Waiting for server to update Export-Master"
         while (-e ~/server/server.cmd) 
           sleep 30
         end
         if (-e ~/server/server.err) then	# something wrong ?
            if (! -z ~/server/server.err) then	# yes
              log "Something went wrong with NSERVER"
	      cat ~/server/server.err | \
		nsmail "NSERVER error" $n_master
	    endif
	    'rm' -f ~/server/server.err
	 endif
         cd $n_import               # Back to import
#
#  Now check if we have to let them know...
#
         set nonomatch
         set Flag=(message.*)
         if (! -e $Flag[1]) then
            echo "No outstanding releases..."
         else
#
#   Show messages for convenience
#
           foreach File ( $Flag ) 
               echo ""
               echo "Message file ${File}:"
               echo "---------------------------------"
               grep "===" $File
           end

#
#   If more than one release pending, modify last one
#
           if ($#Flag > 1 ) then
             if (-e $Tmpfile) then
                'rm' -f $Tmpfile
             endif
             cat $Flag[$#Flag] | awk \
               '{ if (done==0 && $1 != "===") print $0; else if ($1 == "===") done=1; }' \
               >$Tmpfile
             grep -h "===" $Flag >>$Tmpfile
             cat $Flag[$#Flag] | awk \
               '{ if (done==1 && $1 != "===") print $0; else if ($1 == "===") done=1; }' \
               >>$Tmpfile

             emacs $Tmpfile
             mv $Tmpfile $Flag[$#Flag]
           endif
#
#   Send mail, this relies on an elm alias  friends_of_newstar !!!!!
#
           echo "Sending out $Flag[$#Flag]"
           nsmail "`cat $n_src/sys/version.idx`" friends_of_newstar <$Flag[$#Flag]
#
#   Message sent, so move message-file(s) out of the way
#
           mv $Flag $n_import/old
         endif
         unset nonomatch

       endif     # Test if allowed to release
#
#
#  %Retrieve files over the network
#
    else if ("$Command" =~ [Rr][Ee][Tt]* ) then
#
#  Enforce working in $n_import
#
       if ($PWD != $n_import) then
          cd $n_import
          echo "--> Now in directory $n_import"
       endif
#
# First argument: groupfile to retrieve
#
       if ("$Files" != "") then
          set grpfile=$Files[1]
          set Files[1]=""; if ($#Files > 1) shift Files
       else
          set grpfile="all"
       endif

       set do_check=1
       if ($grpfile =~ [Aa][Ll][Ll] || $grpfile =~ [Aa]) then
          if ($grpfile =~ [Aa]) set do_check=0
          set grpfile=sys/database.idx
          if (-e database.idx) then           # Remove old version if any
             'rm' -f database.idx
          endif
       endif
#
# Remaining arguments: internet address
#
       set tmp=( $n_remote )
       set iaddr=$tmp[1]
       set iuser=$tmp[2]
       set iroot=$tmp[3]
       unset tmp

       set noglob
       set Files=( $Files "" "" "" )               # Make sure they exist
       if ("$Files[1]" != "") set iaddr=$Files[1]
       if ("$Files[2]" != "") set iuser=$Files[2]
       if ("$Files[3]" != "") set iroot=$Files[3]
       unset noglob
#
# Get password (we do not put that in any file!)
#
       set ipass=""
       if (${iuser} == anonymous) then
          set ipass="${USER}@`domainname`"
       else
          while ("${ipass}" == "")
             echo -n "Enter password for ${iuser}: "
             stty -echo; set ipass=($<); stty echo; echo "xyz1jkl"
          end
       endif
       if ($n_site == nfra) then
          set tmpcd=" "
          set tmpgf=" "
       else
          set nuse=newstar_use_${C_Date}${C_Time}.$n_site
	  if ($n_site == wsrt && $iuser == newstar) then
            set tmpcd="cd /home/rzmws0/hjv/newstar/use"
	  else
            set tmpcd="cd /pub/incoming"
	  endif
          set tmpgf="put newstar.use $nuse"
       endif
#
# Move to $n_import and check wether the file is there, if not get it.
#
       if (! -e $grpfile:t) then
          ftp -n -v -i  $iaddr <<_EOD_
quote user $iuser
quote pass $ipass
ascii
cd $iroot
get $grpfile $grpfile:t
cd ../import
get $grpfile $grpfile:t
$tmpcd
$tmpgf
bye
_EOD_
       endif

       if (! -e $grpfile:t) then
          log "Cannot retrieve $grpfile, try again..."
          set Flag="error"
       else
#
# Create new overview file of use of Newstar
# Save the current one to .old in case of
#
          if ($n_site != nfra) then
            if (-e $n_import/newstar.use.old) then
              'rm' -f $n_import/newstar.use.old
            endif
	    mv $n_import/newstar.use $n_import/newstar.use.old
	    touch $n_import/newstar.use
	    chmod a+rw $n_import/newstar.use
          endif
#
# Find differences between NFRA and local database
#
         if ($grpfile == sys/database.idx ) then
            if ($do_check) then
              log "Creating fresh local database..."
              if (-e $n_work/database.idx) then
                 'rm' -f $n_work/database.idx
              endif
              grep '\.exe' $n_import/database.idx >$n_work/database.idx

              set nonomatch
              foreach dir ( $n_src/* )
                if ( -d $dir ) then
                   log "Scanning $dir..."
#
# If in NSTAR_DIR, check dependencies, else just checksum
#
                   if ("$NSTAR_DIR" =~ *$dir:t* ) then
                      $n_exe/genaid.exe fstat -i -t:^exe @ $dir/*.grp \
                                  >>$n_work/database.idx
                   else
                      $n_exe/genaid.exe fstat -t:^exe @ $dir/*.grp \
                                  >>$n_work/database.idx
                   endif
                endif
              end
              cp $n_work/database.idx $n_src/sys/database.idx
              unset nonomatch
            endif

            log "Comparing local database and database from $iaddr"
            set tmp=$grpfile:t
            set grpfile=retrieved.grp
            $n_exe/genaid.exe compare -t:^exe $tmp $n_src/sys/database.idx >$Tmpfile
            grep -v "lock.idx" $Tmpfile  >$grpfile
            'rm' -f $Tmpfile
            log "Differences are in $grpfile"
         endif
         set grpfile=$grpfile:t
         set Flag="ok"
       endif
#
# General log
#
       echo "$C_Date $C_Time - $n_arch - retr $grpfile:t $iaddr $iroot $iuser " >>$n_root/updates.log
#
#
# At nfra: check locks, if anything is locked we should have a problem
#
       if ("$Flag" == "ok" && $n_site == nfra) then
            echo ""
            echo "Checking locks..."
            $n_exe/genaid.exe files -t:^exe $grpfile > $Tmpfile
            if (-e $n_src/sys/lock.idx) then
               set nline = `cat $Tmpfile | wc -l`   # Count lines
               @ iline = -9         # Means 1 after first increment by 10
               while ($iline < $nline)              # Lines left?
                 @ iline = $iline + 10              # Next series of 10
                 foreach file ( `tail +$iline $Tmpfile | head -10` )
                   set Lock=(` grep $file $n_src/sys/lock.idx `)
                   if ("$file" != "+doc/nnews.hlp" && "$Lock" != "") then
                      echo "Warning: $Lock"
                      set Flag="lock"
                   endif
                   unset Lock
                 end
               end
            endif

            if ($Flag == "lock") then
               cat <<_EOD_

Found files that were locked, cannot retrieve.
Please check and edit \$n_src/sys/lock.idx

_EOD_
            else 
#
# Remove old locks, make lock for Newstar manager
#
              echo ""
              echo "Making locks for files to be retrieved..."
              if (-e $n_src/sys/lock.idx) then
                 set nline = `cat $Tmpfile | wc -l`   # Count lines
                 @ iline = -9         # Means 1 after first increment by 10
                 while ($iline < $nline)              # Lines left?
                   @ iline = $iline + 10              # Next series of 10
                   foreach file ( `tail +$iline $Tmpfile | head -10` )
                     echo \
          "+$file locked User=Newstar Date=$C_Date/$C_Time" \
                                 |  tee -a  $n_src/sys/lock.idx
                   end
                 end
              endif
            endif
       endif
#
#
#  Confirm retrieval
#
       if ("$Flag" == "ok") then
          cat $grpfile | tee -a $Logfile
          if ("$Mode" == "Update") then
             set Flag="Yes"
          else
             echo -n "Retrieve these files (y,n)? [y] "
             set Flag=($<)
          endif
       endif

       if ("$Flag" == "" || "$Flag" =~ [Yy]* ) then
#
# Try to get them three times, if all files received, quit as well
#
            @ ii = 3
            while ( $ii > 0 )
#
# Retrieve contents of groupfile, skip files have been retrieved correctly
#
              cat <<_EOD_ >$Tmpfile
quote user $iuser
quote pass $ipass
cd $iroot
_EOD_
#
# With -Import switch, search files in remote $n_import, else below remote $n_src
#
              if ($_Import) then
                 echo "cd ../import" >>$Tmpfile
                 $n_exe/genaid.exe import -i -t:^exe $grpfile >>$Tmpfile
              else
                 $n_exe/genaid.exe import -c -t:^exe $grpfile >>$Tmpfile
              endif
              echo bye >>$Tmpfile

              if ( "$ii" != "3") then
                 log ""
                 log "Trying again ...."
              endif
              ftp -n -v -i  $iaddr <$Tmpfile | tee -a $Logfile
              'rm' -f $Tmpfile

              log ""
              log "Checking retrieved files... "
              $n_exe/genaid.exe check -c -t:^exe $grpfile >$Tmpfile
              if ( -z $Tmpfile ) then
                 @ ii = -999
                 log "All files received correctly."
              else
                 @ ii = $ii - 1
                 log "Not all files received correctly:"
                 cat $Tmpfile | tee -a $Logfile
              endif
              'rm' -f $Tmpfile
            end
            
            if ( $ii == 0) then
               cat <<_EOD_ | tee -a $Logfile

Not all files were retrieved after 3 tries...
You should try once more:  nup retrieve $grpfile.
This will only retrieve the missing or defect files.

_EOD_
            else
               set Flag="ok"
            endif
       endif
#
#  Tell the user how to proceed
#
       if ("$Flag" == "ok") then
          cat <<_EOD_ | tee -a $Logfile

All files have been correctly retrieved. 

To install the files, please enter:

     nup build $grpfile  -Update 

on the following hosts:  $n_hosts

_EOD_
       else
          cat <<_EOD_ | tee -a $Logfile

*************** Retrieve errors occured **********************

The log-file will be mailed to  $n_master.

Please inform this account of additional information that might be
connected with the errors (recent change of operation system, disk
space problems etc). The Newstar group will contact you about the 
problems as soon as possible.

*****************************************************************

_EOD_
            cat $Logfile | \
              nsmail "Newstar_crash_on_$n_site/$n_arch" $n_master 
            set Mode="Quit"
       endif
#
#
#  %Clean: Make the directory structure consistent with the groupfiles
#
    else if ("$Command" =~ [Cc][Ll]*) then
#
#  Scan only relevant groupfiles in master source tree
#
       set Home=$cwd
       cd $n_src
       echo "--> Now in directory "\$n_src
#
#  Make sure empty temporary files exist
#
       if (-e $Tmpfile.1) then
          'rm' -f $Tmpfile.1
       endif
       touch $Tmpfile.1

       if (-e $Tmpfile.2) then
          'rm' -f $Tmpfile.2
       endif
       touch $Tmpfile.2
#
#  No argument: check all directories and files in $n_src itself
#
       if ("$Files" == "" ) then
         echo "$C_Date $C_Time - $n_arch - clean all  " >>$n_root/updates.log
         log "Scanning all groupfiles ..."
         $n_exe/genaid.exe files -t:^exe */*.grp >$Tmpfile.1
         log "Scanning all files..."
         find * -print >$Tmpfile.2   # Remember we are in $n_src
#
#  Expand master database, find all files below $n_src
#
       else if ($Files[1] =~ [Aa][Ll][Ll]) then
         echo "$C_Date $C_Time - $n_arch - clean all (database) " >>$n_root/updates.log
         log "Scanning all-files database..."
         $n_exe/genaid.exe files -t:^exe $n_src/sys/database.idx >$Tmpfile.1
         log "Scanning all files..."
         find * -print | grep -v 'upd.*\.log' >$Tmpfile.2   
#
#  Expand all groupfiles in directories, find files in those directories
#
       else
         echo "$C_Date $C_Time - $n_arch - clean $Files " >>$n_root/updates.log
         foreach dir ( $Files )
            if (! -e $dir && -e $n_src/$dir ) set dir=$n_src/$dir
            if ( -d $dir ) then
               log "Scanning groupfiles and files in $dir..."
               $n_exe/genaid.exe files -t:^exe $dir/*.grp >>$Tmpfile.1
               find $dir -print >$Tmpfile.2   # Remember we are in $n_src
            endif
         end
       endif
#
#  Sort on filenames
#
       log "Sorting contents of groupfiles"
       sort -u $Tmpfile.1 >$Tmpfile.1.s
       log "Sorting filelist"
       sort -u $Tmpfile.2 >$Tmpfile.2.s
#
#  Make the difference: 
#
#    > file    means it exists but is not in a groupfile
#    < file    means it is in a groupfile but does not exist
#
       log "Comparing..."
       diff $Tmpfile.1.s $Tmpfile.2.s | \
         awk '{ if ($1 == ">" || $1 == "<") print $2}' >$Tmpfile
       'rm' -f $Tmpfile.1 $Tmpfile.1.s $Tmpfile.2 $Tmpfile.2.s
#
#  Verify each file
#
       set Flag=( `echo $n_install | tr ',/:' '   ' ` )  #get machines to do
       set nline = `cat $Tmpfile | wc -l`	# Count lines
       @ iline = -9			# Means 1 after first increment by 10
       while ($iline < $nline)		# Lines left?
        @ iline = $iline + 10		# Next series of 10
        foreach file ( `tail +$iline $Tmpfile | head -10` )
         if (! -d $file) then
            if (-e $file ) then 
               if ("$_Confirm" == 0) then
                  'rm' -f $file
               else if ("$file:e" == ""    || "$file:e" == "log" || \
                   "$file:e" == "tmp" || "$file:e" == "LOG" || \
                   "$file:e" == "lis" || "$file:e" == "old" || \
                   "$file:e" =~ ??    || "$file:e" =~ ?????* ||\
                   "$file" =~ *~ ) then
                  'rm' -f $file
               else
                  echo -n "Remove "; 'rm' -i $file
               endif
               if (-e $file) then
                 log "$file not deleted"
               else
                 log "$file deleted"
               endif 
            else
	       if ("$file:e" =~ x?? || "$file:e" =~ a??) then
		  foreach aa ( $Flag )
		    if ("$file:e" =~ ?$aa) then
		      echo "$file missing ..."
		    endif
		  end
	       else
		  echo "$file missing ..."
	       endif
            endif
         endif
        end
       end
#
#  Back to original directory, clean up
#
      'rm' -f $Tmpfile
       echo "--> Back in $Home"
       cd $Home
       unset Home nonomatch
#
#
#  %Check the current implementation of Newstar against the database
#
    else if ("$Command" =~ [Cc][Hh]*) then
#
#  If no argument given: all
#
       if ("$Files[1]" =~ [Aa][Ll][Ll] || "$Files[1]" == "") set Files="fhdle"
#
#  General log
# 
       echo "$C_Date $C_Time - $n_arch - check $Files " >>$n_root/updates.log
#
#  Verify all files...
#
       if ($Files[1] =~ *f* && -e $n_src/sys/database.idx) then
          log "Checking master source tree against database"
          set grpfile=$n_import/get${C_Date}.grp
          $n_exe/genaid.exe check -t:^exe $n_src/sys/database.idx >$grpfile

          log "" 
          if (-e $grpfile && ! -z $grpfile) then
             log " Created "\$n_import"/get${C_Date}.grp "
             cat $grpfile
             log "" 
             log " Update implementation with: nup retrieve get${C_Date}"
          else
             if (-e $grpfile) then
                'rm' -f $grpfile
             endif
             log " As far as can be checked, you have a proper source tree"
             log " Check for any revisions with: nup retrieve "
          endif
          log ""
       endif
#
#  First update the documentation (15 min.)
#
       if ($Files[1] =~ *h* ) then
          if ("$n_site" == nfra ) then
             echo "Updating documentation (15 min.)"
	     $n_src/sys/document.csh all
             set Files[1]="d$Files[1]"
	  endif
       endif
#
#  Scan all subdirectories of $n_src and build a new database
#
       if ($Files[1] =~ *d* ) then

          if (-e $n_work/database.idx) then
             'rm' -f $n_work/database.idx
          endif
          grep '\.exe' $n_src/sys/database.idx >$n_work/database.idx

          set nonomatch
          foreach dir ( $n_src/* )
            if ( -d $dir ) then
               log "Scanning $dir..."
#
# If in NSTAR_DIR, check dependencies, else just checksum
#
               if ("$NSTAR_DIR" =~ *$dir:t* ) then
                  $n_exe/genaid.exe fstat -i -t:^exe @ $dir/*.grp \
                              >>$n_work/database.idx
               else
                  $n_exe/genaid.exe fstat -t:^exe @ $dir/*.grp \
                              >>$n_work/database.idx
               endif
            endif
          end
          cp $n_work/database.idx $n_src/sys/database.idx
          unset nonomatch
          log "Check current implementation with: nup retrieve "
       endif
#
#  Integrity check on the libraries
#
       if ($Files[1] =~ *l* ) then
          log "Checking libraries..."
          set grpfile=$n_import/lib${C_Date}$n_arch.grp
          if (-e $grpfile) then
             'rm' -f $grpfile
          endif       

          foreach dir ( dwarf nst wng )
            log "   ${dir}lib.olb"
            if (-e $Tmpfile) then
               'rm' -f $Tmpfile
            endif
            if (-e $Tmpfile.1) then
               'rm' -f $Tmpfile.1
            endif       
            if (-e $Tmpfile.2) then
               'rm' -f $Tmpfile.2
            endif       
#
#  For each entry: get name and date
#
            ar tv $n_lib/${dir}lib.olb | awk '\
/.*\.o/ { im=NF-4; id=NF-3; iy=NF-1; \
  if ($im == "Jan") mon=1;           \
  if ($im == "Feb") mon=2;           \
  if ($im == "Mar") mon=3;           \
  if ($im == "Apr") mon=4;           \
  if ($im == "May") mon=5;           \
  if ($im == "Jun") mon=6;           \
  if ($im == "Jul") mon=7;           \
  if ($im == "Aug") mon=8;           \
  if ($im == "Sep") mon=9;           \
  if ($im == "Oct") mon=10;          \
  if ($im == "Nov") mon=11;          \
  if ($im == "Dec") mon=12;          \
  printf("%2.2d%2.2d%2.2d  %s\n",$iy,mon,$id,$NF); }' | sort >$Tmpfile
#
#  Extract relevant part of database
#
            if ($dir == nst) then
               $n_exe/genaid.exe select                            \
                   -t:s\$/cee/cun/c$n_arch/for/fun/fsc/f$n_arch/dsc  \
                   $n_src/sys/database.idx |                       \
                grep '^n.*/'  >$Tmpfile.2
            else
               $n_exe/genaid.exe select                            \
                   -t:s\$/cee/cun/c$n_arch/for/fun/fsc/f$n_arch/dsc  \
                   $n_src/sys/database.idx |                       \
                grep "^${dir}/"  >$Tmpfile.2
            endif
#
#  Does any symbol occur twice?
#
            set nline = `cat $Tmpfile | wc -l`   # Count lines
            @ iline = -9         # Means 1 after first increment by 10
            while ($iline < $nline)              # Lines left?
             @ iline = $iline + 10              # Next series of 10
             foreach file ( `tail +$iline $Tmpfile | head -10` )
#
#  Store the date field for the next file
#
               if ("$file" !~ *.o) then
                  set fDate=$file
               else 
#
#  Find the entry in the database selection (c/fortran file or dsc-file)
#
                 set Flag=( `grep '^.*/'$file:r'\.' $Tmpfile.2` )
                 if ("$Flag" == "" && $file =~ *_bd.o) then
                    set dscfile=`echo $file | sed s/_bd.o/.dsc/`
                    set Flag=( `grep '^.*/'$dscfile $Tmpfile.2` )
                    unset dscfile
                 endif
#
#  If no corresponding file in the database, remove the entry...
#
                 if ("$Flag" == "") then
                     log "No source in database for $file"
                     echo $file  >>$Tmpfile.1
#
#  If multiple entries: remove all entries and write to groupfile
#              
                 else if (`grep -c " ${file:r}\.o" $Tmpfile` != 1) then
                    echo $file  >>$Tmpfile.1
                    log "Multiple entries for $file"
                    echo +$Flag[1]  >>$grpfile
#
#  Check the date of the source file in the database against the library
#
                 else
#
#  If too old, log and write to groupfile
#
                    if ($Flag[2] > $fDate) then
                       echo $file  >>$Tmpfile.1
                       log "Out of date: $file ($fDate) $Flag[1] ($Flag[2])"
                       echo +$Flag[1] >>$grpfile
                    endif
                 endif
               endif
             end        # foreach file
            end 
#
#  For all files in the database: check wether the file is in the library
#
            set nline = `cat $Tmpfile.2 | wc -l`   # Count lines
            @ iline = -9         # Means 1 after first increment by 10
            while ($iline < $nline)              # Lines left?
             @ iline = $iline + 10              # Next series of 10
             foreach file ( `tail +$iline $Tmpfile.2 | head -10` )
               if ("$file" =~ *.* && "$file:e" != "dsc") then
                  set Flag=$file:t
                  set Flag=${Flag:r}.o
                  if (`grep -c " $Flag"\$ $Tmpfile` == 0) then
                     log "$file is not in the archive..."
                     echo +$file >>$grpfile
                  endif
               endif
             end
            end
#
#  Now do all library operations
#
            if (-e $Tmpfile.1 && ! -z $Tmpfile.1) then
               set Flag=(`cat $Tmpfile.1`)
 	       if (! $?ARD) setenv ARD "ar dv"
               log `$ARD $n_lib/${dir}lib.olb $Flag `
            endif
            ranlib $n_lib/${dir}lib.olb

          end          # foreach dir
#
#  Clean up
#
          if (-e $Tmpfile) then
              'rm' -f $Tmpfile
          endif
          if (-e $Tmpfile.1) then
              'rm' -f $Tmpfile.1
          endif
          if (-e $Tmpfile.2) then
              'rm' -f $Tmpfile.2
          endif
#
#  Inform user on repairs
#
          log "" 
          if (-e $grpfile && ! -z $grpfile) then
             log " Created "\$n_import"/lib${C_Date}$n_arch.grp "
             sort -u -o $grpfile $grpfile
             log "" 
             log " Repair libraries with: nup build "\$n_import"/lib${C_Date}$n_arch"
#
#  If Update mode: insert extra command to update the libraries
#
             if ("$Mode" == "Update") then
                set Upd_list=( build $grpfile $Upd_list )
             endif

          else
             if (-e $grpfile) then
                'rm' -f $grpfile
             endif
             log " Libraries seem to be all right"
          endif
       endif
#
#  Check version of executables with respect to database
#
       if ($Files[1] =~ *e* ) then
#
#  Reset error-count, initialise temp-list
#
          @ Errors = 0
          if (-e $Tmpfile) then
             'rm' -f $Tmpfile
          endif
#
#  Rebuild utility programs always
#
          set Input_file=( $n_src/sys/*.c )
#
#  Check any precompiled libraries
#
          set Files=(`$n_exe/genaid.exe files -t:a$n_arch $n_src/*/*.grp`)
          foreach File ( $Files )
          end
#
#  Check any precompiled executables
#
          set Files=(`$n_exe/genaid.exe files -t:x$n_arch $n_src/*/*.grp`)
          foreach File ( $Files )
          end

          source $n_src/sys/compile.csh
#
#  Get list of exe's from groupfiles, check version numbers
#  Assume the list of exe's is less than 500...
#
          set Files=(`$n_exe/genaid.exe files -t:exe $n_src/*/*.grp`)
          foreach File ( $Files )
            if ($File !~ *abpx_* || $?n_doabp ) then
              set v_exe=("" "")
              set v_idx=("" "")
              set File=`echo $File:t | tr '[A-Z]' '[a-z]'`
              set Flag="${File}: No executable"
              if (-e $n_exe/$File) then
                 set Flag =( `grep $File $n_src/sys/database.idx` "" "")
                 set v_idx=( `echo $Flag[2] | awk -F. '{ print $1,$2}' ` "" "" )
                 set Flag =( `what $n_exe/$File | grep %NST% ` "" "" )
                 set v_exe=( `echo $Flag[2] | awk -F. '{ print $1,$2}' ` "" "" )
              endif
              set Input_file=""
              if ("$v_exe" == " " || "$v_idx" == " ") then
                 set Input_file=($File)
	      else if ("$v_exe" == "" || "$v_idx" == "") then
                 set Input_file=($File)  
  	      else 
	         if ("$v_exe[1]" < "$v_idx[1]" || \
                     "$v_exe[2]" < "$v_idx[2]") then
                   set Input_file=($File)
	         endif
	      endif
              if ("$Input_file" != "") then
                 if (`grep -c -i $Input_file $n_src/dwarf/src.grp` != 0) set _Alternate=1 
                 source $n_src/sys/compile.csh
                 set _Alternate=0
                 set Flag =( `what $n_exe/$File | grep %NST% ` '(updated)' )
              endif
              echo $Flag | sed -e s/%NST%// >>$Tmpfile
            endif
          end
#
# Errors occurred, give a message (different for nfra and elsewhere)
#
          if ("$n_site" != nfra) then
             if ($Errors != 0 ) then
                set Flag=(`cat $n_src/sys/version.idx`)
                cat <<_EOD_ | tee -a $Logfile

*************** Installation errors occured **********************

The log-file will be mailed to  $n_master.

Please inform this account of additional information that might be
connected with the errors (recent change of operation system, disk
space problems etc). The Newstar group will contact you about the 
problems as soon as possible.

Your present executables should be still correct. 
Your source tree seems to be $Flag for 
Your executables seem to be:

`cat $Tmpfile`

*****************************************************************

_EOD_
                cat $Logfile | \
                nsmail "Newstar_crash_on_$n_site/$n_arch" $n_master 
                set Mode="Quit"
             else
#
#  No errors: inform NFRA if run on remote site
#
                nsmail "Newstar_update_on_$n_site/$n_arch" $n_master <<_EOD_

Newstar executables have been updated on $n_site ($n_arch) at $C_Date.

The current version at $n_site is:

`cat $n_src/sys/version.idx`

The executables have version:

`cat $Tmpfile`

Yours truly,

update.csh
_EOD_
             endif
          endif
       endif
#
#
#  %Diff: compare files in $n_import with versions in master
#
    else if ("$Command" =~ [Dd]*) then
#
#  Enforce working in $n_import
#
       if ($PWD != $n_import) then
          cd $n_import
          echo "--> Now in directory $n_import"
       endif
#
#  Get groupfile to diff
#
       if ("$Files" == "") then
          echo -n "Enter name of groupfile to diff: "
          set noglob             # Don't expand wildcards right now
          set Files=( $< )       # Read from stdin
          set Files=( $Files )   # Split in multiple words
          unset noglob
       endif
#
#  Expand the groupfile(s) and compare the files 
#
       foreach grpfile ( $Files )
          if ("$grpfile:e" == "") set grpfile=$grpfile.grp
          set dfile=$grpfile.dif
          if (-e $dfile) then
             'rm' -f $dfile
          endif
          echo "Differences introduced by $grpfile"             >$dfile
          echo "Made at $C_Date/$C_Time on $n_site ($n_arch) " >>$dfile

          $n_exe/genaid.exe files -t:^exe $grpfile >$Tmpfile         

          foreach file ( `cat $Tmpfile` )
             if (-e $n_import/$file:t) then
                if (-e $n_src/$file) then
                   echo " "                                     >>$dfile
                   echo "diff $n_import/$file:t  $n_src/$file"  >>$dfile
                   diff $n_import/$file:t $n_src/$file          >>$dfile
                else
                   echo " "               >>$dfile
                   echo "New file: $file" >>$dfile
                endif
             endif
          end
          more $dfile
          log "Differences are listed in $dfile"
       end
#
#
#  %Save is the backup command
#
    else if ("$Command" =~ [Ss]* ) then
       set Tapes=( "A" "B" "C")

       set Home=$cwd
       cd $n_root
       echo " --> Now in directory "\$n_root

       tail backups.txt                                 # Show last backups
       set Flag=`tail -1l backups.txt`                  # Get very last one
       if ("$Flag" == "") set Flag="::"
       set Tape=(`echo $Flag | awk -F: '{ print $2}'`)  # Get last tape
       set Unit=(`echo $Flag | awk -F: '{ print $3}'`)  # Get previous command
       if ("$Tape" == "") set Tape="Unknown"
       if ("$Unit" == "") set Unit="$MAG8"

       @ ii = 1
       while ( $ii < $#Tapes && $Tapes[$ii] != $Tape )
          @ ii = $ii + 1
       end
       if ( $Tapes[$ii] != $Tape ) then
          echo "Unknown tape $Tape..."
          set Tape="$Tapes[1]"
       else if ( $ii == $#Tapes ) then
          set Tape="$Tapes[1]"
       else 
          @ ii = $ii + 1
          set Tape="$Tapes[$ii]"
       endif
       echo "Suggested tape for backup:  ====== $Tape ======"
       echo -n "Tape for backup [$Tape]: "
       set ans=($<); if ("$ans" != "") set Tape="$ans"

       echo -n "Tapeunit for backup [$Unit]: "
       set ans=($<); if ("$ans" != "") set Unit="$ans"

       echo "$C_Date $C_Time - $n_arch - save $Tape $Unit " >>$n_root/updates.log
       echo "${C_Date}/${C_Time}/$n_arch($HOST):${Tape}:${Unit}" | tee -a backups.txt
#
# Backup in background, route output by mail
#
       if ($n_arch == hp ) then
          set Rew="mt -t $Unit rew"
       else
          set Rew="mt -f $Unit rew"
       endif
 
       ( ( tar cf $Unit *; $Rew; tar tf $Unit ) |& \
         nsmail "Backup of Master tree" $USER@`domainname` )&
       
       cd $Home
#
#
#  %Pack: Another archiving command
#
    else if ("$Command" =~ [Pp][Aa][Cc][Kk] ) then
       if ("$Files" == "") then
          echo -n "Enter name of directory (e.g. src, hlp, exe, nscan, exe/sw): "
          set Files=( $< )
          set Files=( $Files )
       endif
       
#
#  Server: make all archives and copy them to the Export-Master
# 
       set do_ftp=0
       if ("$Files" =~ [Ss][Ee][Rr][Vv][Ee][Rr]) then
          set Files="all"
          if (! $?n_ftp || "$n_site" != nfra) then
            log "Error: can only copy to Export-Master from NFRA Master"
          else
            set do_ftp=1
            set ipass=""
            while ("${ipass}" == "")
               echo -n "Enter password for Newstar on ${n_ftp}: "
               stty -echo; set ipass=($<); stty echo; echo "xyz1jkl"
            end
          endif
       endif   

#
# If all files have been asked, check installed architectures from n_install
#
       if ("$Files" =~ [Aa][Ll][Ll]) then
          set Files=( src hlp lib/inc )
          set Flag=( `echo $n_install | tr ',/:' '   ' ` )
          foreach aa ( $Flag )
            set Files=( $Files lib/$aa exe/$aa )
          end
       endif

       foreach dir ( $Files )
         unset Source
         if (-d $n_root/$dir ) then
            set Source=$n_root/$dir
            set tarfile=nstar_$dir.tgz
         else if (-d $n_src/$dir ) then
            set Source=$n_src/$dir
            set tarfile=nstar_src_$dir.tgz
         else if (-d $dir ) then
            log "Can only tar Newstar Master tree directories"
         else
            log "Error: directory $dir does not exist"
         endif
         if ($?Source) then
            set tarfile=`echo $tarfile | tr '/' '_'`
            if ($cwd =~ $n_src/*) then 
               set tarfile=$n_src/$tarfile
            else
               set tarfile=$cwd/$tarfile
            endif
            if ("$Mode" == "Menu") then
               echo -n "Enter name of tarfile [$tarfile]: "
               set tmp=($<)
               if ("$tmp" != "") set tarfile=$tmp
               unset tmp
            endif
            if (-e $tarfile || -e $tarfile) then
               'rm' $tarfile*
               if (! -e $tarfile && ! -e $tarfile) then
                  log "Removed existing $tarfile"
               endif
            endif
            log "Creating tar-file $tarfile"
#
# Tar the files and compress, exclude core, *.tar* *.x?? *.a?? and *.old
#
            set Home=$cwd
            cd $Source
            echo "Ignore any no match messages..."
	    if ($dir == "hlp") then
               ls core */core *\.tar* *\.tgz* *\.*\.tgz */*\.tar* *\.a?? */*\.a?? \
                           *\.old  */*\.old >$Tmpfile

	    else
               ls core */core *\.tar* */*\.tar* *\.*tgz* \.x?? *\.a?? */*\.x?? */*\.a?? \
                           *\.old  */*\.old >$Tmpfile

	    endif
            tar czfX $tarfile $Tmpfile *
            'rm' -f $Tmpfile
            cd $Home
#
# Copy to export master, also copy updates.html here
#
            if ($do_ftp) then
              log "Copying $tarfile to the ftp area..."
	      set tmp=$tarfile:t
              ftp -n -v -i  $n_ftp <<_EOD_
quote user newstar
quote pass $ipass
cd /ftp/newstar
binary
put $tarfile $tmp
cd /www/newstar
ascii
put $n_root/updates.html updates.html
bye
_EOD_
#              'rm' $tarfile
         


   endif
#
# For the source tree, make separate archives for the binaries etc
#
            if ("$Source" == "$n_src") then
               cd $Source
               set Flag=( `echo $n_install | tr ',/:' '   ' ` )
               foreach aa ( $Flag )
                 log "Creating tar-file ${tarfile:r}_$aa.tgz"
                 tar czf ${tarfile:r}_$aa.tgz */*.x$aa */*.a$aa
                 if ($do_ftp) then
                   log "Copying ${tarfile:r}_$aa.tgz to the ftp area..."
		   set tmp=$tarfile:t
                   ftp -n -v -i  $n_ftp <<_EOD_
quote user newstar
quote pass $ipass
cd /ftp/newstar
binary
put ${tarfile:r}_$aa.tgz ${tmp:r}_$aa.tgz
bye
_EOD_
                   'rm' ${tarfile:r}_$aa.tgz
                 endif
               end
            endif   

         endif
       end  
       unset Home tarfile dir
#
#  %Group  Combine or spilt groupfiles
#
    else if ("$Command" =~ [Gg]* ) then

       if ("$Files" == "") then
          echo "Need to specify at least one groupfile"
       else if (`grep -c '^+' $Files[1]` != 0) then
          foreach File ($Files)
            $n_exe/genaid.exe split $File ${C_Date}_c
          end
          ls -l *${C_Date}_c.grp
       else
#
# Make unique name
#
          set grpfile=upd${C_Date}.grp
          @ ii = 0
          while (-e $grpfile)
            @ ii = $ii + 1
            set grpfile=upd${C_Date}$ii.grp
          end
          unset ii 
          echo "\!+$grpfile  combined groupfile made by $USER" >$grpfile
#
# Process all groupfiles
#
          $n_exe/genaid.exe group $Files >>$grpfile
          log "Output is in $grpfile"
       endif 


    else    # Other command
      echo ""
      echo "Error: Invalid or ambiguous command $Command"
      echo ""
    endif   # End of if (Command == ...)

end        # End of while (Menu mode)

Abort_exit:

#
# Handle any pending library actions left after an abort
#
if ("$_Objectlib" != "") then
   set Input_file=$_Objectlib
   source $n_src/sys/compile.csh
endif

if ("$_Textlib" != "") then
  set Input_file=$_Textlib
  source $n_src/sys/compile.csh
endif

if (-e $n_work/update.lock) then
   'rm' -f $n_work/update.lock
endif

if (-e $Tmpfile) then
   'rm' -f $Tmpfile
endif
