#!/bin/csh -f
#set echo
#+
#
#  shadow.csh    
#     CMV 930526	Created
#     CMV 931018	Added link setup, mail for checkin
#     CMV 931020	Changed call to switches
#     CMV 931111	Improved locking
#     CMV 931116	Add abbreviations for executables in checkin
#     CMV 931124	Put and get command are now here,
#			better error log, overwrite existing links
#     CMV 931223	Change handling of libraries
#     HjV 940412	Use $n_ulib iso. $n_lib for use private library
#     CMV 940506	Allow multiple comment lines for checkin, 
#			check locks in advance
#     JPH 940715	Link: replace identical copy of master file by a soft
#			 link (with message)
#     JPH 940720	Bug fix
#     JPH 940721	Remove soft link before trying to make new one
#			Suppress standard "Linked" messages so the important 
#			 ones will clearly stand out
#     JPH 940726	Out: #rm $Target before recreating it as a copy
#			Change cmp checks to use $status because certain 
#			 differences were not detected the old way
#     CMV 940805	Out: really remove existing links, check if 
#			 true file exists, copy from import if file exists 
#			 there; In: ask questions at the end of checking
#			 all files to save typing in case of locks, overwrite
#			 files in import owned by same User.
#     CMV 940811	Out: lock for present out-checker also if not
#			 person who did the import
#     CMV 940829	Keep copy of all imported files in $n_import/backup
#     JPH 940916	Preserve file date in checkout copy
#     CMV 941102	Copy to ftp-area at checkin, test grpfile in import/old
#     CMV 941122	Checkin: Also test groupfile in $n_import/old for rm
#     HjV 9601021	In UNLOCK-part: Add directory when making link to file
#			Lock file with correct name (not with UNKNOWN)
#     JPH 960327	Fix HjV 9601021 so it will work correctly with
#			 second-level subdirectories (as in the doc tree)
#     JPH 960808	Exit with nonzero status on compilation failure
#     HjV 970408        Fix problem with anounymous ftp: wrong passwd
#
#
#  shadow.csh  Interface to shadow system commands
#
#  Use "shadow help" for information on usage
#
#
#-

onintr Abort_exit
#
# Check environment, set defaults
#
source $n_src/sys/initcompile.csh

#
#  Decode switches, get command or set menu mode if none given
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

while ( "$Mode" != "Quit")
   
   if ( "$Mode" == "Menu" ) then
      echo "Valid commands are: help, build, link, get, put, quit"
      echo "                    out=checkout, in=checkin, unlock."
      echo -n "Enter a command: "
      set Command=($<)
      set Files=""
      set noglob; set Command=( $Command ); unset noglob
      set Options=""; source $n_src/sys/switches.csh
      if ($#Command > 1) then
         set noglob; set Files=( $Command[2-] ); unset noglob
         set Command=$Command[1]
      endif
   else
      set Mode="Quit"
   endif

   if ("$Command" == "" || "$Command" =~ [Qq]*) then
      set Mode="Quit"

   else if ("$Command" =~ [Hh]* ) then
      cat <<_EOD_
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

  shadow can be called in one of the following ways:


   shadow

      Without options, enter menu mode, which
      prompts you for commands and filenames

   shadow  build  [switches | file] 

      Compile one or more files. All files have to be fully 
      specified (that is either full pathname or the path with
      respect to current directory).
      If no files are present, their names are read from the 
      standard input.

      You may specify groupfiles prefixed by an @-sign, in 
      which case the contents of the file will be processed.

      Switches are specified like -Debug,Alternate,List,Print and
      may be abbreviated. For more information, give switch -Help 
      All switches on the command line are applied to all files. 
      Switches appearing within a groupfile are only applied to 
      the file after which they appear.

    shadow link [setup | dir ...]

      Create a shadow tree starting at the current directory.
      You should have set $n_usrc already.

      If you do not specify a directory, and you are in $n_usrc, the
      standard Newstar program subdirectories (not sys and doc etc)
      will be shadowed. If you do not specify a directory and you are
      in a subdirectory of $n_usrc, that directory will be shadowed.
      If you do specify a directory, and that directory is a subdirectory
      of $n_src, the specified directory will be shadowed.

  NOTYET    If one or more groupfiles are given, only the files listed
  NOTYET    therein will be shadowed.

      If the argument setup is given, any missing shadow top-directories
      are created and a shadow copy of the include files is made.


    shadow checkout [switches | file] ...
    shadow out      [switches | file] ...

      Copy one or more files to the current directory, 
      leaving a lock in the locking database.
      Groupfiles prefixed with an @-sign will be replaced
      by their contents. If no files are present, their names 
      are read from the standard input.
      If the file does not exist but is found in a textlibrary, 
      it will be extracted.

    shadow checkin  [switches | file] ...
    shadow in       [switches | file] ...

      A groupfile will be created containing the specified files,
      with filesize and revision date included. If files are spread 
      over multiple directories, they should either root in $n_src or 
      in $n_usrc. Otherwise you will be prompted for the directory 
      where the files should land in the master system.
      Filenames appear in the groupfile with their respect to \$n_src.
      If run from the Master, the groupfile is created in \$n_import
      If run from a user system, the groupfile and all files 
      mentioned in it will be copied to \$n_import unless they were
      locked by another user. Any existing locks are removed after
      the copy, and new locks for the Newstar manager are made.


    shadow unlock [switches | file] ...

      Unlock one or more files that have been previously checked out.
      Unlocking means that the user's file is replaced by a link, and 
      that the lock is removed from the locking database.
  |   Future extensions might involve switches to check for differences.

      NB: unlocking is different from checking files in!!!
          checkin involves making a groupfile and copying files
          to $n_import. The files get a new lock for the Newstar 
          manager. Files are unlocked after the Newstar manager moved 
          the updated files into the master source tree.
    

     shadow put [groupfile] ...

         put all files in the groupfiles in the corresponding tlb

     shadow get [groupfile] ...
        
         retrieve files from a tlb's corresponding to the groupfiles.


#-------------------------------------------------------------------------#

_EOD_

#
#
#  Build, Checkout, Checkin and Unlock all need a filelist
#
   else if ("$Command" =~ [Bb]* || \
            "$Command" =~ [Cc]*[Oo][Uu][Tt] || \
            "$Command" =~ [Oo]* || \
            "$Command" =~ [Cc]*[Ii][Nn] || \
            "$Command" =~ [Ii]* || \
            "$Command" =~ [Uu]* ) then
#
#  For checkin, we first need to do some checks and setup
#
     if ("$Command" =~ [Cc]*[Ii][Nn] || \
         "$Command" =~ [Ii]*) then
#
#  Find out in which directory files should end up:
#       
       set Lock_found=0
       if ($cwd == $n_usrc || $cwd == $n_src || $cwd == $n_import) then
          set Target=""
       else if ($cwd:h == $n_usrc || $cwd:h == $n_src) then
          set Target="${cwd:t}/"
       else
          echo "You are not in a Master or user source tree."
          echo "Specify the directory in the Master source tree"
          echo "below which the files should finally end up."
          echo -n "Enter name of subdirectory (eg nscan, doc): "
          set Target=$<
          set Target="$Target/"
       endif
       echo ""
#
# Create a temporary output file
#
       cat  <<_EOD_       >$Tmpfile.in
! 
!${Target}File                      Size    Chsum  YYMMDD
! 
_EOD_

     endif
#
#
#  If no files given, read from input
#
     unset Read_files
     if ("$Files" == "") set Read_files

#
#  Process all files from the commandline or read up to an empty line
#
     while ("$Files" != "" || $?Read_files )

       if ( "$Files" == "" && $?Read_files ) then
          echo -n "Enter filename: "
          set noglob             # Don't expand wildcards right now
          set Files=( $< )       # Read from stdin
          set Files=( $Files )   # Split in multiple words
          unset noglob
          if ("$Files" == "") then
             unset Read_files
             set Files=( "" )    # Make sure Files[1] exists
          endif
       endif 
#
#  Though this test may seem a bit queer at first sight, it prevents
#  us from "long word" errors if $Files contains more than some
#  thousand characters. The other way to get around this is by using
#  a foreach, but if so, we cannot push expanded groupfiles on $Files
#
       while ( "$Files[1]" != "")
#
#  Get the first word from $Files, make lowercase and remove from list
#
         set noglob; 
         set Input_file=`echo $Files[1] | tr '[A-Z]' '[a-z]'`
         set Files[1]=""; if ($#Files > 1) shift Files 
         unset noglob; 
#
#  If switches/options: process as local options (valid for next file only)
#
         if ("$Input_file" =~ -* || "$Input_file" =~ +* ) then
            set Options=( local  $Input_file ); 
            source $n_src/sys/switches.csh

#
#  If the next file is a groupfile: expand it to it's contents
#
         else if ("$Input_file" =~ @?* ) then
#
#  Remove @-sign and add default extension, expand wildcards
#
             set noglob; 
             set Input_file=`echo $Input_file | tr -d "@" `
             if ("$Input_file:e" == "") set Input_file="$Input_file.grp"
             unset noglob
             set Input_file=( $Input_file )
#
#  Expand wildcards and process all groupfiles. We enter into some 
#  ugly processing here, because I cannot rely on all architectures
#  being able to handle arbitrary long environment variables (though 
#  both Sun and HP get pretty far!). Therefore I only expand the
#  first groupfile, and put any remaining groupfiles back on the list.
#  This should give no delay in the processing, but limits the number
#  of items present in $Files at any given time.
#
             set nonomatch
             if (-e $Tmpfile) 'rm' -f $Tmpfile
             foreach grpfile ( $Input_file )
               if (-e $grpfile ) then
                  if ( ! -e $Tmpfile) then
                     if ($grpfile:h == $grpfile) set grpfile=./$grpfile
                     echo " "
                     echo "======= Expanding groupfile $grpfile ======="
                     $n_exe/genaid.exe expand -t:$_Types $grpfile > $Tmpfile
                  else
                     set Files=( '@'$grpfile $Files )
                  endif
               else
                 echo "Error: cannot find groupfile $grpfile"
               endif
             end
             unset nonomatch grpfile
             if (-e $Tmpfile) then
                if (! -z $Tmpfile) \
                   set Files=( `cat $Tmpfile` $Files )
                'rm' -f $Tmpfile
             endif
#
#  It's an ordinary file, pass to the compiler or other command:
#
         else
#
#
#  Compile the file(s) after wildcard expansion etc.
#
#  Although I cannot really advise it, a user can give -Obj:mylib to
#  move objects into his favourite private library. This is taken care of 
#  by the lines which refer to $_Objectlib and $_Objectlist.
#
#
            if ("$Command" =~ [Bb]* ) then

                if ("$_Objectlib" != "") then
                   if ($_Objectlib !~ $n_ulib/*lib.olb) \
                      set _Objectlib=$n_ulib/${_Objectlib}lib.olb
                   if (-e $n_work/${_Objectlib:t}.list) then
                      'rm' -f $n_work/${_Objectlib:t}.list
                   endif
                   set Input_file=( $Input_file $_Objectlib )
                endif

                if ("$_Textlib" != "") then
                   if ($_Textlib !~ $n_ulib/*lib.olb) \
                      set _Textlib=$n_ulib/${_Textlib}lib.olb
                   if (-e $n_work/${_Textlib:t}.list) then
                      'rm' -f $n_work/${_Textlib:t}.list
                   endif
                   set Input_file=( $Input_file $_Textlib )
                endif

                source $n_src/sys/compile.csh  

                if ($?Abort_flag) goto Abort_exit
#
#
#  Checkout: copy the file(s) from the master tree to a user directory
#
            else if ("$Command" =~ [Cc]*[Oo][Uu][Tt] ||\
                     "$Command" =~ [Oo]* ) then

              if ( $_Select ) then

                foreach File ( $Input_file )
#
#  Locate the file in the master source tree
#
                  echo " "
                  unset Source Target
                  set nonomatch
#
#  File was specified with respect to the top of the Master source tree,
#  and we are in the top of our private tree. Checkin to the corresponding
#  directory in our own tree.
#
                  if ($cwd == $n_usrc && -e $n_src/$File) then
                     set Source=$File 
                     set Target=./$File
#
#  Idem, but not at top of our own source tree, checkin to current directory
#
                  else if (-e $n_src/$File) then
                     set Source=$File 
                     set Target=./$File:t
#
#  File was specified with respect to subdirectory of Master source tree,
#  and we are in the corresponding subdir of our private tree. Checkin
#  to that directory.
#
                  else if ( $cwd:h == $n_usrc && -e $n_src/$cwd:t/$File) then
                     set Source=$cwd:t/$File
                     set Target=./$File
#
#  Just a filename was given, and such a file exists in a subdirectory
#  of the master. Since we are not in our private source tree (would have
#  been catched earlier) we check in to current directory.
#
                 else if ( $File:t == $File && -e $n_src/*/$File) then
                     set Source=( $n_src/*/$File )
                     set Source=`echo $Source[1] | sed -e "s%^$n_src/%%"`
                     if ($cwd == $n_usrc) then
                        set Target=$Source
                     else
                        set Target=./$File
                     endif
#
#  One level deeper
#
                 else if ( $File:t == $File && -e $n_src/*/*/$File) then
                     set Source=( $n_src/*/*/$File )
                     set Source=`echo $Source[1] | sed -e "s%^$n_src/%%"`
                     if ($cwd == $n_usrc) then
                        set Target=$Source
                     else
                        set Target=./$File
                     endif
#
# File was completely specified. If stripping n_src makes any difference,
# the file is in the Master source tree. If not, the file exists 
# somewhere outside the tree, and we are not interested.
#
# If we are somewhere in our own private tree, checkin to the correct
# subdirectory of that tree. Otherwise, checkin to the current dir.
#
                  else if (-e $File ) then
                    set Source=`echo $File | sed -e "s%^$n_src/%%"`
                    if ("$Source" != "$File") then
	                if ($cwd == $n_usrc || $cwd:h == $n_usrc) then
                           set Target=$n_usrc/$Source
                        else
                           set Target=./$File:t
                        endif
                     else
                        unset Source
                        echo "Warning: File $File is not in the Master tree"
                     endif
#
# If anything else fails, try to locate it in a tlb, this only works
# if we are in a subdirectory of the Master.
#
                  else if ($cwd:h == $n_src && -e $n_src/${cwd:t}lib.tlb) then
                     ar xv $n_src/${cwd:h}lib.tlb $File:t 
                     echo "$File:t extracted from tlb"
#
#  Although it seems unlikely regarding our previous effords, 
#  we still may not be able to locate the file...
#
                  else
                     echo "Error: Cannot locate $File"
                  endif

                  unset nonomatch
#
#  If located, remove any existing links, copy and lock the file
#
                  if ($?Source) then
                      if (-e $n_import/$Source:t) then
                         echo "Taking $Source from "\$n_import
                         set Full_source=$n_import/$Source:t
                      else
                         set Full_source=$n_src/$Source
                      endif

                      if (-e $Target) then
                         if ("`ls -F $Target`" =~ *@ )  then
                           'rm' -f $Target
                         else 
			   cmp $Full_source $Target
    			   if ($status) then 
                              echo "File $Target already exists..."
                              diff -b $Full_source:t $Target
                              echo -n "Remove "; 'rm' -i $Target
                           else
                             'rm' -f $Target
                           endif
                         endif
                      endif
#
#  If new file should be created, copy either from source tree or from import
#
                      if (! -e $Target) then
                         echo "checkout: $Source --> $Target"
                         cp -p $Full_source $Target 
                      endif
#
# Check and set the lock
# Removing the old lock also replaces "imported" by "locked" if from import
#
                      if (-e $n_src/sys/lock.idx) then
                        set Lock=(` grep $Source $n_src/sys/lock.idx `)
                        if ("$Lock" != "" && "$Lock" !~ *User=$USER*) then
                          echo "Warning:  $Lock"
                        endif
                        if ("$Lock" == "" || "$Lock" =~ *imported* || \
                                             "$Lock" =~ *User=$USER*) then
                          if ("$Lock" != "") then   # Remove old lock
                            cp $n_src/sys/lock.idx $n_work/lock.old
                            grep -v $Source $n_work/lock.old \
                                   >$n_src/sys/lock.idx
                          endif
                          echo \
         "+$Source locked User=$USER Date=$C_Date/$C_Time" \
                                |  tee -a  $n_src/sys/lock.idx
                        endif
                        unset Lock
                     endif

                  endif  # endif source found

                  unset Target Source
                end
              else
                 echo "You should not checkout $Input_file on $n_arch"
              endif
#
#
#  Unlock files from usersystem
#
            else if ("$Command" =~ [Uu]* ) then

                set Home=$cwd;

                foreach File ( $Input_file )
#
#  If we are somewhere in our private source tree, we can match the 
#  file with respect to the Master source tree, otherwise we just match
#  the name. This removes locks to ALL matching files. In practice, there
#  will be no files with identical names, and it is even less likely that
#  a user will lock out multiple files with the same name, and if so
#  it is not unreasonable to require him to work in a shadow tree...
#
                  if ($cwd == $n_usrc) then
                    if ($File:t == $File && -e $n_src/*/$File) then
                       set Full=( $n_src/*/$File )
                       set Full=`echo $Full[1] | sed -e "s%^$n_src/%%"`
                    else if ($File:t == $File && -e $n_src/*/*/$File) then
                       set Full=( $n_src/*/*/$File )
                       set Full=`echo $Full[1] | sed -e "s%^$n_src/%%"`
                    endif
                  endif
                  if ($File:h != $File) cd $File:h
                  set Full=$cwd/$File:t
                  cd $Home
#
#  If in shadow tree, replace existing file with a link 
#  (if not already a link!), otherwise delete it.
#
                  if (-e $Full && $Full !~ $n_src/* ) then

                    if ("`ls -F $Full`" !~ *@ ) then

                         set nonomatch
                         if (-e $n_import/$Full:t) then
			    cmp $Full $n_import/$Full:t
    			    if ($status) then                			
			       diff -b $Full $n_import/$Full:t
                               echo -n "Remove "; 'rm' -i $Full
                            else
                               'rm' -f $Full
                            endif
                         else if (-e $n_src/*/$Full:t) then
			    cmp $Full $n_src/*/$Full:t
    			    if ($status) then                			
			       diff -b $Full $n_src/*/$Full:t
                               echo -n "Remove "; 'rm' -i $Full
                            else
                               'rm' -f $Full
                            endif
                         else
                            echo -n "Remove "; 'rm' -i $Full
                         endif
                         unset nonomatch

                         if (-e $Full ) then
                            echo "***** Warning: $Full not deleted..."
                         else if ( "$Full" =~ $n_usrc/* ) then
			    set Master = \
				`echo $Full | sed -e "s:${n_uroot}:${n_root}:"`
                            ln -s $Master $Full 
## HjV 9601021 was :        ln -s $n_src/${cwd:t}/$File $Full 
 			    echo "File $File replaced by soft link"
                         else
                                 echo "File $Full has been deleted"
                         endif
                     endif
                  endif
#
#  Remove lock if file previously locked by this user
#
#  Owner of Newstar master source tree can unlock any file
#
                  if (-e $n_src/sys/lock.idx) then
                     set File=`echo $Full | sed -e "s%^$n_usrc/%%"`
                     if ($File == $Full) set File=/$File:t
                     set Lock=(` grep $File $n_src/sys/lock.idx `)
                     if ("$Lock" != "") then
                        if ("$Lock" =~ *User=$USER* || -o $n_src ) then
                           cp $n_src/sys/lock.idx $n_work/lock.old
                           grep -v $File $n_work/lock.old >$n_src/sys/lock.idx
                           'rm' -f $n_work/lock.old
                           echo "Removed: $Lock"
                           if (-e $n_import/$Full:t) echo \
         "$Lock[1] imported User=$USER Date=$C_Date/$C_Time" \
                             |  tee -a  $n_src/sys/lock.idx
                        else
                           echo "Warning: $Lock"
                        endif
                     else
                        echo "$File:t was not locked"
                     endif
                     unset Lock
                  else
                     echo "No locking database"
                  endif
               end

               unset Home
#
#
#  Checkin: write filename to groupfile
#
            else if ( "$Command" =~ [Cc]*[Ii][Nn] || \
                      "$Command" =~ [Ii]* ) then
                set Home=$cwd;

                foreach File ( $Input_file )
#
#  Give the file a full path and make it relative to current dir
#
                  if ($File:h != $File) cd $File:h
                  set Full=$cwd/$File:t
                  set File=`echo $Full | sed -e "s%^$Home/%%"`
                  cd $Home
#
# The file should exist and root in current dir
#
                  if ("$Full:e" == "exe") then
                     echo "Warning: $Full:t ignored, specify exe-files later"
                  else if (! -e $Full) then
                     echo "Error: $Full does not exist..."
                  else if ( $File == $Full && $File !~ $n_src/* ) then
                     echo "Error: File should root in current directory"
                  else
#
# It's there all right, make groupfile entry and check any locks
#
                    if (! -e $n_src/sys/lock.idx) then
                      set Lock=""
                    else
                      set Lock=(`grep +$Target$File $n_src/sys/lock.idx`)
                    endif
#
# If no lock or locked by this user, append to groupfile
#
                    if ("$Lock" == "" || "$Lock" =~ *User=$USER* ) then
                       $n_exe/genaid.exe fstat -t:$_Types +$Target $File \
                              >>$Tmpfile.in
                       echo "$File ok"
                    else
                       set Lock_found=1
                       echo "$Lock"
                    endif
                  endif
                end

            endif  # End of if (command)

            if ("$Save_switch" != "") set $Save_switch   # Restore switches

         endif     # End of if (groupfile) 
       end         # End of while (Files left)

     end   # End of while (Files left)
#
#  For build, report the total number of errors
#
     if ("$Command" =~ [Bb]* ) then
        if ("$Errors" == 0) then
           echo "Congratulations: no errors occurred"
        else
           echo "Total number of errors: $Errors "
        endif
#
#  For checkin, we may want to move all files to $n_import
#
     else if ("$Command" =~ [Cc]*[Ii][Nn] || "$Command" =~ [Ii]* ) then
#
#  If locks where found, doo nothing at all
#
      if ($Lock_found) then
         cat <<_EOD_

Found files that were locked by other users, so you cannot move your files
to \$n_import. Please contact your local Newstar manager...

_EOD_
      else 

#
# Create a groupfile for export
#
        set Flag=$USER
        if ("$USER" == devoscm) set Flag=cmv
        if ("$USER" == noordam) set Flag=jen
        if ("$USER" == wbrouw)  set Flag=wnb
        if ("$USER" == newstar) set Flag=x

        set Outfile=upd${C_Date}${Flag}.grp
        @ ii = 1
        while ( -e $Outfile || -e $n_import/$Outfile || -e $n_import/old/$Outfile) 
           if (! -e $n_import/$Outfile && ! -e $n_import/old/$Outfile) then
              echo -n "Remove old groupfile "; 'rm' -i $Outfile
           endif
           if ( -e $Outfile || -e $n_import/$Outfile || -e $n_import/old/$Outfile) then
              set Outfile=upd${C_Date}${Flag}$ii.grp
           endif
           @ ii = $ii + 1
        end

        echo "\!+ $Outfile" >$Outfile
        cat  <<_EOD_       >>$Outfile
!
! Export of updated files for Newstar ($C_Date $C_Time $n_arch)
! Groupfile created by $USER ($Myname) at $n_site (${HOST})
!
_EOD_
        echo ""
        echo "The comment will show up in nnews later, so please"
        echo "include the names of programs that are affected"
        echo -n "Enter a comment: "
        set Flag=( $< )
        while ("$Flag" != "") 
          echo -n "If this is a bug solution, enter the bug-id here: "
          set Bug=( $< )
          if ("$Bug" != "") set Flag=( $Flag " - bug $Bug " )
          echo "\! Subject: $Flag" >>$Outfile
          echo -n "Enter more comments [no more]: "
          set Flag=( $< )
        end
#
#  Append temporary groupfile
#
        cat $Tmpfile.in >>$Outfile
        'rm' -f $Tmpfile.in
#
#  Append a list of executables
#
        echo 'Possible shorthands: @all, @n[ewstar], @d[warf], @a[bp]'
        echo -n "Enter any executables to be rebuilt (default extension .exe): "
        set Flag=($<)
        if ("$Flag" != "") then
           echo "! "             >>$Outfile
           echo "! Executables " >>$Outfile
           echo "! "             >>$Outfile

           set Flag=( $Flag )
           foreach File ( $Flag )
              set File=$File:t
              if ($File =~ @[Aa][Ll][Ll]) then
                 grep -h '^[^ ]*\.[Ee][Xx][Ee]' $n_src/*/*.grp       >>$Outfile
              else if ($File =~ @[Aa]*) then
                 grep -h '^[^ ]*\.[Ee][Xx][Ee]' $n_src/dwarf/abp.grp >>$Outfile
              else if ($File =~ @[Dd]*) then
                 grep -h '^[^ ]*\.[Ee][Xx][Ee]' $n_src/dwarf/src.grp >>$Outfile
              else if ($File =~ @[Nn]*) then
                 grep -h '^[Nn][^ ]*\.[Ee][Xx][Ee]' $n_src/n*/*.grp  >>$Outfile
              else
                if ("$File:e" != "exe") set File=${File:r}.exe 
                echo "$File" >>$Outfile
              endif
           end
        endif
#
#  Edit the file?
#
        echo "! End of groupfile $Outfile" >>$Outfile
        echo ""
        cat $Outfile
        echo ""
        echo -n "Do you want to edit the groupfile (y,n)? [n] "
        set Flag=($<)
        if ("$Flag" =~ [Yy]*) then
           if ($?EDITOR) then
              $EDITOR $Outfile
           else
              emacs $Outfile
           endif
        endif
#
#  Decide wether or not to copy the files
#
        echo ""
        echo -n "Move files to "\$n_import" (y,n)? [y] "
        set Flag=($<)
        if ("$Flag" !~ [Nn]*) then
#
# Copy the files to n_import (sed returns the filename w.r.t. $cwd)
#  Do not copy if the file resides below $n_src
# Make new locks indicating the thing is in import. 
# Do not lock for Newstar (as was old practice) to allow repeated
# checkins by the same user.
#
           $n_exe/genaid.exe files -t:^exe $Outfile > $Tmpfile

           set remote_import=( $n_remote )
           if (-e $Tmpfile.1) then
              'rm' -f $Tmpfile.1
           endif
           echo "quote user anonymous"           >$Tmpfile.1
           echo "quote pass $USER@`domainname`" >>$Tmpfile.1
           echo "ascii"                         >>$Tmpfile.1
           echo "cd $remote_import[3]"          >>$Tmpfile.1
           echo "cd ../import"                  >>$Tmpfile.1
           echo "put $Outfile $Outfile:t"       >>$Tmpfile.1

           foreach file ( `cat $Tmpfile | sed -e "s%^$Target%%"` )
             if ($cwd != $n_src && $cwd:h != $n_src) then
               echo "cp $file $n_import"
               cp ./$file $n_import
               chmod a+rw $n_import/$file:t
               if ($file:e == csh) chmod a+x $n_import/$file:t
             endif
             echo "put ./$file $file:t"                       >>$Tmpfile.1
             echo "put ./$file backup/${file:t}.${Outfile:r}" >>$Tmpfile.1
           end
           echo "bye" >>$Tmpfile.1
           ftp -n -v -i $remote_import[1]  <$Tmpfile.1
           'rm' -f $Tmpfile.1

           if (-e $n_src/sys/lock.idx) then
             foreach file ( `cat $Tmpfile` )
               set Lock=(`grep $file $n_src/sys/lock.idx`)
               if ("$Lock" != "") then
                  cp $n_src/sys/lock.idx $n_work/lock.old
                  grep -v $file $n_work/lock.old >$n_src/sys/lock.idx
               endif
               echo \
         "+$file imported User=$USER Date=$C_Date/$C_Time" \
                             |  tee -a  $n_src/sys/lock.idx
             end
            'rm' -f $n_work/lock.old
           endif
           'rm' -f $Tmpfile

           echo "cp $Outfile $n_import"
           cp $Outfile $n_import
           chmod a+rw $n_import/$Outfile:t
#
# Notify Newstar account
#
           echo "Notification will be sent to $USER and $n_master"
           cat $Outfile | nsmail "Checkin by $USER" $n_master $USER
        endif
      endif
      unset Lock_found
     endif
#
#
#  %Link command:
#
   else if ("$Command" =~ [Ll]* ) then

#
# Setup suboption: create shadow directories, update links in n_uinc
#
     if ("$Files[1]" =~ [Ss][Ee][Tt]* ) then
        if ($?n_uroot) then
           if (! -d $n_uroot) then
              echo "Creating $n_uroot"
              mkdir $n_uroot
           endif
        endif
        if ($?n_usrc) then
           if (! -d $n_usrc && $n_usrc != __undefined__) then
              echo "Creating $n_usrc"
              mkdir $n_usrc
           endif
        endif
        if ($?n_uinc) then
           if (! -d $n_uinc) then
              set dir=$n_uinc
              set dir=$dir:h
              if (! -d $dir) then
                 echo "Creating $dir"
                 mkdir $dir
              endif
              echo "Creating $n_uinc"
              mkdir $n_uinc
           endif
        endif
        if ($?n_ulib) then
           if (! -d $n_ulib) then
              set dir=$n_ulib
              set dir=$dir:h
              if (! -d $dir) then
                 echo "Creating $dir"
                 mkdir $dir
              endif
              echo "Creating $n_ulib"
              mkdir $n_ulib
           endif
        endif
        if ($?n_uexe) then
           if (! -d $n_uexe) then
              set dir=$n_uexe
              set dir=$dir:h
              if (! -d $dir) then
                 echo "Creating $dir"
                 mkdir $dir
              endif
              echo "Creating $n_uexe"
              mkdir $n_uexe
           endif
        endif        
        if ($?n_work) then
           if (! -d $n_work) then
              set dir=$n_work
              set dir=$dir:h
              if (! -d $dir) then
                 echo "Creating $dir"
                 mkdir $dir
              endif
              echo "Creating $n_work"
              mkdir $n_work
           endif
        endif        

        echo -n "Update links in "\$n_uinc" (y,n)? [y] "
        set ans=($<)
        if ($ans !~ [Nn]*) then
           ln -s $n_inc/* $n_uinc
        endif

     else if ($n_usrc == __undefined__) then
        echo "You should have defined "\$n_usrc" before you can link\!"
        echo "Please read "\$n_src/sys/newstar_init.csh" for information..."
     else if ($cwd != $n_usrc && $cwd:h != $n_usrc ) then
        echo "First change directory to "\$n_usrc" or a subdirectory thereof..."
     else if ($cwd != $n_usrc && ! -d $n_src/$cwd:t ) then
        echo "$cwd:t is not a (linkable) Newstar directory"
     else

       if ("$Files" != "") then
          set dir=( $Files )
          cd $n_usrc
          echo "Now in $n_usrc"
       else if ($cwd == $n_usrc) then
          set dir=( $NSTAR_DIR )
       else
          set dir=$cwd:t
       endif

       echo "Making links for directories $dir to $cwd"
#
#  Find all groupfiles (should be *.grp, but we still have nscanyymmdd.grp's)
#
       foreach subdir ( $dir )

          echo "======= Working on $n_src/$subdir ========"
          foreach grpfile ( $n_src/$subdir/???.grp ) 

             echo "======= Making links for groupfile $grpfile:t ======="
             if (! -e $n_usrc/$subdir) then
                mkdir $n_usrc/$subdir
                echo "Made subdirectory $subdir"
             endif
#
# Take all the efford of redirection and `cat` to avoid "long words" and
# errors due to pipes within ` ` (the latter should be no problem, but 
# we should not press our luck to the edges...)
#
             $n_exe/genaid.exe files -t:^exe $grpfile >$Tmpfile
             foreach file ( `cat $Tmpfile` )

                set file=`echo $file | sed -e s@$n_src/@@` 
                if (! -e $n_src/$file) then 
                   echo "File $file does not exist in Master ...."
                else
#
# Silently remove existing links, skip existing files since user
# should explicitly unlock them.
#
                   if (-e $n_usrc/$file) then
                      if ("`ls -F $n_usrc/$file`" =~ *@ ) then
                         'rm' -f $n_usrc/$file
                      else
			 cmp $n_usrc/$file $n_src/$file >& /dev/null
			 if (! $status) then
			    'rm' $n_usrc/$file
			    ln -s $n_src/$file $n_usrc/$file
			    echo \
			"	${file}: replaced identical copy by link"
			 else
                            echo \
			"	${file}: left variant copy"
			 endif
                      endif
                   endif
                   if ("$file:h" != "" && ! -d "$file:h") mkdir $file:h
                   if (! -e $n_usrc/$file) then
		      rm >&/dev/null $n_usrc/$file
					# may be soft link to nonexistent file
                      ln -s $n_src/$file $n_usrc/$file
##                    echo "Linked $file"
                   endif
                endif
             end

             'rm' -f $Tmpfile
          end
       end
     endif      
#
#
# %Put: Move files into text-library
#
    else if ("$Command" =~ [Pp][Uu][Tt]) then
#
#  Get names of groupfiles
#
       if ("$Files" == "") then
          echo -n "Enter name of groupfile(s): "
          set noglob             # Don't expand wildcards right now
          set Files=( $< )       # Read from stdin
          set Files=( $Files )   # Split in multiple words
          unset noglob
       endif
#
#  Expand them and update text-libraries
#  
       foreach grpfile ( $Files )
          if ( "$grpfile:e" == "" ) set grpfile=$grpfile.grp
          if (! -e $grpfile ) then
             echo "Groupfile $grpfile does not exist..."
          else
             set archive=${grpfile:r}.tlb
             echo "Updating text-library $archive"
             $n_exe/genaid.exe files -t:^exe $grpfile >$Tmpfile
             ar rv $archive `cat $Tmpfile`
            'rm' -f $Tmpfile
          endif
       end
       unset archive grpfile
#
# %Get: Get files from text-library
#
    else if ("$Command" =~ [Gg][Ee][Tt]) then
#
#  Get names of groupfiles
#
       if ("$Files" == "") then
          echo -n "Enter name of groupfile(s): "
          set noglob             # Don't expand wildcards right now
          set Files=( $< )       # Read from stdin
          set Files=( $Files )   # Split in multiple words
          unset noglob
       endif
#
#  Get each groupfile if necessary, expand and extract the files
#  
       foreach grpfile ( $Files )
          if ( "$grpfile:e" == "" ) set grpfile=$grpfile.grp
          set archive=${grpfile:r}.tlb
          if (! -e $archive ) then
             echo "Text-library $archive does not exist..."
          else
             if (! -e $grpfile) then
                ar xvo $archive $grpfile
             endif
             if (! -e $grpfile) then
                echo "Groupfile $grpfile does not exist in library"
             else
                echo "Extracting from text-library $archive"
                $n_exe/genaid.exe files -t:^exe $grpfile >$Tmpfile
                ar xvo $archive `cat $Tmpfile`
                'rm' -f $Tmpfile
             endif
          endif
       end
       unset archive grpfile

   else    # Other command
     echo ""
     echo "Error: Invalid or ambiguous command $Command"
     echo ""
   endif   # End of if (Command == ...)

end        # End of while (Menu mode)


Abort_exit:

if (-e $Tmpfile) 'rm' -f $Tmpfile
if ($?Errors) then
   exit $Errors
endif
