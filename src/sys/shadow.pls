#+ shadow.pls
#   created by wbrouw on norma at Tue Jun 21 13:26:58 LST 1994 
#-
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
#
#
#  shadow.csh  Interface to shadow system commands
#
#  Use "shadow help" for information on usage
#
#
#-
#+
# Preamble
#
unless (defined $VMS) {                                # check for environment
  if ($ENV{"SHELL"}) {                                 # aid routines unix
    unshift(@INC,$ENV{'n_src'}.'/sys');} 
  else {                                               # aid routines VMS
    unshift(@INC,'N_SRC:[SYS]');} 
  unless (require 'c2aid.pls') { 
    print "Fatal: Cannot load c2aid.pls properly"; exit;} 
  &ENV_IMPORT;                                         # get environment
  $argv=join(' ',@ARGV);}                              # get command arguments
if (&ft("e",&fp("r","$0").".csh") &&                   # renew main routine
                        (&ft("M","$0") > &ft("M","$n_src/sys/csh2p.pls") || 
                        &ft("M","$0") > &ft("M",&fp("r","$0").".csh"))) { 
  $status=&system("perl ".&fnp("$n_src/sys/csh2p.pls")." ". 
                        &fp("r","$0"));} 
#
# Start translated script
#-
sub shadow__pls { 
  $SIG{'INT'}= Abort_exit_shadow ; 
#
# Check environment, set defaults
#
  &source( &fn(  $n_src ."/sys/initcompile.csh" ) ) ; 
#
#  Decode switches, get command or set menu mode if none given
#
  $noglob='' ; $Command= &fn(  $argv ) ; undef $noglob ; 
  $Options=  '' ; &source( &fn(  $n_src ."/sys/switches.csh" ) ) ; 
  $Files=  '' ; 
  if ( !&eq(   $Command ,  '' ) ) { 
    $Mode=  "Command" ; 
    if ( &vn($Command) > 1 ) { 
      $noglob='' ; $Files= &fn(  (split(' ',$Command)) [ 2 -1 .. 
                        &vn($Command)-1 ] ) ; undef $noglob ; 
      $Command= &fn(  (split(' ',$Command)) [ 1 -1 ] ) ; 
    } 
  } 
  else { 
    $Mode=  "Menu" ; 
    $Command=  '' ; 
  } 
  while ( !&eq(   $Mode ,  "Quit" ) ) { 
    if ( &eq(   $Mode ,  "Menu" ) ) { 
      &echo( '' ,   "Valid commands are: help, build, link, get, " 
                        ."put, quit" , "" ) ; 
      &echo( '' ,   "                    out=checkout, in=checkin" 
                        .", unlock." , "" ) ; 
      &echo( "-n" ,   "Enter a command: " , "" ) ; 
      $Command=  ($_=scalar(<STDIN>), chop, $_) ; 
      $Files=  '' ; 
      $noglob='' ; $Command= &fn(  $Command ) ; undef $noglob ; 
      $Options=  '' ; &source( &fn(  $n_src ."/sys/switches.csh" ) ) ; 
      if ( &vn($Command) > 1 ) { 
        $noglob='' ; $Files= &fn(  (split(' ',$Command)) [ 2 -1 .. 
                        &vn($Command)-1 ] ) ; undef $noglob ; 
        $Command= &fn(  (split(' ',$Command)) [ 1 -1 ] ) ; 
      } 
    } 
    else { 
      $Mode=  "Quit" ; 
    } 
    if ( &eq(   $Command ,  '' ) || &peq(   $Command , "[Qq]*" ) ) { 
      $Mode=  "Quit" ; 
    } 
    elsif ( &peq(   $Command , "[Hh]*" ) ) { 
      sub C2_t1_shadow { 
        local(*TMP); 
        open(TMP,">txt$$.tmp"); 
        print TMP  "#+++++++++++++++++++++++++++++++++++++++++++" 
                        ."+++++++++++++++++++++++++++++++++#" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "  shadow can be called in one of the followi" 
                        ."ng ways:" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "   shadow" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "      Without options, enter menu mode, whic" ."h" 
                        ."\n" ; 
        print TMP  "      prompts you for commands and filenames" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "   shadow  build  [switches | file] " ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "      Compile one or more files. All files h" 
                        ."ave to be fully " ."\n" ; 
        print TMP  "      specified (that is either full pathnam" 
                        ."e or the path with" ."\n" ; 
        print TMP  "      respect to current directory)." ."\n" ; 
        print TMP  "      If no files are present, their names a" 
                        ."re read from the " ."\n" ; 
        print TMP  "      standard input." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "      You may specify groupfiles prefixed by" 
                        ." an @-sign, in " ."\n" ; 
        print TMP  "      which case the contents of the file wi" 
                        ."ll be processed." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "      Switches are specified like -Debug,Alt" 
                        ."ernate,List,Print and" ."\n" ; 
        print TMP  "      may be abbreviated. For more informati" 
                        ."on, give switch -Help " ."\n" ; 
        print TMP  "      All switches on the command line are a" 
                        ."pplied to all files. " ."\n" ; 
        print TMP  "      Switches appearing within a groupfile " 
                        ."are only applied to " ."\n" ; 
        print TMP  "      the file after which they appear." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    shadow link [setup | dir ...]" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "      Create a shadow tree starting at the c" 
                        ."urrent directory." ."\n" ; 
        print TMP  "      You should have set " . $n_usrc ." already." 
                        ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  
                        "      If you do not specify a directory, and you are in " 
                        . $n_usrc .", the" ."\n" ; 
        print TMP  "      standard Newstar program subdirectorie" 
                        ."s (not sys and doc etc)" ."\n" ; 
        print TMP  "      will be shadowed. If you do not specif" 
                        ."y a directory and you are" ."\n" ; 
        print TMP  "      in a subdirectory of " . $n_usrc 
                        .", that directory will be shadowed." ."\n" ; 
        print TMP  "      If you do specify a directory, and tha" 
                        ."t directory is a subdirectory" ."\n" ; 
        print TMP  "      of " . $n_src 
                        .", the specified directory will be shadowed." 
                        ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "  NOTYET    If one or more groupfiles are gi" 
                        ."ven, only the files listed" ."\n" ; 
        print TMP  "  NOTYET    therein will be shadowed." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "      If the argument setup is given, any mi" 
                        ."ssing shadow top-directories" ."\n" ; 
        print TMP  "      are created and a shadow copy of the i" 
                        ."nclude files is made." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    shadow checkout [switches | file] ..." ."\n" ; 
        print TMP  "    shadow out      [switches | file] ..." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "      Copy one or more files to the current " 
                        ."directory, " ."\n" ; 
        print TMP  "      leaving a lock in the locking database" ."." 
                        ."\n" ; 
        print TMP  "      Groupfiles prefixed with an @-sign wil" 
                        ."l be replaced" ."\n" ; 
        print TMP  "      by their contents. If no files are pre" 
                        ."sent, their names " ."\n" ; 
        print TMP  "      are read from the standard input." ."\n" ; 
        print TMP  "      If the file does not exist but is foun" 
                        ."d in a textlibrary, " ."\n" ; 
        print TMP  "      it will be extracted." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    shadow checkin  [switches | file] ..." ."\n" ; 
        print TMP  "    shadow in       [switches | file] ..." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "      A groupfile will be created containing" 
                        ." the specified files," ."\n" ; 
        print TMP  "      with filesize and revision date includ" 
                        ."ed. If files are spread " ."\n" ; 
        print TMP  
                        "      over multiple directories, they should either root in " 
                        . $n_src ." or " ."\n" ; 
        print TMP  "      in " . $n_usrc 
                        .". Otherwise you will be prompted for the dir" 
                        ."ectory " ."\n" ; 
        print TMP  "      where the files should land in the mas" 
                        ."ter system." ."\n" ; 
        print TMP  "      Filenames appear in the groupfile with" 
                        ." their respect to \$n_src." ."\n" ; 
        print TMP  "      If run from the Master, the groupfile " 
                        ."is created in \$n_import" ."\n" ; 
        print TMP  "      If run from a user system, the groupfi" 
                        ."le and all files " ."\n" ; 
        print TMP  "      mentioned in it will be copied to \$n_" 
                        ."import unless they were" ."\n" ; 
        print TMP  "      locked by another user. Any existing l" 
                        ."ocks are removed after" ."\n" ; 
        print TMP  "      the copy, and new locks for the Newsta" 
                        ."r manager are made." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    shadow unlock [switches | file] ..." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "      Unlock one or more files that have bee" 
                        ."n previously checked out." ."\n" ; 
        print TMP  "      Unlocking means that the user's file i" 
                        ."s replaced by a link, and " ."\n" ; 
        print TMP  "      that the lock is removed from the lock" 
                        ."ing database." ."\n" ; 
        print TMP  "  |   Future extensions might involve switch" 
                        ."es to check for differences." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "      NB: unlocking is different from checki" 
                        ."ng files in!!!" ."\n" ; 
        print TMP  "          checkin involves making a groupfil" 
                        ."e and copying files" ."\n" ; 
        print TMP  "          to " . $n_import 
                        .". The files get a new lock for the Newstar " 
                        ."\n" ; 
        print TMP  "          manager. Files are unlocked after " 
                        ."the Newstar manager moved " ."\n" ; 
        print TMP  "          the updated files into the master " 
                        ."source tree." ."\n" ; 
        print TMP  "    " ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     shadow put [groupfile] ..." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "         put all files in the groupfiles in " 
                        ."the corresponding tlb" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     shadow get [groupfile] ..." ."\n" ; 
        print TMP  "        " ."\n" ; 
        print TMP  "         retrieve files from a tlb's corresp" 
                        ."onding to the groupfiles." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "#-------------------------------------------" 
                        ."------------------------------#" ."\n" ; 
        print TMP  '' ."\n" ; 
        close(TMP); 
        "txt$$.tmp";} 
      &cat( '' ,  &C2_t1_shadow , "" ) ; 
#
#
#  Build, Checkout, Checkin and Unlock all need a filelist
#
    } 
    elsif ( &peq(   $Command , "[Bb]*" ) || &peq(   $Command , 
                        "[Cc]*[Oo][Uu][Tt]" ) || &peq(   $Command , "[Oo]*" 
                        ) || &peq(   $Command , "[Cc]*[Ii][Nn]" ) || &peq(  
                         $Command , "[Ii]*" ) || &peq(   $Command , "[Uu]*" 
                        ) ) { 
#
#  For checkin, we first need to do some checks and setup
#
      if ( &peq(   $Command , "[Cc]*[Ii][Nn]" ) || &peq(   $Command , 
                        "[Ii]*" ) ) { 
#
#  Find out in which directory files should end up:
#       
        if ( &eq( $cwd , $n_usrc ) || &eq( $cwd , $n_src ) || &eq( $cwd , 
                        $n_import ) ) { 
          $Target=  '' ; 
        } 
        elsif ( &eq( &fp('h', $cwd ) , $n_usrc ) || &eq( &fp('h', $cwd ) , 
                        $n_src ) ) { 
          $Target=   &fp('t', $cwd ) ."/" ; 
        } 
        else { 
          &echo( '' ,   "You are not in a Master or user source tree." , "" 
                        ) ; 
          &echo( '' ,   "Specify the directory in the Master source t" 
                        ."ree" , "" ) ; 
          &echo( '' ,   "below which the files should finally end up." , "" 
                        ) ; 
          &echo( "-n" ,   "Enter name of subdirectory (eg nscan, doc): " , 
                        "" ) ; 
          $Target=  ($_=scalar(<STDIN>), chop, $_) ; 
          $Target=   $Target ."/" ; 
        } 
#
# Create a groupfile for export
#
        $Flag= &fn(  $USER ) ; 
        if ( &eq(   $USER , "devoscm" ) ) { $Flag= "cmv" ; } 
        if ( &eq(   $USER , "noordam" ) ) { $Flag= "jen" ; } 
        if ( &eq(   $USER , "newstar" ) ) { $Flag= "x" ; } 
        $Outfile= &fn( "upd" . $C_Date . $Flag .".grp" ) ; 
        $ii= 1 ; 
        while ( &ft('e', $Outfile ) || &ft('e',  $n_import ."/" . $Outfile 
                        ) ) { 
          if ( ! &ft('e',  $n_import ."/" . $Outfile ) ) { 
            &echo( "-n" ,   "Remove old groupfile " , "" ) ; &rm( "-i" ,  
                        &fn(  $Outfile ) ) ; 
          } 
          if ( &ft('e', $Outfile ) || &ft('e',  $n_import ."/" . $Outfile ) 
                        ) { 
            $Outfile= &fn( "upd" . $C_Date . $Flag . $ii .".grp" ) ; 
          } 
          $ii= $ii + 1 ; 
        } 
        &echo( '' ,   "\!+ " . $Outfile , ''. &fn(  $Outfile ) ) ; 
        sub C2_t2_shadow { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  "!" ."\n" ; 
          print TMP  "! Export of updated files for Newstar (" . $C_Date 
                        ." " . $C_Time ." " . $n_arch .")" ."\n" ; 
          print TMP  "! Groupfile created by " . $USER ." (" . $Myname 
                        .") at " . $n_site ." (" . $HOST .")" ."\n" ; 
          print TMP  "!" ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &cat( '' ,  &C2_t2_shadow , '>'. &fn(  $Outfile ) ) ; 
        &echo( '' ,   "The comment may show up in nnews later, so p" 
                        ."lease" , "" ) ; 
        &echo( '' ,   "include name of programs affected" , "" ) ; 
        &echo( "-n" ,   "Enter a comment: " , "" ) ; 
        $Flag=  ($_=scalar(<STDIN>), chop, $_) ; 
        while ( !&eq(   $Flag ,  '' ) ) { 
          &echo( "-n" ,   "If this is a bug solution, enter the bug-id " 
                        ."here: " , "" ) ; 
          $Bug=  ($_=scalar(<STDIN>), chop, $_) ; 
          if ( !&eq(   $Bug ,  '' ) ) { $Flag= &fn(  $Flag ) .' '.  
                        " - bug " . $Bug ." " ; } 
          &echo( '' ,   "\! Subject: " . $Flag , '>'. &fn(  $Outfile ) ) ; 
          &echo( "-n" ,   "Enter more comments [no more]: " , "" ) ; 
          $Flag=  ($_=scalar(<STDIN>), chop, $_) ; 
        } 
        sub C2_t3_shadow { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  "! " ."\n" ; 
          print TMP  "!" . $Target 
                        ."File                      Size    Chsum  YYM" 
                        ."MDD" ."\n" ; 
          print TMP  "! " ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &cat( '' ,  &C2_t3_shadow , '>'. &fn(  $Outfile ) ) ; 
      } 
#
#
#  If no files given, read from input
#
      undef $Read_files ; 
      if ( &eq(   $Files ,  '' ) ) { $Read_files='' ; } 
#
#  Process all files from the commandline or read up to an empty line
#
      while ( !&eq(   $Files ,  '' ) || defined($Read_files) ) { 
        if ( &eq(   $Files ,  '' ) && defined($Read_files) ) { 
          &echo( "-n" ,   "Enter filename: " , "" ) ; 
          $noglob='' ;                                 # Don't expand wildcards right now
          $Files=  ($_=scalar(<STDIN>), chop, $_) ;    # Read from stdin
          $Files= &fn(  $Files ) ;                     # Split in multiple words
          undef $noglob ; 
          if ( &eq(   $Files ,  '' ) ) { 
            undef $Read_files ; 
            $Files=  '' ;                              # Make sure Files[1] exists
          } 
        } 
#
#  Though this test may seem a bit queer at first sight, it prevents
#  us from "long word" errors if $Files contains more than some
#  thousand characters. The other way to get around this is by using
#  a foreach, but if so, we cannot push expanded groupfiles on $Files
#
        while ( !&eq(   (split(' ',$Files)) [ 1 -1 ] ,  '' ) ) { 
#
#  Get the first word from $Files, make lowercase and remove from list
#
          $noglob='' ; 
          $Input_file=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  
                        (split(' ',$Files)) [ 1 -1 ] ) , "p$$.tmp01" ) , 
                        &tr( '' , '[A-Z]' , '[a-z]' , "p$$.tmp01" , 
                        "p$$.tmp00" ) ) ; 
          @Files=split(' ',$Files); splice(@Files, "1" -1,1,  '' ); 
                        $Files=join(' ',@Files); if ( &vn($Files) > 1 ) { 
                        @Files=split(' ',$Files) ; shift(@Files) ; 
                        $Files=join(' ',@Files) ; } 
          undef $noglob ; 
#
#  If switches/options: process as local options (valid for next file only)
#
          if ( &peq(   $Input_file , "-*" ) || &peq(   $Input_file , "+*" ) 
                        ) { 
            $Options= "local" .' '. &fn(  $Input_file ) ; 
            &source( &fn(  $n_src ."/sys/switches.csh" ) ) ; 
#
#  If the next file is a groupfile: expand it to it's contents
#
          } 
          elsif ( &peq(   $Input_file , "@?*" ) ) { 
#
#  Remove @-sign and add default extension, expand wildcards
#
            $noglob='' ; 
            $Input_file=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $Input_file 
                        ) , "p$$.tmp01" ) , &tr( "-d" ,  "@" , '' , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
            if ( &eq(   &fp('e', $Input_file ) ,  '' ) ) { $Input_file=   
                        $Input_file .".grp" ; } 
            undef $noglob ; 
            $Input_file= &fn(  $Input_file ) ; 
#
#  Expand wildcards and process all groupfiles. We enter into some 
#  ugly processing here, because I cannot rely on all architectures
#  being able to handle arbitrary long environment variables (though 
#  both Sun and HP get pretty far!). Therefore I only expand the
#  first groupfile, and put any remaining groupfiles back on the list.
#  This should give no delay in the processing, but limits the number
#  of items present in $Files at any given time.
#
            $nonomatch='' ; 
            if ( &ft('e', $Tmpfile ) ) { &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
                        } 
            for $grpfile__x (split(' ',join(' ' , &fn(  $Input_file ) ))) { 
                        $grpfile=$grpfile__x ; 
              if ( &ft('e', $grpfile ) ) { 
                if ( ! &ft('e', $Tmpfile ) ) { 
                  if ( &eq( &fp('h', $grpfile ) , $grpfile ) ) { $grpfile= 
                        &fn( "./" . $grpfile ) ; } 
                  &echo( '' ,   " " , "" ) ; 
                  &echo( '' ,   "======= Expanding groupfile " . $grpfile 
                        ." =======" , "" ) ; 
                  &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "expand" .' '. 
                        &fn( "-t:" . $_Types ) .' '. &fn(  $grpfile ) , ''. 
                        &fn(  $Tmpfile ) ) ; 
                } 
                else { 
                  $Files= '@' . $grpfile .' '. &fn(  $Files ) ; 
                } 
              } 
              else { 
                &echo( '' ,   "Error: cannot find groupfile " . $grpfile , 
                        "" ) ; 
              } 
            } 
            undef $nonomatch ; undef $grpfile ; 
            if ( &ft('e', $Tmpfile ) ) { 
              if ( ! &ft('z', $Tmpfile ) ) { $Files=  &Pipe("p$$.tmp00", 
                        &cat( '' ,  &fn(  $Tmpfile ) , "p$$.tmp00" ) ) 
                        .' '. &fn(  $Files ) ; } 
              &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
            } 
#
#  It's an ordinary file, pass to the compiler or other command:
#
          } 
          else { 
#
#
#  Compile the file(s) after wildcard expansion etc.
#
#  Although I cannot really advise it, a user can give -Obj:mylib to
#  move objects into his favourite private library. This is taken care of 
#  by the lines which refer to $_Objectlib and $_Objectlist.
#
#
            if ( &peq(   $Command , "[Bb]*" ) ) { 
              if ( !&eq(   $_Objectlib ,  '' ) ) { 
                if ( !&peq( $_Objectlib ,  $n_ulib ."/*lib.olb" ) ) { 
                        $_Objectlib= &fn(  $n_ulib ."/" . $_Objectlib 
                        ."lib.olb" ) ; } 
                if ( &ft('e',  $n_work ."/" . &fp('t', $_Objectlib ) 
                        .".list" ) ) { 
                  &rm( "-f" ,  &fn(  $n_work ."/" . &fp('t', $_Objectlib ) 
                        .".list" ) ) ; 
                } 
                $Input_file= &fn(  $Input_file ) .' '. &fn(  $_Objectlib ) 
                        ; 
              } 
              if ( !&eq(   $_Textlib ,  '' ) ) { 
                if ( !&peq( $_Textlib ,  $n_ulib ."/*lib.olb" ) ) { 
                        $_Textlib= &fn(  $n_ulib ."/" . $_Textlib 
                        ."lib.olb" ) ; } 
                if ( &ft('e',  $n_work ."/" . &fp('t', $_Textlib ) .".list" 
                        ) ) { 
                  &rm( "-f" ,  &fn(  $n_work ."/" . &fp('t', $_Textlib ) 
                        .".list" ) ) ; 
                } 
                $Input_file= &fn(  $Input_file ) .' '. &fn(  $_Textlib ) ; 
              } 
              &source( &fn(  $n_src ."/sys/compile.csh" ) ) ; 
              if ( defined($Abort_flag) ) { &Abort_exit_shadow ; } 
#
#
#  Checkout: copy the file(s) from the master tree to a user directory
#
            } 
            elsif ( &peq(   $Command , "[Cc]*[Oo][Uu][Tt]" ) || &peq(   
                        $Command , "[Oo]*" ) ) { 
              if ( $_Select ) { 
                for $File__x (split(' ',join(' ' , &fn(  $Input_file ) 
                        ))) { $File=$File__x ; 
#
#  Locate the file in the master source tree
#
                  &echo( '' ,   " " , "" ) ; 
                  undef $Source ; undef $Target ; 
#
#  If the file exists, give it an absolute path 
#
#                  if (-e $File) then
#                     set Home=$cwd;
#                     if ($File:h != $File) cd $File:h
#                     set File=$cwd/$File:t
#                     cd $Home; 
#                     unset Home 
#                  endif
#
                  $nonomatch='' ; 
#
#  File was specified with respect to the top of the Master source tree,
#  and we are in the top of our private tree. Checkin to the corresponding
#  directory in our own tree.
#
                  if ( &eq( $cwd , $n_usrc ) && &ft('e',  $n_src ."/" . 
                        $File ) ) { 
                    $Source= &fn(  $File ) ; 
                    $Target= &fn( "./" . $File ) ; 
#
#  Idem, but not at top of our own source tree, checkin to current directory
#
                  } 
                  elsif ( &ft('e',  $n_src ."/" . $File ) ) { 
                    $Source= &fn(  $File ) ; 
                    $Target= &fn( "./" . &fp('t', $File ) ) ; 
#
#  File was specified with respect to subdirectory of Master source tree,
#  and we are in the corresponding subdir of our private tree. Checkin
#  to that directory.
#
                  } 
                  elsif ( &eq( &fp('h', $cwd ) , $n_usrc ) && &ft('e',  
                        $n_src ."/" . &fp('t', $cwd ) ."/" . $File ) ) { 
                    $Source= &fn(  &fp('t', $cwd ) ."/" . $File ) ; 
                    $Target= &fn( "./" . $File ) ; 
#
#  Just a filename was given, and such a file exists in a subdirectory
#  of the master. Since we are not in our private source tree (would have
#  been catched earlier) we check in to current directory.
#
                  } 
                  elsif ( &eq( &fp('t', $File ) , $File ) && &ft('e',  
                        $n_src ."/*/" . $File ) ) { 
                    $Source= &fn(  $n_src ."/*/" . $File ) ; 
                    $Source=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  
                        (split(' ',$Source)) [ 1 -1 ] ) , "p$$.tmp01" ) , 
                        &sed( "-e" ,  "s%^" . $n_src ."/%%" , "p$$.tmp01" , 
                        "p$$.tmp00" ) ) ; 
                    $Target= &fn( "./" . $File ) ; 
#
# File was completely specified. If stripping n_src makes any difference,
# the file is in the Master source tree. If not, the file exists 
# somewhere outside the tree, and we are not interested.
#
# If we are somewhere in our own private tree, checkin to the correct
# subdirectory of that tree. Otherwise, checkin to the current dir.
#
                  } 
                  elsif ( &ft('e', $File ) ) { 
                    $Source=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $File ) 
                        , "p$$.tmp01" ) , &sed( "-e" ,  "s%^" . $n_src 
                        ."/%%" , "p$$.tmp01" , "p$$.tmp00" ) ) ; 
                    if ( !&eq(   $Source ,   $File ) ) { 
                      if ( &eq( $cwd , $n_usrc ) || &eq( &fp('h', $cwd ) , 
                          $n_usrc ) ) { 
                        $Target= &fn(  $n_usrc ."/" . $Source ) ; 
                      } 
                      else { 
                        $Target= &fn( "./" . &fp('t', $File ) ) ; 
                      } 
                    } 
                    else { 
                      undef $Source ; 
                      &echo( '' ,   "Warning: File " . $File 
                          ." is not in the Master tree" , "" ) ; 
                    } 
#
# If anything else fails, try to locate it in a tlb, this only works
# if we are in a subdirectory of the Master.
#
                  } 
                  elsif ( &eq( &fp('h', $cwd ) , $n_src ) && &ft('e',  
                        $n_src ."/" . &fp('t', $cwd ) ."lib.tlb" ) ) { 
                    &ar( "xv" ,  &fn(  $n_src ."/" . &fp('h', $cwd ) 
                        ."lib.tlb" ) .' '. &fn(  &fp('t', $File ) ) , "" ) 
                        ; 
                    &echo( '' ,    &fp('t', $File ) ." extracted from tlb" 
                        , "" ) ; 
#
#  Although it seems unlikely regarding our previous effords, 
#  we still may not be able to locate the file...
#
                  } 
                  else { 
                    &echo( '' ,   "Error: Cannot locate " . $File , "" ) ; 
                  } 
                  undef $nonomatch ; 
#
#  If located, remove any existing links, copy and lock the file
#
                  if ( defined($Source) ) { 
                    if ( &ft('e', $Target ) ) { 
                      if ( &peq(   &Pipe("p$$.tmp00", &ls( "-F" ,  &fn(  
                          $Target ) , "p$$.tmp00" ) ) , "*@" ) ) { 
                        &rm( "-f" ,  &fn(  $Target ) ) ; 
                      } 
                      else { 
                        $Flag=  &Pipe("p$$.tmp00", &cmp(  &fn(  $n_src ."/" 
                            . $Source ) .' '. &fn(  $Target ) , "p$$.tmp00" 
                            ) ) ; 
                        if ( !&eq(   $Flag ,  '' ) ) { 
                          &echo( '' ,   "File " . $Target 
                              ." already exists..." , "" ) ; 
                          &diff( "-b" ,  &fn(  $n_src ."/" . $Source ) 
                              .' '. &fn(  $Target ) , "" ) ; 
                          &echo( "-n" ,   "Remove " , "" ) ; &rm( "-i" ,  
                              &fn(  $Target ) ) ; 
                        } 
                        else { 
                          &rm( "-f" ,  &fn(  $Target ) ) ; 
                        } 
                      } 
                    } 
                    if ( ! &ft('e', $Target ) ) { 
                      &echo( '' ,  'checkout: $n_src/' .  $Source ." --> " 
                          . $Target , "" ) ; 
                      &cp( '' ,  &fn(  $n_src ."/" . $Source ) .' '. &fn(  
                          $Target ) ) ; 
                    } 
                    if ( &ft('e',  $n_src ."/sys/lock.idx" ) ) { 
                      $Lock=  &Pipe("p$$.tmp00", &grep( '' , &fn(  $Source 
                          ) ,  &fn(  $n_src ."/sys/lock.idx" ) , 
                          "p$$.tmp00" ) ) ; 
                      if ( !&eq(   $Lock ,  '' ) ) { 
                        &echo( '' ,   "Warning:  " . $Lock , "" ) ; 
                      } 
                      else { 
                        &echo( '' ,    $Source ." locked User=" . $USER 
                            ." Date=" . $C_Date ."/" . $C_Time , 
                            "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  $n_src 
                            ."/sys/lock.idx" ) , "p$$.tmp00" ) ; 
                      } 
                      undef $Lock ; 
                    } 
                  } 
                  undef $Target ; undef $Source ; 
                } 
              } 
              else { 
                &echo( '' ,   "You should not checkout " . $Input_file 
                        ." on " . $n_arch , "" ) ; 
              } 
#
#
#  Unlock files from usersystem
#
            } 
            elsif ( &peq(   $Command , "[Uu]*" ) ) { 
              $Home= &fn(  $cwd ) ; 
              for $File__x (split(' ',join(' ' , &fn(  $Input_file ) ))) { 
                        $File=$File__x ; 
#
#  If we are somewhere in our private source tree, we can match the 
#  file with respect to the Master source tree, otherwise we just match
#  the name. This removes locks to ALL matching files. In practice, there
#  will be no files with identical names, and it is even less likely that
#  a user will lock out multiple files with the same name, and if so
#  it is not unreasonable to require him to work in a shadow tree...
#
                if ( !&eq( &fp('h', $File ) , $File ) ) { &cd(  &fn(  
                        &fp('h', $File ) ) ) ; } 
                $Full= &fn(  $cwd ."/" . &fp('t', $File ) ) ; 
                &cd(  &fn(  $Home ) ) ; 
#
#  If in shadow tree, replace existing file with a link 
#  (if not already a link!), otherwise delete it.
#
                if ( &ft('e', $Full ) && !&peq( $Full ,  $n_src ."/*" ) ) { 
                  if ( !&peq(   &Pipe("p$$.tmp00", &ls( "-F" ,  &fn(  $Full 
                        ) , "p$$.tmp00" ) ) , "*@" ) ) { 
                    $nonomatch='' ; 
                    if ( &ft('e',  $n_src ."/*/" . &fp('t', $Full ) ) ) { 
                      $Flag=  &Pipe("p$$.tmp00", &cmp(  &fn(  $Full ) .' '. 
                          &fn(  $n_src ."/*/" . &fp('t', $Full ) ) , 
                          "p$$.tmp00" ) ) ; 
                      if ( !&eq(   $Flag ,  '' ) ) { 
                        &diff( "-b" ,  &fn(  $Full ) .' '. &fn(  $n_src 
                            ."/*/" . &fp('t', $Full ) ) , "" ) ; 
                        &echo( "-n" ,   "Remove " , "" ) ; &rm( "-i" ,  
                            &fn(  $Full ) ) ; 
                      } 
                      else { 
                        &rm( "-f" ,  &fn(  $Full ) ) ; 
                      } 
                    } 
                    else { 
                      &echo( "-n" ,   "Remove " , "" ) ; &rm( "-i" ,  &fn(  
                          $Full ) ) ; 
                    } 
                    undef $nonomatch ; 
                    if ( &ft('e', $Full ) ) { 
                      &echo( '' ,   "***** Warning: " . $Full 
                          ." not deleted..." , "" ) ; 
                    } 
                    elsif ( &peq(   $Full ,  $n_usrc ."/*" ) ) { 
                      &ln( "-s" ,  &fn(  $n_src ."/" . $File ) .' '. &fn(  
                          $Full ) ) ; 
                      &echo( '' ,   "File " . $File 
                          ." replaced by soft link" , "" ) ; 
                    } 
                    else { 
                      &echo( '' ,   "File " . $Full ." has been deleted" , 
                          "" ) ; 
                    } 
                  } 
                } 
#
#  Remove lock if file previously locked by this user
#
#  Owner of Newstar master source tree can unlock any file
#
                if ( &ft('e',  $n_src ."/sys/lock.idx" ) ) { 
                  $File=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $Full ) , 
                        "p$$.tmp01" ) , &sed( "-e" ,  "s%^" . $n_usrc 
                        ."/%%" , "p$$.tmp01" , "p$$.tmp00" ) ) ; 
                  if ( &eq( $File , $Full ) ) { $File= &fn( "/" . &fp('t', 
                        $File ) ) ; } 
                  $Lock=  &Pipe("p$$.tmp00", &grep( '' , &fn(  $File ) ,  
                        &fn(  $n_src ."/sys/lock.idx" ) , "p$$.tmp00" ) ) ; 
                  if ( !&eq(   $Lock ,  '' ) ) { 
                    if ( &peq(   $Lock , "*User=" . $USER ."*" ) || 
                        &ft('o', $n_src ) ) { 
                      &cp( '' ,  &fn(  $n_src ."/sys/lock.idx" ) .' '. &fn( 
                           $n_work ."/lock.old" ) ) ; 
                      &grep( "-v" , &fn(  $File ) ,  &fn(  $n_work 
                          ."/lock.old" ) , ''. &fn(  $n_src 
                          ."/sys/lock.idx" ) ) ; 
                      &echo( '' ,   "Removed: " . $Lock , "" ) ; 
                    } 
                    else { 
                      &echo( '' ,   "Warning: " . $Lock , "" ) ; 
                    } 
                  } 
                  else { 
                    &echo( '' ,    &fp('t', $File ) ." was not locked" , "" 
                        ) ; 
                  } 
                  undef $Lock ; 
                } 
                else { 
                  &echo( '' ,   "No locking database" , "" ) ; 
                } 
              } 
              undef $Home ; 
#
#
#  Checkin: write filename to groupfile
#
            } 
            elsif ( &peq(   $Command , "[Cc]*[Ii][Nn]" ) || &peq(   
                        $Command , "[Ii]*" ) ) { 
              $Home= &fn(  $cwd ) ; 
              for $File__x (split(' ',join(' ' , &fn(  $Input_file ) ))) { 
                        $File=$File__x ; 
#
#  Give the file a full path and make it relative to current dir
#
                if ( !&eq( &fp('h', $File ) , $File ) ) { &cd(  &fn(  
                        &fp('h', $File ) ) ) ; } 
                $Full= &fn(  $cwd ."/" . &fp('t', $File ) ) ; 
                $File=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $Full ) , 
                        "p$$.tmp01" ) , &sed( "-e" ,  "s%^" . $Home ."/%%" 
                        , "p$$.tmp01" , "p$$.tmp00" ) ) ; 
                &cd(  &fn(  $Home ) ) ; 
#
# The file should exist and root in current dir
#
                if ( &eq(   &fp('e', $Full ) ,  "exe" ) ) { 
                  &echo( '' ,   "Warning: " . &fp('t', $Full ) 
                        ." ignored, specify exe-files later" , "" ) ; 
                } 
                elsif ( ! &ft('e', $Full ) ) { 
                  &echo( '' ,   "Error: " . $Full ." does not exist..." , 
                        "" ) ; 
                } 
                elsif ( &eq( $File , $Full ) && !&peq( $File ,  $n_src 
                        ."/*" ) ) { 
                  &echo( '' ,   
                        "Error: File should root in current directory" , "" 
                        ) ; 
                } 
                else { 
#
# It's there all right, put it in the groupfile
#
                  &echo( '' ,   "Calculating checksum for " . $File ." " , 
                        "" ) ; 
                  &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "fstat" .' '. 
                        &fn( "-t:" . $_Types ) .' '. &fn( "+" . $Target ) 
                        .' '. &fn(  $File ) , '>'. &fn(  $Outfile ) ) ; 
                } 
              } 
            }                                          # End of if (command)
            if ( !&eq(   $Save_switch ,  '' ) ) {      # Restore switches
                        &set($Save_switch) ; } 
          }                                            # End of if (groupfile)
        }                                              # End of while (Files left)
      }                                                # End of while (Files left)
#
#  For build, report the total number of errors
#
      if ( &peq(   $Command , "[Bb]*" ) ) { 
        if ( &eq(   $Errors , 0 ) ) { 
          &echo( '' ,   "Congratulations: no errors occurred" , "" ) ; 
        } 
        else { 
          &echo( '' ,   "Total number of errors: " . $Errors ." " , "" ) ; 
        } 
#
#  For checkin, we may want to move all files to $n_import
#
      } 
      elsif ( &peq(   $Command , "[Cc]*[Ii][Nn]" ) || &peq(   $Command , 
                        "[Ii]*" ) ) { 
#
#  Append a list of executables
#
        &echo( '' ,  'Possible shorthands: @all, @n[ewstar], @d[wa' 
                        .'rf], @a[bp]' , "" ) ; 
        &echo( "-n" ,   "Enter any executables to be rebuilt (default" 
                        ." extension .exe): " , "" ) ; 
        $Flag=  ($_=scalar(<STDIN>), chop, $_) ; 
        if ( !&eq(   $Flag ,  '' ) ) { 
          &echo( '' ,   "! " , '>'. &fn(  $Outfile ) ) ; 
          &echo( '' ,   "! Executables " , '>'. &fn(  $Outfile ) ) ; 
          &echo( '' ,   "! " , '>'. &fn(  $Outfile ) ) ; 
          $Flag= &fn(  $Flag ) ; 
          for $File__x (split(' ',join(' ' , &fn(  $Flag ) ))) { 
                        $File=$File__x ; 
            $File= &fn(  &fp('t', $File ) ) ; 
            if ( &peq( $File , "@[Aa][Ll][Ll]" ) ) { 
              &grep( "-h" , '^[^ ]*\.[Ee][Xx][Ee]' ,  &fn(  $n_src 
                        ."/*/*.grp" ) , '>'. &fn(  $Outfile ) ) ; 
            } 
            elsif ( &peq( $File , "@[Aa]*" ) ) { 
              &grep( "-h" , '^[^ ]*\.[Ee][Xx][Ee]' ,  &fn(  $n_src 
                        ."/dwarf/abp.grp" ) , '>'. &fn(  $Outfile ) ) ; 
            } 
            elsif ( &peq( $File , "@[Dd]*" ) ) { 
              &grep( "-h" , '^[^ ]*\.[Ee][Xx][Ee]' ,  &fn(  $n_src 
                        ."/dwarf/sys.grp" ) , '>'. &fn(  $Outfile ) ) ; 
            } 
            elsif ( &peq( $File , "@[Nn]*" ) ) { 
              &grep( "-h" , '^[Nn][^ ]*\.[Ee][Xx][Ee]' ,  &fn(  $n_src 
                        ."/n*/*.grp" ) , '>'. &fn(  $Outfile ) ) ; 
            } 
            else { 
              if ( !&eq(   &fp('e', $File ) ,  "exe" ) ) { $File= &fn(  
                        &fp('r', $File ) .".exe" ) ; } 
              &echo( '' ,    $File , '>'. &fn(  $Outfile ) ) ; 
            } 
          } 
        } 
#
#  Edit the file?
#
        &echo( '' ,   "! End of groupfile " . $Outfile , '>'. &fn(  
                        $Outfile ) ) ; 
        &echo( '' ,   '' , "" ) ; 
        &cat( '' ,  &fn(  $Outfile ) , "" ) ; 
        &echo( '' ,   '' , "" ) ; 
        &echo( "-n" ,   "Do you want to edit the groupfile (y,n)? [n]" ." " 
                        , "" ) ; 
        $Flag=  ($_=scalar(<STDIN>), chop, $_) ; 
        if ( &peq(   $Flag , "[Yy]*" ) ) { 
          if ( defined($EDITOR) ) { 
            &dollar("EDITOR" ,  &fn(  $Outfile ) , "" ) ; 
          } 
          else { 
            &doalias('emacs' , &fn(  $Outfile ) ) ; 
          } 
        } 
#
#  Decide wether or not to copy the files
#
        &echo( '' ,   '' , "" ) ; 
        &echo( "-n" ,   "Move files to " ."\$n_import" . " (y,n)? [y] " , 
                        "" ) ; 
        $Flag=  ($_=scalar(<STDIN>), chop, $_) ; 
        if ( !&peq(   $Flag , "[Nn]*" ) ) { 
          &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "files" .' '. "-t:^exe" 
                        .' '. &fn(  $Outfile ) , ''. &fn(  $Tmpfile ) ) ; 
#
#  Before we copy the files, first check if any of them had 
#  been locked by someone else. If so, ask Newstar manager to get the
#  files himself, since installation may be not so trivial.
#
          &echo( '' ,   '' , "" ) ; 
          &echo( '' ,   "Checking locks..." , "" ) ; 
          $Flag=  "ok" ; 
          if ( &ft('e',  $n_src ."/sys/lock.idx" ) ) { 
            for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &cat( '' 
                        ,  &fn(  $Tmpfile ) , "p$$.tmp00" ) ) ))) { 
                        $file=$file__x ; 
              $Lock=  &Pipe("p$$.tmp00", &grep( '' , &fn(  $file ) ,  &fn(  
                        $n_src ."/sys/lock.idx" ) , "p$$.tmp00" ) ) ; 
              if ( !&eq(   $Lock ,  '' ) && !&peq(   $Lock , "*User=" . 
                        $USER ."*" ) ) { 
                &echo( '' ,   "Warning: " . $Lock , "" ) ; 
                $Flag=  "lock" ; 
              } 
              undef $Lock ; 
            } 
          } 
          if ( &eq( $Flag ,  "lock" ) ) { 
            sub C2_t4_shadow { 
              local(*TMP); 
              open(TMP,">txt$$.tmp"); 
              print TMP  '' ."\n" ; 
              print TMP  "Found files that were locked by other users," 
                        ." so you cannot move your files" ."\n" ; 
              print TMP  "to \$n_import. Please contact your local New" 
                        ."star manager..." ."\n" ; 
              print TMP  '' ."\n" ; 
              close(TMP); 
              "txt$$.tmp";} 
            &cat( '' ,  &C2_t4_shadow , "" ) ; 
          } 
          else { 
#
# Copy the files to n_import (sed returns the filename w.r.t. $cwd)
#
            for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &cat( '' 
                        ,  &fn(  $Tmpfile ) , "p$$.tmp01" ) , &sed( "-e" ,  
                        "s%^" . $Target ."%%" , "p$$.tmp01" , "p$$.tmp00" ) 
                        ) ))) { $file=$file__x ; 
              &echo( '' ,   "cp " . $file ." " . $n_import , "" ) ; 
              &cp( '' ,  &fn( "./" . $file ) .' '. &fn(  $n_import ) ) ; 
              &chmod( "a+rw" ,  &fn(  $n_import ."/" . &fp('t', $file ) ) ) 
                        ; 
              if ( &eq( &fp('e', $file ) , "csh" ) ) { &chmod( "a+x" ,  
                        &fn(  $n_import ."/" . &fp('t', $file ) ) ) ; } 
            } 
            &echo( '' ,   "cp " . $Outfile ." " . $n_import , "" ) ; 
            &cp( '' ,  &fn(  $Outfile ) .' '. &fn(  $n_import ) ) ; 
            &chmod( "a+rw" ,  &fn(  $n_import ."/" . &fp('t', $Outfile ) ) 
                        ) ; 
#
# Notify Newstar account
#
            &echo( '' ,   "Notification will be sent to " . $USER ." and " 
                        . $n_master , "" ) ; 
            &cat( '' ,  &fn(  $Outfile ) , "p$$.tmp00" ) ; &elm( "-s" ,   
                        "Checkin by " . $USER .' '. &fn(  $n_master ) .' '. 
                        &fn(  $USER ) , "p$$.tmp00" ) ; 
#
# Remove old locks, make lock for Newstar manager
#
            &echo( '' ,   '' , "" ) ; 
            &echo( '' ,   "Making new locks..." , "" ) ; 
            if ( &ft('e',  $n_src ."/sys/lock.idx" ) ) { 
              for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &cat( 
                        '' ,  &fn(  $Tmpfile ) , "p$$.tmp00" ) ) ))) { 
                        $file=$file__x ; 
                $Lock=  &Pipe("p$$.tmp00", &grep( '' , &fn(  $file ) ,  
                        &fn(  $n_src ."/sys/lock.idx" ) , "p$$.tmp00" ) ) ; 
                if ( !&eq(   $Lock ,  '' ) ) { 
                  &cp( '' ,  &fn(  $n_src ."/sys/lock.idx" ) .' '. &fn(  
                        $n_work ."/lock.old" ) ) ; 
                  &grep( "-v" , &fn(  $file ) ,  &fn(  $n_work ."/lock.old" 
                        ) , ''. &fn(  $n_src ."/sys/lock.idx" ) ) ; 
                  &echo( '' ,   "Removed: " . $Lock , "" ) ; 
                } 
                &echo( '' ,    $file ." locked User=Newstar Date=" . 
                        $C_Date ."/" . $C_Time , "p$$.tmp00" ) ; &tee( "-a" 
                        ,  &fn(  $n_src ."/sys/lock.idx" ) , "p$$.tmp00" ) 
                        ; 
              } 
            } 
          } 
          &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
        } 
      } 
#
#
#  %Link command:
#
    } 
    elsif ( &peq(   $Command , "[Ll]*" ) ) { 
#
# Setup suboption: create shadow directories, update links in n_uinc
#
      if ( &peq(   (split(' ',$Files)) [ 1 -1 ] , "[Ss][Ee][Tt]*" ) ) { 
        if ( defined($n_uroot) ) { 
          if ( ! &ft('d', $n_uroot ) ) { 
            &echo( '' ,   "Creating " . $n_uroot , "" ) ; 
            &mkdir(  &fn(  $n_uroot ) ) ; 
          } 
        } 
        if ( defined($n_usrc) ) { 
          if ( ! &ft('d', $n_usrc ) && !&eq( $n_usrc , "__undefined__" ) 
                        ) { 
            &echo( '' ,   "Creating " . $n_usrc , "" ) ; 
            &mkdir(  &fn(  $n_usrc ) ) ; 
          } 
        } 
        if ( defined($n_uinc) ) { 
          if ( ! &ft('d', $n_uinc ) ) { 
            $dir= &fn(  $n_uinc ) ; 
            $dir= &fn(  &fp('h', $dir ) ) ; 
            if ( ! &ft('d', $dir ) ) { 
              &echo( '' ,   "Creating " . $dir , "" ) ; 
              &mkdir(  &fn(  $dir ) ) ; 
            } 
            &echo( '' ,   "Creating " . $n_uinc , "" ) ; 
            &mkdir(  &fn(  $n_uinc ) ) ; 
          } 
        } 
        if ( defined($n_ulib) ) { 
          if ( ! &ft('d', $n_ulib ) ) { 
            $dir= &fn(  $n_ulib ) ; 
            $dir= &fn(  &fp('h', $dir ) ) ; 
            if ( ! &ft('d', $dir ) ) { 
              &echo( '' ,   "Creating " . $dir , "" ) ; 
              &mkdir(  &fn(  $dir ) ) ; 
            } 
            &echo( '' ,   "Creating " . $n_ulib , "" ) ; 
            &mkdir(  &fn(  $n_ulib ) ) ; 
          } 
        } 
        if ( defined($n_uexe) ) { 
          if ( ! &ft('d', $n_uexe ) ) { 
            $dir= &fn(  $n_uexe ) ; 
            $dir= &fn(  &fp('h', $dir ) ) ; 
            if ( ! &ft('d', $dir ) ) { 
              &echo( '' ,   "Creating " . $dir , "" ) ; 
              &mkdir(  &fn(  $dir ) ) ; 
            } 
            &echo( '' ,   "Creating " . $n_uexe , "" ) ; 
            &mkdir(  &fn(  $n_uexe ) ) ; 
          } 
        } 
        if ( defined($n_work) ) { 
          if ( ! &ft('d', $n_work ) ) { 
            $dir= &fn(  $n_work ) ; 
            $dir= &fn(  &fp('h', $dir ) ) ; 
            if ( ! &ft('d', $dir ) ) { 
              &echo( '' ,   "Creating " . $dir , "" ) ; 
              &mkdir(  &fn(  $dir ) ) ; 
            } 
            &echo( '' ,   "Creating " . $n_work , "" ) ; 
            &mkdir(  &fn(  $n_work ) ) ; 
          } 
        } 
        &echo( "-n" ,   "Update links in " ."\$n_uinc" . " (y,n)? [y] " , 
                        "" ) ; 
        $ans=  ($_=scalar(<STDIN>), chop, $_) ; 
        if ( !&peq( $ans , "[Nn]*" ) ) { 
          &ln( "-s" ,  &fn(  $n_inc ."/*" ) .' '. &fn(  $n_uinc ) ) ; 
        } 
      } 
      elsif ( &eq( $n_usrc , "__undefined__" ) ) { 
        &echo( '' ,   "You should have defined " . $n_usrc 
                        ." before you can link\!" , "" ) ; 
        &echo( '' ,   "Please read " . $n_src 
                        ."/sys/newstar_init.csh for information..." , "" ) 
                        ; 
      } 
      elsif ( !&eq( $cwd , $n_usrc ) && !&eq( &fp('h', $cwd ) , $n_usrc ) 
                        ) { 
        &echo( '' ,   "First change directory to " ."\$n_usrc" . 
                        " or a subdirectory thereof..." , "" ) ; 
      } 
      elsif ( !&eq( $cwd , $n_usrc ) && ! &ft('d',  $n_src ."/" . &fp('t', 
                        $cwd ) ) ) { 
        &echo( '' ,    &fp('t', $cwd ) 
                        ." is not a (linkable) Newstar directory" , "" ) ; 
      } 
      else { 
        if ( !&eq(   $Files ,  '' ) ) { 
          $dir= &fn(  $Files ) ; 
          &cd(  &fn(  $n_usrc ) ) ; 
          &echo( '' ,   "Now in " . $n_usrc , "" ) ; 
        } 
        elsif ( &eq( $cwd , $n_usrc ) ) { 
          $dir= &fn(  $NSTAR_DIR ) ; 
        } 
        else { 
          $dir= &fn(  &fp('t', $cwd ) ) ; 
        } 
        &echo( '' ,   "Making links for directories " . $dir ." to " . $cwd 
                        , "" ) ; 
#
#  Find all groupfiles (should be *.grp, but we still have nscanyymmdd.grp's)
#
        for $subdir__x (split(' ',join(' ' , &fn(  $dir ) ))) { 
                        $subdir=$subdir__x ; 
          &echo( '' ,   "======= Working on " . $n_src ."/" . $subdir 
                        ." ========" , "" ) ; 
          for $grpfile__x (split(' ',join(' ' , &fn(  $n_src ."/" . $subdir 
                        ."/???.grp" ) ))) { $grpfile=$grpfile__x ; 
            &echo( '' ,   "======= Making links for groupfile " . &fp('t', 
                        $grpfile ) ." =======" , "" ) ; 
            if ( ! &ft('e',  $n_usrc ."/" . $subdir ) ) { 
              &mkdir(  &fn(  $n_usrc ."/" . $subdir ) ) ; 
              &echo( '' ,   "Made subdirectory " . $subdir , "" ) ; 
            } 
#
# Take all the efford of redirection and `cat` to avoid "long words" and
# errors due to pipes within ` ` (the latter should be no problem, but 
# we should not press our luck to the edges...)
#
            &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "files" .' '. 
                        "-t:^exe" .' '. &fn(  $grpfile ) , ''. &fn(  
                        $Tmpfile ) ) ; 
            for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &cat( '' 
                        ,  &fn(  $Tmpfile ) , "p$$.tmp00" ) ) ))) { 
                        $file=$file__x ; 
              $file=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $file ) , 
                        "p$$.tmp01" ) , &sed( "-e" , &fn( "s@" . $n_src 
                        ."/@@" ) , "p$$.tmp01" , "p$$.tmp00" ) ) ; 
              if ( ! &ft('e',  $n_src ."/" . $file ) ) { 
                &echo( '' ,   "File " . $file 
                        ." does not exist in Master ...." , "" ) ; 
              } 
              else { 
#
# Silently remove existing links, skip existing files since user
# should explicitly unlock them.
#
                if ( &ft('e',  $n_usrc ."/" . $file ) ) { 
                  if ( &peq(   &Pipe("p$$.tmp00", &ls( "-F" ,  &fn(  
                        $n_usrc ."/" . $file ) , "p$$.tmp00" ) ) , "*@" ) 
                        ) { 
                    &rm( "-f" ,  &fn(  $n_usrc ."/" . $file ) ) ; 
                  } 
                  else { 
                    &echo( '' ,   "File " . $file 
                        ." exists, not overwritten" , "" ) ; 
                  } 
                } 
                if ( !&eq(   &fp('h', $file ) ,  '' ) && ! &ft('d',   
                        &fp('h', $file ) ) ) { &mkdir(  &fn(  &fp('h', 
                        $file ) ) ) ; } 
                if ( ! &ft('e',  $n_usrc ."/" . $file ) ) { 
                  &ln( "-s" ,  &fn(  $n_src ."/" . $file ) .' '. &fn(  
                        $n_usrc ."/" . $file ) ) ; 
                  &echo( '' ,   "Linked " . $file , "" ) ; 
                } 
              } 
            } 
            &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
          } 
        } 
      } 
#
#
# %Put: Move files into text-library
#
    } 
    elsif ( &peq(   $Command , "[Pp][Uu][Tt]" ) ) { 
#
#  Get names of groupfiles
#
      if ( &eq(   $Files ,  '' ) ) { 
        &echo( "-n" ,   "Enter name of groupfile(s): " , "" ) ; 
        $noglob='' ;                                   # Don't expand wildcards right now
        $Files=  ($_=scalar(<STDIN>), chop, $_) ;      # Read from stdin
        $Files= &fn(  $Files ) ;                       # Split in multiple words
        undef $noglob ; 
      } 
#
#  Expand them and update text-libraries
#  
      for $grpfile__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $grpfile=$grpfile__x ; 
        if ( &eq(   &fp('e', $grpfile ) ,  '' ) ) { $grpfile= &fn(  
                        $grpfile .".grp" ) ; } 
        if ( ! &ft('e', $grpfile ) ) { 
          &echo( '' ,   "Groupfile " . $grpfile ." does not exist..." , "" 
                        ) ; 
        } 
        else { 
          $archive= &fn(  &fp('r', $grpfile ) .".tlb" ) ; 
          &echo( '' ,   "Updating text-library " . $archive , "" ) ; 
          &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "files" .' '. "-t:^exe" 
                        .' '. &fn(  $grpfile ) , ''. &fn(  $Tmpfile ) ) ; 
          &ar( "rv" ,  &fn(  $archive ) .' '.  &Pipe("p$$.tmp00", &cat( '' 
                        ,  &fn(  $Tmpfile ) , "p$$.tmp00" ) ) , "" ) ; 
          &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
        } 
      } 
      undef $archive ; undef $grpfile ; 
#
# %Get: Get files from text-library
#
    } 
    elsif ( &peq(   $Command , "[Gg][Ee][Tt]" ) ) { 
#
#  Get names of groupfiles
#
      if ( &eq(   $Files ,  '' ) ) { 
        &echo( "-n" ,   "Enter name of groupfile(s): " , "" ) ; 
        $noglob='' ;                                   # Don't expand wildcards right now
        $Files=  ($_=scalar(<STDIN>), chop, $_) ;      # Read from stdin
        $Files= &fn(  $Files ) ;                       # Split in multiple words
        undef $noglob ; 
      } 
#
#  Get each groupfile if necessary, expand and extract the files
#  
      for $grpfile__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $grpfile=$grpfile__x ; 
        if ( &eq(   &fp('e', $grpfile ) ,  '' ) ) { $grpfile= &fn(  
                        $grpfile .".grp" ) ; } 
        $archive= &fn(  &fp('r', $grpfile ) .".tlb" ) ; 
        if ( ! &ft('e', $archive ) ) { 
          &echo( '' ,   "Text-library " . $archive ." does not exist..." , 
                        "" ) ; 
        } 
        else { 
          if ( ! &ft('e', $grpfile ) ) { 
            &ar( "xvo" ,  &fn(  $archive ) .' '. &fn(  $grpfile ) , "" ) ; 
          } 
          if ( ! &ft('e', $grpfile ) ) { 
            &echo( '' ,   "Groupfile " . $grpfile 
                        ." does not exist in library" , "" ) ; 
          } 
          else { 
            &echo( '' ,   "Extracting from text-library " . $archive , "" ) 
                        ; 
            &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "files" .' '. 
                        "-t:^exe" .' '. &fn(  $grpfile ) , ''. &fn(  
                        $Tmpfile ) ) ; 
            &ar( "xvo" ,  &fn(  $archive ) .' '.  &Pipe("p$$.tmp00", &cat( 
                        '' ,  &fn(  $Tmpfile ) , "p$$.tmp00" ) ) , "" ) ; 
            &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
          } 
        } 
      } 
      undef $archive ; undef $grpfile ; 
    }                                                  # Other command
    else { 
      &echo( '' ,   '' , "" ) ; 
      &echo( '' ,   "Error: Invalid or ambiguous command " . $Command , "" 
                        ) ; 
      &echo( '' ,   '' , "" ) ; 
    }                                                  # End of if (Command == ...)
  }                                                    # End of while (Menu mode)
  &Abort_exit_shadow ; 
  sub  Abort_exit_shadow { 
    ; 
    if ( &ft('e', $Tmpfile ) ) { &rm( "-f" ,  &fn(  $Tmpfile ) ) ; } 
#
#+ Postamble
#
#
# Finish main routine
#
    &exit('');} 
  &exit('');} 
#
# Call main routine
#
eval('&shadow__pls');
1;
#-
