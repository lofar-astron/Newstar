#+ update.pls
#   created by wbrouw on norma at Mon Jun 20 14:03:14 LST 1994 
#-
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
#
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
sub update__pls { 
  if ( ! &ft('o', $n_src ) ) { 
    &echo( '' ,   "***** You can only run update as the Newstar" 
                        ." manager..." , "" ) ; 
    &exit( '' ) ; 
  } 
  if ( !&eq( $cwd , $n_src ) && !&peq( $cwd ,  $n_src ."/*" ) && !&eq( $cwd 
                        , $n_import ) ) { 
    &echo( '' ,   "***** You are not allowed to run update from " . $cwd 
                        ."..." , "" ) ; 
    &echo( '' ,   "You should be either in " ."\$n_src" . 
                        " or one of it's subdirectories," , "" ) ; 
    &echo( '' ,   "or in directory " ."\$n_import" . ", not in " . $cwd 
                        ."..." , "" ) ; 
    if ( &ft('d', $n_import ) ) { 
      &cd(  &fn(  $n_import ) ) ; 
      &echo( '' ,   "Now in " . $n_import , "" ) ; 
    } 
    else { 
      &exit( '' ) ; 
    } 
  } 
  $SIG{'INT'}= Abort_exit_update ; 
#
#  Setup logging, initialise, check filesystem etc.
#
  &alias( 'log', '&doalias_x('. ''. '"!*"'. ','. '\'>\''. ','. 
                        '"p$$.tmp00"'. ')'. ';'. '&echo('. 
                        '&D_input(\'sw\')'. ','. '&D_input(\'inw\')'. ','. 
                        '&D_input(\'out\')'. ')'. ';'. '&tee('. '"-a"'. 
                        ','. ''. '&fn('. ''. '$Logfile'. ')'. ','. 
                        '"p$$.tmp00"'. ')'. ';'. '', "") ; 
  &source( &fn(  $n_src ."/sys/initcompile.csh" ) ) ; 
#
#  Decode switches, get command, or set menu mode if none given.
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
#
#
# Check the file system and try to make any missing directories
#
  if ( ! &ft('d', $n_lib ) ) { 
    $n_tmp= &fn(  $n_lib ) ; 
    if ( ! &ft('d', &fp('h', $n_tmp ) ) ) { 
      &doalias('log' ,  ">>>>>>>> Creating root of library tree... " . 
                        &fp('h', $n_tmp ) ) ; 
      &mkdir(  &fn(  &fp('h', $n_tmp ) ) ) ; 
    } 
    &doalias('log' ,  ">>>>>>>> Creating library directory for " . $n_arch 
                        ."...  " . $n_lib ) ; 
    &mkdir(  &fn(  $n_lib ) ) ; 
    if ( ! &ft('d', $n_lib ) ) { 
      &doalias('log' ,  " " ) ; 
      &doalias('log' ,  "Could not create library directory " . $n_lib 
                        ."..." ) ; 
      &Abort_exit_update ; 
    } 
  } 
  if ( ! &ft('d', $n_inc ) ) { 
    &doalias('log' ,  ">>>>>>>> Creating include directory " . $n_inc ) ; 
    &mkdir(  &fn(  $n_inc ) ) ; 
    if ( ! &ft('d', $n_inc ) ) { 
      &doalias('log' ,  " " ) ; 
      &doalias('log' ,  "Could not create include directory " . $n_inc 
                        ."..." ) ; 
      &Abort_exit_update ; 
    } 
  } 
  if ( ! &ft('d', $n_exe ) ) { 
    $n_tmp= &fn(  $n_exe ) ; 
    if ( ! &ft('d', &fp('h', $n_tmp ) ) ) { 
      &doalias('log' ,  ">>>>>>>> Creating root of binary tree... " . 
                        &fp('h', $n_tmp ) ) ; 
      &mkdir(  &fn(  &fp('h', $n_tmp ) ) ) ; 
    } 
    &doalias('log' ,  ">>>>>>>> Creating binary directory for " . $n_arch 
                        ."...  " . $n_exe ) ; 
    &mkdir(  &fn(  $n_exe ) ) ; 
    if ( ! &ft('d', $n_exe ) ) { 
      &doalias('log' ,  " " ) ; 
      &doalias('log' ,  "Could not create binary directory " . $n_exe 
                        ."..." ) ; 
      &Abort_exit_update ; 
    } 
  } 
  if ( ! &ft('d', $n_hlp ) ) { 
    &doalias('log' ,  ">>>>>>>> Creating hypertext directory " . $n_hlp ) ; 
    &mkdir(  &fn(  $n_hlp ) ) ; 
    if ( ! &ft('d', $n_hlp ) ) { 
      &doalias('log' ,  " " ) ; 
      &doalias('log' ,  "Could not create include directory " . $n_hlp 
                        ."..." ) ; 
      &Abort_exit_update ; 
    } 
  } 
  if ( ! &ft('d', $n_tst ) ) { 
    $n_tmp= &fn(  $n_tst ) ; 
    if ( ! &ft('d', &fp('h', $n_tmp ) ) ) { 
      &doalias('log' ,  ">>>>>>>> Creating root of test binary tree... " . 
                        &fp('h', $n_tmp ) ) ; 
      &mkdir(  &fn(  &fp('h', $n_tmp ) ) ) ; 
    } 
    &doalias('log' ,  ">>>>>>>> Creating test binary directory for " . 
                        $n_arch ."...  " . $n_tst ) ; 
    &mkdir(  &fn(  $n_tst ) ) ; 
    if ( ! &ft('d', $n_tst ) ) { 
      &doalias('log' ,  " " ) ; 
      &doalias('log' ,  "Could not create test binary directory " . $n_tst 
                        ."..." ) ; 
      &Abort_exit_update ; 
    } 
  } 
  if ( ! &ft('d', $n_work ) ) { 
    $n_tmp= &fn(  $n_work ) ; 
    if ( ! &ft('d', &fp('h', $n_tmp ) ) ) { 
      &doalias('log' ,  ">>>>>>>> Creating root of work directory tree... " 
                        . &fp('h', $n_tmp ) ) ; 
      &mkdir(  &fn(  &fp('h', $n_tmp ) ) ) ; 
    } 
    &doalias('log' ,  ">>>>>>>> Creating work directory for " . $n_arch 
                        ."... " . $n_work ) ; 
    &mkdir(  &fn(  $n_work ) ) ; 
    if ( ! &ft('d', $n_work ) ) { 
      &doalias('log' ,  " " ) ; 
      &doalias('log' ,  "Could not create work directory " . $n_work ."..." 
                        ) ; 
      &Abort_exit_update ; 
    } 
  } 
  if ( ! &ft('d', $n_import ) ) { 
    &doalias('log' ,  ">>>>>>>> Creating directory for import...  " . 
                        $n_import ) ; 
    &mkdir(  &fn(  $n_import ) ) ; 
    &chmod( "a+rwx" ,  &fn(  $n_import ) ) ; 
    if ( ! &ft('d', $n_import ) ) { 
      &doalias('log' ,  " " ) ; 
      &doalias('log' ,  "Could not create import directory " . $n_import 
                        ."..." ) ; 
      &Abort_exit_update ; 
    } 
  } 
#
#
# Remove any existing locks and create a new one
#
  if ( &ft('e',  $n_work ."/update.lock" ) ) { 
    &echo( '' ,   "Cannot update on " . $n_arch .": " . &Pipe("p$$.tmp00", 
                        &cat( '' ,  &fn(  $n_work ."/update.lock" ) , 
                        "p$$.tmp00" ) ) , "" ) ; 
    &echo( "-n" ,   "Remove " , "" ) ; &rm( "-i" ,  &fn(  $n_work 
                        ."/update.lock" ) ) ; 
    if ( &ft('e',  $n_work ."/update.lock" ) ) { &Abort_exit_update ; } 
  } 
  &echo( '' ,   "Locked by " . $USER ." at " . $C_Date ." " . $C_Time , ''. 
                        &fn(  $n_work ."/update.lock" ) ) ; 
#
# Check wether the various precompilers exist
#
  if ( ! &ft('e',  $n_exe ."/genaid.exe" ) ) { 
    &doalias('log' ,  "Building utility program genaid (" . $n_exe 
                        ."/genaid.exe)" ) ; 
    &dollar("CC" ,  "-o" .' '. &fn(  $n_exe ."/genaid.exe" ) .' '. &fn(  
                        $n_src ."/sys/genaid.c" ) , "" ) ; 
    if ( ! &ft('e',  $n_exe ."/genaid.exe" ) ) { &Abort_exit_update ; } 
  } 
  if ( ! &ft('e',  $n_exe ."/docaid.exe" ) ) { 
    &doalias('log' ,  "Building documentation program docaid (" . $n_exe 
                        ."/docaid.exe)" ) ; 
    &dollar("CC" ,  "-o" .' '. &fn(  $n_exe ."/docaid.exe" ) .' '. &fn(  
                        $n_src ."/sys/docaid.c" ) , "" ) ; 
    if ( ! &ft('e',  $n_exe ."/docaid.exe" ) ) { &Abort_exit_update ; } 
  } 
  if ( ! &ft('e',  $n_exe ."/wntinc.exe" ) ) { 
    &doalias('log' ,  "Missing dsc-compiler (" . $n_exe ."/wntinc.exe)" ) ; 
    &doalias('log' ,  "Run  update build wntinc  to build it first" ) ; 
#   if ("$Mode" != "Menu") goto Abort_exit
  } 
  if ( ! &ft('e',  $n_exe ."/sys_bldppd.exe" ) ) { 
    &doalias('log' ,  "Missing ppd-compiler (" . $n_exe ."/sys_bldppd.exe)" 
                        ) ; 
#   if ("$Mode" != "Menu") goto Abort_exit
  } 
#
#
# If in Menu mode, repeatedly ask commands, else just one command
#
  while ( !&eq(   $Mode ,  "Quit" ) ) { 
    if ( &eq(   $Mode ,  "Menu" ) ) { 
      &echo( '' ,   "Commands are: update, build, cont,  check, r" 
                        ."etrieve, clean, " , "" ) ; 
      &echo( '' ,   "              diff,   pack,  group, help,  q" ."uit" , 
                        "" ) ; 
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
    elsif ( &eq(   $Mode ,  "Update" ) ) { 
      $Command= &fn(  (split(' ',$Upd_list)) [ 1 -1 ]  # Get next command
                        ) ; 
      if ( &vn($Upd_list) > 1 ) { 
        @Upd_list=split(' ',$Upd_list) ;               # Get rid of command
                        shift(@Upd_list) ; $Upd_list=join(' ',@Upd_list) ; 
        $Files= &fn(  (split(' ',$Upd_list)) [ 1 -1 ]  # And it's argument
                        ) ; 
        if ( &vn($Upd_list) > 1 ) {                    # Get rid of argument
                        @Upd_list=split(' ',$Upd_list) ; shift(@Upd_list) ; 
                        $Upd_list=join(' ',@Upd_list) ; } 
      } 
      else { 
        $Files=  '' ; 
      } 
    } 
    else { 
      $Mode=  "Quit" ; 
    } 
    if ( &eq(   $Command ,  '' ) || &peq(   $Command , "[Qq]*" ) ) { 
      $Mode=  "Quit" ; 
#
#  %Help command:
#
    } 
    elsif ( &peq(   $Command , "[Hh]*" ) ) { 
      sub C2_t1_update { 
        local(*TMP); 
        open(TMP,">txt$$.tmp"); 
        print TMP  "#+++++++++++++++++++++++++++++++++++++++++++" 
                        ."++++++++++++++++++++++++++++++#" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  " Update is used for maintenance of the Newst" 
                        ."ar master system. " ."\n" ; 
        print TMP  " You are only allowed to run update when you" 
                        ." are the owner of \$n_root." ."\n" ; 
        print TMP  " " ."\n" ; 
        print TMP  " Update can only be run from the root of the" 
                        ." master source tree (\$n_src), " ."\n" ; 
        print TMP  " from one of it's subdirectories (\$n_src/*)" 
                        ." or from the directory for" ."\n" ; 
        print TMP  " import of new files (\$n_import). Update le" 
                        ."aves a full transaction log" ."\n" ; 
        print TMP  " in \$n_src, with name 'updyymmdd[i].log' (w" 
                        ."here yymmdd is the current " ."\n" ; 
        print TMP  " date and i is an integer 1,2,...)." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  " For program development and debugging, plea" 
                        ."se use the \"shadow\" command," ."\n" ; 
        print TMP  " in particular shadow build." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  " Update can be called in one of the followin" 
                        ."g ways:" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     nup " ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Enter a menu mode where all options " 
                        ."listed below can be " ."\n" ; 
        print TMP  "        chosen. Additional arguments will be" 
                        ." prompted for." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     nup update" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        This will check your current impleme" 
                        ."ntation against the" ."\n" ; 
        print TMP  "        master copy at NFRA, retrieve any ne" 
                        ."w files, build them," ."\n" ; 
        print TMP  "        clean up your local copy and librari" 
                        ."es and rebuild any" ."\n" ; 
        print TMP  "        out-of-date executables." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     nup build  [switches | groupfile | 'all" 
                        ."' ] ..." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Accepts one or more groupfiles which" 
                        ." should be either " ."\n" ; 
        print TMP  "        in the current dir or in a subdirect" 
                        ."ory of the master source " ."\n" ; 
        print TMP  "        tree (the extension .grp may be ommi" 
                        ."tted). Normally no switches " ."\n" ; 
        print TMP  "        will be given (this is updating the " 
                        ."master, so you should in " ."\n" ; 
        print TMP  "        principle hardcode things in files i_" . 
                        $n_arch . $n_site .".csh)." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        If you specify the -Update switch, e" 
                        ."xecutable files will end up " ."\n" ; 
        print TMP  "        in \$n_exe, if you specify -NUpdate " 
                        ."they land in \$n_tst." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        The groupfiles are scanned and all f" 
                        ."iles contained therein" ."\n" ; 
        print TMP  "        that are relevant to this architectu" 
                        ."re are compiled." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Processing takes place in a pre-defi" 
                        ."ned order of filetypes," ."\n" ; 
        print TMP  "        you may select some types only by us" 
                        ."ing the -Types switch." ."\n" ; 
        print TMP  "        You may select to compile only a sub" 
                        ."set of the filetypes by" ."\n" ; 
        print TMP  "        setting -Types:ask, which will cause" 
                        ." the update to prompt " ."\n" ; 
        print TMP  "        you (in advance) for the requested p" ."asses." 
                        ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        If the groupfile was in \$n_import a" 
                        ."nd no errors occurred then" ."\n" ; 
        print TMP  "        all files in the groupfiles (except " 
                        .".exe) are copied into their " ."\n" ; 
        print TMP  "        appropriate directory in the Master " 
                        ."source tree. " ."\n" ; 
        print TMP  "        Once successfully copied, each file " 
                        ."is unlocked in the locking " ."\n" ; 
        print TMP  "        database (lock.idx). The master data" 
                        ."base (database.idx) is" ."\n" ; 
        print TMP  "        updated." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        If there are dependent files (that i" 
                        ."s: files that need to be " ."\n" ; 
        print TMP  "        rebuilt because an include file has " 
                        ."been updated), their names" ."\n" ; 
        print TMP  "        are stored in \$n_work/depend.grp.Th" 
                        ."is file is processed at the" ."\n" ; 
        print TMP  "        end of each pass. This ensures that " 
                        ."any dependencies introduced" ."\n" ; 
        print TMP  "        by the dependent files are teated pr" 
                        ."operly (eg. an update of " ."\n" ; 
        print TMP  "        a DSF file causes some dependent DSC" 
                        ." files to be rebuilt, and" ."\n" ; 
        print TMP  "        these DSC files cause some Fortan an" 
                        ."d C sources to be rebuilt etc." ."\n" ; 
        print TMP  "        Checking of dependencies can be disa" 
                        ."bled with the -NCheck switch." ."\n" ; 
        print TMP  " " ."\n" ; 
        print TMP  "        The file depend.grp is kept in \$n_w" 
                        ."ork for inspection" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        The groupfile should explicitly ment" 
                        ."ion the .exe files that " ."\n" ; 
        print TMP  "        need to be rebuilt!  " ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    nup continue  [switches | groupfile | 'a" 
                        ."ll' ] ..." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Resumes a build that has crashed som" 
                        ."ewhere. To rebuild some" ."\n" ; 
        print TMP  "        files, please edit \$n_work/continue" .".idx" 
                        ."\n" ; 
        print TMP  "        " ."\n" ; 
        print TMP  "        Use this option with care." ."\n" ; 
        print TMP  "        " ."\n" ; 
        print TMP  "    nup retrieve ['all' | groupfile [inet-ad" 
                        ."dress [user [directory]]] ]" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Default internet specifications are " 
                        ."'all' and the NFRA Master node." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        If the literal 'all' is specified, t" 
                        ."he Master database will" ."\n" ; 
        print TMP  "        be retrieved from the remote node. I" 
                        ."f no local database " ."\n" ; 
        print TMP  "        exists, it will be updated (this wil" 
                        ."l take some time but is " ."\n" ; 
        print TMP  "        the only safe way to proceed). The m" 
                        ."aster database will be " ."\n" ; 
        print TMP  "        checked against the local database. " 
                        ."From this comparison," ."\n" ; 
        print TMP  "        a groupfile is constructed." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        If a groupfile is specified in stead" 
                        ." of 'all', it will be" ."\n" ; 
        print TMP  "        retrieved from the remote node if it" 
                        ." does not already exist." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        All files in the (constructed or ret" 
                        ."rieved) groupfile " ."\n" ; 
        print TMP  "        will be retrieved over the network." ."\n" ; 
        print TMP  "        To update the files, use the \"nup b" 
                        ."uild retrieved\" command." ."\n" ; 
        print TMP  "        All files are received in " . $n_import 
                        .", which should be empty." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     nup diff [groupfile] ..." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Compare files in " . $n_import 
                        ." (listed in the groupfile) with the" ."\n" ; 
        print TMP  "        versions in the master tree." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     nup clean [directory | 'all']" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Verify the specified directory in th" 
                        ."e source tree against " ."\n" ; 
        print TMP  "        the groupfiles in that directory. " ."\n" ; 
        print TMP  "        If no directory is specified, check " 
                        ."them all. " ."\n" ; 
        print TMP  "        If the literal 'all' is specified, c" 
                        ."heck against the" ."\n" ; 
        print TMP  "        the master database (" . $n_src 
                        ."/sys/database.idx)." ."\n" ; 
        print TMP  "        Remove (with confirm) any files that" 
                        ." exist but are not " ."\n" ; 
        print TMP  "        mentioned in a groupfile," ."\n" ; 
        print TMP  "        Report any files that do not exist b" 
                        ."ut are in a groupfile." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     nup check ['all' | fdl ]" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Default for the argument is all" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        If the argument is all or contains a" ."n f:" 
                        ."\n" ; 
        print TMP  "          Verify the master source tree agai" 
                        ."nst the master database." ."\n" ; 
        print TMP  "          Checksums, sizes and dates are com" 
                        ."pared. A groupfile is " ."\n" ; 
        print TMP  "          created in " . $n_import 
                        ." with entries for defect and missing " ."\n" ; 
        print TMP  "          files, this groupfile can be proce" 
                        ."ssed with \"nup retrieve\"." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        If the argument is all or contains a" ." d:" 
                        ."\n" ; 
        print TMP  "          A new master database is created w" 
                        ."hich reflects the current " ."\n" ; 
        print TMP  "          situation." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        If the argument is all or contains a" ."n l:" 
                        ."\n" ; 
        print TMP  "          The object libraries are checked f" 
                        ."or double entries and" ."\n" ; 
        print TMP  "          out-of-date files." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     nup save" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Start a full backup of the master sy" 
                        ."stem (in the background) " ."\n" ; 
        print TMP  "        All files below \$n_root will be wri" 
                        ."tten to tape. A log is" ."\n" ; 
        print TMP  "        kept in \$n_root/backups.txt. A roul" 
                        ."ating tape pool can be " ."\n" ; 
        print TMP  "        used. The actual command for the bac" 
                        ."kup can be specified by" ."\n" ; 
        print TMP  "        the user. Default is the command pre" 
                        ."viously used." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     nup pack [name_of_directory... | 'all']" ."\n" ; 
        print TMP  "      " ."\n" ; 
        print TMP  "        Put all files below the directories " 
                        ."in a tar file (name " ."\n" ; 
        print TMP  "        defaults to nstar_name.tar.Z, where " 
                        ."name is the last item " ."\n" ; 
        print TMP  "        in the directory specification or, i" 
                        ."f the directory roots in" ."\n" ; 
        print TMP  "        " . $n_src .", " . $n_exe ." or " . $n_lib 
                        .": nstar_yyy_name.tar.Z, where" ."\n" ; 
        print TMP  "        yyy is either src, exe or lib. " ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        If the literal 'all' is given, archi" 
                        ."ves will be made for the" ."\n" ; 
        print TMP  "        source tree, " . $n_inc .", " . $n_hlp 
                        ." and the executable trees for hp " ."\n" ; 
        print TMP  "        and sw." ."\n" ; 
        print TMP  "        " ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     The following command is an interface b" 
                        ."etween old and new style" ."\n" ; 
        print TMP  "     groupfiles for update and retrieval:" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "     nup group groupfile(s)" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "          if a single input groupfile is spe" 
                        ."cified and if it contains" ."\n" ; 
        print TMP  "          lines starting with +, it is split" 
                        ." out in a series of files" ."\n" ; 
        print TMP  "          for the corresponding directories" ."\n" ; 
        print TMP  " " ."\n" ; 
        print TMP  "          otherwise the groupfiles are trans" 
                        ."formed in a single groupfile" ."\n" ; 
        print TMP  "          as expected by retrieve." ."\n" ; 
        print TMP  "           " ."\n" ; 
        print TMP  "#-------------------------------------------" 
                        ."------------------------------#" ."\n" ; 
        print TMP  '' ."\n" ; 
        close(TMP); 
        "txt$$.tmp";} 
      &cat( '' ,  &C2_t1_update , "" ) ; 
#
#
#   %Update command:
#
    } 
    elsif ( &peq(   $Command , "[Uu]*" ) ) { 
#
# First time, mode will not be Update
#
      if ( !&eq(   $Mode ,  "Update" ) ) { 
#
#  Is it a valid host?
#
        $Hosts=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $n_hosts ) , 
                        "p$$.tmp01" ) , &tr( '' , ',' , ' ' , "p$$.tmp01" , 
                        "p$$.tmp00" ) ) ; 
        $Flag=  '' ; 
        for $name__x (split(' ',join(' ' , &fn(  $Hosts ) ))) { 
                        $name=$name__x ; 
          if ( &eq(   $name , $HOST ) ) { $Flag= &fn(  $name ) ; } 
        } 
        if ( &eq(   $Flag ,  '' ) ) { 
          &doalias('log' ,  "Invalid host " . $HOST 
                        .", you should run on one of " . $n_hosts ) ; 
#
#  Yes, we run a couple of commands, which one depends on the argument
#
        } 
        elsif ( &eq(   $Files ,  '' ) ) { 
          &cd(  &fn(  $n_import ) ) ; 
          &echo( '' ,   "Now in " . $n_import , "" ) ; 
          $_Update= "1" ;                              # For Build
          $_Confirm='' ;                               # For Clear
          $Mode= "Update" ;                            # Process multiple commands
          $Upd_list= "retrieve" .' '. "all" .' '. "build" .' '. "retrieved" 
                        .' '. "clear" .' '. "all" .' '. "check" .' '. "l" 
                        .' '. "check" .' '. "e" .' '. "update" .' '.  '' ; 
        } 
        elsif ( &eq(   $Files ,  "rsh" ) ) { 
          &cd(  &fn(  $n_import ) ) ; 
          &echo( '' ,   "Now in " . $n_import , "" ) ; 
          $_Update= "1" ;                              # For Build
          $_Confirm='' ;                               # For Clear
          $Mode= "Update" ;                            # Process multiple commands
          $Upd_list= "build" .' '. "retrieved" .' '. "check" .' '. "l" 
                        .' '. "check" .' '. "e" .' '. "quit" .' '.  '' ; 
        } 
        else { 
          &doalias('log' ,  "Invalid argument for Update command" ) ; 
        } 
#
#  Not first time, we spawn a series of remote commands
#
      } 
      else { 
        if ( ! defined($RSH) ) {                       # Make sure we have rsh
          $RSH= "rsh" ; &ENV_EXPORT( RSH , "rsh" ) ; 
          if ( &eq( $n_arch , "hp" ) ) { $RSH= "remsh" ; &ENV_EXPORT( RSH , 
                        "remsh" ) ; } 
        } 
        for $name__x (split(' ',join(' ' , &fn(  $Hosts ) ))) { 
                        $name=$name__x ; 
          if ( !&eq(   $name , $HOST ) ) { 
            &echo( '' ,   "Now trying to update on " . $HOST 
                        ." with command" , "" ) ; 
            &echo( '' ,    $RSH ." " . $name ." " , "" ) ; 
            &echo( '' ,   "( source " . $n_src ."/sys/newstar_" . $n_site 
                        .".csh; nup update rsh)" , "" ) ; 
            &dollar("RSH" ,  &fn(  $name ) .' '. '( source ' . $n_src 
                        ."/sys/newstar_" . $n_site .".csh" 
                        .'; nup update rsh)' , "" ) ; 
          } 
        } 
        $Mode=  "Quit" ; 
      } 
#
#
#   %Build command:
#
    } 
    elsif ( &peq(   $Command , "[Bb]*" ) || &peq(   $Command , 
                        "[Cc][Oo][Nn][Tt]*" ) ) { 
      if ( &peq(   $Command , "[Bb]*" ) ) { 
        $Cmd=  "build" ; 
        if ( &ft('e',  $n_work ."/continue.idx" ) ) {  # Files already done
          &rm( "-f" ,  &fn(  $n_work ."/continue.idx" ) ) ; 
        } 
        if ( &ft('e',  $n_work ."/depend.grp" ) ) {    # Remaining dependencies
          &rm( "-f" ,  &fn(  $n_work ."/depend.grp" ) ) ; 
        } 
        $nonomatch='' ; 
        $file= &fn(  $n_work ."/*.?lb.list" ) ;        # Files to be archived
        if ( &ft('e', (split(' ',$file)) [ 1 -1 ] ) ) { 
          &rm( "-f" ,  &fn(  $n_work ."/*.?lb.list" ) ) ; 
        } 
        undef $nonomatch ; 
      } 
      else { 
        $Cmd=  "cont" ; 
      } 
      if ( $_Update ) { 
        $Cmd=   $Cmd ." -U  " ; 
      } 
      else { 
        $Cmd=   $Cmd ." -NU " ; 
        $n_uexe= &fn(  $n_tst ) ; &ENV_EXPORT( n_uexe  # Executables/ppd files in $n_tst
                        , &fn(  $n_tst ) ) ; 
      } 
      if ( ! &ft('e',  $n_work ."/continue.idx" ) ) { &touch( '' ,  &fn(  
                        $n_work ."/continue.idx" ) ) ; } 
      if ( ! &ft('e',  $n_work ."/depend.grp" ) ) { &touch( '' ,  &fn(  
                        $n_work ."/depend.grp" ) ) ; } 
#
# Save the current version. We need to increase the version if we update 
# from $n_import, and we need to do that only once.
#
      $O_Version= &fn(  $C_Version ) ; 
#
# Get input files, check defaults, prepare for general log
#
      if ( &eq(   $Mode ,  "Update" ) ) { $_Types=  "^exe" ; } 
      if ( &eq(   $Files ,  '' ) ) { 
        &echo( "-n" ,   "Enter name of groupfile(s) or all: " , "" ) ; 
        $noglob='' ;                                   # Don't expand wildcards right now
        $Files=  ($_=scalar(<STDIN>), chop, $_) ;      # Read from stdin
        $Files= &fn(  $Files ) ;                       # Split in multiple words
        undef $noglob ; 
      } 
      if ( &eq(   $Files ,  '' ) || &peq(   $Files , "[Aa][Ll][Ll]" ) ) { 
        $Cmd=   $Cmd ." -NC all" ; 
        $Files=  $n_src .'/*/???.grp' ; 
        $_Check='' ; 
      } 
      elsif ( &peq(   $Files , "[Ww][Nn][Tt][Ii][Nn][Cc]" ) ) { 
        $Cmd=   $Cmd ." -NC wntinc" ; 
        $Files= &fn(  $n_src ."/wng/wnt_boot" ) .' '. &fn(  $n_src 
                        ."/wng/wng" ) .' '. &fn(  $n_src ."/wng/wnc" ) 
                        .' '. &fn(  $n_src ."/wng/wnf" ) .' '. &fn(  $n_src 
                        ."/wng/wnt" ) ; 
        $_Check='' ; 
      } 
      elsif ( $_Check ) { 
        $Cmd=   $Cmd ." -C  " . $Files ; 
      } 
      else { 
        $Cmd=   $Cmd ." -NC " . $Files ; 
      } 
#
#  To process files just retrieved, move to $n_import
#
      if ( &eq(   $Files ,  "retrieved" ) && !&eq( $PWD , $n_import ) ) { 
        &cd(  &fn(  $n_import ) ) ; 
        &echo( '' ,   "Now in " . $n_import , "" ) ; 
      } 
#
#
#  Expand wildcards, check existence of all files in advance.
#  Only files in $n_src/* and $n_import are allowed.
#  The current directory will be $n_src, $n_src/* or $n_import.
#
      $Input_file=  '' ; 
      $noglob='' ; $nonomatch='' ; 
      for $File__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $File=$File__x ; 
        if ( &ft('d', $File ) ) { 
          undef $noglob ; $File= &fn(  $File ."/???.grp" ) ; $noglob='' ; 
        } 
        elsif ( &eq(   &fp('e', $File ) ,  '' ) ) { 
          $File= &fn(  $File .".grp" ) ; 
        } 
        $Input_file= &fn(  $Input_file ) .' '. &fn(  $File ) ; 
      } 
      undef $noglob ; 
      $Input_file= &fn(  $Input_file ) ; 
      $Files=  '' ; 
      for $File__x (split(' ',join(' ' , &fn(  $Input_file ) ))) { 
                        $File=$File__x ; 
        if ( &ft('e', $File ) ) { 
          if ( &eq( &fp('h', $File ) , $File ) ) {     # Only name given
            $File= &fn(  $cwd ."/" . $File ) ; 
            if ( &eq( $cwd , $n_import ) ) { 
              &doalias('log' ,  "%%%%%%% Updating " . &fp('t', $File ) 
                        ." from " ."\$n_import..." ) ; 
            } 
          }                                            # wrt n_src
          elsif ( !&peq( $File , "*/*/" . &fp('t', $File ) ) && &eq( $cwd , 
                        $n_src ) ) { 
            $File= &fn(  $n_src ."/" . $File ) ; 
          } 
          if ( &peq( $File ,  $n_src ."/*/" . &fp('t', $File ) ) || &eq( 
                        $File ,  $n_import ."/" . &fp('t', $File ) ) ) { 
            $Files= &fn(  $Files ) .' '. &fn(  $File ) ; 
          } 
          else { 
            &doalias('log' ,  "Specify files in Master source tree or " 
                        ."\$n_import" ) ; 
            &doalias('log' ,   $File ." ignored." ) ; 
          } 
        } 
        else { 
          &doalias('log' ,  "Cannot find groupfile " . $File .", ignored." 
                        ) ; 
        } 
      } 
      undef $nonomatch ; 
#
# Select the types to be processed and do global log
#
      $typelist=  "grp/idx/kwa" .' '.  "tex/txt/hlp/html/gif/gfs" .' '.  
                        "scn/wmp/mdl/ngf/flf" .' '.  "csh/com/c" ."\$" 
                        .' '.  "x" . $n_arch ."/a" . $n_arch .' '.  
                        "inc/dsf/pef/def" .' '.  "dsc" .' '.  
                        "f/for/fsc/fun/f" . $n_arch .' '.  "cee/csc/cun/c" 
                        . $n_arch ."/s" ."\$" .' '.  "exe" .' '.  "pin/psc" 
                        ; 
      if ( !&eq(   $_Types ,  "." ) && !&eq(   $_Types ,  '' ) ) { 
        if ( &peq(   $_Types , "^[Ee][Xx][Ee]" ) ) { 
          @typelist=split(' ',$typelist); splice(@typelist, "10" -1,1,  '' 
                        ); $typelist=join(' ',@typelist); 
          $typelist= &fn(  $typelist ) ; 
        } 
        elsif ( !&peq(   $_Types , "[Aa][Ss][Kk]" ) ) { 
          $typelist=   $_Types ; 
        } 
        else { 
          &echo( '' ,   "Selecting file-types to process: " , "" ) ; 
          $tmp=  "groupfiles" .' '.  "help files" .' '.  "data files" .' '. 
                         "scripts" .' '.  "special binaries" .' '.  
                        "include files" .' '.  "dsc definition files" .' '. 
                         "fortran sources" .' '.  "c sources and macros" 
                        .' '.  "executables" .' '.  "pin-files" ; 
          $ii= 1 ; 
          while ( $ii <= &vn($typelist) ) { 
            &echo( "-n" ,   "Do " . (split(' ',$tmp)) [ $ii -1 ] ." (" . 
                        (split(' ',$typelist)) [ $ii -1 ] .")? (y,n) [Y] " 
                        , "" ) ; 
            $Flag=  ($_=scalar(<STDIN>), chop, $_) ; 
            if ( &peq( $Flag , "[Nn]*" ) ) { 
                        @typelist=split(' ',$typelist); splice(@typelist, 
                        &fn(  $ii ) -1,1,  '' ); 
                        $typelist=join(' ',@typelist); } 
            $ii= $ii + 1 ; 
          } 
          undef $tmp ; 
          $typelist= &fn(  $typelist ) ; 
        } 
        &echo( '' ,    $C_Date ." " . $C_Time ." - " . $n_arch ." - " . 
                        $Cmd ." (" . $typelist .") " , '>'. &fn(  $n_root 
                        ."/updates.log" ) ) ; 
      } 
      else { 
        &echo( '' ,    $C_Date ." " . $C_Time ." - " . $n_arch ." - " . 
                        $Cmd ." (all) " , '>'. &fn(  $n_root 
                        ."/updates.log" ) ) ; 
      } 
      $Errors= 0 ; 
      &doalias('log' ,  "Logging information in " . $Logfile ) ; 
      &doalias('log' ,  "%%%%%%% Errors so far: " . $Errors ." (" . 
                        &Pipe("p$$.tmp00", &date( "p$$.tmp00" ) ) . ")" ) ; 
#
#  The check flag determines wether we want to do dependecy checking
#
      if ( $_Check ) { 
        $Depend= &fn(  $n_work ."/depend.grp" ) ; 
      } 
      else { 
        $Depend=  '' ; 
        &doalias('log' ,  "%%%%%% No dependency checks" ) ; 
      } 
      for $ftype__x (split(' ',join(' ' , &fn(  $typelist ) ))) { 
                        $ftype=$ftype__x ; 
        if ( &eq(   $ftype ,  "exe" ) && !&eq( $Errors , 0 ) ) { 
          &doalias('log' ,  "======== Errors found, skip executables ====" 
                        ."===" ) ; 
        } 
        else { 
          &doalias('log' ,  " " ) ; 
          &doalias('log' ,  "======== Pass for filetypes " . $ftype 
                        ." ========" ) ; 
          for $grpfile__x (split(' ',join(' ' , &fn(  $Files ) , &fn(  
                        $Depend ) ))) { $grpfile=$grpfile__x ; 
            if ( &eq( $grpfile ,  $n_work ."/depend.grp" ) ) { 
              &echo( '' ,   "Checking dependencies..." , "" ) ; 
              &sort( "-u -o" ,  &fn(  $n_work ."/depend.grp" ) .' '. &fn(  
                        $n_work ."/depend.grp" ) , "" ) ; 
            } 
            $Newlib= &fn(  &fp('h', $grpfile ) ) ; 
            if ( &eq( $Newlib , $grpfile ) ) { $Newlib= &fn(  &fp('h', $cwd 
                        ) ) ; } 
            $Newlib= &fn(  &fp('t', $Newlib ) ) ; 
            if ( &peq(   $Newlib , "n*" ) ) { $Newlib= "nst" ; } 
            &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "expand" .' '. &fn( 
                        "-t:" . $ftype ) .' '. &fn(  $grpfile ) , ''. &fn(  
                        $Tmpfile ) ) ; 
            if ( ! &ft('z', $Tmpfile ) ) { 
              &doalias('log' ,  " " ) ; 
              &doalias('log' ,  "=== Groupfile " . $grpfile ." " ) ; 
            } 
            $nline=  &Pipe("p$$.tmp00", &cat( '' ,     # Count lines
                        &fn(  $Tmpfile ) , "p$$.tmp01" ) , &wc( "-l" , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
            $iline= -9 ;                               # Means 1 after first increment by 10
            while ( $iline < $nline ) {                # Lines left?
              $iline= $iline + 10 ;                    # Next series of 10
              for $wfile__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", 
                        &tail( "+$iline" ,  &fn(  $Tmpfile ) , "p$$.tmp01" 
                        ) , &head( "-10" , "p$$.tmp01" , "p$$.tmp00" ) ) 
                        ))) { $wfile=$wfile__x ; 
                if ( &peq(   $wfile , "-*" ) || &peq(   $wfile , "+*" ) ) { 
                  $Options= "local" .' '. &fn(  $wfile ) ; 
                  &source( &fn(  $n_src ."/sys/switches.csh" ) ) ; 
                } 
                else { 
                  $Flag=  &Pipe("p$$.tmp00", &grep( '' , '^' . $wfile .'$' 
                        ,  &fn(  $n_work ."/continue.idx" ) , "p$$.tmp00" ) 
                        ) ; 
                  if ( !&eq(   $Flag ,  '' ) ) { 
                    &doalias('log' ,  "Skipping " . $wfile 
                        ." (already done)" ) ; 
                  } 
                  elsif ( &peq( $wfile , "abpx_*.exe" ) && ! 
                        defined($n_doabp) ) { 
                    &doalias('log' ,  "Not compiling abp-executables" ) ; 
                  } 
                  else { 
                    $Input_file= &fn(  $wfile ) ; 
                    if ( &eq( $grpfile ,  $n_import ."/" . &fp('t', 
                        $grpfile ) ) || &eq( $grpfile ,  $n_work 
                        ."/depend.grp" ) ) { 
                      $Newlib= &fn(  &fp('h', $wfile ) ) ; 
                      $Newlib= &fn(  &fp('t', $Newlib ) ) ; 
                      if ( &peq(   $Newlib , "n*" ) ) { $Newlib= "nst" ; } 
#
# If we take a file from $n_import, we have to increase the version number
#
                      if ( &eq( $cwd , $n_import ) && &ft('e',  $n_import 
                          ."/" . &fp('t', $wfile ) ) ) { 
                        $Input_file= &fn(  $n_import ."/" . &fp('t', $wfile 
                            ) ) ; 
                        if ( &eq(   $O_Version ,   $C_Version ) ) { 
                          if ( &eq( $n_site , "nfra" ) ) { 
                            $C_Version=  &Pipe("p$$.tmp00", &echo( '' ,  
                                &fn(  $C_Version ) , "p$$.tmp01" ) , &awk( 
                                "-F." , '{ printf "%s.%d",$1,$2+1 }' , 
                                "p$$.tmp01" , "p$$.tmp00" ) ) ; 
                          } 
                          else { 
                            $C_Version=  &Pipe("p$$.tmp00", &echo( '' ,  
                                &fn(  $C_Version ) , "p$$.tmp01" ) , &awk( 
                                "-F." , '{ printf "%s.%d.%d",$1,$2,$3+1 }' , 
                                "p$$.tmp01" , "p$$.tmp00" ) ) ; 
                          } 
                        } 
                      } 
                    } 
#
# If this file is in different library, handle pending library operations
#
                    if ( !&eq(  $n_lib ."/" . $Newlib ."lib.olb" , 
                        $_Objectlib ) ) { 
                      &doalias('log' ,  "Library: " . &fp('t', $_Objectlib 
                          ) ." -> " . $Newlib ."lib.olb" ) ; 
                      if ( !&eq(   $_Objectlib ,  '' ) ) { $Input_file= 
                          &fn(  $_Objectlib ) .' '. &fn(  $Input_file ) ; } 
                      $_Objectlib= &fn(  $n_lib ."/" . $Newlib ."lib.olb" ) 
                          ; 
                    } 
                    if ( !&eq(  $n_src ."/" . $Newlib ."lib.olb" , 
                        $_Textlib ) ) { 
                      if ( !&eq(   $_Textlib ,  '' ) ) { 
                        $Input_file= &fn(  $n_src ."/" . $_Textlib ) .' '. 
                            &fn(  $Input_file ) ; 
                        $_Textlib= &fn(  $n_src ."/" . $Newlib ."lib.tlb" ) 
                            ; 
                      } 
                    } 
#
#  Compile, check dependencies and restore original switches
#
                    &source( &fn(  $n_src ."/sys/compile.csh" ) ) ; 
                    if ( defined($Abort_flag) ) { &Abort_exit_update ; } 
                    &echo( '' ,  &fn(  $wfile ) , '>'. &fn(  $n_work 
                        ."/continue.idx" ) ) ; 
                    $wfile= &fn(  &fp('t', $wfile ) ) ; 
                    if ( &eq(   &fp('e', $wfile ) ,  "dsc" ) ) { 
                      &grep( '' ,  "@" . &fp('r', $wfile ) ,  &fn(  $n_src 
                          ."/sys/database.idx" ) , '>'. &fn(  $n_work 
                          ."/depend.grp" ) ) ; 
                    } 
                    else { 
                      &grep( '' ,  "@" . $wfile ,  &fn(  $n_src 
                          ."/sys/database.idx" ) , '>'. &fn(  $n_work 
                          ."/depend.grp" ) ) ; 
                    } 
                    if ( !&eq(   $Save_switch ,  '' ) ) { 
                        &set($Save_switch) ; } 
                  } 
                } 
              } 
            } 
            if ( &ft('e', $Tmpfile ) ) { 
              &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
            } 
          } 
          &doalias('log' ,  "Error so far: " . $Errors ." (" . 
                        &Pipe("p$$.tmp00", &date( "p$$.tmp00" ) ) . ")" ) ; 
#
# Handle any pending library actions
#
          if ( !&eq(   $_Objectlib ,  '' ) ) { 
            $Input_file= &fn(  $_Objectlib ) ; 
            &source( &fn(  $n_src ."/sys/compile.csh" ) ) ; 
          } 
          if ( !&eq(   $_Textlib ,  '' ) ) { 
            $Input_file= &fn(  $_Textlib ) ; 
            &source( &fn(  $n_src ."/sys/compile.csh" ) ) ; 
          } 
        }                                              # If exe and errors
      } 
#
# Check any pending dependencies
#
      &echo( '' ,   "Checking pending dependencies..." , "" ) ; 
      &sort( "-u -o" ,  &fn(  $n_work ."/depend.grp" ) .' '. &fn(  $n_work 
                        ."/depend.grp" ) , "" ) ; 
      &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "expand" .' '. &fn(  
                        $grpfile ) , ''. &fn(  $Tmpfile ) ) ; 
      $nline=  &Pipe("p$$.tmp00", &cat( '' ,  &fn(     # Count lines
                        $Tmpfile ) , "p$$.tmp01" ) , &wc( "-l" , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
      $iline= -9 ;                                     # Means 1 after first increment by 10
      while ( $iline < $nline ) {                      # Lines left?
        $iline= $iline + 10 ;                          # Next series of 10
        for $wfile__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &tail( 
                        "+$iline" ,  &fn(  $Tmpfile ) , "p$$.tmp01" ) , 
                        &head( "-10" , "p$$.tmp01" , "p$$.tmp00" ) ) ))) { 
                        $wfile=$wfile__x ; 
          if ( !&peq(   $wfile , "-*" ) && !&peq(   $wfile , "+*" ) ) { 
            $Flag=  &Pipe("p$$.tmp00", &grep( '' , '^' . $wfile .'$' ,  
                        &fn(  $n_work ."/continue.idx" ) , "p$$.tmp00" ) ) 
                        ; 
            if ( &eq(   $Flag ,  '' ) ) { &echo( '' ,   
                        "Error: remaining dependency " . $wfile , "" ) ; } 
          } 
        } 
      } 
      &doalias('log' ,  "Total number of errors: " . $Errors ) ; 
      &doalias('log' ,  "Logging information in " . $Logfile ) ; 
#
#
#  Compilation errors
#
      if ( !&eq( $Errors , 0 ) ) { 
        &doalias('log' ,  "Compilation errors: cannot move files into " . 
                        $n_src ."..." ) ; 
#
#  -NUp was used, do not update files in the master tree
#
      } 
      elsif ( ! $_Update ) { 
        &doalias('log' ,  "You used -NUpdate, the master source tree re" 
                        ."mains intact" ) ; 
#
#  If it was an update, no errors occurred, and we were working in $n_import,
#  then files can be moved in the master source tree. 
# 
      } 
      elsif ( &eq( $cwd , $n_import ) ) { 
#
#  Remove references to groupfiles not in $n_import
#
        $Flag=  '' ; 
        for $grpfile__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $grpfile=$grpfile__x ; 
          if ( &ft('e',  $n_import ."/" . &fp('t', $grpfile ) ) ) { $Flag= 
                        &fn(  $Flag ) .' '. &fn(  $grpfile ) ; } 
        } 
        $Files= &fn(  $Flag ) ; 
#
#  Move files in the master (if that is still necessary)
#
        $files_moved= 0 ; 
        for $grpfile__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $grpfile=$grpfile__x ; 
          &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "files" .' '. "-t:^exe" 
                        .' '. &fn(  $grpfile ) , ''. &fn(  $Tmpfile ) ) ; 
          $nline=  &Pipe("p$$.tmp00", &cat( '' ,       # Count lines
                        &fn(  $Tmpfile ) , "p$$.tmp01" ) , &wc( "-l" , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
          $iline= -9 ;                                 # Means 1 after first increment by 10
          while ( $iline < $nline ) {                  # Lines left?
            $iline= $iline + 10 ;                      # Next series of 10
            for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &tail( 
                        "+$iline" ,  &fn(  $Tmpfile ) , "p$$.tmp01" ) , 
                        &head( "-10" , "p$$.tmp01" , "p$$.tmp00" ) ) ))) { 
                        $file=$file__x ; 
#
#  If the file does no longer exist in $n_import is has been moved in 
#  $n_src earlier, so no need to worry.
#
              if ( &ft('e',  $n_import ."/" . &fp('t', $file ) ) ) { 
                $files_moved= $files_moved + 1 ; 
                if ( !&eq(   &fp('h', $file ) ,  '' ) && ! &ft('d',   
                        $n_src ."/" . &fp('h', $file ) ) ) { 
                  &mkdir(  &fn(  $n_src ."/" . &fp('h', $file ) ) ) ; 
                  &echo( '' ,   "Created subdirectory " . &fp('h', $file ) 
                        , "" ) ; 
                } 
                $Flag= &fn(  &fp('t', $file ) ) ; 
                if ( &ft('e',  $n_ulib ."/" . &fp('r', $Flag ) .".o" ) ) { 
                  &rm( "-f" ,  &fn(  $n_ulib ."/" . &fp('r', $Flag ) .".o" 
                        ) ) ; 
                } 
                if ( &ft('e',  $n_lib ."/" . &fp('r', $Flag ) .".o" ) ) { 
                  &rm( "-f" ,  &fn(  $n_lib ."/" . &fp('r', $Flag ) .".o" ) 
                        ) ; 
                } 
                if ( &ft('e',  $n_src ."/" . $file ) ) { 
                  &mv( '' ,  &fn(  $n_src ."/" . $file ) .' '. &fn(  $n_src 
                        ."/" . $file .".old" ) ) ; 
                } 
                &cp( '' ,  &fn(  $n_import ."/" . &fp('t', $file ) ) .' '. 
                        &fn(  $n_src ."/" . $file ) ) ; 
                $Flag=  &Pipe("p$$.tmp00", &cmp(  &fn(  $n_import ."/" . 
                        &fp('t', $file ) ) .' '. &fn(  $n_src ."/" . $file 
                        ) , "p$$.tmp00" ) ) ; 
                if ( !&eq(   $Flag ,  '' ) ) { 
                  &doalias('log' ,  "Error moving " . &fp('t', $file ) 
                        .": " . $Flag ) ; 
                  $Errors= $Errors + 1 ; 
                  if ( &ft('e',  $n_src ."/" . $file .".old" ) ) { 
                    &mv( '' ,  &fn(  $n_src ."/" . $file .".old" ) .' '. 
                        &fn(  $n_src ."/" . $file ) ) ; 
                  } 
                } 
                else { 
#
# At nfra: also move the file to the ftp area
#
                  if ( &eq(   $n_site , "nfra" ) && defined($n_ftp) ) { 
#                          if ("$file:h" != "") then
#                             set Flag=(`rsh $n_ftp \
#                               'if ( -d ~ftp/newstar/'$file:h' ) echo ok' `)
#                             if ("$Flag" != "ok") then
#                                (rsh $n_ftp mkdir '~ftp/newstar/'$file:h )|\
#                                  tee -a $Logfile
#                                echo 'Created subdirectory ~ftp/newstar/'$file:h
#                             endif
#                          endif
                    &rsh( &fn(  $n_ftp ) ,  "cp" .' '. &fn(  $n_import ."/" 
                        . &fp('t', $file ) ) .' '. '~ftp/newstar/src/' . 
                        $file , "" , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  
                        $Logfile ) , "p$$.tmp00" ) ; 
                  } 
#
# Update database.idx: for NSTAR_DIR check dependencies, else just checksum
#
                  &cp( '' ,  &fn(  $n_src ."/sys/database.idx" ) .' '. &fn( 
                         $n_work ."/database.old" ) ) ; 
                  &grep( "-v" , &fn(  $file ) ,  &fn(  $n_work 
                        ."/database.old" ) , ''. &fn(  $n_src 
                        ."/sys/database.idx" ) ) ; 
                  $dir= &fn(  &fp('h', $file ) ) ; 
                  if ( &peq(   $NSTAR_DIR , "*" . &fp('t', $dir ) ."*" ) 
                        ) { 
                    &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "fstat" .' '. 
                        "-i" .' '. "-c" .' '. &fn(  $n_src ."/" . $file ) , 
                        '>'. &fn(  $n_src ."/sys/database.idx" ) ) ; 
                  } 
                  else { 
                    &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "fstat" .' '. 
                        "-c" .' '. &fn(  $n_src ."/" . $file ) , '>'. &fn(  
                        $n_src ."/sys/database.idx" ) ) ; 
                  } 
                  &cp( '' ,  &fn(  $n_src ."/sys/lock.idx" ) .' '. &fn(  
                        $n_work ."/lock.old" ) ) ; 
                  &grep( "-v" , &fn(  $file ) ,  &fn(  $n_work ."/lock.old" 
                        ) , ''. &fn(  $n_src ."/sys/lock.idx" ) ) ; 
                  &rm( "-f" ,  &fn(  $n_import ."/" . &fp('t', $file ) ) ) 
                        ; 
                  &doalias('log' ,   $file 
                        ." updated in master tree and database." ) ; 
                } 
              } 
            } 
          } 
          &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
        } 
#
#
# Errors moving files into the master, indicate with revision number
#
        if ( !&eq( $Errors , 0 ) ) { 
          &doalias('log' ,  "Errors moving files into the master..." ) ; 
          &echo( '' ,   "*** Incomplete revision *** " , '>'. &fn(  $n_src 
                        ."/sys/version.idx" ) ) ; 
#
# We moved files into the master, so we have a new revision. 
#
        } 
        elsif ( !&eq( $files_moved , 0 ) ) { 
          &mv( '' ,  &fn(  $n_src ."/sys/version.idx" ) .' '. &fn(  $n_src 
                        ."/sys/version.idx.old" ) ) ; 
          &echo( '' ,   "Newstar Release " . $C_Version , ''. &fn(  $n_src 
                        ."/sys/version.idx" ) ) ; 
#
# Flag a local revision
#
          if ( !&eq( $n_site , "nfra" ) ) { 
            if ( &eq( &Pipe("p$$.tmp00", &cat( '' ,  &fn(  $Files ) , 
                        "p$$.tmp01" ) , &grep( "-c +sys/version.idx" , '' , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) , 0 ) ) { &echo( '' , 
                          "*** Local revision ***" , '>'. &fn(  $n_src 
                        ."/sys/version.idx" ) ) ; } 
#
#
# If this is the NFRA master, update revision history and nnews.hlp
#
          } 
          else { 
            sub C2_t2_update { 
              local(*TMP); 
              open(TMP,">txt$$.tmp"); 
              print TMP  '' ."\n" ; 
              print TMP  "<DT>" . $C_Date ." <STRONG>" . &Pipe("p$$.tmp00", 
                        &cat( '' ,  &fn(  $n_src ."/sys/version.idx" ) , 
                        "p$$.tmp00" ) ) ."</STRONG>" ."\n" ; 
              close(TMP); 
              "txt$$.tmp";} 
            &cat( '' ,  &C2_t2_update , '>'. &fn(  $n_root 
                        ."/server/newstar/updates.html" ) ) ; 
            &echo( '' ,   "1 NNews" , ''. &fn(  $Tmpfile ) ) ; 
            for $grpfile__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $grpfile=$grpfile__x ; 
              $Flag=  "File " . &fp('t', $grpfile ) ." updated" ; 
              if ( &eq( &Pipe("p$$.tmp00", &grep( "-c" ,   $Flag ,  &fn(  
                        $n_root ."/updates.log" ) , "p$$.tmp00" ) ) , 0 ) 
                        ) { 
                &echo( '' ,    $C_Date ." " . $C_Time ." - " . $n_arch 
                        ." - " . $Flag , '>'. &fn(  $n_root ."/updates.log" 
                        ) ) ; 
#
# Add it to the index of updates
#
                sub C2_t3_update { 
                  local(*TMP); 
                  open(TMP,">txt$$.tmp"); 
                  print TMP  "<DT>" . $C_Date 
                        ." <A HREF=/htbin/nview/import/" . &fp('t', 
                        $grpfile ) .">" . &fp('t', $grpfile ) ."</A>" ."\n" 
                        ; 
                  print TMP   &Pipe("p$$.tmp00", &grep( '' , 'Subject:' ,  
                        &fn(  $grpfile ) , "p$$.tmp01" ) , &sed( "-e" , 
                        's/\!.*Subject:/<DD>/' , "p$$.tmp01" , "p$$.tmp00" 
                        ) ) ." " ."\n" ; 
                  close(TMP); 
                  "txt$$.tmp";} 
                &cat( '' ,  &C2_t3_update , '>'. &fn(  $n_root 
                        ."/server/newstar/updates.html" ) ) ; 
#
# Update nnews and add it to the grpfile
#
                &echo( "-n" ,   " " . $C_Date , '>'. &fn(  $Tmpfile ) ) ; 
                &grep( '' , "Subject:" ,  &fn(  $grpfile ) , "p$$.tmp00" ) 
                        ; &sed( "-e" , 's/\!.*Subject://' , "p$$.tmp00" , 
                        '>'. &fn(  $Tmpfile ) ) ; 
                &echo( '' ,   "+doc/nnews.hlp" , '>'. &fn(  $grpfile ) ) ; 
#
#  Check wether it solved a bug, if so: release it
#
                if ( !&eq( &Pipe("p$$.tmp00", &grep( "-c -e" , '- bug ' ,  
                        &fn(  $grpfile ) , "p$$.tmp00" ) ) , 0 ) ) { 
                  $Bug=  &Pipe("p$$.tmp00", &grep( "-e" , '- bug ' ,  &fn(  
                        $grpfile ) , "p$$.tmp00" ) ) ; 
                  sub C2_t4_update { 
                    local(*TMP); 
                    open(TMP,">txt$$.tmp"); 
                    print TMP  "Done with update" ."\n" ; 
                    print TMP  "import/" . &fp('t', $grpfile ) ."\n" ; 
                    print TMP  "n" ."\n" ; 
                    print TMP  "y" ."\n" ; 
                    close(TMP); 
                    "txt$$.tmp";} 
                  &docsh( &fn(  $n_src ."/sys/bugreport.csh" ) ,  "release" 
                        .' '. &fn(  (split(' ',$Bug)) [ &vn($Bug) -1 ] ) 
                        .' '. &C2_t4_update , "" ) ; 
                } 
              }                                        # if (first update)
            }                                          # foreach grpfile ()
#
# Append the remainder of nnews.hlp and put the new version in the system
#
            &tail( "+2l" ,  &fn(  $n_src ."/doc/nnews.hlp" ) , '>'. &fn(  
                        $Tmpfile ) ) ; 
            &mv( '' ,  &fn(  $n_src ."/doc/nnews.hlp" ) .' '. &fn(  $n_src 
                        ."/doc/nnews.hlp.old" ) ) ; 
            &mv( '' ,  &fn(  $Tmpfile ) .' '. &fn(  $n_src 
                        ."/doc/nnews.hlp" ) ) ; 
            &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
#
# Compose a mail message about this fresh release. 
#
            $Flag=  "Newstar Release " . $C_Version ; 
            sub C2_t5_update { 
              local(*TMP); 
              open(TMP,">txt$$.tmp"); 
              print TMP  '' ."\n" ; 
              print TMP  "From: The Newstar Master account" ."\n" ; 
              print TMP  "To:   All Friends of Newstar" ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "Concern: " . $Flag ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  
                        "                                                       Dwingeloo, " 
                        . $C_Date ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "Dear Friends of Newstar," ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "A new Newstar revision has just been install" 
                        ."ed in the Master system at NFRA:" ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP   &Pipe("p$$.tmp00", &cat( '' ,  &fn(  $Files ) , 
                        "p$$.tmp01" ) , &grep( '' , "Subject:" , 
                        "p$$.tmp01" , "p$$.tmp02" ) , &tr( '' , '\!' , ' ' 
                        , "p$$.tmp02" , "p$$.tmp00" ) ) ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "To upgrade your installation, login as the N" 
                        ."ewstar manager, initialise" ."\n" ; 
              print TMP  "the Newstar environment (e.g. source ~newsta" 
                        ."r/src/sys/newstar_????.csh) " ."\n" ; 
              print TMP  "and enter:" ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "       nup update" ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "and follow the instructions given by that co" 
                        ."mmand." ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "Please direct any problems or questions to  " . 
                        $n_master ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "Your sincerely," ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "The Newstar Project Team." ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "PS: Groupfiles involved in this update: " ."\n" ; 
              close(TMP); 
              "txt$$.tmp";} 
            &cat( '' ,  &C2_t5_update , ''. &fn( "message." . $C_Version ) 
                        ) ; 
            for $grpfile__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $grpfile=$grpfile__x ; 
              &echo( '' ,   "      " . &fp('t', $grpfile ) ." " , '>'. &fn( 
                        "message." . $C_Version ) ) ; 
            } 
#
#   Give an oppurtunity to edit the message and the subjects
#     (some people give those weird subjects in their groupfiles...)
#
            &mem(  &fn(  $n_src ."/doc/nnews.hlp" ) .' '. &fn( "message." . 
                        $C_Version ) ) ; 
#
#   This relies on an elm alias  friends_of_newstar !!!!!
#
            &elm( "-s" ,    $Flag .' '. "friends_of_newstar" ,  &fn( 
                        "message." . $C_Version ) ) ; 
#
#   We changed files in the master, so update the database
#
            $Input_file= &fn(  $n_src ."/sys/version.idx" ) .' '. &fn(  
                        $n_src ."/doc/nnews.hlp" ) ; 
            &source( &fn(  $n_src ."/sys/compile.csh" ) ) ; 
            &cp( '' ,  &fn(  $n_src ."/sys/database.idx" ) .' '. &fn(  
                        $n_work ."/database.old" ) ) ; 
            &grep( "-v +doc/nnews.hlp" , &fn(  $n_work ."/database.old" ) , 
                        "" , "p$$.tmp00" ) ; &grep( "-v +sys/version.idx" , 
                        '' , "p$$.tmp00" , ''. &fn(  $n_src 
                        ."/sys/database.idx" ) ) ; 
            &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "fstat" .' '. &fn(  
                        $n_src ."/sys/version.idx" ) .' '. &fn(  $n_src 
                        ."/doc/nnews.hlp" ) , '>'. &fn(  $n_src 
                        ."/sys/database.idx" ) ) ; 
#
#   Finally move database.idx, version.idx, nnews.hlp, lock.idx 
#   and the groupfiles to the ftp area
#
            if ( defined($n_ftp) ) { 
              &rsh( &fn(  $n_ftp ) ,  "cp" .' '. &fn(  $n_src 
                        ."/sys/database.idx" ) .' '. '~ftp/newstar/src/sys' 
                        , "" , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  $Logfile 
                        ) , "p$$.tmp00" ) ; 
              &rsh( &fn(  $n_ftp ) ,  "cp" .' '. &fn(  $n_src 
                        ."/sys/lock.idx" ) .' '. '~ftp/newstar/src/sys' , 
                        "" , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  $Logfile ) 
                        , "p$$.tmp00" ) ; 
              &rsh( &fn(  $n_ftp ) ,  "cp" .' '. &fn(  $n_src 
                        ."/sys/version.idx" ) .' '. '~ftp/newstar/src/sys' 
                        , "" , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  $Logfile 
                        ) , "p$$.tmp00" ) ; 
              &rsh( &fn(  $n_ftp ) ,  "cp" .' '. &fn(  $n_src 
                        ."/doc/nnews.hlp" ) .' '. '~ftp/newstar/src/doc' , 
                        "" , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  $Logfile ) 
                        , "p$$.tmp00" ) ; 
              &rsh( &fn(  $n_ftp ) ,  "cp" .' '. &fn(  $Files ) .' '. 
                        '~ftp/newstar/import' , "" , "p$$.tmp00" ) ; &tee( 
                        "-a" ,  &fn(  $Logfile ) , "p$$.tmp00" ) ; 
            } 
          }                                            # if (nfra)
        }                                              # if (files_moved)
      }                                                # if (in import)
#
#
# Errors occurred, give a message (different for nfra and elsewhere)
#
      if ( !&eq( $Errors , 0 ) ) { 
        if ( &eq(   $n_site , "nfra" ) ) { 
          sub C2_t6_update { 
            local(*TMP); 
            open(TMP,">txt$$.tmp"); 
            print TMP  '' ."\n" ; 
            print TMP  "Errors during execution of " . $Cmd ."\n" ; 
            print TMP  '' ."\n" ; 
            print TMP  "Libraries and \$n_inc have most probably bee" 
                        ."n cluttered. " ."\n" ; 
            print TMP  
                        "Either correct the errors and try again: nup build " 
                        . &fp('t', $grpfile ) .", " ."\n" ; 
            print TMP  "or reconstruct the libraries etc. (only the " 
                        ."potentially damaged files):" ."\n" ; 
            print TMP  "    cd \$n_src; nup build \$n_import/" . &fp('t', 
                        $grpfile ) ."\n" ; 
            print TMP  '' ."\n" ; 
            close(TMP); 
            "txt$$.tmp";} 
          &cat( '' ,  &C2_t6_update , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  
                        $Logfile ) , "p$$.tmp00" ) ; 
        } 
        else { 
          $Flag=  &Pipe("p$$.tmp00", &cat( '' ,  &fn(  $n_src 
                        ."/sys/version.idx" ) , "p$$.tmp00" ) ) ; 
          sub C2_t7_update { 
            local(*TMP); 
            open(TMP,">txt$$.tmp"); 
            print TMP  '' ."\n" ; 
            print TMP  "*************** Installation errors occured " 
                        ."**********************" ."\n" ; 
            print TMP  '' ."\n" ; 
            print TMP  "The log-file will be mailed to  " . $n_master ."." 
                        ."\n" ; 
            print TMP  '' ."\n" ; 
            print TMP  "Please inform this account of additional inf" 
                        ."ormation that might be" ."\n" ; 
            print TMP  "connected with the errors (recent change of " 
                        ."operation system, disk" ."\n" ; 
            print TMP  "space problems etc). The Newstar group will " 
                        ."contact you about the " ."\n" ; 
            print TMP  "problems as soon as possible." ."\n" ; 
            print TMP  '' ."\n" ; 
            print TMP  "Your present executables are still correct. " ."\n" 
                        ; 
            print TMP  "You seem to have " . $Flag ."\n" ; 
            print TMP  '' ."\n" ; 
            print TMP  "********************************************" 
                        ."*********************" ."\n" ; 
            print TMP  '' ."\n" ; 
            close(TMP); 
            "txt$$.tmp";} 
          &cat( '' ,  &C2_t7_update , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  
                        $Logfile ) , "p$$.tmp00" ) ; 
          &cat( '' ,  &fn(  $Logfile ) , "p$$.tmp00" ) ; &elm( "-s" ,   
                        "Newstar_crash_on_" . $n_site ."/" . $n_arch .' '. 
                        &fn(  $n_master ) , "p$$.tmp00" ) ; 
          $Mode=  "Quit" ; 
        } 
#
#  No errors: inform NFRA if run on remote site
#
      } 
      elsif ( !&eq(   $n_site , "nfra" ) ) { 
        sub C2_t8_update { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  '' ."\n" ; 
          print TMP  "Newstar has been updated on " . $n_site ." (" . 
                        $n_arch .") at " . $C_Date .":" ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "    " . $Cmd ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "The current version at " . $n_site ." is now:" ."\n" 
                        ; 
          print TMP  '' ."\n" ; 
          print TMP   &Pipe("p$$.tmp00", &cat( '' ,  &fn(  $n_src 
                        ."/sys/version.idx" ) , "p$$.tmp00" ) ) ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "Yours truly," ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "update.csh" ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &elm( "-s" ,   "Newstar_update_on_" . $n_site ."/" . $n_arch .' '. 
                        &fn(  $n_master ) ,  &C2_t8_update ) ; 
      } 
#
#
#  %Retrieve files over the network
#
    } 
    elsif ( &peq(   $Command , "[Rr]*" ) ) { 
#
#  Enforce working in $n_import
#
      if ( !&eq( $PWD , $n_import ) ) { 
        &cd(  &fn(  $n_import ) ) ; 
        &echo( '' ,   "--> Now in directory " . $n_import , "" ) ; 
      } 
#
# First argument: groupfile to retrieve
#
      if ( !&eq(   $Files ,  '' ) ) { 
        $grpfile= &fn(  (split(' ',$Files)) [ 1 -1 ] ) ; 
        @Files=split(' ',$Files); splice(@Files, "1" -1,1,  '' ); 
                        $Files=join(' ',@Files); if ( &vn($Files) > 1 ) { 
                        @Files=split(' ',$Files) ; shift(@Files) ; 
                        $Files=join(' ',@Files) ; } 
      } 
      else { 
        $grpfile=  "all" ; 
      } 
      $do_check= "1" ; 
      if ( &peq( $grpfile , "[Aa][Ll][Ll]" ) || &peq( $grpfile , "[Aa]" ) 
                        ) { 
        if ( &peq( $grpfile , "[Aa]" ) ) { $do_check='' ; } 
        $grpfile= "sys/database.idx" ; 
        if ( &ft('e', "database.idx" ) ) {             # Remove old version if any
          &rm( "-f" ,  "database.idx" ) ; 
        } 
      } 
#
# Remaining arguments: internet address
#
      $tmp= &fn(  $n_remote ) ; 
      $iaddr= &fn(  (split(' ',$tmp)) [ 1 -1 ] ) ; 
      $iuser= &fn(  (split(' ',$tmp)) [ 2 -1 ] ) ; 
      $iroot= &fn(  (split(' ',$tmp)) [ 3 -1 ] ) ; 
      undef $tmp ; 
      $noglob='' ; 
      $Files= &fn(  $Files ) .' '.  '' .' '.  ''       # Make sure they exist
                        .' '.  '' ; 
      if ( !&eq(   (split(' ',$Files)) [ 1 -1 ] ,  '' ) ) { $iaddr= &fn(  
                        (split(' ',$Files)) [ 1 -1 ] ) ; } 
      if ( !&eq(   (split(' ',$Files)) [ 2 -1 ] ,  '' ) ) { $iuser= &fn(  
                        (split(' ',$Files)) [ 2 -1 ] ) ; } 
      if ( !&eq(   (split(' ',$Files)) [ 3 -1 ] ,  '' ) ) { $iroot= &fn(  
                        (split(' ',$Files)) [ 3 -1 ] ) ; } 
      undef $noglob ; 
#
# Get password (we do not put that in any file!)
#
      $ipass=  '' ; 
      if ( &eq( $iuser , "anonymous" ) ) { 
        $ipass=   $USER ."@" . &Pipe("p$$.tmp00", &domainname( "p$$.tmp00" 
                        ) ) ; 
      } 
      else { 
        while ( &eq(   $ipass ,  '' ) ) { 
          &echo( "-n" ,   "Enter password for " . $iuser .": " , "" ) ; 
          &stty( "-echo" ) ; $ipass=  ($_=scalar(<STDIN>), chop, $_) ; 
                        &stty( "echo" ) ; &echo( '' ,   "xyz1jkl" , "" ) ; 
        } 
      } 
      $nuse= &fn( "newstar_use_" . $C_Date . $C_Time ."." . $n_site ) ; 
#
# Move to $n_import and check wether the file is there, if not get it.
#
      if ( ! &ft('e', &fp('t', $grpfile ) ) ) { 
        sub C2_t9_update { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  "quote user " . $iuser ."\n" ; 
          print TMP  "quote pass " . $ipass ."\n" ; 
          print TMP  "ascii" ."\n" ; 
          print TMP  "cd " . $iroot ."\n" ; 
          print TMP  "get " . $grpfile ." " . &fp('t', $grpfile ) ."\n" ; 
          print TMP  "cd ../import" ."\n" ; 
          print TMP  "get " . $grpfile ." " . &fp('t', $grpfile ) ."\n" ; 
          print TMP  "cd /pub/incoming" ."\n" ; 
          print TMP  "put newstar.use " . $nuse ."\n" ; 
          print TMP  "bye" ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &ftp( "-n -v -i" ,  &fn(  $iaddr ) .' '. &C2_t9_update , "" ) ; 
      } 
      if ( ! &ft('e', &fp('t', $grpfile ) ) ) { 
        &doalias('log' ,  "Cannot retrieve " . $grpfile .", try again..." ) 
                        ; 
        $Flag=  "error" ; 
      } 
      else { 
#
# Create new overview file of use of Newstar
# Save the current one to .old in case of
#
        if ( &ft('e',  $n_import ."/newstar.use.old" ) ) { 
          &rm( "-f" ,  &fn(  $n_import ."/newstar.use.old" ) ) ; 
        } 
        &mv( '' ,  &fn(  $n_import ."/newstar.use" ) .' '. &fn(  $n_import 
                        ."/newstar.use.old" ) ) ; 
        &touch( '' ,  &fn(  $n_import ."/newstar.use" ) ) ; 
        &chmod( "a+rw" ,  &fn(  $n_import ."/newstar.use" ) ) ; 
#
# Find differences between NFRA and local database
#
        if ( &eq( $grpfile , "sys/database.idx" ) ) { 
          if ( $do_check ) { 
            &doalias('log' ,  "Creating fresh local database..." ) ; 
            if ( &ft('e',  $n_work ."/database.idx" ) ) { 
              &rm( "-f" ,  &fn(  $n_work ."/database.idx" ) ) ; 
            } 
            &grep( '' , '\.exe' ,  &fn(  $n_import ."/database.idx" ) , ''. 
                        &fn(  $n_work ."/database.idx" ) ) ; 
            $nonomatch='' ; 
            for $dir__x (split(' ',join(' ' , &fn(  $n_src ."/*" ) ))) { 
                        $dir=$dir__x ; 
              if ( &ft('d', $dir ) ) { 
                &doalias('log' ,  "Scanning " . $dir ."..." ) ; 
#
# If in NSTAR_DIR, check dependencies, else just checksum
#
                if ( &peq(   $NSTAR_DIR , "*" . &fp('t', $dir ) ."*" ) ) { 
                  &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "fstat" .' '. 
                        "-i" .' '. "-t:^exe" .' '. "@" .' '. &fn(  $dir 
                        ."/*.grp" ) , '>'. &fn(  $n_work ."/database.idx" ) 
                        ) ; 
                } 
                else { 
                  &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "fstat" .' '. 
                        "-t:^exe" .' '. "@" .' '. &fn(  $dir ."/*.grp" ) , 
                        '>'. &fn(  $n_work ."/database.idx" ) ) ; 
                } 
              } 
            } 
            &cp( '' ,  &fn(  $n_work ."/database.idx" ) .' '. &fn(  $n_src 
                        ."/sys/database.idx" ) ) ; 
            undef $nonomatch ; 
          } 
          &doalias('log' ,  "Comparing local database and database from " . 
                        $iaddr ) ; 
          $tmp= &fn(  &fp('t', $grpfile ) ) ; 
          $grpfile= "retrieved.grp" ; 
          &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "compare" .' '. 
                        "-t:^exe" .' '. &fn(  $tmp ) .' '. &fn(  $n_src 
                        ."/sys/database.idx" ) , ''. &fn(  $Tmpfile ) ) ; 
          &grep( "-v" ,  "lock.idx" ,  &fn(  $Tmpfile ) , ''. &fn(  
                        $grpfile ) ) ; 
          &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
          &doalias('log' ,  "Differences are in " . $grpfile ) ; 
        } 
        $grpfile= &fn(  &fp('t', $grpfile ) ) ; 
        $Flag=  "ok" ; 
      } 
#
# General log
#
      &echo( '' ,    $C_Date ." " . $C_Time ." - " . $n_arch ." - retr " . 
                        &fp('t', $grpfile ) ." " . $iaddr ." " . $iroot 
                        ." " . $iuser ." " , '>'. &fn(  $n_root 
                        ."/updates.log" ) ) ; 
#
#
# At nfra: check locks, if anything is locked we should have a problem
#
      if ( &eq(   $Flag ,  "ok" ) && &eq( $n_site , "nfra" ) ) { 
        &echo( '' ,   '' , "" ) ; 
        &echo( '' ,   "Checking locks..." , "" ) ; 
        &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "files" .' '. "-t:^exe" 
                        .' '. &fn(  $grpfile ) , ''. &fn(  $Tmpfile ) ) ; 
        if ( &ft('e',  $n_src ."/sys/lock.idx" ) ) { 
          $nline=  &Pipe("p$$.tmp00", &cat( '' ,       # Count lines
                        &fn(  $Tmpfile ) , "p$$.tmp01" ) , &wc( "-l" , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
          $iline= -9 ;                                 # Means 1 after first increment by 10
          while ( $iline < $nline ) {                  # Lines left?
            $iline= $iline + 10 ;                      # Next series of 10
            for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &tail( 
                        "+$iline" ,  &fn(  $Tmpfile ) , "p$$.tmp01" ) , 
                        &head( "-10" , "p$$.tmp01" , "p$$.tmp00" ) ) ))) { 
                        $file=$file__x ; 
              $Lock=  &Pipe("p$$.tmp00", &grep( '' , &fn(  $file ) ,  &fn(  
                        $n_src ."/sys/lock.idx" ) , "p$$.tmp00" ) ) ; 
              if ( !&eq(   $file ,  "+doc/nnews.hlp" ) && !&eq(   $Lock ,  
                        '' ) ) { 
                &echo( '' ,   "Warning: " . $Lock , "" ) ; 
                $Flag=  "lock" ; 
              } 
              undef $Lock ; 
            } 
          } 
        } 
        if ( &eq( $Flag ,  "lock" ) ) { 
          sub C2_t10_update { 
            local(*TMP); 
            open(TMP,">txt$$.tmp"); 
            print TMP  '' ."\n" ; 
            print TMP  "Found files that were locked, cannot retriev" ."e." 
                        ."\n" ; 
            print TMP  "Please check and edit \$n_src/sys/lock.idx" ."\n" ; 
            print TMP  '' ."\n" ; 
            close(TMP); 
            "txt$$.tmp";} 
          &cat( '' ,  &C2_t10_update , "" ) ; 
        } 
        else { 
#
# Remove old locks, make lock for Newstar manager
#
          &echo( '' ,   '' , "" ) ; 
          &echo( '' ,   "Making locks for files to be retrieved..." , "" ) 
                        ; 
          if ( &ft('e',  $n_src ."/sys/lock.idx" ) ) { 
            $nline=  &Pipe("p$$.tmp00", &cat( '' ,     # Count lines
                        &fn(  $Tmpfile ) , "p$$.tmp01" ) , &wc( "-l" , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
            $iline= -9 ;                               # Means 1 after first increment by 10
            while ( $iline < $nline ) {                # Lines left?
              $iline= $iline + 10 ;                    # Next series of 10
              for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &tail( 
                        "+$iline" ,  &fn(  $Tmpfile ) , "p$$.tmp01" ) , 
                        &head( "-10" , "p$$.tmp01" , "p$$.tmp00" ) ) ))) { 
                        $file=$file__x ; 
                &echo( '' ,    $file ." locked User=Newstar Date=" . 
                        $C_Date ."/" . $C_Time , "p$$.tmp00" ) ; &tee( "-a" 
                        ,  &fn(  $n_src ."/sys/lock.idx" ) , "p$$.tmp00" ) 
                        ; 
              } 
            } 
          } 
        } 
      } 
#
#
#  Confirm retrieval
#
      if ( &eq(   $Flag ,  "ok" ) ) { 
        &cat( '' ,  &fn(  $grpfile ) , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  
                        $Logfile ) , "p$$.tmp00" ) ; 
        if ( &eq(   $Mode ,  "Update" ) ) { 
          $Flag=  "Yes" ; 
        } 
        else { 
          &echo( "-n" ,   "Retrieve these files (y,n)? [y] " , "" ) ; 
          $Flag=  ($_=scalar(<STDIN>), chop, $_) ; 
        } 
      } 
      if ( &eq(   $Flag ,  '' ) || &peq(   $Flag , "[Yy]*" ) ) { 
#
# Try to get them three times, if all files received, quit as well
#
        $ii= 3 ; 
        while ( $ii > 0 ) { 
#
# Retrieve contents of groupfile, skip files have been retrieved correctly
#
          sub C2_t11_update { 
            local(*TMP); 
            open(TMP,">txt$$.tmp"); 
            print TMP  "quote user " . $iuser ."\n" ; 
            print TMP  "quote pass " . $ipass ."\n" ; 
            print TMP  "cd " . $iroot ."\n" ; 
            close(TMP); 
            "txt$$.tmp";} 
          &cat( '' ,  &C2_t11_update , ''. &fn(  $Tmpfile ) ) ; 
          &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "import" .' '. "-c" 
                        .' '. "-t:^exe" .' '. &fn(  $grpfile ) , '>'. &fn(  
                        $Tmpfile ) ) ; 
          &echo( '' ,  "bye" , '>'. &fn(  $Tmpfile ) ) ; 
          if ( !&eq(   $ii ,  "3" ) ) { 
            &doalias('log' ,  '' ) ; 
            &doalias('log' ,  "Trying again ...." ) ; 
          } 
          &ftp( "-n -v -i" ,  &fn(  $iaddr ) .' '. &fn(  $Tmpfile ) , 
                        "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  $Logfile ) , 
                        "p$$.tmp00" ) ; 
          &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
          &doalias('log' ,  '' ) ; 
          &doalias('log' ,  "Checking retrieved files... " ) ; 
          &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "check" .' '. "-c" .' '. 
                        "-t:^exe" .' '. &fn(  $grpfile ) , ''. &fn(  
                        $Tmpfile ) ) ; 
          if ( &ft('z', $Tmpfile ) ) { 
            $ii= -999 ; 
            &doalias('log' ,  "All files received correctly." ) ; 
          } 
          else { 
            $ii= $ii - 1 ; 
            &doalias('log' ,  "Not all files received correctly:" ) ; 
            &cat( '' ,  &fn(  $Tmpfile ) , "p$$.tmp00" ) ; &tee( "-a" ,  
                        &fn(  $Logfile ) , "p$$.tmp00" ) ; 
          } 
          &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
        } 
        if ( &eq( $ii , 0 ) ) { 
          sub C2_t12_update { 
            local(*TMP); 
            open(TMP,">txt$$.tmp"); 
            print TMP  '' ."\n" ; 
            print TMP  "Not all files were retrieved after 3 tries.." ."." 
                        ."\n" ; 
            print TMP  "You should try once more:  nup retrieve " . 
                        $grpfile ."." ."\n" ; 
            print TMP  "This will only retrieve the missing or defec" 
                        ."t files." ."\n" ; 
            print TMP  '' ."\n" ; 
            close(TMP); 
            "txt$$.tmp";} 
          &cat( '' ,  &C2_t12_update , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  
                        $Logfile ) , "p$$.tmp00" ) ; 
        } 
        else { 
          $Flag=  "ok" ; 
        } 
      } 
#
#  Tell the user how to proceed
#
      if ( &eq(   $Flag ,  "ok" ) ) { 
        sub C2_t13_update { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  '' ."\n" ; 
          print TMP  "All files have been correctly retrieved. " ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "To install the files, please enter:" ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "     nup build " . $grpfile ."  -Update " ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "on the following hosts:  " . $n_hosts ."\n" ; 
          print TMP  '' ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &cat( '' ,  &C2_t13_update , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  
                        $Logfile ) , "p$$.tmp00" ) ; 
      } 
      else { 
        sub C2_t14_update { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  '' ."\n" ; 
          print TMP  "*************** Retrieve errors occured ****" 
                        ."******************" ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "The log-file will be mailed to  " . $n_master ."." 
                        ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "Please inform this account of additional inf" 
                        ."ormation that might be" ."\n" ; 
          print TMP  "connected with the errors (recent change of " 
                        ."operation system, disk" ."\n" ; 
          print TMP  "space problems etc). The Newstar group will " 
                        ."contact you about the " ."\n" ; 
          print TMP  "problems as soon as possible." ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "********************************************" 
                        ."*********************" ."\n" ; 
          print TMP  '' ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &cat( '' ,  &C2_t14_update , "p$$.tmp00" ) ; &tee( "-a" ,  &fn(  
                        $Logfile ) , "p$$.tmp00" ) ; 
        &cat( '' ,  &fn(  $Logfile ) , "p$$.tmp00" ) ; &elm( "-s" ,   
                        "Newstar_crash_on_" . $n_site ."/" . $n_arch .' '. 
                        &fn(  $n_master ) , "p$$.tmp00" ) ; 
        $Mode=  "Quit" ; 
      } 
#
#
#  %Clean: Make the directory structure consistent with the groupfiles
#
    } 
    elsif ( &peq(   $Command , "[Cc][Ll]*" ) ) { 
#
#  Scan only relevant groupfiles in master source tree
#
      $Home= &fn(  $cwd ) ; 
      &cd(  &fn(  $n_src ) ) ; 
      &echo( '' ,   "--> Now in directory " ."\$n_src" , "" ) ; 
#
#  Make sure empty temporary files exist
#
      if ( &ft('e',  $Tmpfile .".1" ) ) { 
        &rm( "-f" ,  &fn(  $Tmpfile .".1" ) ) ; 
      } 
      &touch( '' ,  &fn(  $Tmpfile .".1" ) ) ; 
      if ( &ft('e',  $Tmpfile .".2" ) ) { 
        &rm( "-f" ,  &fn(  $Tmpfile .".2" ) ) ; 
      } 
      &touch( '' ,  &fn(  $Tmpfile .".2" ) ) ; 
#
#  No argument: check all directories and files in $n_src itself
#
      if ( &eq(   $Files ,  '' ) ) { 
        &echo( '' ,    $C_Date ." " . $C_Time ." - " . $n_arch 
                        ." - clean all  " , '>'. &fn(  $n_root 
                        ."/updates.log" ) ) ; 
        &doalias('log' ,  "Scanning all groupfiles ..." ) ; 
        &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "files" .' '. "-t:^exe" 
                        .' '. &fn( "*/*.grp" ) , ''. &fn(  $Tmpfile .".1" ) 
                        ) ; 
        &doalias('log' ,  "Scanning all files..." ) ; 
        &find(  &fn( "*" ) .' '. "-print" , ''. &fn(   # Remember we are in $n_src
                        $Tmpfile .".2" ) ) ; 
#
#  Expand master database, find all files below $n_src
#
      } 
      elsif ( &peq( (split(' ',$Files)) [ 1 -1 ] , "[Aa][Ll][Ll]" ) ) { 
        &echo( '' ,    $C_Date ." " . $C_Time ." - " . $n_arch 
                        ." - clean all (database) " , '>'. &fn(  $n_root 
                        ."/updates.log" ) ) ; 
        &doalias('log' ,  "Scanning all-files database..." ) ; 
        &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "files" .' '. "-t:^exe" 
                        .' '. &fn(  $n_src ."/sys/database.idx" ) , ''. 
                        &fn(  $Tmpfile .".1" ) ) ; 
        &doalias('log' ,  "Scanning all files..." ) ; 
        &find(  &fn( "*" ) .' '. "-print" , "p$$.tmp00" ) ; &grep( "-v" , 
                        'upd.*\.log' , "p$$.tmp00" , ''. &fn(  $Tmpfile 
                        .".2" ) ) ; 
#
#  Expand all groupfiles in directories, find files in those directories
#
      } 
      else { 
        &echo( '' ,    $C_Date ." " . $C_Time ." - " . $n_arch ." - clean " 
                        . $Files ." " , '>'. &fn(  $n_root ."/updates.log" 
                        ) ) ; 
        for $dir__x (split(' ',join(' ' , &fn(  $Files ) ))) { $dir=$dir__x 
                        ; 
          if ( ! &ft('e', $dir ) && &ft('e',  $n_src ."/" . $dir ) ) { 
                        $dir= &fn(  $n_src ."/" . $dir ) ; } 
          if ( &ft('d', $dir ) ) { 
            &doalias('log' ,  "Scanning groupfiles and files in " . $dir 
                        ."..." ) ; 
            &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "files" .' '. 
                        "-t:^exe" .' '. &fn(  $dir ."/*.grp" ) , '>'. &fn(  
                        $Tmpfile .".1" ) ) ; 
            &find(  &fn(  $dir ) .' '. "-print" , ''.  # Remember we are in $n_src
                        &fn(  $Tmpfile .".2" ) ) ; 
          } 
        } 
      } 
#
#  Sort on filenames
#
      &doalias('log' ,  "Sorting contents of groupfiles" ) ; 
      &sort( "-u" ,  &fn(  $Tmpfile .".1" ) , ''. &fn(  $Tmpfile .".1.s" ) 
                        ) ; 
      &doalias('log' ,  "Sorting filelist" ) ; 
      &sort( "-u" ,  &fn(  $Tmpfile .".2" ) , ''. &fn(  $Tmpfile .".2.s" ) 
                        ) ; 
#
#  Make the difference: 
#
#    > file    means it exists but is not in a groupfile
#    < file    means it is in a groupfile but does not exist
#
      &doalias('log' ,  "Comparing..." ) ; 
      &diff( '' ,  &fn(  $Tmpfile .".1.s" ) .' '. &fn(  $Tmpfile .".2.s" ) 
                        , "p$$.tmp00" ) ; &awk( '' , 
                        '{ if ($1 == ">" || $1 == "<") print $2}' , 
                        "p$$.tmp00" , ''. &fn(  $Tmpfile ) ) ; 
      &rm( "-f" ,  &fn(  $Tmpfile .".1" ) .' '. &fn(  $Tmpfile .".1.s" ) 
                        .' '. &fn(  $Tmpfile .".2" ) .' '. &fn(  $Tmpfile 
                        .".2.s" ) ) ; 
#
#  Verify each file
#
      $Flag=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(     #get machines to do
                        $n_install ) , "p$$.tmp01" ) , &tr( '' , ',/:' , 
                        '   ' , "p$$.tmp01" , "p$$.tmp00" ) ) ; 
      $nline=  &Pipe("p$$.tmp00", &cat( '' ,  &fn(     # Count lines
                        $Tmpfile ) , "p$$.tmp01" ) , &wc( "-l" , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
      $iline= -9 ;                                     # Means 1 after first increment by 10
      while ( $iline < $nline ) {                      # Lines left?
        $iline= $iline + 10 ;                          # Next series of 10
        for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &tail( 
                        "+$iline" ,  &fn(  $Tmpfile ) , "p$$.tmp01" ) , 
                        &head( "-10" , "p$$.tmp01" , "p$$.tmp00" ) ) ))) { 
                        $file=$file__x ; 
          if ( ! &ft('d', $file ) ) { 
            if ( &ft('e', $file ) ) { 
              if ( &eq(   $_Confirm , 0 ) ) { 
                &rm( "-f" ,  &fn(  $file ) ) ; 
              } 
              elsif ( &eq(   &fp('e', $file ) ,  '' ) || &eq(   &fp('e', 
                        $file ) ,  "log" ) || &eq(   &fp('e', $file ) ,  
                        "tmp" ) || &eq(   &fp('e', $file ) ,  "LOG" ) || 
                        &eq(   &fp('e', $file ) ,  "lis" ) || &eq(   
                        &fp('e', $file ) ,  "old" ) || &peq(   &fp('e', 
                        $file ) , "??" ) || &peq(   &fp('e', $file ) , 
                        "?????*" ) || &peq(   $file , "*~" ) ) { 
                &rm( "-f" ,  &fn(  $file ) ) ; 
              } 
              else { 
                &echo( "-n" ,   "Remove " , "" ) ; &rm( "-i" ,  &fn(  $file 
                        ) ) ; 
              } 
              if ( &ft('e', $file ) ) { 
                &doalias('log' ,   $file ." not deleted" ) ; 
              } 
              else { 
                &doalias('log' ,   $file ." deleted" ) ; 
#                 if ("$file:e" =~ f?? || "$file:e" =~ c??) then
#                    set dir=$file:h
#                    if ("$dir" =~ n*) set dir=nst
#                    set file=$file:t
#	 	     if (! $?ARD) setenv ARD "ar dv"
#                    log `$ARD $n_lib/${dir}lib.olb ${file:r}.o`
#                 endif
              } 
            } 
            else { 
              if ( &peq(   &fp('e', $file ) , "x??" ) || &peq(   &fp('e', 
                        $file ) , "a??" ) ) { 
                for $aa__x (split(' ',join(' ' , &fn(  $Flag ) ))) { 
                        $aa=$aa__x ; 
                  if ( &peq(   &fp('e', $file ) , "?" . $aa ) ) { 
                    &echo( '' ,    $file ." missing ..." , "" ) ; 
                  } 
                } 
              } 
              else { 
                &echo( '' ,    $file ." missing ..." , "" ) ; 
              } 
            } 
          } 
        } 
      } 
#
#  Back to original directory, clean up
#
      &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
      &echo( '' ,   "--> Back in " . $Home , "" ) ; 
      &cd(  &fn(  $Home ) ) ; 
      undef $Home ; undef $nonomatch ; 
#
#
#  %Check the current implementation of Newstar against the database
#
    } 
    elsif ( &peq(   $Command , "[Cc][Hh]*" ) ) { 
#
#  If no argument given: all
#
      if ( &peq(   (split(' ',$Files)) [ 1 -1 ] , "[Aa][Ll][Ll]" ) || &eq(  
                         (split(' ',$Files)) [ 1 -1 ] ,  '' ) ) { $Files=  
                        "fdl" ; } 
#
#  General log
# 
      &echo( '' ,    $C_Date ." " . $C_Time ." - " . $n_arch ." - check " . 
                        $Files ." " , '>'. &fn(  $n_root ."/updates.log" ) 
                        ) ; 
#
#  Verify all files...
#
      if ( &peq( (split(' ',$Files)) [ 1 -1 ] , "*f*" ) && &ft('e',  $n_src 
                        ."/sys/database.idx" ) ) { 
        &doalias('log' ,  "Checking master source tree against database" ) 
                        ; 
        $grpfile= &fn(  $n_import ."/get" . $C_Date .".grp" ) ; 
        &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "check" .' '. "-t:^exe" 
                        .' '. &fn(  $n_src ."/sys/database.idx" ) , ''. 
                        &fn(  $grpfile ) ) ; 
        &doalias('log' ,  '' ) ; 
        if ( &ft('e', $grpfile ) && ! &ft('z', $grpfile ) ) { 
          &doalias('log' ,  " Created " ."\$n_import" . "/get" . $C_Date 
                        .".grp " ) ; 
          &cat( '' ,  &fn(  $grpfile ) , "" ) ; 
          &doalias('log' ,  '' ) ; 
          &doalias('log' ,  " Update implementation with: nup retrieve get" 
                        . $C_Date ) ; 
        } 
        else { 
          if ( &ft('e', $grpfile ) ) { 
            &rm( "-f" ,  &fn(  $grpfile ) ) ; 
          } 
          &doalias('log' ,  " As far as can be checked, you have a proper" 
                        ." source tree" ) ; 
          &doalias('log' ,  " Check for any revisions with: nup retrieve " 
                        ) ; 
        } 
        &doalias('log' ,  '' ) ; 
      } 
#
#  Scan all subdirectories of $n_src and build a new database
#
      if ( &peq( (split(' ',$Files)) [ 1 -1 ] , "*d*" ) ) { 
        if ( &ft('e',  $n_work ."/database.idx" ) ) { 
          &rm( "-f" ,  &fn(  $n_work ."/database.idx" ) ) ; 
        } 
        &grep( '' , '\.exe' ,  &fn(  $n_src ."/sys/database.idx" ) , ''. 
                        &fn(  $n_work ."/database.idx" ) ) ; 
        $nonomatch='' ; 
        for $dir__x (split(' ',join(' ' , &fn(  $n_src ."/*" ) ))) { 
                        $dir=$dir__x ; 
          if ( &ft('d', $dir ) ) { 
            &doalias('log' ,  "Scanning " . $dir ."..." ) ; 
#
# If in NSTAR_DIR, check dependencies, else just checksum
#
            if ( &peq(   $NSTAR_DIR , "*" . &fp('t', $dir ) ."*" ) ) { 
              &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "fstat" .' '. "-i" 
                        .' '. "-t:^exe" .' '. "@" .' '. &fn(  $dir 
                        ."/*.grp" ) , '>'. &fn(  $n_work ."/database.idx" ) 
                        ) ; 
            } 
            else { 
              &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "fstat" .' '. 
                        "-t:^exe" .' '. "@" .' '. &fn(  $dir ."/*.grp" ) , 
                        '>'. &fn(  $n_work ."/database.idx" ) ) ; 
            } 
          } 
        } 
        &cp( '' ,  &fn(  $n_work ."/database.idx" ) .' '. &fn(  $n_src 
                        ."/sys/database.idx" ) ) ; 
        undef $nonomatch ; 
        &doalias('log' ,  "Check current implementation with: nup retri" 
                        ."eve " ) ; 
      } 
#
#  Integrity check on the libraries
#
      if ( &peq( (split(' ',$Files)) [ 1 -1 ] , "*l*" ) ) { 
        &doalias('log' ,  "Checking libraries..." ) ; 
        $grpfile= &fn(  $n_import ."/lib" . $C_Date . $n_arch .".grp" ) ; 
        if ( &ft('e', $grpfile ) ) { 
          &rm( "-f" ,  &fn(  $grpfile ) ) ; 
        } 
        for $dir__x (split(' ',join(' ' , "dwarf" , "nst" , "wng" ))) { 
                        $dir=$dir__x ; 
          &doalias('log' ,  "   " . $dir ."lib.olb" ) ; 
          if ( &ft('e', $Tmpfile ) ) { 
            &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
          } 
          if ( &ft('e',  $Tmpfile .".1" ) ) { 
            &rm( "-f" ,  &fn(  $Tmpfile .".1" ) ) ; 
          } 
          if ( &ft('e',  $Tmpfile .".2" ) ) { 
            &rm( "-f" ,  &fn(  $Tmpfile .".2" ) ) ; 
          } 
#
#  For each entry: get name and date
#
          &ar( "tv" ,  &fn(  $n_lib ."/" . $dir ."lib.olb" ) , "p$$.tmp00" 
                        ) ; &awk( '' , 
                        '/.*\.o/ { im=NF-4; id=NF-3; iy=NF-1;   if ($' 
                        .'im == "Jan") mon=1;             if ($im == "' 
                        .'Feb") mon=2;             if ($im == "Mar") m' 
                        .'on=3;             if ($im == "Apr") mon=4;  ' 
                        .'           if ($im == "May") mon=5;         ' 
                        .'    if ($im == "Jun") mon=6;             if ' 
                        .'($im == "Jul") mon=7;             if ($im ==' 
                        .' "Aug") mon=8;             if ($im == "Sep")' 
                        .' mon=9;             if ($im == "Oct") mon=10' 
                        .';            if ($im == "Nov") mon=11;      ' 
                        .'      if ($im == "Dec") mon=12;            p' 
                        .'rintf("%2.2d%2.2d%2.2d  %s\n",($iy)%100,mon,' 
                        .'$id,$NF); }' , "p$$.tmp00" , "p$$.tmp01" ) ; 
                        &sort( '' , "p$$.tmp01" , ''. &fn(  $Tmpfile ) ) ; 
#
#  Extract relevant part of database
#
          if ( &eq( $dir , "nst" ) ) { 
            &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "select" .' '. &fn( 
                        "-t:s\$/cee/cun/c" . $n_arch ."/for/fun/fsc/f" . 
                        $n_arch ."/dsc" ) .' '. &fn(  $n_src 
                        ."/sys/database.idx" ) , "p$$.tmp00" ) ; &grep( '' 
                        , '^n.*/' , "p$$.tmp00" , ''. &fn(  $Tmpfile .".2" 
                        ) ) ; 
          } 
          else { 
            &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "select" .' '. &fn( 
                        "-t:s\$/cee/cun/c" . $n_arch ."/for/fun/fsc/f" . 
                        $n_arch ."/dsc" ) .' '. &fn(  $n_src 
                        ."/sys/database.idx" ) , "p$$.tmp00" ) ; &grep( '' 
                        ,  "^" . $dir ."/" , "p$$.tmp00" , ''. &fn(  
                        $Tmpfile .".2" ) ) ; 
          } 
#
#  Does any symbol occur twice?
#
          $nline=  &Pipe("p$$.tmp00", &cat( '' ,       # Count lines
                        &fn(  $Tmpfile ) , "p$$.tmp01" ) , &wc( "-l" , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
          $iline= -9 ;                                 # Means 1 after first increment by 10
          while ( $iline < $nline ) {                  # Lines left?
            $iline= $iline + 10 ;                      # Next series of 10
            for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &tail( 
                        "+$iline" ,  &fn(  $Tmpfile ) , "p$$.tmp01" ) , 
                        &head( "-10" , "p$$.tmp01" , "p$$.tmp00" ) ) ))) { 
                        $file=$file__x ; 
#
#  Store the date field for the next file
#
              if ( !&peq(   $file , "*.o" ) ) { 
                $fDate= &fn(  $file ) ; 
              } 
              else { 
#
#  Find the entry in the database selection (c/fortran file or dsc-file)
#
                $Flag=  &Pipe("p$$.tmp00", &grep( '' , '^.*/' . &fp('r', 
                        $file ) .'\.' ,  &fn(  $Tmpfile .".2" ) , 
                        "p$$.tmp00" ) ) ; 
                if ( &eq(   $Flag ,  '' ) && &peq( $file , "*_bd.o" ) ) { 
                  $dscfile=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $file ) 
                        , "p$$.tmp01" ) , &sed( '' , "s/_bd.o/.dsc/" , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
                  $Flag=  &Pipe("p$$.tmp00", &grep( '' , '^.*/' . $dscfile 
                        ,  &fn(  $Tmpfile .".2" ) , "p$$.tmp00" ) ) ; 
                  undef $dscfile ; 
                } 
#
#  If no corresponding file in the database, remove the entry...
#
                if ( &eq(   $Flag ,  '' ) ) { 
                  &doalias('log' ,  "No source in database for " . $file ) 
                        ; 
                  &echo( '' ,  &fn(  $file ) , '>'. &fn(  $Tmpfile .".1" ) 
                        ) ; 
#
#  If multiple entries: remove all entries and write to groupfile
#              
                } 
                elsif ( !&eq( &Pipe("p$$.tmp00", &grep( "-c" ,  " " . 
                        &fp('r', $file ) ."\.o" ,  &fn(  $Tmpfile ) , 
                        "p$$.tmp00" ) ) , 1 ) ) { 
                  &echo( '' ,  &fn(  $file ) , '>'. &fn(  $Tmpfile .".1" ) 
                        ) ; 
                  &doalias('log' ,  "Multiple entries for " . $file ) ; 
                  &echo( "+$Flag[1]" , '' , '>'. &fn(  $grpfile ) ) ; 
#
#  Check the date of the source file in the database against the library
#
                } 
                else { 
#
#  If too old, log and write to groupfile
#
                  if ( (split(' ',$Flag)) [ 2 -1 ] > $fDate ) { 
                    &doalias('log' ,  "Out of date: " . $file ." (" . 
                        $fDate .") " . (split(' ',$Flag)) [ 1 -1 ] ." (" . 
                        (split(' ',$Flag)) [ 2 -1 ] .")" ) ; 
                    &echo( "+$Flag[1]" , '' , '>'. &fn(  $grpfile ) ) ; 
                  } 
                } 
              } 
            }                                          # foreach file
          } 
#
#  For all files in the database: check wether the file is in the library
#
          $nline=  &Pipe("p$$.tmp00", &cat( '' ,       # Count lines
                        &fn(  $Tmpfile .".2" ) , "p$$.tmp01" ) , &wc( "-l" 
                        , "p$$.tmp01" , "p$$.tmp00" ) ) ; 
          $iline= -9 ;                                 # Means 1 after first increment by 10
          while ( $iline < $nline ) {                  # Lines left?
            $iline= $iline + 10 ;                      # Next series of 10
            for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &tail( 
                        "+$iline" ,  &fn(  $Tmpfile .".2" ) , "p$$.tmp01" ) 
                        , &head( "-10" , "p$$.tmp01" , "p$$.tmp00" ) ) 
                        ))) { $file=$file__x ; 
              if ( &peq(   $file , "*.*" ) && !&eq(   &fp('e', $file ) ,  
                        "dsc" ) ) { 
                $Flag= &fn(  &fp('t', $file ) ) ; 
                $Flag= &fn(  &fp('r', $Flag ) .".o" ) ; 
                if ( &eq( &Pipe("p$$.tmp00", &grep( "-c" ,  " " . $Flag 
                        ."\$" ,  &fn(  $Tmpfile ) , "p$$.tmp00" ) ) , 0 ) 
                        ) { 
                  &doalias('log' ,   $file ." is not in the archive..." ) ; 
                  &echo( "+$file" , '' , '>'. &fn(  $grpfile ) ) ; 
                } 
              } 
            } 
          } 
#
#  Now do all library operations
#
          if ( &ft('e',  $Tmpfile .".1" ) && ! &ft('z',  $Tmpfile .".1" ) 
                        ) { 
            $Flag=  &Pipe("p$$.tmp00", &cat( '' ,  &fn(  $Tmpfile .".1" ) , 
                        "p$$.tmp00" ) ) ; 
            if ( ! defined($ARD) ) { $ARD=  "ar dv" ; &ENV_EXPORT( ARD ,  
                        "ar dv" ) ; } 
            &doalias('log' ,  &Pipe("p$$.tmp00", &dollar("ARD" ,  &fn(  
                        $n_lib ."/" . $dir ."lib.olb" ) .' '. &fn(  $Flag ) 
                        , "p$$.tmp00" ) ) ) ; 
          } 
        }                                              # foreach dir
#
#  Clean up
#
        if ( &ft('e', $Tmpfile ) ) { 
          &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
        } 
        if ( &ft('e',  $Tmpfile .".1" ) ) { 
          &rm( "-f" ,  &fn(  $Tmpfile .".1" ) ) ; 
        } 
        if ( &ft('e',  $Tmpfile .".2" ) ) { 
          &rm( "-f" ,  &fn(  $Tmpfile .".2" ) ) ; 
        } 
#
#  Inform user on repairs
#
        &doalias('log' ,  '' ) ; 
        if ( &ft('e', $grpfile ) && ! &ft('z', $grpfile ) ) { 
          &doalias('log' ,  " Created " ."\$n_import" . "/lib" . $C_Date . 
                        $n_arch .".grp " ) ; 
          &sort( "-u -o" ,  &fn(  $grpfile ) .' '. &fn(  $grpfile ) , "" ) 
                        ; 
          &doalias('log' ,  '' ) ; 
          &doalias('log' ,  " Repair libraries with: nup build " 
                        ."\$n_import" . "/lib" . $C_Date . $n_arch ) ; 
#
#  If Update mode: insert extra command to update the libraries
#
          if ( &eq(   $Mode ,  "Update" ) ) { 
            $Upd_list= "build" .' '. &fn(  $grpfile ) .' '. &fn(  $Upd_list 
                        ) ; 
          } 
        } 
        else { 
          if ( &ft('e', $grpfile ) ) { 
            &rm( "-f" ,  &fn(  $grpfile ) ) ; 
          } 
          &doalias('log' ,  " Libraries seem to be all right" ) ; 
        } 
      } 
#
#  Check version of executables with respect to database
#
      if ( &peq( (split(' ',$Files)) [ 1 -1 ] , "*e*" ) ) { 
#
#  Reset error-count, initialise temp-list
#
        $Errors= 0 ; 
        if ( &ft('e', $Tmpfile ) ) { 
          &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
        } 
#
#  Rebuild utility programs always
#
        $Input_file= &fn(  $n_src ."/sys/*.c" ) ; 
        &source( &fn(  $n_src ."/sys/compile.csh" ) ) ; 
#
#  Get list of exe's from groupfiles, check version numbers
#  Assume the list of exe's is less than 500...
#
        $Files=  &Pipe("p$$.tmp00", &doexe( &fn(  $n_exe ."/genaid.exe" ) , 
                         "files" .' '. "-t:exe" .' '. &fn(  $n_src 
                        ."/*/*.grp" ) , "p$$.tmp00" ) ) ; 
        for $File__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $File=$File__x ; 
          if ( !&peq( $File , "abpx_*" ) || defined($n_doabp) ) { 
            $v_exe=  '' .' '.  '' ; 
            $v_idx=  '' .' '.  '' ; 
            $File=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  &fp('t', $File ) 
                        ) , "p$$.tmp01" ) , &tr( '' , '[A-Z]' , '[a-z]' , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
            $Flag=   $File .": No executable" ; 
            if ( &ft('e',  $n_exe ."/" . $File ) ) { 
              $Flag=  &Pipe("p$$.tmp00", &grep( '' , &fn(  $File ) ,  &fn(  
                        $n_src ."/sys/database.idx" ) , "p$$.tmp00" ) ) 
                        .' '.  '' .' '.  '' ; 
              $v_idx=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  
                        (split(' ',$Flag)) [ 2 -1 ] ) , "p$$.tmp01" ) , 
                        &awk( "-F." , '{ print $1,$2}' , "p$$.tmp01" , 
                        "p$$.tmp00" ) ) .' '.  '' .' '.  '' ; 
              $Flag=  &Pipe("p$$.tmp00", &what( '' ,  &fn(  $n_exe ."/" . 
                        $File ) , "p$$.tmp01" ) , &grep( '' , "%NST%" , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) .' '.  '' .' '.  '' ; 
              $v_exe=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  
                        (split(' ',$Flag)) [ 2 -1 ] ) , "p$$.tmp01" ) , 
                        &awk( "-F." , '{ print $1,$2}' , "p$$.tmp01" , 
                        "p$$.tmp00" ) ) .' '.  '' .' '.  '' ; 
            } 
            $Input_file=  '' ; 
            if ( &eq(   $v_exe ,  " " ) || &eq(   $v_idx ,  " " ) ) { 
              $Input_file= &fn(  $File ) ; 
            } 
            elsif ( &eq(   $v_exe ,  '' ) || &eq(   $v_idx ,  '' ) ) { 
              $Input_file= &fn(  $File ) ; 
            } 
            else { 
              if (   (split(' ',$v_exe)) [ 1 -1 ] <   (split(' ',$v_idx)) [ 
                        1 -1 ] ||   (split(' ',$v_exe)) [ 2 -1 ] <   
                        (split(' ',$v_idx)) [ 2 -1 ] ) { 
                $Input_file= &fn(  $File ) ; 
              } 
            } 
            if ( !&eq(   $Input_file ,  '' ) ) { 
              if ( !&eq( &Pipe("p$$.tmp00", &grep( "-c" , &fn(  $Input_file 
                        ) ,  &fn(  $n_src ."/dwarf/src.grp" ) , "p$$.tmp00" 
                        ) ) , 0 ) ) { $_Alternate= "1" ; } 
              &source( &fn(  $n_src ."/sys/compile.csh" ) ) ; 
              $_Alternate='' ; 
              $Flag=  &Pipe("p$$.tmp00", &what( '' ,  &fn(  $n_exe ."/" . 
                        $File ) , "p$$.tmp01" ) , &grep( '' , "%NST%" , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) .' '. '(updated)' ; 
            } 
            &echo( '' ,  &fn(  $Flag ) , "p$$.tmp00" ) ; &sed( "-e" , 
                        "s/%NST%//" , "p$$.tmp00" , '>'. &fn(  $Tmpfile ) ) 
                        ; 
          } 
        } 
#
# Errors occurred, give a message (different for nfra and elsewhere)
#
        if ( !&eq(   $n_site , "nfra" ) ) { 
          if ( !&eq( $Errors , 0 ) ) { 
            $Flag=  &Pipe("p$$.tmp00", &cat( '' ,  &fn(  $n_src 
                        ."/sys/version.idx" ) , "p$$.tmp00" ) ) ; 
            sub C2_t15_update { 
              local(*TMP); 
              open(TMP,">txt$$.tmp"); 
              print TMP  '' ."\n" ; 
              print TMP  "*************** Installation errors occured " 
                        ."**********************" ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "The log-file will be mailed to  " . $n_master 
                        ."." ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "Please inform this account of additional inf" 
                        ."ormation that might be" ."\n" ; 
              print TMP  "connected with the errors (recent change of " 
                        ."operation system, disk" ."\n" ; 
              print TMP  "space problems etc). The Newstar group will " 
                        ."contact you about the " ."\n" ; 
              print TMP  "problems as soon as possible." ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "Your present executables should be still cor" 
                        ."rect. " ."\n" ; 
              print TMP  "Your source tree seems to be " . $Flag ." for " 
                        ."\n" ; 
              print TMP  "Your executables seem to be:" ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP   &Pipe("p$$.tmp00", &cat( '' ,  &fn(  $Tmpfile ) , 
                        "p$$.tmp00" ) ) ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "********************************************" 
                        ."*********************" ."\n" ; 
              print TMP  '' ."\n" ; 
              close(TMP); 
              "txt$$.tmp";} 
            &cat( '' ,  &C2_t15_update , "p$$.tmp00" ) ; &tee( "-a" ,  &fn( 
                         $Logfile ) , "p$$.tmp00" ) ; 
            &cat( '' ,  &fn(  $Logfile ) , "p$$.tmp00" ) ; &elm( "-s" ,   
                        "Newstar_crash_on_" . $n_site ."/" . $n_arch .' '. 
                        &fn(  $n_master ) , "p$$.tmp00" ) ; 
            $Mode=  "Quit" ; 
          } 
          else { 
#
#  No errors: inform NFRA if run on remote site
#
            sub C2_t16_update { 
              local(*TMP); 
              open(TMP,">txt$$.tmp"); 
              print TMP  '' ."\n" ; 
              print TMP  "Newstar executables have been updated on " . 
                        $n_site ." (" . $n_arch .") at " . $C_Date ."." 
                        ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "The current version at " . $n_site ." is:" ."\n" 
                        ; 
              print TMP  '' ."\n" ; 
              print TMP   &Pipe("p$$.tmp00", &cat( '' ,  &fn(  $n_src 
                        ."/sys/version.idx" ) , "p$$.tmp00" ) ) ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "The executables have version:" ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP   &Pipe("p$$.tmp00", &cat( '' ,  &fn(  $Tmpfile ) , 
                        "p$$.tmp00" ) ) ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "Yours truly," ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "update.csh" ."\n" ; 
              close(TMP); 
              "txt$$.tmp";} 
            &elm( "-s" ,   "Newstar_update_on_" . $n_site ."/" . $n_arch 
                        .' '. &fn(  $n_master ) ,  &C2_t16_update ) ; 
          } 
        } 
      } 
#
#
#  %Diff: compare files in $n_import with versions in master
#
    } 
    elsif ( &peq(   $Command , "[Dd]*" ) ) { 
#
#  Enforce working in $n_import
#
      if ( !&eq( $PWD , $n_import ) ) { 
        &cd(  &fn(  $n_import ) ) ; 
        &echo( '' ,   "--> Now in directory " . $n_import , "" ) ; 
      } 
#
#  Get groupfile to diff
#
      if ( &eq(   $Files ,  '' ) ) { 
        &echo( "-n" ,   "Enter name of groupfile to diff: " , "" ) ; 
        $noglob='' ;                                   # Don't expand wildcards right now
        $Files=  ($_=scalar(<STDIN>), chop, $_) ;      # Read from stdin
        $Files= &fn(  $Files ) ;                       # Split in multiple words
        undef $noglob ; 
      } 
#
#  Expand the groupfile(s) and compare the files 
#
      for $grpfile__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $grpfile=$grpfile__x ; 
        if ( &eq(   &fp('e', $grpfile ) ,  '' ) ) { $grpfile= &fn(  
                        $grpfile .".grp" ) ; } 
        $dfile= &fn(  $grpfile .".dif" ) ; 
        if ( &ft('e', $dfile ) ) { 
          &rm( "-f" ,  &fn(  $dfile ) ) ; 
        } 
        &echo( '' ,   "Differences introduced by " . $grpfile , ''. &fn(  
                        $dfile ) ) ; 
        &echo( '' ,   "Made at " . $C_Date ."/" . $C_Time ." on " . $n_site 
                        ." (" . $n_arch .") " , '>'. &fn(  $dfile ) ) ; 
        &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "files" .' '. "-t:^exe" 
                        .' '. &fn(  $grpfile ) , ''. &fn(  $Tmpfile ) ) ; 
        for $file__x (split(' ',join(' ' ,  &Pipe("p$$.tmp00", &cat( '' ,  
                        &fn(  $Tmpfile ) , "p$$.tmp00" ) ) ))) { 
                        $file=$file__x ; 
          if ( &ft('e',  $n_import ."/" . &fp('t', $file ) ) ) { 
            if ( &ft('e',  $n_src ."/" . $file ) ) { 
              &echo( '' ,   " " , '>'. &fn(  $dfile ) ) ; 
              &echo( '' ,   "diff " . $n_import ."/" . &fp('t', $file ) 
                        ."  " . $n_src ."/" . $file , '>'. &fn(  $dfile ) ) 
                        ; 
              &diff( '' ,  &fn(  $n_import ."/" . &fp('t', $file ) ) .' '. 
                        &fn(  $n_src ."/" . $file ) , '>'. &fn(  $dfile ) ) 
                        ; 
            } 
            else { 
              &echo( '' ,   " " , '>'. &fn(  $dfile ) ) ; 
              &echo( '' ,   "New file: " . $file , '>'. &fn(  $dfile ) ) ; 
            } 
          } 
        } 
        &more( '' ,  &fn(  $dfile ) , "" ) ; 
        &doalias('log' ,  "Differences are listed in " . $dfile ) ; 
      } 
#
#
#  %save is the backup command
#
    } 
    elsif ( &peq(   $Command , "[Ss]*" ) ) { 
      $Tapes=  "A" .' '.  "B" .' '.  "C" ; 
      $Home= &fn(  $cwd ) ; 
      &cd(  &fn(  $n_root ) ) ; 
      &echo( '' ,   " --> Now in directory " ."\$n_root" , "" ) ; 
      &tail( '' ,  "backups.txt" , "" ) ;              # Show last backups
      $Flag=  &Pipe("p$$.tmp00", &tail( "-1l" ,        # Get very last one
                        "backups.txt" , "p$$.tmp00" ) ) ; 
      if ( &eq(   $Flag ,  '' ) ) { $Flag=  "::" ; } 
      $Tape=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(     # Get last tape
                        $Flag ) , "p$$.tmp01" ) , &awk( "-F:" , 
                        '{ print $2}' , "p$$.tmp01" , "p$$.tmp00" ) ) ; 
      $Unit=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(     # Get previous command
                        $Flag ) , "p$$.tmp01" ) , &awk( "-F:" , 
                        '{ print $3}' , "p$$.tmp01" , "p$$.tmp00" ) ) ; 
      if ( &eq(   $Tape ,  '' ) ) { $Tape=  "Unknown" ; } 
      if ( &eq(   $Unit ,  '' ) ) { $Unit=   $MAG8 ; } 
      $ii= 1 ; 
      while ( $ii < &vn($Tapes) && !&eq( (split(' ',$Tapes)) [ $ii -1 ] , 
                        $Tape ) ) { 
        $ii= $ii + 1 ; 
      } 
      if ( !&eq( (split(' ',$Tapes)) [ $ii -1 ] , $Tape ) ) { 
        &echo( '' ,   "Unknown tape " . $Tape ."..." , "" ) ; 
        $Tape=   (split(' ',$Tapes)) [ 1 -1 ] ; 
      } 
      elsif ( &eq( $ii , &vn($Tapes) ) ) { 
        $Tape=   (split(' ',$Tapes)) [ 1 -1 ] ; 
      } 
      else { 
        $ii= $ii + 1 ; 
        $Tape=   (split(' ',$Tapes)) [ $ii -1 ] ; 
      } 
      &echo( '' ,   "Suggested tape for backup:  ====== " . $Tape 
                        ." ======" , "" ) ; 
      &echo( "-n" ,   "Tape for backup [" . $Tape ."]: " , "" ) ; 
      $ans=  ($_=scalar(<STDIN>), chop, $_) ; if ( !&eq(   $ans ,  '' ) ) { 
                        $Tape=   $ans ; } 
      &echo( "-n" ,   "Tapeunit for backup [" . $Unit ."]: " , "" ) ; 
      $ans=  ($_=scalar(<STDIN>), chop, $_) ; if ( !&eq(   $ans ,  '' ) ) { 
                        $Unit=   $ans ; } 
      &echo( '' ,    $C_Date ." " . $C_Time ." - " . $n_arch ." - save " . 
                        $Tape ." " . $Unit ." " , '>'. &fn(  $n_root 
                        ."/updates.log" ) ) ; 
      &echo( '' ,    $C_Date ."/" . $C_Time ."/" . $n_arch ."(" . $HOST 
                        ."):" . $Tape .":" . $Unit , "p$$.tmp00" ) ; &tee( 
                        "-a" ,  "backups.txt" , "p$$.tmp00" ) ; 
#
# Backup in background, route output by mail
#
      if ( &eq( $n_arch , "hp" ) ) { 
        $Rew=  "mt -t " . $Unit ." rew" ; 
      } 
      else { 
        $Rew=  "mt -f " . $Unit ." rew" ; 
      } 
      &tar( "cf" ,  &fn(  $Unit ) .' '. &fn( "*" ) ,   ### &
                        "p$$.tmp00" ) ; &dollar("Rew" , "" , "p$$.tmp00" ) 
                        ; &tar( "tf" ,  &fn(  $Unit ) , "p$$.tmp00" ) ; 
                        &elm( "-s" ,   "Backup of Master tree" .' '.  $USER 
                        ."@" . &Pipe("p$$.tmp00", &domainname( "p$$.tmp00" 
                        ) ) , "p$$.tmp00" ) ; 
      &cd(  &fn(  $Home ) ) ; 
#
#
#  %Pack: Another archiving command
#
    } 
    elsif ( &peq(   $Command , "[Pp][Aa][Cc][Kk]" ) ) { 
      if ( &eq(   $Files ,  '' ) ) { 
        &echo( "-n" ,   "Enter name of directory (e.g. src, exe, nsca" 
                        ."n, exe/sw): " , "" ) ; 
        $Files=  ($_=scalar(<STDIN>), chop, $_) ; 
        $Files= &fn(  $Files ) ; 
      } 
      if ( &peq(   $Files , "[Aa][Ll][Ll]" ) ) { 
        $Files= "src" .' '. "lib/inc" .' '. "lib/sw" .' '. "lib/hp" .' '. 
                        "exe/sw" .' '. "exe/hp" .' '. "exe/html" ; 
      } 
      if ( defined($n_ftp) ) { &doalias('log' ,  
                        "Archives will be moved to the ftp-area" ) ; } 
      for $dir__x (split(' ',join(' ' , &fn(  $Files ) ))) { $dir=$dir__x ; 
        undef $Source ; 
        if ( &ft('d',  $n_root ."/" . $dir ) ) { 
          $Source= &fn(  $n_root ."/" . $dir ) ; 
          $tarfile= &fn( "nstar_" . $dir .".tar" ) ; 
        } 
        elsif ( &ft('d',  $n_src ."/" . $dir ) ) { 
          $Source= &fn(  $n_src ."/" . $dir ) ; 
          $tarfile= &fn( "nstar_src_" . $dir .".tar" ) ; 
        } 
        elsif ( &ft('d', $dir ) ) { 
          &doalias('log' ,  "Can only tar Newstar Master tree directories" 
                        ) ; 
        } 
        else { 
          &doalias('log' ,  "Error: directory " . $dir ." does not exist" ) 
                        ; 
        } 
        if ( defined($Source) ) { 
          $tarfile=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $tarfile ) , 
                        "p$$.tmp01" ) , &tr( '' , '/' , '_' , "p$$.tmp01" , 
                        "p$$.tmp00" ) ) ; 
          if ( &peq( $cwd ,  $n_src ."/*" ) ) { 
            $tarfile= &fn(  $n_src ."/" . $tarfile ) ; 
          } 
          else { 
            $tarfile= &fn(  $cwd ."/" . $tarfile ) ; 
          } 
          if ( &eq(   $Mode ,  "Menu" ) ) { 
            &echo( "-n" ,   "Enter name of tarfile [" . $tarfile ."]: " , 
                        "" ) ; 
            $tmp=  ($_=scalar(<STDIN>), chop, $_) ; 
            if ( !&eq(   $tmp ,  '' ) ) { $tarfile= &fn(  $tmp ) ; } 
            undef $tmp ; 
          } 
          if ( &ft('e', $tarfile ) || &ft('e',  $tarfile .".Z" ) ) { 
            &rm( '' ,  &fn(  $tarfile ."*" ) ) ; 
            if ( ! &ft('e', $tarfile ) && ! &ft('e',  $tarfile .".Z" ) ) { 
              &doalias('log' ,  "Removed existing " . $tarfile ) ; 
            } 
          } 
          &doalias('log' ,  "Creating tar-file " . $tarfile ) ; 
#
# Tar the files and compress, exclude core, *.tar* *.x?? *.a?? and *.old
#
          $Home= &fn(  $cwd ) ; 
          &cd(  &fn(  $Source ) ) ; 
          &echo( '' ,   "Ignore any no match messages..." , "" ) ; 
          &ls( '' ,  "core" .' '. &fn( "*/core" ) .' '. &fn( "*.tar*" ) 
                        .' '. &fn( "*/*.tar*" ) .' '. &fn( "*.x??" ) .' '. 
                        &fn( "*.a??" ) .' '. &fn( "*/*.x??" ) .' '. &fn( 
                        "*/*.a??" ) .' '. &fn( "*.old" ) .' '. &fn( 
                        "*/*.old" ) , ''. &fn(  $Tmpfile ) ) ; 
          &tar( "cfX" ,  &fn(  $tarfile ) .' '. &fn(  $Tmpfile ) .' '. &fn( 
                        "*" ) , "" ) ; 
          &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
          &cd(  &fn(  $Home ) ) ; 
          &echo( '' ,   "Compressing to " . $tarfile .".Z " , "" ) ; 
          if ( defined($n_ftp) ) { 
            &compress(  &fn(  $tarfile ) , "" ) ; &rsh( &fn(  $n_ftp ) ,  
                        "mv" .' '. &fn(  $tarfile ."*" ) .' '. 
                        '~ftp/newstar' , "" , "" ) ; 
          } 
          else { 
            &compress(  &fn(  $tarfile ) , "" ) ; 
          } 
#
# For the source tree, make separate archives for the binaries etc
#
          if ( &eq(   $Source ,   $n_src ) ) { 
            &cd(  &fn(  $Source ) ) ; 
            $Flag=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $n_install ) , 
                        "p$$.tmp01" ) , &tr( '' , ',/:' , '   ' , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
            for $aa__x (split(' ',join(' ' , &fn(  $Flag ) ))) { $aa=$aa__x 
                        ; 
              &doalias('log' ,  "Creating tar-file " . &fp('r', $tarfile ) 
                        ."_" . $aa .".tar" ) ; 
              &tar( "cf" ,  &fn(  &fp('r', $tarfile ) ."_" . $aa .".tar" ) 
                        .' '. &fn( "*/*.x" . $aa ) .' '. &fn( "*/*.a" . $aa 
                        ) , "" ) ; 
              if ( defined($n_ftp) ) { 
                &rsh( &fn(  $n_ftp ) ,  "mv" .' '. &fn(  &fp('r', $tarfile 
                        ) ."_" . $aa .".tar" ) .' '. '~ftp/newstar' , "" , 
                        "" ) ; 
              } 
            } 
          } 
        } 
      } 
      undef $Home ; undef $tarfile ; undef $dir ; 
#
#  %Group  Combine or spilt groupfiles
#
    } 
    elsif ( &peq(   $Command , "[Gg]*" ) ) { 
      if ( &eq(   $Files ,  '' ) ) { 
        &echo( '' ,   "Need to specify at least one groupfile" , "" ) ; 
      } 
      elsif ( !&eq( &Pipe("p$$.tmp00", &grep( "-c" , '^+' ,  &fn(  
                        (split(' ',$Files)) [ 1 -1 ] ) , "p$$.tmp00" ) ) , 
                        0 ) ) { 
        for $File__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $File=$File__x ; 
          &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "split" .' '. &fn(  
                        $File ) .' '. &fn(  $C_Date ."_c" ) , "" ) ; 
        } 
        &ls( "-l" ,  &fn( "*" . $C_Date ."_c.grp" ) , "" ) ; 
      } 
      else { 
#
# Make unique name
#
        $grpfile= &fn( "upd" . $C_Date .".grp" ) ; 
        $ii= 0 ; 
        while ( &ft('e', $grpfile ) ) { 
          $ii= $ii + 1 ; 
          $grpfile= &fn( "upd" . $C_Date . $ii .".grp" ) ; 
        } 
        undef $ii ; 
        &echo( '' ,   "\!+" . $grpfile ."  combined groupfile made by " . 
                        $USER , ''. &fn(  $grpfile ) ) ; 
#
# Process all groupfiles
#
        &doexe( &fn(  $n_exe ."/genaid.exe" ) ,  "group" .' '. &fn(  $Files 
                        ) , '>'. &fn(  $grpfile ) ) ; 
        &doalias('log' ,  "Output is in " . $grpfile ) ; 
      } 
    }                                                  # Other command
    else { 
      &echo( '' ,   '' , "" ) ; 
      &echo( '' ,   "Error: Invalid or ambiguous command " . $Command , "" 
                        ) ; 
      &echo( '' ,   '' , "" ) ; 
    }                                                  # End of if (Command == ...)
  }                                                    # End of while (Menu mode)
  &Abort_exit_update ; 
  sub  Abort_exit_update { 
    ; 
#
# Handle any pending library actions left after an abort
#
    if ( !&eq(   $_Objectlib ,  '' ) ) { 
      $Input_file= &fn(  $_Objectlib ) ; 
      &source( &fn(  $n_src ."/sys/compile.csh" ) ) ; 
    } 
    if ( !&eq(   $_Textlib ,  '' ) ) { 
      $Input_file= &fn(  $_Textlib ) ; 
      &source( &fn(  $n_src ."/sys/compile.csh" ) ) ; 
    } 
    if ( &ft('e',  $n_work ."/update.lock" ) ) { 
      &rm( "-f" ,  &fn(  $n_work ."/update.lock" ) ) ; 
    } 
    if ( &ft('e', $Tmpfile ) ) { 
      &rm( "-f" ,  &fn(  $Tmpfile ) ) ; 
    } 
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
eval('&update__pls');
1;
#-
