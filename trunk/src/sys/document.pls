#+ document.pls
#   created by wbrouw on norma at Tue Jun 21 13:21:46 LST 1994 
#-
#! /bin/csh -f
#+ document.csh
#
#   CMV 930713  Created
#   CMV 931111  Original memo file not autmatically deleted
#   CMV 931116  Changed for httpd 1.0
#   CMV 931206  Use nview (httpd/htbin) for extracted documentation
#   CMV 940506  Make summary <PRE> formatted
#   CMV 940530	Preserve BELL in script
#
# This script coordinates all actions for documentation maintenance
#
# It does not contain full checking on the environment etc.
#
#---------------------------------------------------------------------
#
# Uncomment the following line for testing purposes...
#set echo
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
sub document__pls { 
  $SIG{'INT'}= Abort_exit_document ; 
#
# Initialise name and date
#
  $Myname=  &Pipe("p$$.tmp00", &awk( "-F:" , '{ if ($1 == "' . $USER 
                        .'") print $5 }' ,  "/etc/passwd" , "p$$.tmp00" ) ) 
                        ; 
  if ( &eq(   $Myname ,  '' ) ) { $Myname=  &Pipe("p$$.tmp00", &whoami( 
                        "p$$.tmp00" ) ) ; } 
  $dt=  &Pipe("p$$.tmp00", &date( "p$$.tmp00" ) ) ; 
  if ( &peq(   (split(' ',$dt)) [ 3 -1 ] , "[1-9]" )   # day
                        ) { @dt=split(' ',$dt); splice(@dt, "3" -1,1,   
                        (split(' ',$dt)) [ 3 -1 ] ); $dt=join(' ',@dt); } 
  $mc= "Jan" .' '. "Feb" .' '. "Mar" .' '. "Apr" .' '. "May" .' '. "Jun" 
                        .' '. "Jul" .' '. "Aug" .' '. "Sep" .' '. "Oct" 
                        .' '. "Nov" .' '. "Dec" ; 
  for $mm__x (split(' ',join(' ' , "01" , "02" , "03" , "04" , "05" , "06" 
                        , "07" , "08" , "09" , "10" , "11" , "12" ))) { 
                        $mm=$mm__x ; 
    if ( &eq(   (split(' ',$dt)) [ 2 -1 ] ,            # month
                        (split(' ',$mc)) [ $mm -1 ] ) ) { last ; } 
  } 
  $yy= (split(' ',$dt)) [ &vn($dt) -1 ] - 1900 ;       # year
  $mh=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(           # hh mm ss
                        (split(' ',$dt)) [ 4 -1 ] ) , "p$$.tmp01" ) , &tr( 
                        "-s" ,  ":" ,  " " , "p$$.tmp01" , "p$$.tmp00" ) ) 
                        ; 
  $C_Date=   $yy . $mm . (split(' ',$dt)) [ 3 -1 ] ;   # date: yymmdd
  $C_Time=   (split(' ',$mh)) [ 1 -1 ] .":" .          # time: hh:mm
                        (split(' ',$mh)) [ 2 -1 ] ; 
  undef $dt ; undef $mc ; undef $mm ; undef $yy ; undef $mh ; 
  if ( ! defined($EDITOR) ) { $EDITOR= "emacs" ; &ENV_EXPORT( EDITOR , 
                        "emacs" ) ; } 
#
# Initialise TeX just in case...
#
  if ( &ft('e', "/usr/local/lib/tex/texsetup" ) ) { 
    if ( ! defined($TEXFONTS) ) { &source( "/usr/local/lib/tex/texsetup" ) 
                        ; } 
    $TEXINPUT=   $n_src ."/doc/cook" ; &ENV_EXPORT( TEXINPUT ,   $n_src 
                        ."/doc/cook" ) ; 
    &alias( 'tex', '$TEXFONTS='. '"/usr/local/lib/tex/fonts"'. ';'. 
                        '&ENV_EXPORT('. 'TEXFONTS'. ','. 
                        '"/usr/local/lib/tex/fonts"'. ')'. ';'. 
                        '&doalias(\'ex\''. ')'. ';'. '', "") ; 
    &alias( 'latex', '$TEXFONTS='. '"/usr/local/lib/tex/fonts"'. ';'. 
                        '&ENV_EXPORT('. 'TEXFONTS'. ','. 
                        '"/usr/local/lib/tex/fonts"'. ')'. ';'. 
                        '&doalias(\'atex\''. ')'. ';'. '', "") ; 
    &alias( 'dvips', '$TEXFONTS='. '"/usr/local/lib/pk/pk300"'. ';'. 
                        '&ENV_EXPORT('. 'TEXFONTS'. ','. 
                        '"/usr/local/lib/pk/pk300"'. ')'. ';'. 
                        '&doalias_x('. ''. 
                        '"-B/usr/local/lib/tex/TeXPS/dvitps-cap"'. ','. 
                        '"-P/usr/local/lib/tex/TeXPS/pro"'. ','. '"!*"'. 
                        ')'. ';'. '&doalias(\'/usr/local/lib/dvitps\''. 
                        ')'. ';'. '', "") ; 
    &alias( 'xdvi', '$TEXFONTS='. '"/usr/local/lib/pk/pk300"'. ';'. 
                        '&ENV_EXPORT('. 'TEXFONTS'. ','. 
                        '"/usr/local/lib/pk/pk300"'. ')'. ';'. 
                        '&doalias(\'/usr/local/lib/tex/xdvi\''. ')'. ';'. 
                        '', "") ; 
  } 
#
#  Get command, or enter menu mode if none given.
#
  $Files=  '' ; 
  if ( !&eq(   (split(' ',$argv)) [ 1 -1 ] ,  '' ) ) { 
    $Mode=  "Command" ; 
    $Command=   (split(' ',$argv)) [ 1 -1 ] ; 
    $noglob='' ; if ( &vn($argv) > 1 ) { $Files= &fn(  (split(' ',$argv)) [ 
                        2 -1 .. &vn($argv)-1 ] ) ; } undef $noglob ; 
  } 
  else { 
    $Mode=  "Menu" ; 
    $Command=  '' ; 
  } 
#
# If in Menu mode, repeatedly ask commands, else just one command
#
  while ( !&eq(   $Mode ,  "Quit" ) ) { 
    if ( &eq(   $Mode ,  "Menu" ) ) { 
      &echo( '' ,   "General commands are:      help, script, pri" 
                        ."nt, hyper, quit" , "" ) ; 
      &echo( '' ,   "Translation commands are:  full, keys, cook," 
                        ." memo, extract" , "" ) ; 
      &echo( "-n" ,   "Enter a command: " , "" ) ; 
      $Command=  ($_=scalar(<STDIN>), chop, $_) ; 
      $Files=  '' ; 
      $Command= &fn(  $Command ) ; 
      if ( &vn($Command) > 1 ) { 
        $noglob='' ; $Files= &fn(  (split(' ',$Command)) [ 2 -1 .. 
                        &vn($Command)-1 ] ) ; undef $noglob ; 
        $Command= &fn(  (split(' ',$Command)) [ 1 -1 ] ) ; 
      } 
    } 
    elsif ( &eq(   $Mode ,  "Update" ) ) { 
      $lupdate=  "keys" .' '.  "cook" ; 
      $Command= &fn(  (split(' ',$lupdate)) [ $iupdate -1 ] ) ; 
      $Files=  "all" ; 
      &echo( '' ,   "***** " . $Command ." all *****" , "" ) ; 
      $iupdate= $iupdate + 1 ; 
      if ( $iupdate > &vn($lupdate) ) { $Mode=  "Quit" ; } 
    } 
    else { 
      $Mode=  "Quit" ; 
    } 
    if ( &eq(   $Command ,  '' ) || &peq( $Command , "[Qq]*" ) ) { 
      $Mode=  "Quit" ; 
#
#  %Full command, scan a list of commands
#      
    } 
    elsif ( &peq( $Command , "[Ff][Uu][Ll]*" ) ) { 
      $Mode=  "Update" ; 
      $iupdate= "1" ; 
#
#  %Help command:
#
    } 
    elsif ( &peq(   $Command , "[Hh][Ee]*" ) ) { 
      sub C2_t1_document { 
        local(*TMP); 
        open(TMP,">txt$$.tmp"); 
        print TMP  "#+++++++++++++++++++++++++++++++++++++++++++" 
                        ."++++++++++++++++++++++++++++++#" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  " Document.csh  is used for maintenance of th" 
                        ."e Newstar documentation." ."\n" ; 
        print TMP  " " ."\n" ; 
        print TMP  " Document can be called in one of the follow" 
                        ."ing ways:" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    document " ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Enter a menu mode where all options " 
                        ."listed below can be " ."\n" ; 
        print TMP  "        chosen. Additional arguments will be" 
                        ." prompted for." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    document script [-p|v] [File]" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Start a script session and convert t" 
                        ."he terminal output " ."\n" ; 
        print TMP  "        to a proper LaTeX file. The file can" 
                        ." (and probably should)" ."\n" ; 
        print TMP  "        be edited by hand and then be moved " 
                        ."into \$n_src/doc/cook" ."\n" ; 
        print TMP  "        The -P switch causes an existing lat" 
                        ."ex file to be printed" ."\n" ; 
        print TMP  "        The -V switch causes an existing lat" 
                        ."ex file to be viewed with xdvi" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    document print [-p|v] [File]" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Print or view an existing LaTeX file" 
                        ." with a title page " ."\n" ; 
        print TMP  "        and a table of contents." ."\n" ; 
        print TMP  "        The -P switch causes an existing lat" 
                        ."ex file to be printed (default)" ."\n" ; 
        print TMP  "        The -V switch causes an existing lat" 
                        ."ex file to be viewed with xdvi" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    document hyper [File]" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Starts the xmosaic hypertext browser" 
                        ." with the specified file." ."\n" ; 
        print TMP  "        The default is the Newstar Home page" ."." 
                        ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    document full" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Updates the html database: equivalen" ."t to " 
                        ."\n" ; 
        print TMP  "          keys all; cook all; " ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    document keys [File...|all]" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Convert one or more PIN/PSC/PEF file" 
                        ."s to LaTeX and html files." ."\n" ; 
        print TMP  "        The files should be given with their" 
                        ." extension, wildcards are" ."\n" ; 
        print TMP  "        allowed. The literal 'all' correspon" 
                        ."ds to all files *.p?? in" ."\n" ; 
        print TMP  "        directories " . $NSTAR_DIR ."." ."\n" ; 
        print TMP  "        The resulting LaTeX files will be in" 
                        ." \$n_src/doc/keys, the" ."\n" ; 
        print TMP  "        html files will be in \$n_hlp" ."\n" ; 
        print TMP  "   " ."\n" ; 
        print TMP  "        NB: In principle this could be done " 
                        ."through the compile script." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    document cook [File...|all]" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Convert one or more LaTeX files into" 
                        ." html files. The literal " ."\n" ; 
        print TMP  "        'all' corresponds to \$n_src/doc/coo" ."k/*" 
                        ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    document memo [<number>|new] [File]" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Edit an existing memo (if <number> g" 
                        ."iven) or create a Newstar Memo " ."\n" ; 
        print TMP  "        header (literal 'new' given)." ."\n" ; 
        print TMP  "        External files may be connected to t" 
                        ."he header." ."\n" ; 
        print TMP  "        Links to other Memo's/cookbook-files" 
                        ."/documentation " ."\n" ; 
        print TMP  "        can be made in the header as well." ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "    document extract [File...]" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "        Extract documentation (between C+/C-" 
                        .") from one or more files " ."\n" ; 
        print TMP  "        into html files. Empty output files " 
                        ."are deleted. The files are" ."\n" ; 
        print TMP  "        moved into directory \$n_root/server" 
                        ."/newstar/extract" ."\n" ; 
        print TMP  '' ."\n" ; 
        print TMP  "#-------------------------------------------" 
                        ."------------------------------#" ."\n" ; 
        print TMP  '' ."\n" ; 
        close(TMP); 
        "txt$$.tmp";} 
      &more( '' ,  &C2_t1_document , "" ) ; 
#
#
#   %Script command:
#
    } 
    elsif ( &peq(   $Command , "[Ss]*" ) ) { 
      $Print='' ; 
      if ( &peq( (split(' ',$Files)) [ 1 -1 ] , "-*" ) ) { 
        if ( &peq( (split(' ',$Files)) [ 1 -1 ] , "-[Pp]" ) ) { $Print= "1" 
                        ; } 
        if ( &peq( (split(' ',$Files)) [ 1 -1 ] , "-[Vv]" ) ) { $Print= "2" 
                        ; } 
        @Files=split(' ',$Files); splice(@Files, "1" -1,1,  '' ); 
                        $Files=join(' ',@Files); if ( &vn($Files) > 1 ) { 
                        @Files=split(' ',$Files) ; shift(@Files) ; 
                        $Files=join(' ',@Files) ; } 
      } 
      $File= &fn(  (split(' ',$Files)) [ 1 -1 ] ) ; 
      if ( &eq(   $File ,  '' ) ) { 
        &echo( "-n" ,   "Enter the name for the output LaTeX file: " , "" ) 
                        ; 
        $File=  ($_=scalar(<STDIN>), chop, $_) ;       # Read from stdin
      } 
      if ( !&eq(   &fp('e', $File ) ,  '' ) ) { $File= &fn(  &fp('r', $File 
                        ) ) ; } 
      if ( ! $Print ) { 
#
# Choice to append in case of split terminal session, 
# make sure file exists by touch-ing it (not really necessary)
#
        if ( &ft('e',  $File .".tex" ) ) { 
          &echo( "-n" ,   "Append to existing LaTeX file (y,n)? [y] " , "" 
                        ) ; 
          $ans=  ($_=scalar(<STDIN>), chop, $_) ; 
          if ( &peq( $ans , "[Nn]*" ) ) { &rm( "-f" ,  &fn(  $File .".tex" 
                        ) ) ; } 
        } 
        &touch( '' ,  &fn(  $File .".tex" ) ) ; 
#
# Start script utility, this will leave you in a subshell
#
        &echo( '' ,   " " , "" ) ; 
        &echo( '' ,   " Initialise Newstar by typing           " ."\$go" , 
                        "" ) ; 
        &echo( '' ,   " Execute all necessary commands, type   exit" 
                        ."   when done..." , "" ) ; 
        &echo( '' ,   " " , "" ) ; 
        $go=  "source " . $n_src ."/sys/newstar_" . $n_site .".csh" ; 
                        &ENV_EXPORT( go ,  "source " . $n_src 
                        ."/sys/newstar_" . $n_site .".csh" ) ; 
        $savbell=  &Pipe("p$$.tmp00", &doexe( &fn(  $n_exe ."/view.exe" ) , 
                         &fn( "dwarf\$0_bell" ) .' '. "/general" , 
                        "p$$.tmp00" ) ) ; &ENV_EXPORT( savbell ,  
                        &Pipe("p$$.tmp00", &doexe( &fn(  $n_exe 
                        ."/view.exe" ) ,  &fn( "dwarf\$0_bell" ) .' '. 
                        "/general" , "p$$.tmp00" ) ) ) ; 
        $n_script=  '' ; &ENV_EXPORT( n_script ,  '' ) ; 
        &doalias('script' , &fn(  $File .".tmp" ) ) ; 
        sub C2_t2_document { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  "bell=" . $savbell ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &doexe( &fn(  $n_exe ."/specify.exe" ) ,  "dwarf" .' '. "/nomenu" 
                        .' '. &C2_t2_document , "" ) ; 
        &doalias('unsetenv' , "n_script" , "savbell" , "go" ) ; 
#
# Transform the terminal output to something more decent
#
        &echo( '' ,   " " , "" ) ; 
        &echo( '' ,   "Converting script output to LaTeX..." , "" ) ; 
        &doexe( &fn(  $n_exe ."/docaid.exe" ) ,  "script" .' '. &fn(  $File 
                        .".tmp" ) , '>'. &fn(  $File .".tex" ) ) ; 
        &echo( '' ,   "LaTeX commands are in " . $File .".tex" , "" ) ; 
#
# Ask for printout 
#
        &echo( "-n" ,   "Make printout (y,p), view (v) or stop (s,n)?" 
                        ." [n] " , "" ) ; 
        $ans=  ($_=scalar(<STDIN>), chop, $_) ; 
        if ( &peq(   $ans , "[Yy]*" ) || &peq(   $ans , "[Pp]*" ) ) { 
                        $Print= "1" ; } 
        if ( &peq(   $ans , "[Vv]*" ) ) { $Print= "2" ; } 
      } 
#
# Make printout
#
      if ( $Print ) { 
        $Tmpfile= &fn(  &fp('t', $File ) ."_tmp" ) ; 
        sub C2_t3_document { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  "\\documentstyle{book}" ."\n" ; 
          print TMP  "\\input{" . $n_src ."/doc/cook/cb_preamble}" ."\n" ; 
          print TMP  "\\input{" . $n_src ."/doc/cook/cb_symbols}" ."\n" ; 
          print TMP  "\\begin{document}" ."\n" ; 
          print TMP  "\\title{NEWSTAR Cookbook - Sample script}" ."\n" ; 
          print TMP  "\\author{" . $Myname ."}" ."\n" ; 
          print TMP  "\\maketitle" ."\n" ; 
          print TMP  "\\include{" . $File ."}" ."\n" ; 
          print TMP  "\\end{document}" ."\n" ; 
          print TMP  "\\end" ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &cat( '' ,  &C2_t3_document , ''. &fn(  $Tmpfile .".tex" ) ) ; 
        &doalias('latex' , &fn(  $Tmpfile ) ) ; 
        if ( &ft('e',  $Tmpfile .".dvi" ) ) { 
          if ( &eq( $Print , 2 ) ) { 
            &mv( '' ,  &fn(  $Tmpfile .".dvi" ) .' '. &fn(  &fp('t', $File 
                        ) .".dvi" ) ) ; 
            &doalias('xdvi' , &fn(  &fp('t', $File ) ) ) ; 
          } 
          else { 
            &doalias('dvips' , &fn(  $Tmpfile ) , ">" , &fn(  $Tmpfile 
                        .".ps" ) ) ; 
            if ( &ft('e',  $Tmpfile .".ps" ) ) { &mv( '' ,  &fn(  $Tmpfile 
                        .".ps" ) .' '. &fn(  &fp('t', $File ) .".ps" ) ) ; 
                        } 
            if ( &ft('e',  &fp('t', $File ) .".ps" ) ) { 
              &echo( '' ,   "Postscript is in " . &fp('t', $File ) 
                        .".ps, trying to print now" , "" ) ; 
              &docsh( &fn(  $n_src ."/sys/wngfex.csh" ) ,  "PS" .' '. &fn(  
                        &fp('t', $File ) .".ps" ) , "" ) ; 
            } 
            else { 
              &echo( '' ,   "Could not produce postscript output..." , "" ) 
                        ; 
            } 
          } 
        } 
        &rm( "-f" ,  &fn(  $Tmpfile .".*" ) ) ; 
      } 
#
#
#   %Print command:
#
    } 
    elsif ( &peq(   $Command , "[Pp]*" ) ) { 
      $Print= "1" ; 
      if ( &peq( (split(' ',$Files)) [ 1 -1 ] , "-*" ) ) { 
        if ( &peq( (split(' ',$Files)) [ 1 -1 ] , "-[Pp]" ) ) { $Print= "1" 
                        ; } 
        if ( &peq( (split(' ',$Files)) [ 1 -1 ] , "-[Vv]" ) ) { $Print= "2" 
                        ; } 
        @Files=split(' ',$Files); splice(@Files, "1" -1,1,  '' ); 
                        $Files=join(' ',@Files); if ( &vn($Files) > 1 ) { 
                        @Files=split(' ',$Files) ; shift(@Files) ; 
                        $Files=join(' ',@Files) ; } 
      } 
      $File= &fn(  (split(' ',$Files)) [ 1 -1 ] ) ; 
      if ( &eq(   $File ,  '' ) ) { 
        &echo( "-n" ,   "Enter the name of the LaTeX file to print: " , "" 
                        ) ; 
        $File=  ($_=scalar(<STDIN>), chop, $_) ;       # Read from stdin
      } 
      if ( !&eq(   &fp('e', $File ) ,  '' ) ) { $File= &fn(  &fp('r', $File 
                        ) ) ; } 
      $Tmpfile= &fn(  &fp('t', $File ) ."_tmp" ) ; 
      &rm( "-f" ,  &fn(  $Tmpfile .".*" ) ) ;          ### > /dev/null
      sub C2_t4_document { 
        local(*TMP); 
        open(TMP,">txt$$.tmp"); 
        print TMP  "\\documentstyle{book}" ."\n" ; 
        print TMP  "\\input{" . $n_src ."/doc/cook/cb_preamble}" ."\n" ; 
        print TMP  "\\input{" . $n_src ."/doc/cook/cb_symbols}" ."\n" ; 
        print TMP  "\\begin{document}" ."\n" ; 
        print TMP  "\\title{NEWSTAR Cookbook (partial printout)}" ."\n" ; 
        print TMP  "\\author{Printed by " . $Myname ."}" ."\n" ; 
        print TMP  "\\maketitle" ."\n" ; 
        print TMP  "\\tableofcontents" ."\n" ; 
        print TMP  "\\include{" . $File ."}" ."\n" ; 
        print TMP  "\\end{document}" ."\n" ; 
        print TMP  "\\end" ."\n" ; 
        close(TMP); 
        "txt$$.tmp";} 
      &cat( '' ,  &C2_t4_document , ''. &fn(  $Tmpfile .".tex" ) ) ; 
      &doalias('latex' , &fn(  $Tmpfile ) ) ; 
      &doalias('latex' , &fn(  $Tmpfile ) ) ; 
      if ( &ft('e',  $Tmpfile .".dvi" ) ) { 
        if ( &eq( $Print , 2 ) ) { 
          &mv( '' ,  &fn(  $Tmpfile .".dvi" ) .' '. &fn(  &fp('t', $File ) 
                        .".dvi" ) ) ; 
          &doalias('xdvi' , &fn(  &fp('t', $File ) ) ) ; 
        } 
        else { 
          &doalias('dvips' , &fn(  $Tmpfile ) , ">" , &fn(  $Tmpfile .".ps" 
                        ) ) ; 
          if ( &ft('e',  $Tmpfile .".ps" ) ) { &mv( '' ,  &fn(  $Tmpfile 
                        .".ps" ) .' '. &fn(  &fp('t', $File ) .".ps" ) ) ; 
                        } 
          if ( &ft('e',  &fp('t', $File ) .".ps" ) ) { 
            &echo( '' ,   "Postscript is in " . $File 
                        .".ps, trying to print now" , "" ) ; 
            &docsh( &fn(  $n_src ."/sys/wngfex.csh" ) ,  "PS" .' '. &fn(  
                        &fp('t', $File ) .".ps" ) , "" ) ; 
          } 
          else { 
            &echo( '' ,   "Could not produce postscript output..." , "" ) ; 
          } 
        } 
      } 
      &rm( "-f" ,  &fn(  $Tmpfile .".*" ) ) ; 
#
#  %Hyper command
#
    } 
    elsif ( &peq(   $Command , "[Hh][Yy]*" ) ) { 
      $File= &fn(  (split(' ',$Files)) [ 1 -1 ] ) ; 
      if ( &eq(   $File ,  '' ) ) { $File= &fn(  $n_hlp ."/newstar.html" ) 
                        ; } 
      &doexe( &fn(  $n_exe ."/docaid.exe" ) ,  "hyper" .' '. &fn(  $File ) 
                        , "" ) ; 
#
#
#  %Keys command
#
    } 
    elsif ( &peq(   $Command , "[Kk]*" ) ) { 
      if ( &eq(   $Files ,  '' ) ) { 
        &echo( "-n" ,   "Enter the name of PIN/PSC/PEF file(s) [All]:" ." " 
                        , "" ) ; 
        $Files=  ($_=scalar(<STDIN>), chop, $_) ; 
        $Files= &fn(  $Files ) ; 
      } 
      if ( &eq(   $Files ,  '' ) || &peq(   $Files , "[Aa][Ll][Ll]" ) ) { 
                        $Files= &fn(  $NSTAR_DIR ) ; } 
      for $File__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $File=$File__x ; 
        if ( &ft('d',  $n_src ."/" . $File ) ) { $File= &fn(  $n_src ."/" . 
                        $File ) ; } 
        if ( &ft('d', $File ) ) { 
          $nonomatch='' ; 
          $Flag=   $File ; 
          $File= &fn(  $File ."/*.p??" ) ; 
          if ( &eq(   $File ,  $Flag .'/*.p??' ) ) { $File=  '' ; } 
          undef $nonomatch ; 
        } 
        if ( !&eq(   $File ,  '' ) ) { &doexe( &fn(  $n_exe ."/docaid.exe" 
                        ) ,  "keys" .' '. &fn(  $File ) , "" ) ; } 
      } 
      &echo( '' ,   "Update the index " ."\$" . 
                        "n_src/doc/progkeys.tex by hand if necessary" , "" 
                        ) ; 
#
#
#  %Cook command
#
    } 
    elsif ( &peq(   $Command , "[Cc]*" ) ) { 
      if ( &eq(   $Files ,  '' ) ) { 
        &echo( "-n" ,   "Enter the name of LaTeX file(s) [All]: " , "" ) ; 
        $Files=  ($_=scalar(<STDIN>), chop, $_) ; 
        $Files= &fn(  $Files ) ; 
      } 
      $Reflist= &fn(  $n_src ."/doc/cook/reflist.txt" ) ; 
      undef $Idx ; 
      if ( &eq(   $Files ,  '' ) || &peq(   $Files , "[Aa][Ll][Ll]" ) ) { 
        $Files= &fn(  $n_src ."/doc/cook/*.tex" ) ; 
        &echo( '' ,   "Processing all files in " . $n_src ."/doc/cook..." , 
                        "" ) ; 
        if ( &ft('e', $Reflist ) ) { &rm( "-f" ,  &fn(  $Reflist ) ) ; } 
        $Idx= &fn(  $n_hlp ."/index_cook.html" ) ; 
      } 
      if ( ! &ft('e', $Reflist ) ) { 
        &echo( '' ,   "Files are in directory " . $n_src ."/doc/cook" , ''. 
                        &fn(  $Reflist ) ) ; 
        &echo( '' ,   " " , '>'. &fn(  $Reflist ) ) ; 
      } 
      if ( defined($Idx) ) { 
        sub C2_t5_document { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  "<TITLE>Newstar Documentation: Cookbook Index" 
                        ."</TITLE>" ."\n" ; 
          print TMP  "<UL>" ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &cat( '' ,  &C2_t5_document , ''. &fn(  $Idx ) ) ; 
      } 
      for $File__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $File=$File__x ; 
        &doexe( &fn(  $n_exe ."/docaid.exe" ) ,  "html" .' '. &fn(  $File ) 
                        , '>'. &fn(  $Reflist ) ) ; 
        if ( defined($Idx) && !&peq( $File , "*/cb_*" ) && !&peq( $File , 
                        "*_keys.tex" ) && !&peq( $File , "*_comm.tex" ) && 
                        !&peq( $File , "*_short.tex" ) ) { 
          $Ref= &fn(  &fp('t', $File ) ) ; 
          $Ref= &fn(  &fp('r', $Ref ) ) ; 
          if ( &peq( $File , "*/fig_*.tex" ) || &peq( $File , "*/tab_*.tex" 
                        ) || &peq( $File , "*/eqn_*.tex" ) ) { 
            &echo( '' ,   "<LI> <A HREF=" . $Ref .".gif><EM>" . $Ref 
                        ."</EM></A>" , '>'. &fn(  $Idx ) ) ; 
          } 
          else { 
            &echo( '' ,   "<LI> <A HREF=" . $Ref .".html>" . &fp('t', $File 
                        ) ."</A>" , '>'. &fn(  $Idx ) ) ; 
          } 
        } 
      } 
      if ( defined($Idx) ) { 
        &echo( '' ,   "</UL>" , '>'. &fn(  $Idx ) ) ; 
      } 
#
#
#  %Memo command
#
    } 
    elsif ( &peq(   $Command , "[Mm]*" ) ) { 
      $server_dir= &fn(  $n_root ."/server/newstar/memo" ) ; 
      if ( ! &ft('d', $server_dir ) ) { 
        &echo( '' ,   " " , "" ) ; 
        &echo( '' ,   "You do not have a memo server directory here" 
                        ."...." , "" ) ; 
        &echo( '' ,   "This directory should be named " . $server_dir , "" 
                        ) ; 
        &echo( '' ,   " " , "" ) ; 
      } 
      else { 
        if ( &eq(   $Files ,  '' ) ) { 
          &echo( "-n" ,   "Enter the number of the memo [new]: " , "" ) ; 
          $Files=  ($_=scalar(<STDIN>), chop, $_) ; 
          if ( &eq(   $Files ,  '' ) ) { $Files=  "new" ; } 
        } 
        $subj=  "<DT><STRONG>Subject:</STRONG>" ; 
#
#  Edit existing memo
#
        if ( !&eq(   (split(' ',$Files)) [ 1 -1 ] ,  "new" ) ) { 
          $memo_id=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  
                        (split(' ',$Files)) [ 1 -1 ] ) , "p$$.tmp01" ) , 
                        &awk( '' , '{printf "%4.4d",$1}' , "p$$.tmp01" , 
                        "p$$.tmp00" ) ) ; 
          $memo_file= &fn(  $server_dir ."/n" . $memo_id .".mem" ) ; 
          if ( ! &ft('e', $memo_file ) ) { 
            &echo( '' ,   "No memo " . $memo_id ." (missing file " . 
                        $memo_file .")" , "" ) ; 
            $memo_file=  '' ; 
          } 
          else { 
            $File=  &Pipe("p$$.tmp00", &grep( '' , '<STRONG>Document:' ,  
                        &fn(  $memo_file ) , "p$$.tmp01" ) , &awk( '' , 
                        '{printf $2}' , "p$$.tmp01" , "p$$.tmp00" ) ) ; 
          } 
#
#  Create new memo in the system
#
        } 
        else { 
          $ii= 1 ; 
          while ( &ft('e',  $server_dir ."/n" . &Pipe("p$$.tmp00", &echo( 
                        '' ,  &fn(  $ii ) , "p$$.tmp01" ) , &awk( '' , 
                        '{printf "%4.4d",$1}' , "p$$.tmp01" , "p$$.tmp00" ) 
                        ) .".mem" ) ) { 
            $ii= $ii + 1 ; 
          } 
          $memo_id=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $ii ) , 
                        "p$$.tmp01" ) , &awk( '' , '{printf "%4.4d",$1}' , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) ; 
          $memo_file= &fn(  $server_dir ."/n" . $memo_id .".mem" ) ; 
#
#  Get pertinent information
#
          &echo( '' ,   "Creating Newstar memo with id-number " . $memo_id 
                        , "" ) ; 
          $File=  '' ; 
          if ( &vn($Files) > 1 ) { 
            if ( !&eq(   (split(' ',$Files)) [ 2 -1 ] ,  '' ) ) { 
              $File= &fn(  (split(' ',$Files)) [ 2 -1 ] ) ; 
              if ( ! &ft('e', $File ) && !&peq( $File , "[Nn][Oo][Nn][Ee]" 
                        ) ) { $File=  '' ; } 
            } 
          } 
          while ( &eq( $File ,  '' ) ) { 
            &echo( "-n" ,   "Associated text file [" . $File ."]: " , "" ) 
                        ; 
            $ans=  ($_=scalar(<STDIN>), chop, $_) ; 
            if ( !&eq(   $ans ,  '' ) ) { 
              $File= &fn(  $ans ) ; 
              if ( ! &ft('e', $File ) && !&peq( $File , "[Nn][Oo][Nn][Ee]" 
                        ) ) { $File=  '' ; } 
            } 
          } 
          $Subject=  "Unknown" ; 
          $Author=   $Myname ; 
          $Status=  "Info" ; 
          $Action=  "None" ; 
          $To=  "Newstar Memo Series" ; 
          &echo( "-n" ,   "Enter subject [" . $Subject ."]: " , "" ) ; 
          $ans=  ($_=scalar(<STDIN>), chop, $_) ; if ( !&eq(   $ans ,  '' ) 
                        ) { $Subject= &fn(  $ans ) ; } 
          &echo( "-n" ,   "Enter author [" . $Author ."]: " , "" ) ; 
          $ans=  ($_=scalar(<STDIN>), chop, $_) ; if ( !&eq(   $ans ,  '' ) 
                        ) { $Author= &fn(  $ans ) ; } 
          &echo( "-n" ,   "Enter status (Proposal/Change/Info) [" . $Status 
                        ."]: " , "" ) ; 
          $ans=  ($_=scalar(<STDIN>), chop, $_) ; if ( !&eq(   $ans ,  '' ) 
                        ) { $Status= &fn(  $ans ) ; } 
          &echo( "-n" ,   "Enter action (Read/Decide/...) [" . $Action 
                        ."]: " , "" ) ; 
          $ans=  ($_=scalar(<STDIN>), chop, $_) ; if ( !&eq(   $ans ,  '' ) 
                        ) { $Action= &fn(  $ans ) ; } 
          &echo( "-n" ,   "Relevant to [" . $To ."]: " , "" ) ; 
          $ans=  ($_=scalar(<STDIN>), chop, $_) ; if ( !&eq(   $ans ,  '' ) 
                        ) { $To= &fn(  $ans ) ; } 
          &echo( "-n" ,   "Update on memo's [None]: " , "" ) ; 
          $Other1=  ($_=scalar(<STDIN>), chop, $_) ; 
          &echo( "-n" ,   "Replaces memo's [None]: " , "" ) ; 
          $Other2=  ($_=scalar(<STDIN>), chop, $_) ; 
          &echo( "-n" ,   "Associated memo's [None]: " , "" ) ; 
          $Other=  ($_=scalar(<STDIN>), chop, $_) ; 
          &echo( "-n" ,   "Associated bug-reports [None]: " , "" ) ; 
          $Bugs=  ($_=scalar(<STDIN>), chop, $_) ; 
#
# Move file into the system
#
          if ( &peq( $File , "[Nn][Oo][Nn][Ee]" ) ) { 
            $File=  "none" ; 
            if ( ! &ft('e',  $server_dir ."/none" ) ) { 
              sub C2_t6_document { 
                local(*TMP); 
                open(TMP,">txt$$.tmp"); 
                print TMP  "<Title>No associated document</Title>" ."\n" ; 
                print TMP  '' ."\n" ; 
                print TMP  "<EM>This memo has no associated file.</EM>" 
                        ."\n" ; 
                print TMP  "<P>" ."\n" ; 
                print TMP  '' ."\n" ; 
                print TMP  "The text of the memo is in the memo header (" 
                        ."Summary)." ."\n" ; 
                print TMP  "Click \"Back\" to read it." ."\n" ; 
                close(TMP); 
                "txt$$.tmp";} 
              &cat( '' ,  &C2_t6_document , ''. &fn(  $server_dir ."/none" 
                        ) ) ; 
            } 
          } 
          else { 
            &cp( "-i" ,  &fn(  $File ) .' '. &fn(  $server_dir ) ) ; 
            &echo( "-n" ,   "Remove: " , "" ) ; &rm( "-i" ,  &fn(  $File ) 
                        ) ; 
            $File= &fn(  &fp('t', $File ) ) ; 
          } 
#
# Create header only if file moved into system
#
          if ( &ft('e',  $server_dir ."/" . $File ) ) { 
#
#  Create the Memo header and move any file into the system
#
            sub C2_t7_document { 
              local(*TMP); 
              open(TMP,">txt$$.tmp"); 
              print TMP  "<TITLE>Newstar Memo # " . $memo_id ."</TITLE>" 
                        ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "<H1>Newstar Memo # " . $memo_id ." </H1>" ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "<DT><STRONG>Document:</STRONG>         " . $File 
                        ."\n" ; 
              print TMP  "<DT><STRONG>Subject:</STRONG>          " . 
                        $Subject ."\n" ; 
              print TMP  "<DT><STRONG>Author:</STRONG>           " . 
                        $Author ."\n" ; 
              print TMP  "<DT><STRONG>Date:</STRONG>             " . 
                        $C_Date ."\n" ; 
              print TMP  "<P>" ."\n" ; 
              print TMP  "<DT><STRONG>Status:</STRONG>           " . 
                        $Status ."\n" ; 
              print TMP  "<DT><STRONG>Action:</STRONG>           " . 
                        $Action ."\n" ; 
              print TMP  "<P>" ."\n" ; 
              print TMP  "<DT><STRONG>To:</STRONG>               " . $To 
                        ."\n" ; 
              print TMP  "<P>" ."\n" ; 
              print TMP  "<H2>Text of the Memo: <A HREF=" . $File .">" . 
                        $File ."</A></H2>" ."\n" ; 
              print TMP  "<P>" ."\n" ; 
              print TMP  '' ."\n" ; 
              close(TMP); 
              "txt$$.tmp";} 
            &cat( '' ,  &C2_t7_document , ''. &fn(  $memo_file ) ) ; 
            if ( !&eq(   $Other1 ,  '' ) ) { 
              &echo( '' ,   " " , '>'. &fn(  $memo_file ) ) ; 
              &echo( '' ,   "<H3>Update on memo's</H3>" , '>'. &fn(  
                        $memo_file ) ) ; 
              &echo( '' ,   " " , '>'. &fn(  $memo_file ) ) ; 
              for $file__x (split(' ',join(' ' , &fn(  $Other1 ) ))) { 
                        $file=$file__x ; 
                $file= "n" . &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $file ) 
                        , "p$$.tmp01" ) , &awk( '' , '{printf "%4.4d",$1}' 
                        , "p$$.tmp01" , "p$$.tmp00" ) ) .".mem" ; 
                &echo( '' ,   "<DT><TT><A HREF=" . $file .">" . $file 
                        ."</A></TT> " , '>'. &fn(  $memo_file ) ) ; 
                if ( &ft('e',  $server_dir ."/" . $file ) ) { 
                  &grep( '' , &fn(  $subj ) ,  &fn(  $server_dir ."/" . 
                        $file ) , "p$$.tmp00" ) ; &sed( '' ,  "s^" . $subj 
                        ."^-^" , "p$$.tmp00" , '>'. &fn(  $memo_file ) ) ; 
                } 
                &echo( '' ,   " " , '>'. &fn(  $server_dir ."/" . $file ) ) 
                        ; 
                &echo( '' ,   "<P><STRONG>Updated in <A HREF=" . &fp('t', 
                        $memo_file ) .">Memo " . $memo_id ."</A></STRONG>" 
                        , '>'. &fn(  $server_dir ."/" . $file ) ) ; 
                &echo( '' ,   " " , '>'. &fn(  $server_dir ."/" . $file ) ) 
                        ; 
              } 
            } 
            if ( !&eq(   $Other2 ,  '' ) ) { 
              &echo( '' ,   " " , '>'. &fn(  $memo_file ) ) ; 
              &echo( '' ,   "<H3>Replaces memo's</H3>" , '>'. &fn(  
                        $memo_file ) ) ; 
              &echo( '' ,   " " , '>'. &fn(  $memo_file ) ) ; 
              for $file__x (split(' ',join(' ' , &fn(  $Other2 ) ))) { 
                        $file=$file__x ; 
                $file= "n" . &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $file ) 
                        , "p$$.tmp01" ) , &awk( '' , '{printf "%4.4d",$1}' 
                        , "p$$.tmp01" , "p$$.tmp00" ) ) .".mem" ; 
                &echo( '' ,   "<DT><TT><A HREF=" . $file .">" . $file 
                        ."</A></TT> " , '>'. &fn(  $memo_file ) ) ; 
                if ( &ft('e',  $server_dir ."/" . $file ) ) { 
                  &grep( '' , &fn(  $subj ) ,  &fn(  $server_dir ."/" . 
                        $file ) , "p$$.tmp00" ) ; &sed( '' ,  "s^" . $subj 
                        ."^-^" , "p$$.tmp00" , '>'. &fn(  $memo_file ) ) ; 
                } 
                &echo( '' ,   " " , '>'. &fn(  $server_dir ."/" . $file ) ) 
                        ; 
                &echo( '' ,   "<P><STRONG>Replaced by <A HREF=" . &fp('t', 
                        $memo_file ) .">Memo " . $memo_id ."</A></STRONG>" 
                        , '>'. &fn(  $server_dir ."/" . $file ) ) ; 
                &echo( '' ,   " " , '>'. &fn(  $server_dir ."/" . $file ) ) 
                        ; 
              } 
            } 
            &echo( '' ,   " " , '>'. &fn(  $memo_file ) ) ; 
            &echo( '' ,   "<H3>Associated Memo's</H3>" , '>'. &fn(  
                        $memo_file ) ) ; 
            &echo( '' ,   " " , '>'. &fn(  $memo_file ) ) ; 
            for $file__x (split(' ',join(' ' , &fn(  $Other ) ))) { 
                        $file=$file__x ; 
              $file= "n" . &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $file ) , 
                        "p$$.tmp01" ) , &awk( '' , '{printf "%4.4d",$1}' , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) .".mem" ; 
              &echo( '' ,   "<DT><TT><A HREF=" . $file .">" . $file 
                        ."</A></TT> " , '>'. &fn(  $memo_file ) ) ; 
              if ( &ft('e',  $server_dir ."/" . $file ) ) { 
                &grep( '' , &fn(  $subj ) ,  &fn(  $server_dir ."/" . $file 
                        ) , "p$$.tmp00" ) ; &sed( '' ,  "s^" . $subj ."^-^" 
                        , "p$$.tmp00" , '>'. &fn(  $memo_file ) ) ; 
              } 
            } 
            &echo( '' ,   "<DT><TT><A HREF=n????.mem> </A></TT> " , '>'. 
                        &fn(  $memo_file ) ) ; 
            &echo( '' ,   " " , '>'. &fn(  $memo_file ) ) ; 
            &echo( '' ,   "<H3>Associated bug-reports</H3>" , '>'. &fn(  
                        $memo_file ) ) ; 
            &echo( '' ,   " " , '>'. &fn(  $memo_file ) ) ; 
            for $file__x (split(' ',join(' ' , &fn(  $Bugs ) ))) { 
                        $file=$file__x ; 
              $file= "n" . &Pipe("p$$.tmp00", &echo( '' ,  &fn(  $file ) , 
                        "p$$.tmp01" ) , &awk( '' , '{printf "%4.4d",$1}' , 
                        "p$$.tmp01" , "p$$.tmp00" ) ) .".prj" ; 
              &echo( '' ,   "<DT><TT><A HREF=../bug/" . $file .">" . $file 
                        ."</A></TT> " , '>'. &fn(  $memo_file ) ) ; 
              if ( &ft('e',  $n_root ."/server/bug/" . $file ) ) { 
                &grep( '' , &fn(  $subj ) ,  &fn(  $n_root ."/server/bug/" 
                        . $file ) , "p$$.tmp00" ) ; &sed( '' ,  "s^" . 
                        $subj ."^-^" , "p$$.tmp00" , '>'. &fn(  $memo_file 
                        ) ) ; 
              } 
            } 
            &echo( '' ,   "<DT><TT><A HREF=../bug/n????.prj> </A></TT> " , 
                        '>'. &fn(  $memo_file ) ) ; 
            sub C2_t8_document { 
              local(*TMP); 
              open(TMP,">txt$$.tmp"); 
              print TMP  "<H3>-------------------</H3>" ."\n" ; 
              print TMP  '' ."\n" ; 
              print TMP  "<P>" ."\n" ; 
              print TMP  "<H2>Summary</H2>" ."\n" ; 
              print TMP  "<PRE>" ."\n" ; 
              print TMP  "<EM>To be filled in...</EM>" ."\n" ; 
              print TMP  "</PRE>" ."\n" ; 
              close(TMP); 
              "txt$$.tmp";} 
            &cat( '' ,  &C2_t8_document , '>'. &fn(  $memo_file ) ) ; 
            &chmod( "a+r" ,  &fn(  $memo_file ) ) ;    # All may read
            &chmod( "g+w" ,  &fn(  $memo_file ) ) ;    # Group members may write
          } 
          else { 
            $memo_file=  '' ; 
          }                                            # If file moved into system
        }                                              # If new memo
#
# Edit the memo file and associated text
#
        if ( !&eq( $memo_file ,  '' ) ) { 
          if ( !&eq(   $File ,  '' ) ) { $File= &fn(  $server_dir ."/" . 
                        $File ) ; } 
          &dollar("EDITOR" ,  &fn(  $memo_file ) .' '. &fn(  $File ) , "" ) 
                        ; 
        } 
#
#  Update the index
#
        $Idx= &fn(  $server_dir ."/memo.idx" ) ; 
        &echo( '' ,   "Building index: " . $Idx , "" ) ; 
        sub C2_t9_document { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  "<TITLE>Newstar Memo System: Master Index</TI" ."TLE>" 
                        ."\n" ; 
          print TMP  "<H1>Index of all Newstar Memo's</H1>" ."\n" ; 
          print TMP  '' ."\n" ; 
          print TMP  "<FORM ACTION=\"/htbin/nsmemo\" METHOD=GET>" ."\n" ; 
          print TMP  "<HR>" ."\n" ; 
          print TMP  "To search for all memos containing some stri" 
                        ."ng in their header, <BR>" ."\n" ; 
          print TMP  "enter the text (or -memonumber) here" ."\n" ; 
          print TMP  "and hit return <INPUT TYPE=\"Text\" NAME=\"i" 
                        ."sindex\">" ."\n" ; 
          print TMP  "<HR>" ."\n" ; 
          print TMP  "</FORM><P>" ."\n" ; 
          print TMP  " " ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &cat( '' ,  &C2_t9_document , ''. &fn(  $Idx ) ) ; 
        for $File__x (split(' ',join(' ' , &fn(  $server_dir ."/n*.mem" ) 
                        ))) { $File=$File__x ; 
          &echo( '' ,   "<DT><TT><A HREF=" . &fp('t', $File ) .">" . 
                        &fp('t', $File ) ."</A></TT>" , '>'. &fn(  $Idx ) ) 
                        ; 
          &grep( '' , &fn(  $subj ) ,  &fn(  $File ) , "p$$.tmp00" ) ; 
                        &sed( '' ,  "s^" . $subj ."^-^" , "p$$.tmp00" , 
                        '>'. &fn(  $Idx ) ) ; 
        } 
      }                                                # if server directory
#
#  %Extract command
#
    } 
    elsif ( &peq(   $Command , "[Ee]*" ) ) { 
      $server_dir= &fn(  $n_root ."/server/newstar/extract" ) ; 
      if ( ! &ft('d', $server_dir ) ) { 
        &echo( '' ,   " " , "" ) ; 
        &echo( '' ,   "You do not have a directory for extracted so" 
                        ."urces here..." , "" ) ; 
        &echo( '' ,   "This directory should be named " . $server_dir , "" 
                        ) ; 
        &echo( '' ,   " " , "" ) ; 
      } 
      else { 
        if ( &eq(   $Files ,  '' ) ) { 
          &echo( "-n" ,   "Enter the name of the source file(s): " , "" ) ; 
          $Files=  ($_=scalar(<STDIN>), chop, $_) ; 
          $Files= &fn(  $Files ) ; 
        } 
        if ( !&eq(   $Files ,  '' ) ) { 
          if ( &peq(   $Files , "[Aa][Ll][Ll]" ) ) { $Files= &fn(  $n_src 
                        ."/sys/*.csh" ) .' '. &fn(  $n_src ."/sys/*.c" ) ; 
                        } 
          for $File__x (split(' ',join(' ' , &fn(  $Files ) ))) { 
                        $File=$File__x ; 
            $Outfile= &fn(  $server_dir ."/" . &fp('t', $File ) ) ; 
            &doexe( &fn(  $n_exe ."/docaid.exe" ) ,  "extract" .' '. &fn(  
                        $File ) , ''. &fn(  $Outfile ) ) ; 
            if ( ! &ft('e', $Outfile ) || &ft('z', $Outfile ) ) { 
              if ( &ft('e', $Outfile ) ) { 
                &rm( "-f" ,  &fn(  $Outfile ) ) ; 
              } 
              &echo( '' ,   "No extractable documentation in " . $File , "" 
                        ) ; 
            } 
            else { 
              &echo( '' ,   "Extracted documentation from " . $File , "" ) 
                        ; 
            } 
          } 
        } 
#
#  Update the index
#
        $Idx= &fn(  $server_dir ."/../index_doc.html" ) ; 
        &echo( '' ,   "Building index: " . $Idx , "" ) ; 
        sub C2_t10_document { 
          local(*TMP); 
          open(TMP,">txt$$.tmp"); 
          print TMP  "<TITLE>Index of extracted documentation</TIT" ."LE>" 
                        ."\n" ; 
          print TMP  "<UL>" ."\n" ; 
          close(TMP); 
          "txt$$.tmp";} 
        &cat( '' ,  &C2_t10_document , ''. &fn(  $Idx ) ) ; 
        for $File__x (split(' ',join(' ' , &fn(  $server_dir ."/*" ) ))) { 
                        $File=$File__x ; 
          &echo( '' ,   "<LI> <A HREF=/htbin/nview/extract/" . &fp('t', 
                        $File ) .">" . &fp('t', $File ) ."</A>" , '>'. &fn( 
                         $Idx ) ) ; 
        } 
        &echo( '' ,   "</UL>" , '>'. &fn(  $Idx ) ) ; 
      }                                                # if directory exists
#
#
#  Invalid command
#
    }                                                  # Other command
    else { 
      &echo( '' ,   '' , "" ) ; 
      &echo( '' ,   "Error: Invalid or ambiguous command " . $Command , "" 
                        ) ; 
      &echo( '' ,   '' , "" ) ; 
    }                                                  # End of if (Command == ...)
  }                                                    # End of while (Menu mode)
  &Abort_exit_document ; 
  sub  Abort_exit_document { 
    ; 
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
eval('&document__pls');
1;
#-
