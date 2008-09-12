#! /bin/csh -f
if (! $?dwrec) then	# this is a kluge that seems to work around the
  setenv dwrec 1	#  csh version in some HP systems that refuses to read 
  csh -f $0 $argv	#  a .csh files to its end
  exit
endif
goto 000

dwrecord.csh - record/replay program run

options (first argument):
	r	replay with parameter script $1.$ext
   	m	run and record program interactively
 	n	(re)number existing $1.$ext
 
arguments
	$1	program name with extension

	The parameter script <program>.pst contains lines of the form

			<keyword>=<value list>.

The lines are indented to show where the program reverts to a previous keyword. 
 Hidden keywords (those that the .psc/.psf file defines with /NOASK) are marked with two leading exclamation marks. 

	dwrecord pipes the script to the program and catches the output in a file <program>_psc.log. For the -m option, the question marks are first filtered out.

	You may manually insert a number of shell commands at the start of the script to initialise, e.g. by cleaning out files that might be in the way. Terminate the script with 'exit'. psc_test will execute the script lines and filter them out before piping the parameter values to the program.
-
    JPH 951113	Adapt from private script 'pst'
    JPH 9606..  Remove synchronous post-processing to capture errors
    JPH 960719  Compress whitespace around ! - Remove logging - Remove backtrack
		 and help-request logic. - Better line numbering
    JPH 960807	Copy user's DWARF symbols to temp. symbol file; use uppercase
		 name because DWARF does not know lower-case names
    JPH 960815	Inhibit interrupts during pipe execution
		Log $tmp.log --> $1.log
    JPH 961017	Bell off i.s.o. on (batch_sync uses line terminator for prompt
		 recognition.)
    JPH 961018	Try $n_uexe for batch_<xxx>
    JPH 961107	onintr - : Otherwise crashes on NPLOT
		Bell ON for record, OFF for replay
    JPH 961212	Workaround for faulty HP csh
		Refine code for selecting $n_uexe/batch_<xxx>.exe
    JPH 9612..	Kluge for HP csh bug
    JPH 961218	Add exit to kluge
    JPH 970204	On control-C exit do kill -INT $$  


000:
##set echo
 	if ($n_arch == hp) alias makenode 'mkfifo node'
	if ($n_arch == sw) alias makenode 'mknod node p'

	pushd >&/dev/null $n_exe
	if ($?n_uexe) then
	  set uexe = $n_uexe
	else
	  set uexe = $n_exe/jph
	endif
 	if (-e $uexe) then
 	  foreach f (batch_*.exe)
	    set $f:r = $n_exe/$f
	    if (-e $uexe/$f) set $f:r = $uexe/$f
	  end
	endif
 	popd >&/dev/null
 
	cp $DWARF_SYMBOLS DWRSYMBOLS.TMP
	setenv DWARF_SYMBOLS DWRSYMBOLS.TMP
	
	onintr cleanup
 	if ("$argv[1]" =~ [mncr]) then
	  set mode = $argv[1]
	  shift argv
	endif 
 	set ext = $argv[1]:e
 	if ($ext =="") then
	  echo \
"FATAL:	argument must have a file extension: <program>.<extension>"
	  exit -1
	endif
	set pr = $argv[1]:r
	set PR = \
`echo $pr | sed -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/' `
 	set log = $pr.$ext.log
	set tmp = $pr.$ext.tmp
	rm -f >&/dev/null $tmp.*
##set echo; set verbose 
	if ($mode == m) then

# temporarily set dwarf bell on

	  $n_exe/specify.exe dwarf/nomenu << END >! /dev/null
	    bell = on

END
 
# make new .$ext file

	  if (-e $pr.$ext) then
	    echo -n "	$pr.$ext exists. Delete? "
	    set x = $<
	    if ($x !~ [Yy]*) exit
 	  endif
	  echo -n "" >! $pr.$ext
 
# execute program run

 	  setenv N_PSCTEST /dev/null
	  onintr -
 	  $n_exe/execute.exe $pr | $batch_log >! $tmp.
  	endif

 	if ($mode =~ [cm]) then

## convert log file to record file, making level indents

	  sed < $tmp. \
	    -e 's:[A-Za-z0-9_$]* *\! *?:/ASK:' \
	  | awk -F'=' \
	   'BEGIN{ n=0; \
bl="                                                                    ";} \
	    /\!\!/{ print $0; next; } \
	    { echo $0; f=0; for (i=1; i<=np; i++ ){ \
		if (i <=np && pl[i] == $1){ np=i; f=1; } \
	      } \
	      if (! f ){ np++; pl[np]= $1; } \
	      printf("%s%s\n", substr (bl,1,2*(np-1)), $0); \
	    }' \
	  >> $pr.$ext
	endif
 
	if ($mode =~ [mn]) then

# Renumber record file: Remove old numbers first

	  sed < $pr.$ext \
	    -e 's: *\![ \!]*[0-9 ]*$: \!:' \
	  >! $tmp.
	  nawk -F'\!' < $tmp. \
	   '{ sep= " "; if (NF==1 ) sep = "\!"; \
 	      printf ("%s %s %d\n", $0, sep, NR); \
 	    }' \
	  >! $pr.$ext
	  exit
	endif

	if ($mode == r) then
 	  
# temporarily set dwarf bell off

	  $n_exe/specify.exe dwarf/nomenu << END |& grep -v 'being taken from'
	    bell = off

END
	  
## Remove csh script and empty lines, remove blanks around commas and excl. 
##  marks

	  expand < $pr.$ext \
	  | sed \
 	    -e '/.*\!\!/d' \
	    -e '/^ *$/d' \
	    -e 's: *, *:,:g' \
	    -e 's: *\! *: ! :g' \
	  >! $tmp.
  
# Execute program with input from $tmp.
# Collect output in _psc.log file, with a branch to awk for immediate 
#  inspection. When awk finds an error report it exits, which will stop the
#  entire pipeline.
#
##set echo
	  onintr -
	  rm node >&/dev/null
	  makenode
   	    setenv N_PSCTEST node
	    $batch_ask $tmp. $N_PSCTEST \
  	    | $n_exe/execute.exe $pr \
	    | $batch_sync \
 	    >! $pr.$ext.log
 	  onintr cleanup
    	endif
#
# Clean up
#
 	rm $tmp.*
cleanup:	
	rm >&/dev/null node $DWARF_SYMBOLS
##	onintr
##	kill -INT $$
