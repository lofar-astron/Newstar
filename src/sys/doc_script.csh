# docScript.csh - sourced from document.csh

       	set Print=0
       	if ($Files[1] =~ -*) then
          if ($Files[1] =~ -[Pp]) set Print=1
          if ($Files[1] =~ -[Vv]) set Print=2
          set Files[1]=""; if ($#Files > 1) shift Files
       	endif

       	set File=$Files[1];
       	if ("$File" == "") then
          echo -n "Enter the name for the output LaTeX file: "
          set File=( $< )       # Read from stdin
       	endif
       	if ("$File:e" != "") set File=$File:r
	if (-e $File.tex) then
	  echo "FATAL:	File $File.tex already exists"
	  exit 1
	endif
#
# Start script utility, this will leave you in a subshell
#
	cat << END
A subshell will be started for logging your terminal dialogue
You must initialise this subshell by typing
		'\$go'
After that, proceed with the session that you want to record. 
		***** DO NOT TYPE AHEAD! *****
Then terminate your session with
		'exit'
END
	cat << END >! docScript.tmp
	  source $n_src/sys/newstar_${n_site}.csh
	  dwspecify dwarf /nomenu << END1
	    bell=on"
END1
	  unalias nscript
	    set prompt = "scr> "
END
	setenv go 'source docScript.tmp'
        script $File.tmp
        unsetenv n_script go
	$n_exe/doc_script.exe $File.tmp >! $File.tex
        echo "Output in $File.tex"

#	rm $File.tmp
	exit

	
