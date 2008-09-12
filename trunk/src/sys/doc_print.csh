#! /bin/csh -f
goto 000

doc_print.csh - to be sourced by document.csh

History:
	JPH 940628	Fix detection of \fig commands
	JPH 940719	Remove <ESC> characters AFTER all include.exe runs
	JPH 940810	Cope with split lines and with \textrefs including text
			 in braces with their first argument 
	JPH 940816	\keyref
			Split off doc_preprocess.csh
			Suppress warings in LaTeX dry run
			Some more common code to doc_preprocess; make it accept
			 .cap input
	JPH 940919	Rename $Tmp.log/.tex in case of error
	JPH 941116	Correct name of outpuf file. (Was <xxx>.tex.ps.)
	JPH 941121	Use @+/@- \textref argt delimiters set by doc_preprocess
			Remove lingering \keyref processing
	JPH 911123	Suppress rawhtml sections
	JPH 941124	2nd version of $Tmp.pre outside conditional
	JPH 941128	Correct \chapter processing
	JPH 941129	Label list in reference section
	JPH 941130	Correct label-list code for case there are no labels
	JPH 941207	n_noref environment variable for private us
	JPH 950208	reject .psc-type files
	JPH 950215	fix processing of ../ references (were mistaken for 
			 label references)
	JPH 950221	temporarily comment out [subeqn]
	HjV 950613	Use latex2html stuff from $n_l2h iso. ~jph
	JPH 950822	Check for existing output file
	JPH 950823	Replace interdocument referencing code by simpler code
			 that relies on l2h mechanism
			Use improved \xxxref parsing by doc_preprocess
	JPH 951013	Reject all but .tex files
	JPH 951016	Accept non-.tex files but output in current directory
			Message if $n_hlp, write-protected
			Force figure positioning to [hbt]
			Make -v default --> always call dvips
			Suppress verbose dvips output
			Format output messages in columns
	JPH 951106	Change order of input-file tests
	JPH 960102	Use $\backslash$ to generate backslash
	JPH 960206	$Tmp --> $Tmp.all so it will be included in final 
			 deletion
	JPH 960208	.fps --> /fig/.ps
	JPH 960326	-e option
	JPH 960426	Fix option processing
000:
# Process inout, initialise
##set echo
	if ($?n_noref) then
	  cat <<END

	Environment variable n_noref set:
	No interdocument refererences will be created

END
	endif
	set Print = 2
	@ sts = 0
       	if (x$Files[1] =~ x-*) then
          if (x$Files[1] =~ x-[Pp]) set Print=1
          if (x$Files[1] =~ x-[Vv]) set Print=2
	  if (x$Files[1] =~ x-[Ss]) set Print=3
	  if (x$Files[1] =~ x-[Ee]) set echo
##          set Files[1]=""; if ($#Files > 1) 
	  shift Files
       	endif

       	if ("$Files" == "") then
          echo -n "Enter the name of the LaTeX file to print: "
     	  set Files=( $< )       # Read from stdin
       	endif

	foreach File ($Files)
	  if ($File =~ *_tmp*) continue

	  echo $File \
	  | awk '{ printf ("	ndoc print %-24s", $1) }'
 	  if (! -e $File) then
	    echo " - not found"
	    continue #break
	  endif
	  if ($File:e =~ p??) then
	    echo " - Bad command: Use 'ndoc Key' for .psc-type file"
	    continue #break
	  endif
#
# Use existing .ps file if it is newer than the source. Environment variable
#  n_force may be set to bypass this test 
#
	  set Name = $File:r
 	  set Target = $Name.ps
	  set target = $Target
	  set local
	  if ($File:e != tex) then
	    echo -n "Not .tex: "
	  else if (-w $n_hlp) then
	    set Target = $n_hlp/$Target
	    set target = '$n_hlp/'$target
	    unset local
	  else
 	    echo -n '$n_hlp read-only: '
	  endif 
	  if ($?local) then
	    echo -n "local output."
	  endif
 	  $n_exe/newerfile.exe $Target $File
 	  if (! $?n_force && $sts == 1) then
	    echo "- output is up-to-date"
 	    goto display
	  endif	  
	  echo ""
#
# Preprocessing common to Cook and Print: $File --> $Tmp.0
#
##set echo
	  source $n_src/sys/doc_preprocess.csh
#
# Process reference commands parsed by doc_preprocess
#
	  sed < $Tmp.0 \
#	srcref \
	    -e 's+@7@{\([^@]*\)@3@+@7@{\\$n\\_src/\1+g' \
#	internal textref \
	   -e 's:@0@[^@]*@1@{\([^@]*\)}@4@{@3@\([^}]*}\):\1 (sec. \\ref{\2):g'\
#	textref with optional label, ascref, psref \
	    -e 's:@4@{\([^@]*\)@3@:@4@{\\$n\\_hlp/\1.ps@3@:g' \
 	    -e 's/@3@\./:./g' \
	    -e 's:@5@{\([^@]*\)@3@:@5@{\\$n\\_doc/txt/\1.txt:g' \
	    -e 's:@6@{\([^@]*\)@3@:@6@{\\$n\\_hlp/\1.ps:g' \
#	replace all xxxref by html command to avoid problems with # sign \
	    -e 's:@0@[^@]*@1@\([^@]*\)@[4-9]@:\\htmladdnormallink{\1}:g' \
	    -e 's:@[0-9]@::g' \
	    -e '/\\begin *{ *rawhtml *}/,/\\end *{ *rawhtml *}/d' \
	  >! $Tmp.text
	  set Inp = $Tmp.input
# 
# Create a general preamble with \inputs where the dry run differs from the
#  later wet run
#
	  cat << END >! $Tmp.rack
%%	    \documentstyle[subeqn]{article}
	    \documentstyle{article}
	    \newcommand{\iinput}[1]{ \input{#1} }
 	    \input $n_doc/latex/hb_print_preamble.sty
	    \input $n_doc/latex/hb_symbols.sty
	    \input $n_l2h/html.sty
	    \iinput{epsf.sty}
	    \newcommand{\fig}[1]{
	      \centering 
	      \leavevmode 
	      \epsfbox{$n_hlp/fig/#1.ps}
	    }
 	    \begin{document}
END
	  if (! $?n_noref && $Ext != cap) then
	    cat << END >> $Tmp.rack
	      {\it Printout of NEWSTAR document chapter - \today }
	      \\\\ \\\\
END
	  endif
	  cat << END >> $Tmp.rack
\input $Tmp.text
\end{document}
END
#
# Execute all \input and remove the ",," escape sequences that may still be 
#  there
# Standardise figure placement to [hbt] and caption entry to []
#
	  $n_exe/include.exe $Tmp.rack $Tmp.all
	  sed < $Tmp.all \
	    -e 's:,,\([A-Za-z]\):$\\backslash$\1:g' \
	    -e '/\\tableofcontents/i\\
\\addtocontents{toc}{ \\setlength{\\parskip}{2pt}}' \
	    -e '/\\tableofcontents/a\\
\\vspace{1cm}' \
	    -e 's:\\begin *{ *figure *} *.[hbtp]*.:\\begin{figure}:' \
	    -e 's:\\begin{figure}:&[hbt]:' \
	    -e 's:\\caption\[\.\]:\\caption[]:' \
 	  >! $Tmp.tex

#
# Run LaTeX twice. The first time we ignore all errors. The second time
#  - we pick up relevant messages from the message stream
#  - we look for the error prompt with exit reply: '? X' in the log file
#  If either are found we report as much as we can pick up
#
 	  echo X | (latex $Tmp.tex) >&/dev/null
	  echo X \
	  | (latex $Tmp.tex) \
	  | sed -n \
	    -e "/^LaTeX Warning: Label/d" \
	    -e "s/^LaTeX Warning: /WARN /p" \
	    -e '/^\!/p' \
	    -e "s/^No file/ERR  No file /w $Tmp.err"
	  grep '^? X$' < $Tmp.log >&/dev/null
	  if (! $status || ! -z $Tmp.err) then
	    cat $Tmp.err
	    set lnr = `sed < $Tmp.log -n -e 's:^l\.\([0-9][0-9]*\).*:\1:p' `
	    if ($lnr != "") then
	      echo "ERROR	line ${lnr}:"
	      awk	< $Tmp.tex '\
	        {d=NR-lnr; if (d<3 && d>-3) {print NR " " $0};} {next}' lnr=$lnr
	    endif
	    cp $Tmp.log $File.log
	    cp $Tmp.tex $File.tmp
	    echo "     see $File.log and $File.tmp for details"
	    @ sts = 1
	    continue
	  endif
 
	  rm >&/dev/null $Target
	  dvips -o $Target $Tmp.dvi \
	  |& sed -e '/Copyright/d' -e '/TeX output/d' \
		 -e '/<texc.pro>/d' -e '/\.ps/d' 
	  rm $Tmp.*
display:
	  if (-e $Target) then
            if ($Print == 2) then
	      echo 'Starting ghostview'
	      ghostview -a4 -nocenter -magstep -1 $Target
	    else if ($Print == 1) then
              echo "Being submitted to printer"
              $n_src/sys/wngfex.csh PS $Target
	    endif
	  endif
	end # file loop

	exit ($sts)
