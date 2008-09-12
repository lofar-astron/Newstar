#  doc_cook.csh - sourced by document.csh
#
#	JPH 940322	Split off from document.csh
#	JPH 940707	(Transfer to master system)
#	JPH 940712	<program>_<KEWORD>.html in <program> subdirectory
#	CMV 940720	Change newstar_home_page to homepage
#	JPH 940720	Typos. - Reactivate trailing blank lines in .html 
#			 file.#			Fix typo in file selection
#			Use symbol l2hdir to locate all latex2html components
#	JPH 940725	Correct insertion of navigation line at end of document
#	JPH 940810	Keyword help files recognised by __ i.s.o. 
#			 uppercase. Cope with split lines and with 
#			 \textrefs including text in braces within their 
#			 first argument
#	JPH 940816	\keyref
#	JPH 940818	split off doc_preprocess.csh; Name --> File
#	JPH 940914	remove old .html file 
#			Make insertion of % at \begin/\end{figure} more specific
#	JPH 940916	Shift common code ro doc_preprocess; accept .cap files
#	JPH 941028	No \keyref. - Handle blanks between \textref arguments
#			Retain leading dots in labels and references
#	JPH 941104	Fix internal references. - Translate verbatim into
#			 rawhtml + <PRE>
#	JPH 941115	Shift \label in figure environment to top, merge code
#			 with that for shifting section labels
#	JPH 941117	Insert control-H behind < so xmosaic will not think it
#			 sees a command.
#			Fix bug in directory insertion for label-less \textrefs
#			 (\. --> \.*)
#	JPH 941121	Move \textref argt delimiting to doc_preprocess
#	JPH 950208	Reject .psc-type files
#	JPH 950222      Fix processing of ../<file> references (were 
#			interpreted as .<label>)
#	HjV 950613	Use latex2html stuff from $n_l2h iso. ~jph
#	JPH 950823	Replace interdocument referencing code by simpler code
#			 that relies on l2h mechanism
#			Use improved \xxxref parsing by doc_preprocess
#	JPH 950927	Srcref --> [Ss]rcref. - Figure labels above FIGURE
#	JPH 951004	Remove old .xbm files from target directory
#	JPH 951013	Reject all but .tex files
#	JPH 951127	Reject untranslatable files (lsq.tex)
#	JPH 960131	Bug fix: missing end on foreach
#	JPH 960206	Correct processing of non-translatable files
#			 \fig command: .ps reference --> /fig/.ps
#	JPH 960326	-e option
#	JPH 960415	Bug fix: Do not insert control-Z in rawhtml sections
#	JPH 960507	Bug fix: Do not remove .xbm files (951004), use a better
#			 method to clear out obsolete ones; also check that all
#			 required files exist
#			Pipe 'yes' into l2h for 'rm' confirmation
#	JPH 960513	Remove $Tmp.*
#
#
# List of files that cannot be processed
#
	  set donotcook = " lsq "

# Check write access to target help directory tree

##set echo
	touch >&/dev/null $n_hlp/$$.tmp
	if ($status) then
	  echo "FATAL	No write access to target directory $n_hlp"
	  exit -1
	endif
	rm -f >&/dev/null $n_hlp/$$.tmp

	if (x$Files[1] =~ x-[Ee]*) then
	  set echo
	  shift Files
	endif

      	if ("$Files" == "") then
          echo -n "Enter the name of LaTeX file(s) [All]: "
          set Files=( $< )
          set Files=( $Files )
	endif
	if ( "$Files" =~ [Aa][Ll][Ll] ) then
	  cd $n_doc/latex
	  set here=$cwd
	  set Files = "*.tex"
       	endif
#
# Process all files requested, eliminating stray _tmp.tex files
#
	foreach File ( $Files )
	  if ($File =~ *_tmp.*) continue
	  if ($File:e != tex) continue
	  echo -n "	ndoc cook  $File"
          set File=$File:t; set ext = $File:e; set name=$File:r
	  if (! -e $File) then
	    echo "	 - not found"
	    continue
	  else if ($ext =~ p??) then
	    echo "	 - bad command: Use 'ndoc Key' for .psc-type file"
	    continue
	  endif
	  set Name = $File:r
#
# Check for files that cannot be translated
#
	  if ("$donotcook" =~ *' '$Name' '* ) then
 	    echo '	 - not translatable: using $n_hlp/'"$Name.ps"
	    rm -f >&/dev/null $n_hlp/src/doc/bin/$Name.ps
	    ln -s $n_hlp/$Name.ps $n_hlp/src/doc/bin/$Name.ps
	    continue
	  endif
#
# Use existing .html file if it is newer than the source. Environment variable
#  n_force may be set to bypass this test 
#
	  $n_exe/newerfile.exe $n_hlp/$Name/$Name.html $File
	  if (! $?n_force && $status == 1) then
	    echo "	 - output.html file is up-to-date"
	    continue
	  endif
	  echo ""
#
# Preprocessing common to Cook and Print: Input $File, output $Tmp.0
#
	  source $n_src/sys/doc_preprocess.csh
##goto 1
#
# Create first part of temporary .tex file 
#
 	  cat << END >! $Tmp.tex
	    \documentstyle{article} \begin{document}
	    \input {$n_l2h/html.sty}
	    \input {$n_doc/latex/hb_cook_preamble.sty}
	    \input {$n_doc/latex/hb_symbols.sty}
END
#
# Complete preprocessing 
#  [remove "."s from references because latex2html does the same for labels
#    (This is not necessary if l2h leaves nonalphanumeric characters in labels;
#     This mod (consisting of removing all the lines with 's/\W//g') has been 
#     made in the jph version and proposed to the provider of l2h, 940526.) ]
# The result is appended to the temporary .tex file
#
	  sed < $Tmp.0 \
# Complete references. @[4-7]@ delimits '{' + file name, @3@: marks the label \
#  or file extension. We must process the latter before the labels \
# \
#	srcref including file extension \
	    -e 's:@7@{\([^@]*\)@3@:@7@{../src/\1:g' \
#	all other . in references delimit labels \
	    -e 's/@3@\./@3@\#./g' \
#	txtref, ascref, psref \
	    -e 's:@4@{\([^@][^@]*\)@3@:@4@{../\1/\1.html@3@:g' \
 	    -e 's:@5@{\([^@]*\)@3@:@5@{../src/doc/txt/\1.txt@3@:g' \
	    -e 's:@6@{\([^@]*\)@3@:@6@{../src/doc/bin/\1.ps@3@:g' \
#	add {} around text arguments \
	    -e 's:@1@\([^@]*\):{\1}:g' \
	    -e 's:@[0-9]@::g' \
# \
# Protect '<' signs by adding a non-printing character, \
#  so xmosaic will not mistake them for commands \
 	    -e '/\\begin{rawhtml}/,/\\end{rawhtml}/\!s:<:<:g' \
 # \
# Interchange \label with the section heading to which it refers, so xmosaic \
#  will jump to the proper place. \
# Place label in figure environment at the beginning \
# Append dummy lines at end to insure that when \
#  the user follows a link, the target will always be displayed at the top of \
#  the screen. \
# \
  	  | nawk -F'	' 	# dummy record separator to avoid overflows \
	   'BEGIN{ lbl=0; n=0;} \
 	    /\\label/{ lbl=1; print $0; next } \
	    { if (lbl ){ \
	        for (i=1; i<=n; i++) print s[i]; n=0; lbl=0; \
	    } } \
  	    /\\begin{figure}/{ s[1]=$0; n=1; next } \
	    /\\section/{ s[1]=$0; n=1; next} \
	    /\\subsection/{ s[1]=$0; n=1; next} \
	    /\\subsubsection/{ s[1]=$0; n=1; next} \
  	    { if (n){ \
		if (! NF){ \
		  next; \
	        }else{ \
		  n++; s[n]=$0; \
	          if (n >5) lbl=1; \
 		  next; \
		} \
	      }else{ \
	    	print $0; \
 	    } } \
	    END { \
	      print "\\begin{rawhtml}"; \
	      for (i=0; i<20; i++){ print "<P>.\n"; } \
	      print "</BODY> </HTML>\\end{rawhtml} "; \
	    }' \
#\
# Insert FIGURE anchors, captions, to avoid l2h limitations \
	  | sed \
 	    -e 's:^[^%]*\\fig *{\([^}]*\)}:\\begin{rawhtml}<A HREF="../fig/\1.ps"><STRONG>FIGURE</STRONG></A>\\end{rawhtml}:' \
	    -e 's:^[^%]*\\ps *{\([^}]*\)}:\\begin{rawhtml}<A HREF="../fig/\1.ps"><STRONG>FIGURE</STRONG></A>\\end{rawhtml}:' \
	    -e 's:^ *\\begin *{ *figure:%&:' -e 's:^ *\\end *{ *figure:%&:' \
	    -e 's:\\caption[ \[\]]*:\\ :' \
# .contents label \
	    -e '/\\tableofcontents/i\\
\\label{.contents}' \
# Boldface document and section titles \
 	    -e 's:\\chapter *{:{\\Large\\bf :' \
	    -e 's:\\[sub]*section[ {]*:&\\bf :' \
# Verbatim environment --> rawhtml. This is because l2h uses a large font for \
#  this environment and we want to use <PRE> instead. \
	    -e 's: *\\begin *{verbatim}:\\begin{rawhtml} <PRE>:' \
	    -e 's:\\end{verbatim}:</PRE> \\end{rawhtml}:' \
	  >> $Tmp.tex
	  echo "\end{document}" >> $Tmp.tex
#
# Convert to .html file, filtering clutter from message output
# latex2html has some hangups about directories, so we run it from $n_hlp, 
#  using the input file name to insure that we get proper output names.
#
# NOTE: Be careful with changing latex2html command-line options since these may
#  have unwanted side effects. For instance, changing to "-info 0" resulted in
#  \subsubsections being shown as \subsections in the Contents list
#
	  cp $Tmp.tex $n_hlp/$name.tex
	  pushd $n_hlp >&/dev/null
	  rm -f >&/dev/null $name/$name.html ## $name/*.xbm

	  echo "yes"		# just in case 'rm' confirmation needed \
	  | $n_l2h/latex2html -reuse -allbitmaps -info "" -dir $n_hlp \
	    -split 0 -link 0 -address "newstar@astron.nl" \
	    -init_file $n_doc/latex2html.pls \
	    $name.tex \
	  |& sed \
	    -e '/^This is /d' -e '/^Computer/d' -e '/^OPENING /d' \
	    -e '/% --- Checking /d' \
	    -e '/^No string /d' -e '/^ *Cannot create/d' -e '/^ *$/d' \
	    -e '/^ *All rights /d' -e '/ NO WARRANTY/d' -e '/Copyright/d' \
	    -e '/^Aladdin Ghostscript/d' -e '/^Distributed /d'\
	    -e '/ cropping /d' -e '/^Done/d' -e '/^Reusing /d' \
	    -e '/\.\.\./d' -e '/^GS>GS>/d' -e '/^LaTeX/d' \
	    -e '/^\*\*\* No address/d'
	  if ($status) then
	    echo "FATAL"
	    cat $n_hlp/$name/*_images.log
	  endif
	  rm -f >&/dev/null $name.tex
#
# Replace escape commas in the .html file.
#
1:
	  cd $n_hlp/$name
	  sed < $name.html \
	     -e 's:,,\([A-Za-z]\):\\\1:g' \
 	  >! $name.tmp
	  rm -f $name.html; mv $name.tmp $name.html
#
# Clean up: Find all references to icons and the like, move them to a temporary #  directrory and delete what is left: These must be obsolete
#
##set echo
	  mkdir >&/dev/null .tmp
	  rm -f >&/dev/null .tmp/*
 	  cat images.pl $name.html \
	  | awk -F'"' \
	     '{ for (i=1; i<=NF; i++){ \
	          if ($i ~ /SRC=/ ){ \
		    i++; print $i; \
	      } } }' \
 	  | sort -u \
	  | tee $Tmp.all \
	  | sed \
	    -e '/\.\./d' \
	  >! $Tmp.x
	  if (! -z $Tmp.x ) then
	    mv `cat $Tmp.x` .tmp
 	    rm -f >&/dev/null *.xbm
	    mv .tmp/* .
	  endif
#
# Check availability of all required icons etc.
#
	  foreach file ( `cat $Tmp.all` )
	    set file0 = $n_hlp/$name/$file
  	    if (! -e $file0) echo \
		"	Missing file $file" 
	  end
	  rm -f $Tmp.*
	  popd >&/dev/null
	  rm -f >&/dev/null $Tmp.*; rm -f $name.ps

	end # file loop
