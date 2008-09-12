# doc_test.csh - test documentation subsystem integrity
# sourced by ndoc test

# NOTE: if/then constructs are used where an in-line if would seem to suffice.
#	This proved to be necessary to make the alias for echo work



#	JPH 951102
#	JPH 951108	Add source copying
#	JPH 951113	Bug fixes
#	JPH 960103	Fix checking of $n_hlp/.ps files
#	JPH 960206	$n_hlp/.fps --> $n_hlp/fig/.ps
#	JPH 960208	Bug fix. Copy doc/html/*.html
#	JPH 960325	Proper handling of .html files.
#			Add icons directory
#			Cross-references
#	JPH 960425	.bbm --> .xbm; nmap/mph.dsc.
#			Copy section: Expand wildcard in target directory for 
#			 also deleting obsolete copies of selected files
#	JPH 960429	Expand wildcard in source directory to preserve wanted
#			 files in target directories (e.g. .ps files)
#	JPH 960430	Remove people.html from copy list
#			.xbm --> .bbm
#			log file
#	JPH 960507	rm $n_hlp/*.tex
 

##set echo
 	set log = $n_doc/doc_test.log 
	echo -n "ndoc test" >! $log
	date >> $log
	echo "" >> $log
	alias echo 'echo \!* | tee -a '$log

# Clean up .tex files from $n_hlp

	rm -f >&/dev/null $n_hlp/*.tex

# Copy source files to $n_hlp. Copy i.s.o. soft link is used because the WWW
#  server will ignore links for security reasons. 
# Define files selection: format is 
#	<quote> <source subdirectory of $n_src> <target subdirectory of $n_hlp> 
#		<file spec> <file spec> ... <quote>
 
	echo ""
	echo 'UPDATE OF $n_hlp-TREE COPIES OF FILES FROM $n_src TREE'
	echo ""
	set list = ( \
	  "doc/txt src/doc/txt *.txt" \
	  "doc/bin src/doc/bin *.ps *.gif" \
	  "doc/icons icons *.gif *.html *.xbm *.remove" \
	  "doc/html . homepage.html" \
	  "doc/html elsewhere_inst_maint elsewhere_inst_maint.html" \
	  "doc/html nfra_config_management nfra_config_management.html" \
 	  "nmap src/nmap mph.dsc" \
	  "nscan src/nscan fdw.dsc ohw.dsc scw.dsc shw.dsc ihw.dsc sch.dsc sth.dsc" \
 	  "wng src/wng gfh.dsc" \
	  )

# copy selected files directory by directory

	while ($#list)
	  set noglob
	  set l = ( $list[1] )
	  shift list
	  set dir = $l[1]		# source directory
 	  shift l
	  set ddir = $l[1]		# target directory
	  shift l
	  echo '	$n_src/'"${dir} to "'$n_hlp/'"${ddir}:"	# directory
	  echo "	  $l"		# files
	  unset noglob
	  pushd $n_src/$dir >&/dev/null
	  set l = ( $l )		# expand wildcards
	  set lr = `'echo' $l | sed -e 's:\.[^ ]*:.ps:g' `
	  cd >&/dev/null $n_hlp
	  rm -f $l $lr			# remove misplaced copies
 	  mkdir -p >&/dev/null $ddir
 	  cd >&/dev/null $ddir
	  rm -f $l			# remove old files
	  cd $n_src/$dir
	  cp $l $n_hlp/$ddir
	  popd >&/dev/null
	end

 	echo ""
 	echo "DOCUMENTATION-SYSTEM INTEGRITY CHECK"
	set nonomatch

# Compare .fig with .cap files: Sould be one-to-one

	echo ""
	echo '  $n_doc/fig: .fig files with missing .cap files'
	cd $n_doc/fig
	foreach f (*.fig)
	  set f = $f:r
	  if (! -e $f.cap) then
	    echo "	$f.fig"
	  endif
	end

	echo ""
	echo '  $n_doc/fig: .cap files with missing .fig files'
	foreach f (*.cap)
	  set f = $f:r
	  if (! -e $f.fig) then
	    echo "	$f.cap"
	  endif
	end

	cd $n_hlp

# Compare fig/.ps files with .fig sources: Sould be one-to-one

	cd fig
	echo ""
	echo '  $n_hlp/fig: .ps files with missing .fig sources' 
	foreach f (*.ps)
	  set f = $f:r
	  if (! -e $n_doc/fig/$f.fig) then
	    echo "	fig/$f.ps removed"
	    rm $f.ps
	  endif
	end
	cd ..

# Compare l2h subdirectories with .tex files and .html: Sould be one-to-one 
 
	echo ""
	echo \
'  $n_hlp: Subdirectories with missing or multiple .tex or .html sources'
	set d = (` find . -name '*' -type d -prune -print | sed -e 's:^\./::' `)
	foreach f ($d)
	  if ($f == src || $f == fig || $f == icons) continue
	  pushd $n_doc >&/dev/null
	  set ff = \
`'echo' */$f.tex html/$f.htm? | sed -e "s:\*/$f.tex::" -e "s:html/$f.htm?::" `
	  popd >&/dev/null
	  if ($#f == 0) then
	    echo "	$f/: removed"
	    rm -r $f 
	  else if ($#ff > 1) then
	    echo "	${f}/: multiple sources"
	    while ("$ff" != "")
	      echo "		$ff[1]"
	      shift ff
	    end
	  endif
	end

 	set ff = ( *.ps )
	if ($ff[1] == '*'.ps) then
	  echo \
'	  ERROR: All $n_hlp/*.ps files lost; do ndoc all to recover'
 	else

# Compare .ps files with fig/.ps files. If both exist, the .ps file is assumed
#  to be obsolete

	  echo ""
	  echo '  $n_hlp: obsolete .ps versions of $n_hlp/fig/.ps files'
	    foreach f ( $ff )
 	      if (-e fig/$f) then
	        echo "	$f: removed"
	        rm $f
	      endif
	    end
	  endif

# Check .ps files. Legal ones are derived from .tex source or copies of a .ps
#  for which no source exists. If it corresponds to a .cap source, it is 
#  obsolete. If it has multiple sources or no source at all, it is reported.

	  echo ""
	  echo '  $n_hlp: .ps files with missing or multiple .tex, .ps sources' 
 	  set ff = ( *.ps )
	  foreach f ($ff)
	    set f = $f:r
  	    if (-e $n_doc/fig/$f.cap) then 
 	      echo "	$f.ps: .cap input - removed"
	      rm -f $f.ps
 	    else
	      pushd $n_doc >&/dev/null
	      set ft = \
	`'echo' */$f.tex bin/$f.p? | sed -e "s:\*/$f.tex::" -e "s:bin/$f.p?::"`
  	      popd >&/dev/null
	      if ($#ft == 0) then
	        echo "	$f.ps: no source"
	      else if ($#ft > 1) then
	        echo "	$f.ps: multiple sources"
	        while ("$ft" != "")
	          echo "		$ft[1]"
	          shift ft
	        end
	      endif
	    endif
 	  end
  
# Compare actual source files with doc.grp

	cd $n_doc
	set tmp = doc_test.tmp
 	ls -Fd1 * */* \
	| sed -e '\:/$:d' \
	      -e '/\*$/d' -e '\:/$:d' \
	      -e '\:^mlink/:d' -e '/_tmp/d' \
	      -e '\:bin/.*\.ps:b 1' \
	        -e '\:bin/:d' \
	      -e ':1' \
	      -e 's:[^@]$:& h:' -e 's:@$: a:' \
	>! $tmp.l
	sed < doc.grp \
	  -e 's:\!.*$::' -e 's:-.*$::' -e 's:[	 ]*$::' -e '/^$/d' \
	  -e 's:[0-9a-z_]*\.grp:& g:p' \
	  -e 's:^bin/[0-9a-z_]*\.ps$:& g:p' \
	  -e 's:^fig/[0-9a-z_]*\.cap$:& g:p' \
	  -e 's:^fig/[0-9a-z_]*\.fig$:& g:p' \
	  -e 's:^html/[0-9a-z_]*\.html$:& g:p' \
	  -e 's:^icons/[0-9a-z_]*\.gif$:& g:p' \
	  -e 's:^icons/[0-9a-z_]*\.html$:& g:p' \
	  -e 's:^intfc/[0-9a-z_]*\.tex$:& g:p' \
	  -e 's:^latex/[0-9a-z_]*\.tex$:& g:p' \
	  -e 's:^txt/[0-9a-z_]*\.txt$:& g:p' \
 	>> $tmp.l
 
  	sort -u $tmp.l \
	| sed -n \
	  -e '$a\\
~'\
  	  -e '/\.cap /p' \
 	  -e '/\.fig /p' \
 	  -e '/\.gif /p' \
	  -e '/\.grp /p' \
	  -e '/\.html /p' \
 	  -e '\:bin/.*\.ps :p' \
 	  -e '/\.tex /p' \
	  -e '/\.txt /p' \
	  -e '/\.xbm /p' \
	| sort -u \
	| awk \
	 'BEGIN{ print " "; \
	    print "  Comparison of actual files with doc.grp"; \
	    print "     h = hardcopy file not listed in doc.grp"; \
	    print "     a = soft link to master not listed in doc.grp"; \
	    print "     g = nonexistent file listed in doc.grp"; \
	    print "  File paths are shown relative to \$n_doc"; print " "; \
	  } \
	  { if ( $1!=p){ \
	      if (n ==1 ){ \
		if (q =="g"){ \
		  printf("	  %1s %-s\n", q,p); \
		}else{ \
		  printf("	%1s   %-s\n", q,p); \
	      } } \
	      n=1; p=$1; q= $2; \
	    }else{ \
 	      n++; \
  	  } }' \
	| tee -a $log \
 	| grep .

# Cross-references in $n_hlp/*/*.html
##set echo
	echo ""
	echo "DOCUMENT CROSS REFERENCES"
	echo
	set tmp = $n_doc/doc_cross.tmp
	cd $n_doc

# List all documents and make awk script to find references to them
 
	'echo' -n "" >! $tmp.0
	'echo' -n "" >! $tmp.1
	'echo' -n "" >! $tmp.5
 	set list = ( \
	  "latex ???*.tex" \
	  "intfc ???*.tex" \
	  "txt ???*.txt" \
	  "fig *.cap" \
	  "bin ???*.gif ???*.ps" \
	  "html ???*.html" \
	  )
	while ( $#list )
 	  set noglob
	  set l = ( $list[1] )
	  shift list
 	  set d = $l[1]
	  shift l
  	  cd $d
	  unset noglob
 	  foreach f ($l)
	    set n = $f:r
	    'echo' "/@$n@@/"'{print @, "'$n'" }' >> $tmp.0
	    'echo' "~- $n" >> $tmp.1
	    'echo' "s/ $n"'$'"/ $f/" >> $tmp.5 
	  end
	  cd ..
  	end

# Process all .html files, collect pairs <file> <reference> 
# (awk refuses to recognise dots, so we convert them to @'s with sed)

 	pushd $n_hlp >&/dev/null
 	foreach f ( ./???*.html ???*/*.html )
	  set f = `'echo' $f | sed -e 's:/: :'`
 	  pushd $f[1] >&/dev/null
	  sed -e 's: @: "'$f[2]' ":' < $tmp.0 >! $tmp.awk
	  sed -e 's:\.:@@:g' -e 's:/:@:g' < $f[2] \
  	  | nawk -f $tmp.awk \
	  >> $tmp.1
 	  popd >&/dev/null
	end
	popd >&/dev/null

# Format and sort, by file and by reference

	cat << END >> $log

File:				  Referred to by file:

END
##set echo

 	sort -u < $tmp.1 \
	| sed -f $tmp.5 \
 	| sort -b +1 -2 \
	| awk \
	 '{ if ($1 ==$2) next; \
	    f=$2; if (f==fp){ f="" }else{ fp=f}; \
	    x=0; if ($1 =="~-" && $2 ==s2 ) x=1; s2= $2; \
	    if (x ==0 ){ \
	      printf ("  %-32s  %-24s\n", f,$1); \
	    }else{ \
	      next; \
	    }; \
	  }' \
	| sed -e 's:~-::' \
	>> $log

	'echo' ""
	'echo' "This overview saved in $log"

	rm -f $tmp.*
