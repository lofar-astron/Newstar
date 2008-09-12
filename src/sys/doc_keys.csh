#! docKeys.csh - sourced from document.csh
goto 000

History:
	JPH 941028	Overhaul to make single .html file per program, do all
			 input parsing with awk
	JPH 941031	Simplify code (no splitting into temp. files per 
			 keyword). .tex file completely TeX-compatible, copy
			 output to latex directory so it is available for
			 printing
	JPH 941103	.pef prompts "may vary per application".
			Independent output --> \input to <pgm>_..._intfc.tex
			Re-introduce file-splitting to get parameters in alphab.
			 order (but avoiding nawk or the open-file overflow in
			 awk).
	JPH 941104	Skip if no keywords. - Use vb and pg flags entirely
			 systematically to avoid unwanted directives
	JPH 941110	Check for long lines. \title
	JPH 941116	Allow comment lines in the middle of a HELP text. As a
			consequence, many small changes in the awk logic to make
			the process robust against minor deviations in the .psc
			format.
			Remove leading and trailing quotes
			Prefix _ with \ in first \textref argument
	HjV 951213	Remove leading quote in PROMPT-line
	JPH 960104	Handle in-line comments in 'KEYWORD=' line
	JPH 960206	Remove blanks from public-parameter references
	JPH 960329	Split lines longer than 80 chars
	JPH 960513	sort -u references to public parameters
			Change reference to DWARF user interface
	JPH 960612	Add a word in document-text literal
			 
000:
##set echo
	if (x$Files[1] =~ x-*) then
	  set ev = $Files[1]
 	  shift Files
	  if (x$ev =~ *e*) set echo
	  if (x$ev =~ *v*) set verbose 
	endif
       	if ("$Files" == "") then
          echo -n "Enter the name of PIN/PSC/PEF file(s) [All]: "
          set Files=( $< )
          set Files=( $Files )
       	endif
#
# Process ALL
#
       	if ("$Files" == "" || "$Files" =~ [Aa][Ll][Ll]) then
	  set here = `pwd`
	  foreach Dir ($NSTAR_DIR)
	    echo "Directory $Dir"
	    cd $n_src/$Dir
	    echo "" | $0 k *.p??		# (answer "" to update question)
	  end
	  cd $here
	  exit
	endif
#
# Process files in the current directory
## Initialise
#
       	foreach file ( $Files )
	  echo -n "	ndoc keys  $file  "
	  set ext = $file:e
	  set pgm = $file:r; set pgm = $pgm:t
	  set tmp = $pgm.tmp
	  set klines = `grep -n '^KEYWORD=' $file | sed -e 's/:.*//' `
	  if (! -e $file) then
	    echo "	 - not found"
	    continue
	  else if ($#klines == 0) then
	    echo " - being skipped: no keywords"
	    continue
	  else
	    echo ""
	    set ll = \
		`awk '{if (l<length) l=length; next} END {print l}' < $file `
	    if ($ll > 80) then
 	      set file1 = $tmp
	      expand < $file \
	      | nawk ' \
		BEGIN{ lw= 80;} \
	        /^ *$/{ print $0; next;} \
	        { l=length; if (substr($0,l,1) ==" ") l--; \
	          for (nb=0; nb<l;){ \
	            b=nb; e=b+lw; if (e>l) e=l; e0=e; \
	            if (l-b <=lw){ \
	              print substr($0,b+1,e-b); next; \
	            }else{ \
	              for (; e>b && substr($0,e,1) !=" "; e--); \
		        nb= e; \
	              for (; e>b && substr($0,e,1) ==" "; e--); \
	              if (e==b){ \
		        if (ll){ \
		          for (e=e0; e <=l && substr($0,e,1) !=" "; e++); \
		          nb=e; e--; \
		        }else{ \
		          e=e0; nb=e0; \
		    } } } \
 	            print substr($0,b+1,e-b); \
	        } }' \
	      >! $file1
	    else
	      set file1 = $file 
 	    endif
	  endif

	  set PGM = \
`echo $pgm | sed -e 'y:abcdefghijklmnopqrstuvwxyz:ABCDEFGHIJKLMNOPQRSTUVWXYZ:' `

	  if ($ext == psc || $ext == pin) then
	    unset pef
	    set type = "private"
	    set Type = "Private"
 	    set out = ${pgm}_private_keys
	  else
	    set pef
	    set type = "public"
	    set Type = "Public"
	    set out = ${pgm}_public_keys
	  endif
	  set tmp = $pgm.tmp
	  rm >&/dev/null $tmp.*
	  set ref = $tmp.ref

# create awk script to process keyword files.
#  (Note that each \\ is reduced to \ as awk reads the script! )
# The following variables are used:
#   d		char	data-type
#   dor		logical	input line is blank
#   end		logical	end of keyword definition (i.e. start of new keyword or 
#			 help for current one)
#   help	logical	input is HELP
#   key		logical	input is KEYWORD
#   h		logical processing HELP text
#   l		number	LENGTH value
#   m		number	MIN_NVALUES value
#   n		number	NVALUES value
#   p		char	prompt text
#   pg		logical	in-samepage-mode flag (\spbegin written)
#   vb		logical in-verbatim-mode flag (\svbegin\begin{verbatim} written}

	  cat <<END >! $tmp.awk
	    BEGIN{
	      d=""; p="";
	      ref= "$tmp" ".ref";
	      printf ("\\\\begin{itemize}\\n") > ref;
	    }
	    {key=0; end=0; help=0; } 
	    /^\!/{ if (! h){ next; } }
	    /^INCLUDE=/{
	      for (i=1; i<=length(\$2); i++ ){
	        if (substr(\$2,i,1) =="_") break;
	      }
	      nm=substr(\$2,1,i-1); \
# nsh textref in two lines so sed can convert the filename alone to lowercase \
	      printf ("\\\\item \\\\textref{%s}\\n{@%s_public_intfc} public keywords\\n", nm, nm) > ref;
	      next;
	    }
	    /KEYWORD=/{
	      key=1;
	      if (substr(\$1,1,1) =="!" ){ key=0;}
	      end=1; h=0; 
	      if (vb ){ vb=0; printf ("\\\\end{verbatim}\\\\svend\\n"); }
	      if (pg ){ pg=0;  printf ("\\\\spend\\n"); }
	    }
	    /HELP=/{ if (substr (\$1,1,1) !="!" ){ help=1; end=1; } }
	    /PROMPT=/{
	      p=substr(\$2,2,length(\$2)-1); 
	      if (pef) p= p " ({\\\\em may vary per application})"; next;
	    }
	    /DATA_.*=/{
	      x=substr(\$2,1,1);
	            if (x=="C" ){ d= "Character";
	      }else if (x=="D" ){ d= "DoublePrecision";
	      }else if (x=="R" ){ d= "Real";
	      }else if (x=="I" ){ d= "Integer";
	      }else if (x=="J" ){ d= "Integer";
	      }else if (x=="L" ){ d= "Yes/No";
	      }else             { d= "?";
	      }; next;
	    }
	    /LENGTH=/{ l=\$2; next; }
	    /NVALUE/{ n=\$2; next; }
	    /MIN_NVAL/{ m =\$2; next; }

	    { if ( end){
	        end=0;
	        if (p !="" || d!="" ){
	          printf ("\\\\spbegin\\n"); pg=1;
		} 
	        if (p !="" ){ 
		  printf ("{\\\\em Prompt:} %s\\\\\\\\ \\n", p); p="";
		}
	        if (d !="" ){ 
		  printf ("{\\\\em Expected input:} %s", d); d="";
	          if (l !=0 ){ printf (" *%d", l); }
	          if (n ==m ){
	            pl= ""; if (n>1) pl= "s";
		    printf (":  %d value%s\\n", n, pl);
	          }else if (m !=0 ){
	            printf (":  %d to %d values\\n", m, n);
	        } }
		if (pg ){ pg=0; printf ("\\\\spend\\n"); }
	    } }
	    { if (key ){
	        key=0;
	        printf ("\\n\\n\\\\subsection{ Parameter %s}\\n", \$2);
	        printf ("\\\\label{@.%s}\\n\\n", \$2);
	        p=""; d=""; l=0; m=1; n=1;
	        next; 
	    } }
	    { if (help ){
	        printf( "\\\\spbegin\\n\\\\svbegin\\\\begin{verbatim}\\n");
		pg=1; vb=1; help=0; h=1; next;
	    } }

	    {if (! h) next; }
	    { dot=0; }
	    /^\\./{ dot=1; }
	    /^ *\$/{ dot=1; }
	    { if (dot ){
	        if (vb ){ vb=0; printf ("\\\\end{verbatim}\\\\svend\\n"); }
		if (pg ){ pg=0;  printf ("\\\\spend\\n"); }
	        next;
	      }else{
	        if (! pg ){ pg=1; printf ("\\\\spbegin\\n"); }
	        if (! vb ){
	          vb=1; printf ("\\\\svbegin\\\\begin{verbatim}\\n");
	    } } }
	    /^\\!/{
	      if (vb ){ vb=0; printf ("\\\\end{verbatim}\\\\svend\\n"); }
	      if (\$0 ~ /\\\\/){
	        print substr(\$0,2,length-1); 
		next;
	      }else{
		next;
	    } }
 	    { if (h ){ printf ("%s\\n", \$0); next; } }
	    END{ 
	      if (vb ){ printf ("\\\\end{verbatim}\\\\svend\\n"); vb=0; }
	      if (pg ){ printf ("\\\\spend \\n"); pg=0; }
	      printf ("\\\\item \\\\textref{DWARF}{introduction.user.interface} user interface\\n") > ref; \
	    } 
END
#
## Initialise the .tef file
	  set date = `date`
	  cat <<END >! $tmp.1
% 	$out.tef 
% 	created from $file on $date 
%
% ********** DO NOT EDIT THIS FILE, but its source! **********
%
END
	  if ($type == private) cat <<END >! $tmp.1

%\section{ References to public interfaces}
%\label{.public}

\input $ref.1
END
##	  cat <<END >! $tmp.1
##
##\section{ Descriptions of the individual parameters}
##\label{.descriptions}

##END
#
## Sort keyword sections in alphabetical order. (A workaround is used i.s.o. 
##  csplit because repeat count on /regexp/ does not work.)
## sed:		Remove in-line comments
##		Remove leading characters in first HELP line
## expand:     	Expand tabs so they are shown correctly in \verbatim mode
## awk: 	Convert .psc/.pef file into tex output
## sed: 	escape those characters that would confuse TeX
##      	suppress empty verbatim sections
##    (A multiple-sed pipe is use because a single sed could not be made to 
##     execute the combined commands correctly.)
#
	  csplit -s -f $tmp. $file1 $klines
	  foreach f ( $tmp.[0-9][0-9] )
##	    set nm = `grep '^KEYWORD=' $f | sed -e 's:.*=::' -e 's:\!.*$::' `
	    set nm = \
		`sed -n <$f -e '/^KEYWORD=/\\!d' -e 's:\\!.*$::' -e 's:.*=::p' `
	    mv $f $tmp.$nm
  	  end
	  cat $tmp.[A-Z]* \
	  | expand \
	  | sed \
	    -e 's:\(..*\)\!:\1:' \
	    -e '/HELP=/{ \
	       i\\
HELP=\
	       s:HELP= *" *::; \
	     }' \
	    -e '{ s:" *$::; s:^" *::; /^ *$/d; }' \
	    -e 's: *$::' -e '$a\\
! ' \
	  | nawk -F'=' -f $tmp.awk pef=$?pef \
	  | sed \
	    -e '/\\label/s:_:.:g' \
	    -e '/\\begin{verbatim}/{ \
	       N; /\\begin{verbatim}.*\\end{verbatim}/d; \
	     }' \
	    -e '/\\begin{verbatim}/,/\\end{verbatim}/\!{ \
	       s:\([^\]\)_:\1\\_:g; \
	       s:<:$<$:g; \
	       s:>:$>$:g; \
	       s:\#:\\\#:g; \
	     }' \
	  | sed \
	    -e '/\\spbegin/{ \
	       N; /\\spbegin.*\\spend/d; \
	     }' \
	    -e '/@/y:ABCDEFGHIJKLMNOPQRSTUVWXYZ:abcdefghijklmnopqrstuvwxyz:' \
	    -e '/\\textref/s:\([A-Z][^\]\)_\([A-Z]\):\1\\_\2:g' \
	    -e 's:@::'g \
	  >> $tmp.1
#
## Convert referenced file names to lower case (for this reason \textref lines
##  were written in ntwo parts above 
## Combine textref lines 2 to 1, sort them and add top and bottom lines
#
	  sed < $ref \
	    -e '/@/y:ABCDEFGHIJKLMNOPQRSTUVWXYZ:abcdefghijklmnopqrstuvwxyz:' \
	  | sed \
	    -e '/DWARF/b' \
	    -e '/\\item/\!b' \
	      -e 'N' -e 's:\n{@:{:' \
	  | sort -u \
	  | sed \
	    -e '1i\\
{\\em See also:}' \
 	    -e '$a\\
\\end{itemize}' \
 	  >! $ref.1	  
#
## Convert to .html
#
	  $n_exe/include.exe $tmp.1 $tmp
	  mv $tmp $n_doc/intfc/$out.tef
	  pushd >&/dev/null $n_doc/intfc
	  setenv n_force
	  echo "no" | $n_src/sys/document.csh cook ${pgm}_${type}_intfc.tex
	  popd >&/dev/null
#
# Clean up
#
	  rm $tmp*
	end	# file loop
