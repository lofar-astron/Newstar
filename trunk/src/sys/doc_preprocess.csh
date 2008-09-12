#! /bin/csh -f
goto 000

doc_preprocess.csh - common .tex-file preprocessing for doc_print.csh and doc_cook.csh, to be sourced by both

Input:  File holds the file to be processed
Output: Name = file name
        Tmp = name for temp. files
        All \input commands are executed recursively
        All figures referred to are processed (.fig --> .ps)
        Short lines are merged, long lines then split into 80-char lines
        File $Tmp.0 is the preprocessed file
        \captions are italicised and given a dummy index argument
        \xxxref first arguments are preprocessed to the format

                @0@<command>@1@{<text>}@<n>@{<file>@3@.<label or extension>}

        with n=4 for \textref, =5 for \ascref and =6 for \psref


History:
        JPH 940818      Creation
                        Improve/correct line concatenation algorithm
        JPH 940829      Again ...
        JPH 940914      \caption processing
        JPH 940916      eliminate fig names with a '>'
                        $File includes extension, $Tmp setup
        JPH 941104      Eliminate comment lines to avoid awk overflow downstream
        JPH 941111      Exclude verbatim sections from line-merging
        JPH 941116      Define Name
        JPH 941121      Delimit \textref arguments
        JPH 941123      Prefix .cap-file \inputs with '../fig/'
                        Process \htmladdnormallink arguments
        JPH 941130      Handle \textrefs with indented continuation line
        JPH 950214      typo
        JPH 950823      Revise inter-document reference processing
        JPH 950928      newline after \label (required for Cook figure 
                         processing)
        JPH 951016      set Ext
        JPH 951127      \n after \label only in figure environment
        JPH 960220      Add leading '}' to chars that force a line break
	JPH 9604..	Revised line-merging algorithm
	JPH 960426	Retain leading % of comments so line break in too long
			 paragraphs are preserved. 
			Use tab record separator for awk.
	JPH 960429	\\ terminator forces line break
			Fix typo in psref processing
			Fix recognition of % in first position
	JPH 960513	Suppress line-merging in verbatim sections

000:
#
# Execute all \input and \include recursively so we have all \textref and 
#  \keyref explicitly present for editing. 
# awk: Concatenate paragraphs to single lines to insure that LaTeX commands
#       are contiguous with their arguments.
#
          set Name = $File:r
          set Ext = $File:e
          set Tmp = ${Name}_tmp
          rm >&/dev/null $Tmp.*
          sed -e '/^\.c+/,/^\.c-/d' < $File \
              -e 's:\\input *{\([^.].*\.cap *}\):\\input{../fig/\1:' \
         >! $Tmp.00
          $n_exe/include.exe $Tmp.00 $Tmp.01

          expand < $Tmp.01 \
          | awk -F'	'   # dummy separator to avoid 'too many fields' error \
  	    '/^[ 	]*$/{printf ("\n\n"); next;}	# empty-ln placeholder \
	     /\\begin *{ *verbatim *}/{verb=1} 		# exclude verbatim \
	     {if (verb ){ print $0; next; }}		#  sections from \
	     /\\end *{ *verbatim *}/{verb=0; next; } 	#   line-merging \
	     /^[ 	\\]/{printf ("\n");}		# 'newline' flag chars \
	     /[^\\]*%/{printf ("\n");} 			# unescaped % char \
	     /^\./{printf ("\n");} 			# '.' command \
	     {printf ("%s "), $0;} \
	     /[^\\]*%/{printf ("\n");} 			# unescaped % char \
	     /\\\\ *$/{printf ("\n");}			# '\\' line terminator \
	     END{printf ("\n");} 			# terminate last line \
	    ' \
 	  | sed \
	    -e '/^$/d'					# remove dummy lines \
	    -e 's:::'					# and placeholders \
# Format \xxxref commands into the form \
#  @1@<command>{<text with \_}{@nn@file with directory, extension and _@3@} \
\
          | sed \
		-e 's:\([^\]%\).*:\1:g' \
		-e 's:^%.*:%:' \
                -e '/\\newcommand/b' \
                -e 's:\\caption *{:\\caption[.]{:' \
                -e 's:\\it *\\it:\\it:' \
                -e 's:\\caption$:\\caption[.]:' \
# Mark reference commands my @0@ \
# Mark start of file argument by @4@ for \textref, \
#  @5@ for \ascref, @6@ for \psref @7@ for \srcref \
# add continuation lines until we have no incomplete first arguments (this  \
# should be unnecessary because line-merging takes care of this; we may try to \
# remove the '/[^@]...' lines when they cause trouble \
                -e ':1' \
                  -e 's:\(\\[Tt]extref *{[^}]*{[^}]*}[^}]*} *\):@0@\1@4@:g' \
                  -e 's:\(\\[Tt]extref *{[^}]*} *\):@0@\1@4@:g' \
                  -e 's:\(\\[Aa]scref *{[^}]*{[^}]*}[^}]*} *\):@0@\1@5@:g' \
                  -e 's:\(\\[Aa]scref *{[^}]*} *\):@0@\1@5@:g' \
                  -e 's:\(\\[Pp]sref *{[^}]*{[^}]*}[^}]*} *\):@0@\1@6@:g' \
                  -e 's:\(\\[Pp]sref *{[^}]*} *\):@0@\1@6@:g' \
                  -e 's:\(\\[Ss]rcref *{[^}]*{[^}]*}[^}]*} *\):@0@\1@7@:g' \
                  -e 's:\(\\[Ss]rcref *{[^}]*} *\):@0@\1@7@:g' \
                  -e '/[^@]\\[Tt]extref/\!b 2' -e 'N' -e 's:\n: :' -e 'b 1' \
                  -e '/[^@]\\[Aa]scref/\!b 2' -e 'N' -e 's:\n: :' -e 'b 1' \
                  -e '/[^@]\\[Pp]sref/\!b 2' -e 'N' -e 's:\n: :' -e 'b 1' \
                  -e '/[^@]\\[Ss]rcref/\!b 2' -e 'N' -e 's:\n: :' -e 'b 1' \
               -e ':2' \
# Split command and text argument \
                -e 's:@0@[^{]*:&@1@:g' \
# Place file argument in @nn@ @3@; \
# add continuation lines until we have no incomplete file arguments \
                -e ':3' \
                  -e 's:@\([4-9]\)@\([^.}]*\)\([\.}]\):@\1\1@\2@3@\3:g' \
                  -e '/@[4-9]@/\!b 4' -e 'N' -e 's:\n: :' -e 'b 3' \
                -e ':4' \
# Clean up; process labels \
                -e 's:@\([4-9]\)\1@:@\1@:g' \
# Replace _ by \_ in text and file arguments \
                -e ':5' \
                  -e 's:\(@[04-9]@[^@]*[^\]\)_:\1\\_:g' \
                  -e 't 5' \
          >! $Tmp.0
#
# Produce .ps files for figures. The code does not discriminate against \fig
#  in a verbatim environment. Where this causes a problem, the "standard"
#  trick of inserting an <ESC> behind the \ must be used, cf. doc_guide.tex. 
#
          set figs = \
`grep '^[^%]*\\fig[ {]' < $Tmp.0 | sed -e '/>/d' -e 's:}.*:.fig:' -e 's:.*{::'`
          $n_src/sys/document.csh Figures $figs
