#! /bin/csh -f
#+ document.csh
#
#   CMV 930713  Created
#   CMV 931111  Original memo file not autmatically deleted
#   CMV 931116  Changed for httpd 1.0
#   CMV 931206  Use nview (httpd/htbin) for extracted documentation
#   JPH 940516  Split off docCook.csh, docPrint.csh, docKeys.csh
#		Overview option
#   JPH march 94-940704 Grand revision		
#   CMV 940712  Minor modifications to fit in master system
#   JPH 940715	Respect user's settings for Xmosaic window size
#   CMV 940719  Respect also users who just want to have the defaults (most),
#               start xmosaic such that ?? can find it,
#               use former docaid routine to translate keyword files.
#   CMV 940720	Make new archive at exit
#   JPH 940815  Use doc_keys.csh i.s.o. genaid.exe for easier experimenting
#   JPH 940818  Fig option: delete $n_hlp/...ps before making new one
#   JPH 940915  'All' option
#		Make dummy for non-existent .fig file
#   JPH 940919	Better error reporting ndoc print/cook
#   JPH 941123	fig command: Look for .ps if .fig not found
#		Add $n_doc/intfc to 'All' processing
#   CMV 941213	Skip xrdb if not in path
#   JPH 950213	Make 'figure not found' a WARNING i.s.o. an ERROR. - Typo in ALL
#   		Suppress 'Update' question in ndoc all
#		Split processing of large file collections (gave Word too long)
#   JPH 950221	Insert ad-hoc path for fig2dev
#   HjV 950613	Use latex2html stuff from $n_l2h  iso. ~jph
#   JPH 950822  Find command
#		Figure command: Check for existing newer output
#		Remove overview command
#   JPH 950905	Revise code for All: Simplify, work around 'Word too long'
#   JPH 950918	Temporary fix for xfig/fig2dev 3.1
#   JPH 950927	.ps --> .fps in test for up-to-date output
#   JPH 951006	Remove tex setup: Rely on host environment.
#   JPH 951101	Format ndoc Fig messages for clarity
#		Add Test command
#   JPH 951106	Fix error in sed extraction of figure scale
#   JPH 951120	Use ~/ for creating xrdb.tmp files.
#		Use xmosaic_restart i.s.o. genaid.exe to start the browser.
#   JPH 960102	Remove FULL option (is now ALL), Update help text
#   JPH 960129	Bug fix in ALL
#   JPH 960206	Figures: $n_hlp/.fps --> $n_hlp/fig/.ps
#   JPH 960208	Fix omission in 960206
#   JPH 960430	Remove old code, fix some messages. - Make doc_all.log
#
#
# This script coordinates all actions for documentation maintenance
#
# It does not contain full checking on the environment etc.
#
#---------------------------------------------------------------------
#
# Uncomment the following line for testing purposes...
##set echo
onintr Abort_exit

#
# Initialise name and date
#
set Myname=`awk -F: '{ if ($1 == "'$USER'") print $5 }' /etc/passwd`
if ("$Myname" == "") set Myname=`whoami`

set dt = (`date`)
if ("$dt[3]" =~ [1-9]) set dt[3] = "0$dt[3]"                # day
set mc=( Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec)
foreach mm ( 01 02 03 04 05 06 07 08 09 10 11 12)
  if ("$dt[2]" == "$mc[$mm]") break                         # month
end
@ yy = $dt[$#dt] - 1900                                     # year
set mh=( `echo $dt[4] | tr -s ":" " "` )                    # hh mm ss
set C_Date="$yy$mm$dt[3]"                                   # date: yymmdd
set C_Time="${mh[1]}:$mh[2]"                                # time: hh:mm
unset dt mc mm yy mh

if (! $?EDITOR) setenv EDITOR emacs

#
# Initialise TeX and the like just in case...
#
#. if (-e /usr/local/lib/tex/texsetup) then
##  if (! $?TEXFONTS )
#.    source /usr/local/lib/tex/texsetup
##  setenv TEXINPUT "$n_src/doc/cook"
#.  alias tex   "setenv TEXFONTS /usr/local/lib/tex/fonts; \tex"
#.  alias latex "setenv TEXFONTS /usr/local/lib/tex/fonts; \latex"
#.  alias dvips "setenv TEXFONTS /usr/local/lib/pk/pk300; /usr/local/lib/dvitps -B/usr/local/lib/tex/TeXPS/dvitps-cap -P/usr/local/lib/tex/TeXPS/pro \!* "
#  source /aips++/aipsinit.csh			# fonts for ghostview
##  alias xdvi  "setenv TEXFONTS /usr/local/lib/pk/pk300; /usr/local/lib/tex/xdvi"
#. endif
#
#  Get command, or enter menu mode if none given.
#

set ask_archive=0    # Do not ask archive by default

set Files=""
if ("$1" != "") then
   set Mode="Command"
   set Command="$1"
   set noglob; if ($#argv > 1) set Files=( $argv[2-] ); unset noglob
else
   set Mode="Menu"
   set Command=""
endif

#
# If in Menu mode, repeatedly ask commands, else just one command
#
while ( "$Mode" != "Quit")
   
   if ( "$Mode" == "Menu" ) then
      echo "General commands are:      help, find, script, hyper, quit"
      echo "Translation commands are:  all, keys, cook, print, figures"
      echo "To check system integrity: test
      echo -n "Enter a command: "
      set Command=($<)
      set Files=""
      set Command=( $Command )
      if ($#Command > 1) then
         set noglob; set Files=( $Command[2-] ); unset noglob
         set Command=$Command[1]
      endif

   else if ( "$Mode" == "Update" ) then
      set Command=$lupdate[$iupdate]
      set Files="all"
      echo "***** $Command all *****"
      
      @ iupdate = $iupdate + 1
      if ($iupdate > $#lupdate) set Mode="Quit"

   else
      set Mode="Quit"
   endif

   if ("$Command" == "" || $Command =~ [Qq]*) then
      set Mode="Quit"
#
#  %All command, regenerate entire documentation system in NoUpdate mode
#
   else if ($Command =~ [Aa][Ll][Ll]) then
     echo -n 'Keys all .p?? files? '; set x = $<
     if ($x =~ [Yy]*) set allp
     echo -n 'PostScript for all .tex documents? '; set x = $<
     if ($x =~ [Yy]*) set allpd
     echo -n 'HTML for all .tex  documents? '; set x = $<
     if ($x =~ [Yy]*) set allcd
     echo -n 'System consistency check? '; set x = $<
     if ($x =~ [Yy]*) set allch
	
     setenv n_force	# signal doc_cook, doc_print to bypass test for existing
			#  output
     pushd $n_src >&/dev/null
     set log = $n_doc/doc_all.log
     echo -n "" >! $log
     if ($?allp) then
       echo 'Hypertext on-line help: ndoc keys $n_src/*/*.p??' |& tee -a $log
       $0 keys all |& tee -a $log
       echo "" |& tee -a $log
       echo "" |& tee -a $log
     endif
     rm -f $n_doc/[il]*/*_tmp.tex
     if ($?allpd) then
       echo 'PostScript documents: ndoc print -s $n_doc/*/*.tex' |& tee -a $log
       cd $n_doc/latex
       $0 print -s [a-m]*.tex |& tee -a $log 
       $0 print -s [n-p]*.tex |& tee -a $log
       $0 print -s [q-z]*.tex |& tee -a $log
       rm >&/dev/null *.ps
       cd $n_doc/intfc
       $0 print -s [a-m]*.tex |& tee -a $log
       $0 print -s n*.tex |& tee -a $log
       $0 print -s [o-z]*.tex |& tee -a $log
       echo "" |& tee -a $log
       rm >&/dev/null *.ps
     endif
     if ($?allcd) then
       echo 'Hypertext documents: ndoc cook $n_doc/latex/*.tex' |& tee -a $log 
       cd $n_doc/latex
       $0 cook [a-m]*.tex |& tee -a $log
       $0 cook [n-p]*.tex |& tee -a $log
       $0 cook [q-z]*.tex |& tee -a $log
       echo "" |& tee -a $log
       if (! $?allp) then
         cd $n_doc/intfc
         $0 cook [a-m]*.tex |& tee -a $log 
         $0 cook n*.tex |& tee -a $log
         $0 cook [p-z]*.tex |& tee -a $log
         echo "" |& tee -a $log
       endif
     endif
     popd >&/dev/null
     if ($?allch) then
       source $n_src/sys/doc_test.csh
       cat $n_doc/doc_test.log >> $log
     endif
     echo 'Log file: $n_doc/doc_all.log'
#
#  %Test command: Check source/output integrity, copy miscellaneous texts
#
   else if ($Command =~ [Tt]*) then
     source $n_src/sys/doc_test.csh
#
#  %Help command:
#
   else if ("$Command" =~ [Hh][Ee]* ) then
       more <<_EOD_
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

 Document.csh  is used for maintenance of the Newstar documentation.
 
Document can be called in one of the following ways:

    document 

        Enter a menu mode where all options listed below can be 
        chosen. Additional arguments will be prompted for.

    document all

	(Re)Create part of or the entire $n_hlp tree from document sources
	You will be prompted for the parts of the tree to be processed

    document cook [File...|all]

        Convert one or more LaTeX files into html files. The literal 
        'all' corresponds to \$n_src/doc/latex/*

    document figures [File]

        Convert xfig figures to encapsulated postscript (this option 
        is called automatically for figures included in latex files)

    document find <string>

	Find and optionally print documents containg <string>. The search is
	case-insensitive. It is not necessary to put <string> in quotes.

    document hyper [File]

        Starts the WWW hypertext browser with the specified file.
	The default browser is the local host's netscape;
	 $n_exe/xmosaic.exe may be used as an alternative.
        The default File is the Newstar Home page.

    document keys [File...|all]

        Convert one or more PIN/PSC/PEF files to LaTeX and html files.
        The files should be given with their extension, wildcards are
        allowed. The literal 'all' corresponds to all files *.p?? in
        directories $NSTAR_DIR.
        The resulting LaTeX files will be in \$n_src/doc/intfc, the
        html files will be in \$n_hlp
   
        NB: In principle this could be done through the compile script.

    document print [-p|v|s] [File]

        Print or view an existing LaTeX file with a title page 
        and a table of contents.
        The -P switch causes an existing latex file to be printed (default)
        The -V switch causes an existing latex file to be viewed with xdvi
	The -S ("syntax") switch processes the file without display

    document script [-p|v] [File]

        Start a script session and convert the terminal output 
        to a proper LaTeX file. The file can (and probably should)
        be edited by hand and then be moved into \$n_src/doc/latex
        The -P switch causes an existing latex file to be printed
        The -V switch causes an existing latex file to be viewed with xdvi

    document test

	Check the correspondence between source files and ouput files:

	  - Delete outputs for which no corresponding inout exists (these are
		assumed to be leftover files)
	  - Report remaining inconsistencies.
	  - Report differences between the source input collection and their
		listing in doc.grp
	  - Copy source files referenced by documents to the \$n_hlp tree 
#-------------------------------------------------------------------------#

_EOD_

	else if ("$Command" =~ [Ff]) then
	  echo "Ambiguous command: Can be Find, Figures or Full"
	else if ("$Command" =~ [Ff][Ii]) then
	  echo "Ambiguous command: Can be Find or Full"
#
# %Find command
#
	else if ("$Command" =~ [Ff][Ii][Nn]*) then
	  source $n_src/sys/doc_find.csh 

# %Figures command
#
	else if ("$Command" =~ [Ff][Ii][Gg]*) then
	  pushd >&/dev/null $n_doc/fig
	  foreach File ($Files)
	    set File = $File:r
	    set Target = $File.ps
	    set target = $Target
	    if (-w $n_hlp) then
	      set Target = $n_hlp/$Target
	      set target = '$n_hlp/'$target
	    endif
	    if (! -e $File.fig) then
 	      set ext = tmp
	      set scale = "1.00"
	      sed < dummy_figure.fig \
		-e "s:##:$File.ps:" \
	      >! $File.$ext
	      echo "	  Making dummy for $File.ps"
 	    else
#
# Check if newer .ps file already exists
#
	      $n_exe/newerfile.exe $n_hlp/fig/$File.ps $n_doc/fig/$File.fig
	      if (! $?n_force && $status == 1) then
	        echo $File.fig \
| awk '{ printf ("	  %-32s output .ps file is up-to-date\n", $1) }'
	        continue
	      endif
#
# Find the scale definition and convert .fig to .ps (Note that a double \ is 
#  needed to escape the !)
#
	      set ext = fig
	      set scale = \
`sed -n -e "/$File.fig/\\!b" -e 's:%.*$::' -e 's:^.* ::' -e 's:[0-9][0-9]$:.&:p' < $n_doc/fig/$File.fig `
	      if ($scale =~ '.'*) set scale = "0$scale"
	      if ($scale == "") set scale = "1.00"
	      echo $File.fig $scale \
	      | awk '{printf ("	  %-32s scale %-s\n", $1, $2); }'
	    endif

	    $n_l2h/fig2dev -L ps -m $scale $File.$ext $File.tmp
##	    /local/bin/fig2dev -L ps -m $scale $File.$ext $File.tmp

# Define a margin around the figure for display by ghostview by redefining
#  the BoundaryBox. (Miraculously, this naive trick works!)
#
	    rm >&/dev/null $n_hlp/fig/$File.ps
	    awk < $File.tmp ' \
	      /%%BoundingBox:/{ \
	        printf("%s %d %d %d %d\n", $1, $2-10, $3-10, $4+10, $5+10);\
	        next;} \
	      {print $0} ' \
	    >! $n_hlp/fig/$File.ps
	    rm >&/dev/null $File.tmp
	  end
	  popd >&/dev/null
#
#   %Script command:
#
    else if ("$Command" =~ [Ss]* ) then
       source $n_src/sys/doc_script.csh
#
#   %Print command:
#
    else if ("$Command" =~ [Pp]* ) then
       source $n_src/sys/doc_print.csh
#
#  %Hyper command
#
# xmosaic spews out a lot of error messages related to some definition table
#  but these seem to do no harm, so we dismiss them
# We use xrdb to temporarily set xmosaic X resources and afterwards to restore
#  the initial condition. The xmosaic width is dictated by the width of 
#  80-char terminal output in scripts. If a user already defined her own
#  size settings, we respect those. 
#
# It is the responsibility of the user to define the size if he wants it to
# be different from normal. Catch prior and next is a different matter,
# it does not hurt if it is set (in fact most people will want it).
#							CMV 940719
#
    else if ("$Command" =~ [Hh][Yy]* ) then
      if (! $?DISPLAY) then
	echo "Cannot start Mosaic, DISPLAY not defined"
      else
       	set File = $Files[1];
       	if ("$File" == "") set File = $n_hlp/homepage.html
#	xrdb -query \
#	| tee xrdb.tmp.0 \
#	| awk -F':' \
#	 'BEGIN{ h=0; w=0;} \
#	  /Mosaic\*defaultHeight/{ h=1; print $0;} \
#	  /Mosaic\*defaultWidth/{ w=1; print $0;} \
#	  {next;} \
#	  END{ \
#	    if (! w){ print "Mosaic*defaultWidth: 870"} \
#	    if (! h){ print "Mosaic*defaultHeight: 1000"}; \
#	  }' \
#	>! xrdb.tmp.1
        unset no_xrdb
	if (-x /usr/local/bin/xrdb) then
	  alias xrdb /usr/local/bin/xrdb
	else if (-x /usr/local/bin/X11/xrdb) then
	  alias xrdb /usr/local/bin/X11/xrdb
	else if (-x /usr/bin/X11/xrdb) then
	  alias xrdb /usr/bin/X11/xrdb
	else if (-x /usr/bin/xrdb) then
	  alias xrdb /usr/bin/xrdb
	else
          set no_xrdb
	endif
	if (! $?no_xrdb) then
           xrdb -query > ~/xrdb.tmp.0
	   cat << END >> ~/xrdb.tmp.1
	  Mosaic*catchPriorAndNext: True
	  Mosaic*postScriptViewerCommand: ghostview -magstep +1 -nocenter
END
	   xrdb -merge ~/xrdb.tmp.1 >&/dev/null
        endif

##	$n_exe/genaid.exe hyper $File
	$n_src/sys/xmosaic_restart.csh

	if (! $?no_xrdb) then
	   xrdb -load ~/xrdb.tmp.0 >&/dev/null
	   'rm' -f ~/xrdb.tmp.?
        endif
      endif
#
#  %Keys command
#
#  All the same, I want those \ref's and the formatting in docaid worked fine.
#							CMV 940719
#
#
    else if ("$Command" =~ [Kk]* ) then
       if ("$Files" == "") then
          echo -n "Enter the name of PIN/PSC/PEF file(s) [All]: "
          set Files=( $< )
          set Files=( $Files )
       endif
       source $n_src/sys/doc_keys.csh
       set ask_archive=1
#
#  %Cook command
#
    else if ("$Command" =~ [Cc]* ) then
      source $n_src/sys/doc_cook.csh
      set ask_archive=1
#
#  Invalid command
#
    else    # Other command
      echo ""
      echo "Error: Invalid or ambiguous command $Command"
      echo ""
    endif   # End of if (Command == ...)

end        # End of while (Menu mode)


if (-o $n_root && $ask_archive) then
   echo -n "Update archive for export? [n] "
   set do_it=($<)
   if ("$do_it" =~ [Yy]*) then
     set here=$cwd
     cd $n_hlp
     tar cvf $n_doc/newstar.hun * | grep -v '^a'
     compress $n_doc/newstar.hun
     if (-e $n_doc/newstar.hun.Z) mv $n_doc/newstar.hun.Z $n_doc/newstar.hun
     echo "You need to do an  nup check d  to insure itegrity of the database"
     cd $here
   endif
endif


Abort_exit:

