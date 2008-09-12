#!/bin/csh -f
# wngfex.csh
# WNB 920911
#
# Revisions:
#       HjV 920914      Add type LA (print text on laser-printer)
#       WNB 920917      New spooling command atnf
#       WNB 920917      Delete setenv WNG_SITE and other typos ({}!!)
#       HjV 920922      Get correct filename and replace loch by locr
#       WNB 921006      Change to non-binary for PostScript
#       WNB 921006      Error in RUG TXA4:: and 5::
#       WNB 921013      Change ATNF for PostScript error
#       WNB 921021      Add A3 plotter
#       WNB 921126      More lines for atnf printer
#       WNB 921130      Change tr for HP
#       HjV 921203      Add site RAIUB
#       HjV 921215      Change for RUG
#       WNB 921222      Add LN, RL, LR
#       WNB 921222      Make it into WNGFEX.SSC; remove A3 etc from non-nfra
#       HjV 930115      Finalize A3 plotter for UNIX
#                       print direct on PS-printer on NFRA for UNIX-machines
#       HjV 930226      Add site WSRT, add HP for NFRA
#	HjV 930414	Take correct PS-printer on NFRA-VAX
#			Change command to print on NFRA-HP
#	HjV 930630	Add site KOSMA, change VAX-NFRA queue CMPQ into CMPS
#	CMV 930707	Split out site dependent parts in separate files
#	CMV 931216	Switched off the 'purge' for logfiles
#	HjV 940331	Add -f to 'rm' and 'mv'
#	CMV 940418	Remove empty log-files
#	JPH 970205	Insert a sleep 5 in EXIT to circumvent problems with
#			 jet5 printer (plot jobs getting lost, print jobs 
#			 getting mixed up
#
# General file handling
#
#       Use as: WNGFEX command name1 name2 action
#       Type can be:
#               SP      spool file nam1 as nam2
#               QM      spool nam1 as nam2 to QMS plotter
#               PS      spool nam1 as nam2 to PS plotter
#               A3      spool nam1 as nam2 to A3-PS plotter
#               LA      spool nam1 as nam2 to LAser printer
#
#               RE      rename file nam1 into nam2
#               CC      concatenate file nam1 onto nam2
#               LN      make logical link nam2 to nam1
#               RL      delete all .log, .tmp, .PLT or size == 0
#                               older than action (or 5) days
#               LR      combine LN and RL
#
#       Action is series of letters:
#               D       delete file after spooling and concatenation
#       or an unsigned value for RL/LR
#
#echo "WNGFEX: $argv"
#set echo

#
#  Need at least command and file to act upon
#
if ($#argv < 2) goto EXIT                       # no file names

#
#  First argument: name of command  ($loa)
#
set loa=`echo $argv[1] | tr '[A-Z]' '[a-z]' ` # type

#
# Second argument: input file
#
set lob=$argv[2]                                # input name

#
# Optional third argument: name for output file ($loc)
#
if ($#argv < 3) then                            # no output name
  set loc=$lob                                  # same
else
  set loc=$argv[3]                              # output name
endif

#
# Optional fourth argument: delete flags ($lod)
#
set lod=""
if ($#argv > 3) set lod=`echo $argv[4] | tr '[A-Z]' '[a-z]' ` # action

#
# Construct name for temporary file
#
set loct=${USER}_${loc:t}
if (-e $loct) then
   'rm' -f $loct
endif

if ($loa =~ re*) then                           # rename
    if (! -e $lob) goto EXIT                        # file unknown
    if (-z $lob) then
      'rm' -f $lob
    else
      'mv' -f $lob $loc
    endif
else if ($loa =~ cc*) then                      # concatenate
    if (! -e $lob) goto EXIT                        # file unknown
    if (-e $loc) then                               # append to known
       @ statx = { cat $lob >> $loc }
    else                                            # copy to unknown
       @ statx = { cat $lob > $loc }
    endif
    if (! $statx) then                              # ok
      if ($lod =~ *d*) 'rm' -f $lob                    # delete
    endif
else if ($loa =~ ln*) then                      # link
    if (! -e $lob) goto EXIT                        # file unknown
    if (-z $lob) then
      'rm' -f $lob
    else
      'rm' -f $loc >& /dev/null                       # remove old link
      ln -s $lob $loc                                 # make link
    endif
else if ($loa =~ lr*) then                      # link and remove
    if (! -e $lob) goto EXIT                        # file unknown
    if (-z $lob) then
      'rm' -f $lob
    else
      'rm' -f $loc >& /dev/null                       # remove old link
      ln -s $lob $loc                                 # make link
    endif
else if ($loa =~ rl*) then                      # remove tmp, log, PLT, size 0
    if ("$lod" == "" || "0123456789" !~ *$lod*) set lod=5
#    set loo='( -name *.[tT][mM][pP] -o -name *.[lL][oO][gG]'
    set loo='( -name *.[tT][mM][pP] '
    set loo="$loo -o -name *.PLT -o -size 0 )"
    set lop="-atime +$lod -exec rm -f {} ;"
    set noglob; find . $loo $lop >& /dev/null; unset noglob
#
# Printing actions
#
else
    if (! -e $lob) goto EXIT                        # file unknown
    if (-e $n_src/sys/wngfex_$n_site.csh) then
       @ statx = 0
       source $n_src/sys/wngfex_$n_site.csh
       if (! $statx) then                              # ok
          if ($lod =~ *d*) 'rm' -f $lob                    # delete
       endif
    else
       echo "Error: cannot find wngfex_$n_site.csh
    endif
endif

EXIT:
if ($?loct) then
  if (-e ${loct}) then
     sleep 5
     'rm' -f ${loct}
  endif
endif

exit
