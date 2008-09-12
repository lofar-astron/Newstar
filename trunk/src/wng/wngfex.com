$!# WNGFEX.SSC
$!# WNB 920911
$!#
$!# Revisions:
$!#       HjV 920914      Add type LA (print text on laser-printer)
$!#       WNB 920917      New spooling command atnf
$!#       WNB 920917      Delete setenv WNG_SITE and other typos ({}!!)
$!#       HjV 920922      Get correct filename and replace loch by locr
$!#       WNB 921006      Change to non-binary for PostScript
$!#       WNB 921006      Error in RUG TXA4:: and 5::
$!#       WNB 921013      Change ATNF for PostScript error
$!#       WNB 921021      Add A3 plotter
$!#       WNB 921126      More lines for atnf printer
$!#       WNB 921130      Change tr for HP
$!#       HjV 921203      Add site RAIUB
$!#       HjV 921215      Change for RUG
$!#       WNB 921222      Add LN, RL, LR
$!#       WNB 921222      Make it into WNGFEX.SSC; remove A3 etc from non-nfra
$!#       HjV 930115      Finalize A3 plotter for UNIX
$!#                       print direct on PS-printer on NFRA for UNIX-machines
$!#       HjV 930226      Add site WSRT, add HP for NFRA
$!#	HjV 930414	Take correct PS-printer on NFRA-VAX
$!#			Change command to print on NFRA-HP
$!#	HjV 930630	Add site KOSMA, change VAX-NFRA queue CMPQ into CMPS
$!#	HjV 930715	Remove a part of QMS
$!#	HjV 930914	Add site ARECB
$!#
$!# General file handling
$!#       Use as: WNGFEX "type" nam1 nam2 action
$!#       Type can be:
$!#               SP      spool file nam1 as nam2
$!#               RE      rename file nam1 into nam2
$!#               CC      concatenate file nam1 onto nam2
$!#               LN      make logical link nam2 to nam1
$!#               RL      delete all .log, .tmp, .PLT or size == 0
$!#                               older than action (or 5) days
$!#               LR      combine LN and RL
$!#               QM      spool nam1 as nam2 to QMS plotter
$!#               PS      spool nam1 as nam2 to PS plotter
$!#               A3      spool nam1 as nam2 to A3-PS plotter
$!#               LA      spool nam1 as nam2 to LAser printer
$!#       Action is series of letters:
$!#               D       delete file after spooling and concatenation
$!#       or an unsigned value for RL/LR
$!#
$       VER=F$VERIFY(0)
$       APPEND="APPEND"                         !MAKE SURE
$       COPY="COPY"
$       DELETE="DELETE"
$       RENAME="RENAME"
$       P1=F$EDIT(P1,"UPCASE")
$       P4=F$EDIT(P4,"UPCASE")
$       IF P3 .EQS. "" THEN P3="''P2'"
$       A=F$SEARCH(P2)                          !SEE IF PRESENT
$       IF A .EQS. "" THEN GOTO EXIT
$       IF F$EXTRACT(0,2,P1) .EQS. "SP" THEN GOTO SPL
$       IF F$EXTRACT(0,2,P1) .EQS. "RE" THEN GOTO REN
$       IF F$EXTRACT(0,2,P1) .EQS. "CC" THEN GOTO CAT
$       IF F$EXTRACT(0,2,P1) .EQS. "LN" THEN GOTO LNK
$       IF F$EXTRACT(0,2,P1) .EQS. "RL" THEN GOTO REM
$       IF F$EXTRACT(0,2,P1) .EQS. "LR" THEN GOTO LRM
$       IF F$EXTRACT(0,2,P1) .EQS. "QM" THEN GOTO QMS
$       IF F$EXTRACT(0,2,P1) .EQS. "PS" THEN GOTO PSP
$       IF F$EXTRACT(0,2,P1) .EQS. "A3" THEN GOTO PA3
$       IF F$EXTRACT(0,2,P1) .EQS. "LA" THEN GOTO LAS
$ EXIT: ON ERROR THEN EXIT
$       VER=F$VERIFY(VER)
$       EXIT
$!#
$!# Spool
$!#
$ SPL:  B=F$EDIT(F$GETJPI("","USERNAME"),"TRIM") !USER NAME
$       C=F$PARSE(P3,,,"NAME","SYNTAX_ONLY")    !FILE NAME
$       D=F$PARSE(P3,,,"TYPE","SYNTAX_ONLY")    !FILE TYPE
$       ON ERROR THEN GOTO EXIT
$        COPY 'A' 'B'_'C''D'
$        IF P4-"D" .NES. P4
$        THEN
$          PRINT/DELETE 'B'_'C''D'
$        ELSE
$          PRINT 'B'_'C''D'
$        ENDIF
$       IF P4-"D" .NES. P4 THEN DELETE 'A'
$       GOTO EXIT
