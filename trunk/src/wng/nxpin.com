$!#  nxpin.ssc
$!# 	WNB 921210
$!# 
$!#  Revisions:
$!#	WNB 921230	Creation message
$!#	WNB 921230	Make SSC
$!#	WNB 930105	HP error embedded newline
$!#	WNB 930429	Make awk for Unix and VMS
$!#	HjV 930518	Typo
$!#	WNB 930615	Delete .TMP propoerly
$!# 
$!# 	Convert a .psc (on standard in) to a pin file (on standard out)
$!#		by expanding includes. End given by single line "endend";
$!#		error messages given in efnm.
$!#	Use as:
$!#		csh -f $WNG/nxpin.sun efnm	(Unix)
$!#		@WNG:NXPIN psc pin error	(VAX)
$!#
$!#	Uses environment variables:
$!#		WNG			where to find wng-type sources
$!#
$!# Intro
$!#
$	ON ERROR THEN GOTO EXEX
$	PID=F$EXTRACT(4,4,F$GETJPI("","PID"))		!FOR FILES
$	DEP=F$ENVIRONMENT("DEPTH")
$       IF F$SEARCH("WNG:GAWK.EXE") .EQS. ""            !NO AWK PRESENT
$       THEN
$         NAWK="Y"                                      !NO AWK
$       ELSE
$         NAWK="$WNG:GAWK"                              !AWK PRESENT
$       ENDIF
$	CLOSE/ERROR=LA1 NXPI'PID''DEP'			!MAKE SURE
$ LA1:	CLOSE/ERROR=LA2 NXPE'PID''DEP'
$ LA2:
$	S1="!+ Created from ''P1' on ''F$TIME()' "+ -
		"at ''F$GETSYI("NODENAME")'"
$	S2="!-"
$!#
$!# Open files
$!#
$	IF NAWK
$	THEN
$	  OPEN/ERROR=ERR/READ NXPI'PID''DEP' 'P1'
$	  WRITE 'P2' "''S1'"
$	  WRITE 'P2' "''S2'"
$	ENDIF
$!#
$!# Read input
$!#
$	IF NAWK						!NO AWK
$	THEN
$ LP1:
$	  READ/ERROR=ERR/END=LP2 NXPI'PID''DEP' L0	!READ LINE
$	  L2=F$EDIT(L0,"COLLAPSE,UNCOMMENT,UPCASE")	!FOR CHECK
$	  IF F$EXTRACT(0,8,L2) .EQS. "INCLUDE="		!DO INCLUDE
$	  THEN
$	    L1=F$EXTRACT(8,-1,L2)			!FILE NAME
$	    IF F$SEARCH("''L1'") .EQS. ""		!NO FILE
$	    THEN
$	      IF F$SEARCH("''P3'") .EQS. ""
$	      THEN
$	        OPEN/ERROR=ERR/WRITE NXPE'PID''DEP' 'P3' !SET ERROR
$	      ELSE
$	        OPEN/ERROR=ERR/APPEND NXPE'PID''DEP' 'P3' !SET ERROR
$	      ENDIF
$	      WRITE NXPE'PID''DEP' "Cannot find include file ''L1'"
$	      CLOSE/ERROR=ERR NXPE'PID''DEP'
$	      GOTO EXEX
$	    ENDIF
$	    WRITE 'P2' "!!"
$	    WRITE 'P2' "!! Include ''L1'"
$	    WRITE 'P2' "!!"
$	    @WNG:NXPIN 'L1' 'P2' 'P3'
$	    WRITE 'P2' "!!"
$	    WRITE 'P2' "!! End include ''L1'"
$	    WRITE 'P2' "!!"
$	  ELSE
$	    WRITE 'P2' L0				!COPY
$	  ENDIF
$	  GOTO LP1					!MORE LINE
$	ELSE						!AWK PRESENT
$         IF F$SEARCH("NAWK''PID'''DEP'.TMP") .NES. "" THEN -
                        DELETE NAWK'PID''DEP'.TMP;*
$         OPEN/ERROR=ERR/WRITE NXPI'PID''DEP' NAWK'PID''DEP'.TMP !CREATE AWK FILE
$         WRITE NXPI'PID''DEP' -
	"function tinc() {"
$         WRITE NXPI'PID''DEP' -
		"if ($0 ~ /^[ 	]*[Ii][Nn][Cc][Ll][Uu][Dd][Ee][ 	]*=/) {"
$         WRITE NXPI'PID''DEP' -
		"split($2,nam,"" ""); cnt += 1; ofl[cnt] = cfl; cfl = nam[1];"
$         WRITE NXPI'PID''DEP' -
		"print ""!!""; print ""!! Include "",cfl; print ""!!"";"
$         WRITE NXPI'PID''DEP' -
		"err = getline <cfl; if (err != 1) {"
$         WRITE NXPI'PID''DEP' -
			"print ""Cannot open "",cfl >>""''P3'""; exit}"
$         WRITE NXPI'PID''DEP' -
		"do {tinc()} while (getline <cfl == 1) ;"
$         WRITE NXPI'PID''DEP' -
		"print ""!!""; print ""!! End include "",cfl; print ""!!"";"
$         WRITE NXPI'PID''DEP' -
		"close(cfl); cfl = ofl[cnt]; cnt -= 1} else {"
$         WRITE NXPI'PID''DEP' -
			"if ($0 != ""endend"") {print}}}"
$         WRITE NXPI'PID''DEP' -
	"BEGIN {FS = ""[=!]""; cnt = 0; cfl = FILENAME;"
$         WRITE NXPI'PID''DEP' -
		"print ""''S1'""; print ""''S2'"";}"
$         WRITE NXPI'PID''DEP' -
	"{tinc()}"
$	  CLOSE NXPI'PID''DEP'
$         NAWK/INPUT=NAWK'PID''DEP'.TMP/OUTPUT='P2' 'P1' !DO INCLUSION
$	ENDIF
$ !
$ LP2:	CLOSE/ERROR=EXEX NXPI'PID''DEP'			!CLOSE INPUT
$	GOTO EXEX
$!#
$!#  EXIT
$!#
$ ERR:
$	IF F$SEARCH("''P3'") .EQS. ""
$	THEN
$	  OPEN/ERROR=ERR/WRITE NXPE'PID''DEP' 'P3'	!SET ERROR
$	ELSE
$	  OPEN/ERROR=ERR/APPEND NXPE'PID''DEP' 'P3'	!SET ERROR
$	ENDIF
$	WRITE NXPE'PID''DEP' "Unexpected I/O error"
$	GOTO EXEX
$ EXEX:
$	CLOSE/ERROR=EX1 NXPI'PID''DEP'			!MAKE SURE
$ EX1:	CLOSE/ERROR=EX2 NXPE'PID''DEP'
$ EX2:
$       IF F$SEARCH("NAWK''PID'''DEP'.TMP") .NES. "" THEN -
                DELETE NAWK'PID''DEP'.TMP;*
$	EXIT
