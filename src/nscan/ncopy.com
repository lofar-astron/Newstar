$ ! Created from ncopy.ssc on Wed Sep 1 08:35:08 METDST 1993 at rzmws5
$ ! 
$ ! 
$!  ncopy.ssc
$!  wnb 910930
$! 
$!   Revisions:
$! 	WNB 920913	Include NGCALC files; more options
$! 	WNB 921006	Make more general
$! 	WNB 921230	Make SSC
$! 	WNB 930901	Make sure aliases
$! 
$!  Copy and convert Newstar data from one machine to other
$! 
$! 	Uses environment: WNG_SITE
$! 
$!  Intro
$! 
$	ON CONTROL_Y THEN GOTO EXEX
$	SET NOVERIFY
$	IF P1 .NES. "" THEN SET VERIFY
$	TELL="WRITE SYS$OUTPUT"			!FOR EASE OF USE
$	DELETE="DELETE"				!FOR SAFETY
$	TELL " "
$	TELL "Copy files from other machine (TCPIP) and convert them."
$	TELL "                        Give empty answers to exit gracefully."
$	TELL " "
$! 
$!  Get data type
$! 
$ LA:	READ/TIME=90/END=LA1/ERROR=LA1 -
		/PROMPT="Type (model, scan, map, gcalc, mongo): " -
		SYS$COMMAND TYPE
$	IF TYPE .EQS. "" THEN GOTO LA1
$	TYPE=F$EDIT(TYPE,"UPCASE")+"XXX"
$	IF F$EXTRACT(0,3,TYPE) .EQS. "MOD" THEN TYPE="MODEL"
$	IF F$EXTRACT(0,3,TYPE) .EQS. "MON" THEN TYPE="MONGO"
$	IF F$EXTRACT(0,2,TYPE) .EQS. "MA" THEN TYPE="MAP"
$	IF F$EXTRACT(0,1,TYPE) .EQS. "S" THEN TYPE="SCAN"
$	IF F$EXTRACT(0,1,TYPE) .EQS. "G" THEN TYPE="GCALC"
$	IF F$LOCATE("XX",TYPE) .LT. F$LENGTH(TYPE)
$	THEN
$	  TELL "Unknown type"
$	  TELL " "
$	  GOTO LA
$	ENDIF
$! 
$!  Get machine info
$! 
$	READ/TIME=90/END=LA1/ERROR=LA1 -
		/PROMPT="Input machine (eg RZMALL): " -
		SYS$COMMAND VMACH
$	IF VMACH .EQS. "" THEN GOTO LA
$	VMACH=F$EDIT(VMACH,"UPCASE")
$! 
$!  Get file info
$! 
$	READ/TIME=90/END=LA1/ERROR=LA1 -
		/PROMPT="''VMACH' user (eg GER): " -
		SYS$COMMAND VUS
$	IF VUS .EQS. "" THEN GOTO LA
$	VUS=F$EDIT(VUS,"LOWERCASE")
$	READ/TIME=90/END=LA1/ERROR=LA1 -
		/PROMPT="''VMACH' full directory (eg /usr/ger/data): " -
		SYS$COMMAND VDIR
$	IF VDIR .EQS. "" THEN GOTO LA
$	VDIR=F$EDIT(VDIR,"LOWERCASE")
$ LC:	READ/TIME=90/END=LA1/ERROR=LA1 -
		/PROMPT="''VMACH' filename (eg NGC891.WMP): " -
		SYS$COMMAND VFIL
$	IF VFIL .EQS. "" THEN GOTO LA
$	VFIL=F$EDIT(VFIL,"UPCASE")
$	READ/TIME=90/END=LA1/ERROR=LA1 -
		/PROMPT="Full directory or empty if current: " -
		SYS$COMMAND ADAT
$	IF ADAT .EQS. "" THEN ADAT=F$ENVIRONMENT("DEFAULT")
$	ANOD="''VFIL'"
$	A=F$SEARCH("''ADAT'''ANOD'")
$	IF A .NES. ""
$	THEN
$	  READ/TIME=90/END=LA1/ERROR=LA1 -
		/PROMPT="Node exists. Delete it? (y,n) [n]: " -
		SYS$COMMAND ANS
$	  IF F$EXTRACT(0,1,F$EDIT(ANS,"UPCASE")) .NES. "Y" THEN GOTO LC
$	  DELETE 'A'
$	  TELL " "
$	  TELL "Node ''A' deleted"
$	ENDIF
$! 
$!  Transfer file
$! 
$	ON ERROR THEN GOTO EXEX
$	TELL " "
$	TELL "Transfering file. ''VMACH' password asked for"
$	TELL " "
$	SET TERM/NOECHO
$	READ/TIME=90/END=LA1/ERROR=LA1 -
		/PROMPT="Password: " -
		SYS$COMMAND PASSW
$	SET TERM/ECHO
$	TELL " "
$	QQQ="BINARY"
$	QQQ1="SET TYPE IMAGE"
$	IF TYPE .EQS. "MONGO"
$	THEN
$	  QQQ=""
$	  QQQ1="SET TYPE ASCII"
$	ENDIF
$	OPEN/WRITE/ERROR=LD2 FILE QQXY.TMP
$	IF WNG_SITE .EQS. "NFRA"
$	THEN
$	  WRITE/ERROR=LD2 FILE "$ FTP"
$	  WRITE/ERROR=LD2 FILE "CONNECT ''VMACH'"
$ 	  WRITE/ERROR=LD2 FILE "LOGIN ""''VUS'"""
$	  WRITE/ERROR=LD2 FILE "''PASSW'"
$	  WRITE/ERROR=LD2 FILE "''QQQ1'"
$	  WRITE/ERROR=LD2 FILE "SET DEF ""''VDIR'"""
$	  WRITE/ERROR=LD2 FILE "GET ''VFIL' ""''ADAT'''ANOD'"""
$	  WRITE/ERROR=LD2 FILE "EXIT"
$	  WRITE/ERROR=LD2 FILE "$ EXIT"
$	ENDIF
$	IF WNG_SITE .EQS. "ATNF"
$	THEN
$	  WRITE/ERROR=LD2 FILE "cd ""''VDIR'"""
$	  WRITE/ERROR=LD2 FILE "''QQQ'"
$	  WRITE/ERROR=LD2 FILE "get ''VFIL' ""''ADAT'''ANOD'"""
$	  WRITE/ERROR=LD2 FILE "quit"
$	ELSE					!RUG
$	  WRITE/ERROR=LD2 FILE "$ FTP -n ''VMACH'"
$ 	  WRITE/ERROR=LD2 FILE "LOGIN ""''VUS'"" ""''PASSW'"""
$	  WRITE/ERROR=LD2 FILE "cd ""''VDIR'"""
$	  WRITE/ERROR=LD2 FILE "''QQQ'"
$	  WRITE/ERROR=LD2 FILE "get ''VFIL' ""''ADAT'''ANOD'"""
$	  WRITE/ERROR=LD2 FILE "quit"
$	  WRITE/ERROR=LD2 FILE "$ EXIT"
$	ENDIF
$	CLOSE/ERROR=LD2 FILE
$	GOTO LD3
$ LD2:	CLOSE/ERROR=LD21 FILE
$ LD21:	TELL " "
$	TELL "Error creating intermediate file, abort."
$	TELL " "
$	GOTO EXEX
$ LD3:	ON ERROR THEN GOTO LD4
$	ASSIGN NL: SYS$OUTPUT
$	IF WNG_SITE .EQS "NFRA" .OR. WNG_SITE .EQS. "RUG"
$	THEN
$	  @QQXY.TMP
$	ELSE
$	  FTP/USER="''VUS'"/PASSW="''PASSW'"/TAKE_FILE=QQXY.TMP 'vmach'
$	ENDIF
$	IF (.NOT. $STATUS) THEN GOTO LD4
$	DEASSIGN SYS$OUTPUT
$	ON ERROR THEN GOTO EXEX
$	IF F$SEARCH(A) .NES. "" THEN GOTO LD5		!OK
$ LD4:	DEASSIGN SYS$OUTPUT
$	ON ERROR THEN GOTO EXEX
$	TELL " "
$	TELL " Error transferring file, abort."
$	TELL " "
$	GOTO EXEX
$ LD5:	TELL " "
$	TELL "Transfer completed, starting conversion."
$	TELL " "
$! 
$!  Conversion
$! 
$	OPEN/WRITE/ERROR=LE2 FILE QQXY.TMP
$	IF TYPE .EQS. "MODEL"
$	THEN
$	  WRITE/ERROR=LE2 FILE "$ DWE NMODEL/LOG=NO"
$	  WRITE/ERROR=LE2 FILE "CVX"
$	  WRITE/ERROR=LE2 FILE """@''ADAT'''ANOD'"""
$	  WRITE/ERROR=LE2 FILE "QUIT"
$	  WRITE/ERROR=LE2 FILE "$ EXIT"
$	ENDIF
$	IF TYPE .EQS. "MAP"
$	THEN
$	  WRITE/ERROR=LE2 FILE "$ DWE NMAP/LOG=NO"
$	  WRITE/ERROR=LE2 FILE "CVX"
$	  WRITE/ERROR=LE2 FILE """@''ADAT'''ANOD'"""
$	  WRITE/ERROR=LE2 FILE "QUIT"
$	  WRITE/ERROR=LE2 FILE "$ EXIT"
$	ENDIF
$	IF TYPE .EQS. "SCAN"
$	THEN
$	  WRITE/ERROR=LE2 FILE "$ DWE NSCAN/LOG=NO"
$	  WRITE/ERROR=LE2 FILE "CVX"
$	  WRITE/ERROR=LE2 FILE """@''ADAT'''ANOD'"""
$	  WRITE/ERROR=LE2 FILE "$ EXIT"
$	ENDIF
$	IF TYPE .EQS. "GCALC"
$	THEN
$	  WRITE/ERROR=LE2 FILE "$ DWE NGCALC/LOG=NO"
$	  WRITE/ERROR=LE2 FILE "CVX"
$	  WRITE/ERROR=LE2 FILE """@''ADAT'''ANOD'"""
$	  WRITE/ERROR=LE2 FILE "$ EXIT"
$	ENDIF
$	CLOSE/ERROR=LE2 FILE
$	GOTO LE3
$ LE2:	CLOSE/ERROR=LE21 FILE
$ LE21:	TELL " "
$	TELL "Error creating intermediate file, abort."
$	TELL " "
$	GOTO EXEX
$ LE3:	ON ERROR THEN GOTO LE4
$	ASSIGN NL: SYS$OUTPUT
$	@QQXY.TMP
$	IF (.NOT. $STATUS) THEN GOTO LE4
$	DEASSIGN SYS$OUTPUT
$	ON ERROR THEN GOTO EXEX
$	GOTO LE5					!OK
$ LE4:	DEASSIGN SYS$OUTPUT
$	ON ERROR THEN GOTO EXEX
$	TELL " Error converting file, abort."
$	TELL " "
$	GOTO EXEX
$ LE5:	DEASSIGN SYS$OUTPUT
$	TELL "Conversion of ''ADAT'''ANOD' finished."
$	TELL " "
$	GOTO EXEX
$! 
$!  Ready
$! 
$ LA1:	TELL "Exit requested"
$	TELL ""
$ EXEX:
$	IF F$SEARCH("QQXY.TMP") .NES. "" THEN DELETE QQXY.TMP;*
$	DEASSIGN SYS$OUTPUT
$	SET TERM/ECHO
$	EXIT
