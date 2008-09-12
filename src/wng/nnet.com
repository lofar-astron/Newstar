$!#  nnet.ssc
$!# 	WNB 920908
$!# 
$!#  Revisions:
$!#	WNB 920922	Use noglob
$!#			Redirect ftp output
$!#	WNB 921002	Overhaul
$!#	WNB 921012	Add node to text
$!#	WNB 921014	Typo
$!#	WNB 921208	Add log
$!#	WNB 921222	Change chmod
$!#	WNB 921230	Make SSC
$!#	WNB 930330	Add .A.. and .X..
$!#	HjV 930630	Add site KOSMA (Multinet)
$!# 
$!# 	Get files across net. Use as:
$!#
$!#		source $WNG/nnet.sun	(Unix)
$!#		@WNG:NNET <file>	(VAX)
$!#
$!#	The command file uses many local nxec variables, and
$!#	environment variables: WNG, WNG_NODE, WNG_NODEUSER, WNG_NODEDIR
$!#	command files:
$!#
$!#
$!#  Get a file across net
$!#
$	ON ERROR THEN GOTO ERR
$	DOFTP="FTP"					!FOR SAFETY
$	L0=F$SEARCH("''FNM'''FTP'")			!FOR REFERENCE
$	ON ERROR THEN GOTO ERR
$	OPEN/WRITE/ERROR=ERR FILE FN'PID''DEP'.TMP
$	L1=F$ELEMENT(0," ",WNG_NODEUSER)		!USER
$	L2=F$ELEMENT(1," ",WNG_NODEUSER)		!PASSWORD
$	L3=CWDT-"["-"]"					!CURRENT DIRECTORY
$	IF F$LOCATE(":",WNG_NODEDIR) .EQ. F$LENGTH(WNG_NODEDIR)
$	THEN						!UNIX
$	  L3=WNG_NODEDIR+"/"+F$EDIT(L3,"LOWERCASE")	!FOREIGN DIRECTORY
$	ELSE						!VMS
$	  L3=WNG_NODEDIR-"]"+"."+L3+"]"			!FOREIGN DIRECTORY
$	ENDIF
$	L4=F$EDIT("''FNM'''FTP'","LOWERCASE")		!FOR UNIX
$	IF WNG_SITE .EQS. "NFRA"
$	THEN
$	  WRITE/ERROR=ERR FILE "$ DOFTP=""FTP"""
$	  WRITE/ERROR=ERR FILE "$ DOFTP"
$	  WRITE/ERROR=ERR FILE "CONNECT ''WNG_NODE'"
$ 	  WRITE/ERROR=ERR FILE "LOGIN ""''L1'"""
$	  WRITE/ERROR=ERR FILE "''L2'"
$	  WRITE/ERROR=ERR FILE "SET DEF ""''L3'"""
$	  IF F$EXTRACT(1,1,FTP) .EQS. "A" .OR. -
		F$EXTRACT(1,1,FTP) .EQS. "X"
$	  THEN
$	    WRITE/ERROR=ERR FILE "SET TYPE IMAGE"
$	  ENDIF
$	  WRITE/ERROR=ERR FILE "GET ""''L4'"""
$	  WRITE/ERROR=ERR FILE "EXIT"
$	  WRITE/ERROR=ERR FILE "$ EXIT"
$	ELSE
$	  IF WNG_SITE .EQS. "ATNF" .OR. WNG_SITE .EQS. "KOSMA"
$	  THEN
$	    WRITE/ERROR=ERR FILE "cd ""''L3'"""
$	    IF F$EXTRACT(1,1,FTP) .EQS. "A" .OR. -
		F$EXTRACT(1,1,FTP) .EQS. "X"
$	    THEN
$	      WRITE/ERROR=ERR FILE "binary"
$	    ENDIF
$	    WRITE/ERROR=ERR FILE "get ""''L4'"" ''FNM'''FTP'"
$	    WRITE/ERROR=ERR FILE "quit"
$	  ELSE					!RUG
$	    WRITE/ERROR=ERR FILE "$ DOFTP=""FTP"""
$	    WRITE/ERROR=ERR FILE "$ DOFTP -n ''WNG_NODE'"
$ 	    WRITE/ERROR=ERR FILE "login ""''L1'"" ""''L2'"""
$	    WRITE/ERROR=ERR FILE "cd ""''L3'"""
$	    IF F$EXTRACT(1,1,FTP) .EQS. "A" .OR. -
		F$EXTRACT(1,1,FTP) .EQS. "X"
$	    THEN
$	      WRITE/ERROR=ERR FILE "binary"
$	    ENDIF
$	    WRITE/ERROR=ERR FILE "get ""''L4'"" ''FNM'''FTP'"
$	    WRITE/ERROR=ERR FILE "quit"
$	    WRITE/ERROR=ERR FILE "$ EXIT"
$	  ENDIF
$	ENDIF
$	CLOSE/ERROR=ERR FILE
$	SET NOON
$	ASSIGN/USER NL: SYS$OUTPUT
$	IF WNG_SITE .EQS "NFRA" .OR. WNG_SITE .EQS. "RUG"
$	THEN
$	  @FN'PID''DEP'.TMP
$	ELSE
$	  DOFTP/USER="''L1'"/PASSW="''L2'"/TAKE=FN'PID''DEP'.TMP -
				'WNG_NODE'
$	ENDIF
$	IF .NOT. $STATUS THEN GOTO ERR
$	SET ON
$	L1=F$SEARCH("''FNM'''FTP'")			!SEE IF DONE
$	IF L1 .EQS. "" .OR. L0 .EQS. L1 THEN GOTO ERR	!NOT DONE
$	MSGT=MSGT+" obtained [''WNG_NODE']"
$	GOTO ERR1
$!#
$!# Ready
$!#
$ ERR:	B1="Not:  "
$ ERR1:	TELL B1+FNM+FTP+MSGT
$	UTELL B1+FNM+FTP+MSGT
$	IF F$SEARCH("FN''PID'''DEP'.TMP") .NES. "" THEN -
			DELETE FN'PID''DEP'.TMP;*	!MAKE SURE
$	EXIT
