$!#  nget.ssc
$!# 	WNB 920908
$!# 
$!#  Revisions:
$!#	WNB 921002	Overhaul
$!#	WNB 921019	Suppress ar messages
$!#	WNB 921130	Cater for long names
$!#	WNB 921208	Add log; -a0 switch
$!#	HjV 921229	Make SSC; HP got only 14 char. in library
$!#	WNB 921230	Correct HP test
$!#	WNB 930325	Cater for different fold
$!#	WNB 930330	Add .A.. and .X..
$!# 
$!# 	Get file from text library. Use as:
$!#
$!#		source $WNG/nget.sun	(Unix)
$!#		@WNG:NGET <file>	(VAX)
$!#
$!#	The command file uses many local nxec variables, and
$!#	environment variables: WNG, WNG_TLB, WNG_TYPE
$!#	command files:
$!#
$	ON ERROR THEN GOTO ERR
$	L0=F$SEARCH("''FNM'''FTP'")			!FOR REFERENCE
$	IF CD_A .NES. "0" .AND. L0 .NES. "" THEN GOTO EXIT !DO NOT DO
$	SET MESSAGE /NOIDEN/NOFACIL/NOSEVER/NOTEXT
$	SET NOON
$	ASSIGN/USER NL: SYS$OUTPUT
$	IF F$EXTRACT(1,1,FTP) .EQS. "A" .OR. -
		F$EXTRACT(1,1,FTP) .EQS. "X"
$	THEN
$	  LIBRARY/TEXT/EXTRACT='FNM''FTP'/OUT='FNM''FTP' 'WNG_TLB''L_D'_AX.TLB
$	ELSE
$	  LIBRARY/TEXT/EXTRACT='FNM''FTP'/OUT='FNM''FTP' 'WNG_TLB''L_D'.TLB
$	ENDIF
$	SET ON
$	SET MESSAGE /IDEN/FACIL/SEVER/TEXT
$	L1=F$SEARCH("''FNM'''FTP'")			!SEE IF DONE
$	IF L1 .EQS. "" .OR. L0 .EQS. L1 THEN GOTO ERR	!NOT DONE
$	IF F$EXTRACT(1,1,FTP) .EQS. "A" .OR. -
		F$EXTRACT(1,1,FTP) .EQS. "X"
$	THEN
$	  MSGT=MSGT+" got [''F$PARSE(L_D,,,"NAME","SYNTAX_ONLY")'_AX.TLB]"
$	ELSE
$	  MSGT=MSGT+" got [''F$PARSE(L_D,,,"NAME","SYNTAX_ONLY")'.TLB]"
$	ENDIF
$	GOTO ERR1
$!#
$!# Ready
$!#
$ !
$ ERR:	B1="Not:  "
$ ERR1:	TELL B1+FNM+FTP+MSGT
$	UTELL B1+FNM+FTP+MSGT
$ EXIT:	EXIT
