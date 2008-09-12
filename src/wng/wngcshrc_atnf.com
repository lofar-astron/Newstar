$!#  wngcshrc_atnf.ssc
$!#  WNB 920911
$!# 
$!#  Revisions:
$!#	WNB 921015	Add WNG_EXE
$!#	WNB 921022	Add MAG tapes
$!#	WNB 921224	Make SSC
$!#	WNB 930303	Test machine type
$!#	WNB 940124	Change directories; add _OLB _TLB
$!#	WNB 940216	Change OLB directories
$!# 
$!# 	Environment for all WN programs
$!#	Call by inserting in .cshrc as source wngcshrc_nfra.sun
$!# 
$	WNG_SITE=="ATNF"
$	WNG_TYPE=="VX"
$	ASSIGN/NOLOG/TRANS=CONCEAL UTIL0:[BOOK.WBROUW.WNB.] WNG_DIR:
$	ASSIGN/NOLOG WNG_DIR:[WNG] WNG			!GENERAL
$	ASSIGN/NOLOG WNG_DIR:[NSCAN] NSC
$	ASSIGN/NOLOG WNG_DIR:[NMAP] NMA
$	ASSIGN/NOLOG WNG_DIR:[NPLOT] NPL
$	ASSIGN/NOLOG WNG_DIR WNG_OLBEXE:
$	WNG_NODE=="norma"
$	WNG_NODEUSER=="wbrouw"
$	WNG_NODEDIR=="/code_norma/nstar"
$	IF F$TRNLNM("MAG0") .EQS. "" THEN -
		ASSIGN/NOLOG/TRANS=CONCEAL ROBIN$MSA0: MAG0
$	IF F$TRNLNM("MAG1") .EQS. "" THEN -
		ASSIGN/NOLOG/TRANS=CONCEAL ROBIN$MUB0: MAG1
$	IF F$TRNLNM("MAG9") .EQS. "" THEN -
		ASSIGN/NOLOG/TRANS=CONCEAL ROBIN$MUC0: MAG9
$	@WNG:WNGCSHRC.COM
$ !
$	EXIT
