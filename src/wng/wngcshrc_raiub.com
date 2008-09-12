$!#  wngcshrc_raiub.ssc
$!#  HjV 921106
$!# 
$!#  Revisions:
$!#	WNB 921224	Make SSC
$!#	HjV 930420	Change WNG_LDFILES, add gids_setup
$!#	HjV 930503	Remove WNG_LDFILES and gids_setup
$!# 
$!# 	Environment for all WN programs
$!#	Call by inserting in .cshrc as source wngcshrc_raiub.sun
$!# 
$	WNG_SITE=="RAIUB"
$	WNG_TYPE=="VX"
$	ASSIGN/NOLOG/TRANS=CONCEAL USER0:[WNB] WNG_DIR:  !NONSENS !!
$	ASSIGN/NOLOG WNG_DIR:[WNG] WNG		!GENERAL
$	ASSIGN/NOLOG WNG_DIR:[NSCAN] NSC
$	ASSIGN/NOLOG WNG_DIR:[NMAP] NMA
$	ASSIGN/NOLOG WNG_DIR:[NPLOT] NPL
$	ASSIGN/NOLOG WNG_DIR WNG_OLBEXE:
$	WNG_NODE=="RZMVX4.NFRA.NL"
$	WNG_NODEUSER=="PRINTVAX PRINTVAX_90A"
$	WNG_NODEDIR=="USER5:[WNB]"
$	@WNG:WNGCSHRC.COM
$ !
$	EXIT
