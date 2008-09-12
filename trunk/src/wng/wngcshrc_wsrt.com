$!#  wngcshrc_wsrt.ssc
$!#  HjV 930120
$!# 
$!#  Revisions:
$!#	HjV 930420	Change WNG_LDFILES, add gids_setup
$!#	HjV 930503	Remove WNG_LDFILES and gids_setup
$!#	HjV 930527	Change WNG_NODE and MAG8
$!# 
$!# 	Environment for all WN programs
$!#	Call by inserting in .cshrc as source wngcshrc_wsrt.sun
$!# 
$	WNG_SITE=="WSRT"
$	WNG_TYPE=="VX"
$	ASSIGN/NOLOG/TRANS=CONCEAL USER5:[WNB.] WNG_DIR:
$	ASSIGN/NOLOG WNG_DIR:[WNG] WNG		!GENERAL
$	ASSIGN/NOLOG WNG_DIR:[NSCAN] NSC
$	ASSIGN/NOLOG WNG_DIR:[NMAP] NMA
$	ASSIGN/NOLOG WNG_DIR:[NPLOT] NPL
$	ASSIGN/NOLOG WNG_DIR WNG_OLBEXE:
$	WNG_NODE=="192.87.1.105"
$	WNG_NODEUSER=="PRINTVAX PRINTVAX_90A"
$	WNG_NODEDIR=="USER5:[WNB]"
$	@WNG:WNGCSHRC.COM
$ !
$	EXIT
