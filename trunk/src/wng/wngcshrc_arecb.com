$!#  wngcshrc_arecb.ssc
$!#  HjV 930914
$!# 
$!#  Revisions:
$!# 
$!#       Environment for all WN programs
$!#       Call by inserting in .cshrc as source wngcshrc_arecb.sun
$!#
$!#	Institute:	ARECIBO Observatory
$!#	Address:	P.O. Box 995, Arecibo
$!#			Puerto Rico 00613, USA
$!#	Contact person:	Tapasi Ghosh
$!#	Email address:	tghosh@naic.edu
$!#	FTP-node(s):	192.65.176.4
$!#	Phone:		(1)-809-878-2612
$!# 
$       WNG_SITE=="ARECB"
$       WNG_TYPE=="VX"
$       ASSIGN/NOLOG/TRANS=CONCEAL USER5:[WNB.] WNG_DIR:
$       ASSIGN/NOLOG WNG_DIR:[WNG] WNG          !GENERAL
$       ASSIGN/NOLOG WNG_DIR:[NSCAN] NSC
$       ASSIGN/NOLOG WNG_DIR:[NMAP] NMA
$       ASSIGN/NOLOG WNG_DIR:[NPLOT] NPL
$       ASSIGN/NOLOG WNG_DIR WNG_OLBEXE:
$       WNG_NODE=="RZMVX4"
$       WNG_NODEUSER=="PRINTVAX PRINTVAX_90A"
$       WNG_NODEDIR=="USER5:[WNB]"
$       @WNG:WNGCSHRC.COM
$ !
$       EXIT
