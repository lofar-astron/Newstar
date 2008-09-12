$!#  wngcshrc_rug.ssc
$!#  WNB 920911
$!# 
$!#  Revisions:
$!#       HjV 921124      Change for new situation in Groningen
$!#       WNB 921224      Make SSC
$!#       HjV 930226      Change WNG_LDFILES
$!#	HjV 930420	Change WNG_LDFILES, add gids_setup
$!#	HjV 930503	Remove WNG_LDFILES and gids_setup
$!#	HjV 930607	Change name of disk (dj3 iso. dj2)
$!#	HjV 930920	Add new DAT-devices
$!# 
$!#       Environment for all WN programs
$!#       Call by inserting in .cshrc as source $WNG/wngcshrc_rug.sun
$!# 
$       WNG_SITE=="RUG"
$       WNG_TYPE=="VX"
$       ASSIGN/NOLOG/TRANS=CONCEAL DU$GWS:[GWSX.WNB.] WNG_DIR:
$       ASSIGN/NOLOG WNG_DIR:[WNG] WNG          !GENERAL
$       ASSIGN/NOLOG WNG_DIR:[NSCAN] NSC
$       ASSIGN/NOLOG WNG_DIR:[NMAP] NMA
$       ASSIGN/NOLOG WNG_DIR:[NPLOT] NPL
$       ASSIGN/NOLOG WNG_DIR WNG_OLBEXE:
$       WNG_NODE=="RZMVX4.NFRA.NL"
$       WNG_NODEUSER=="PRINTVAX PRINTVAX_90A"
$       WNG_NODEDIR=="USER5:[WNB]"
$       @WNG:WNGCSHRC.COM
$ !
$       EXIT
