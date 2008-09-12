$!#  wngcshrc_nfra.ssc
$!#  WNB 920911
$!# 
$!#  Revisions:
$!#       WNB 921022      Add magtapes
$!#       HJV 921201      Hostname Alliant in uppercase
$!#       WNB 921224      Make SSC
$!#       HjV 930226      Add HP workstations
$!#	HjV 930420	Change WNG_LDFILES, add gids_setup
$!#	HjV 930503	Remove WNG_LDFILES and gids_setup
$!#	HjV 930607	Do not use ~ anymore, use full pathname
$!#	HjV 930621	Change test HOSTTYPE for HP and SUN
$!#	CMV 930721	Add LD_LIBRARY_PATH for SUN
$!#	CMV 930805	Some temporary changes: documentation programs
$!# 
$!#       Environment for all WN programs
$!#       Call by inserting in .cshrc as source wngcshrc_nfra.sun
$!# 
$       WNG_SITE=="NFRA"
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
$       IF F$TRNLNM("MAG0") .EQS. "" THEN -
		ASSIGN/NOLOG/TRANS=CONCEAL RZMVX4$MUB0: MAG0
$       IF F$TRNLNM("MAG1") .EQS. "" THEN -
		ASSIGN/NOLOG/TRANS=CONCEAL RZMVX4$MUC0: MAG1
$       IF F$TRNLNM("MAG4") .EQS. "" THEN -
		ASSIGN/NOLOG/TRANS=CONCEAL RZMVX4$MUA0: MAG4
$       IF F$TRNLNM("MAG5") .EQS. "" THEN -
		ASSIGN/NOLOG/TRANS=CONCEAL RZMVX4$MUA1: MAG5
$       IF F$TRNLNM("MAG9") .EQS. "" THEN -
		ASSIGN/NOLOG/TRANS=CONCEAL RZMVX5$MUA0: MAG9
$       IF F$TRNLNM("MAG8") .EQS. "" THEN -
		ASSIGN/NOLOG/TRANS=CONCEAL RZMVX5$MKA500: MAG8
$       IF F$TRNLNM("MAG7") .EQS. "" THEN -
		ASSIGN/NOLOG/TRANS=CONCEAL RZMSUR$MUA0: MAG7
$       @WNG:WNGCSHRC.COM
$ !
$       EXIT
