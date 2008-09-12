$!+
$!  newstar_env.com - make sure general settings have been made
$!
$!  Revisions:
$!	WNB 940311	Make COM
$!       940314 HjV      When HOST contains dots, take first part.
$!       940322 HjV      Make contents HOST, HOSTTYPE lowercase
$!
$!  This script sets HOSTTYPE, HOST and USER if they are not yet defined
$!-
$!
$!  Init
$!
$	DEFINE="DEFINE"
$!
$!  Define the name of the current host
$!
$	IF F$TRNLNM("N_HOST") .EQS. "" THEN -
	    DEFINE/NOLOG N_HOST "''F$EDIT(F$GETSYI("NODENAME"),"LOWERCASE")'"
$!  remove part after dot
$!  set to lowercase
$!
$!  Define the hosttype (all these crazy paths taken from cshrc.csh@rug)
$!
$	IF F$TRNLNM("HOSTTYPE") .EQS. "" THEN -
		DEFINE/NOLOG HOSTTYPE VAX
$	ARCH==F$TRNLNM("HOSTTYPE")		! Make sure we have an arch
$!
$!  Set username
$!
$	IF F$TRNLNM("USER") .EQS. "" THEN -
		DEFINE/NOLOG USER 'F$GETJPI("","USERNAME")'
$!
$!  Make sure some domainname is given
$!
$	DEFINE/NOLOG DOMAINNAME "''F$TRNLNM("UCX$BIND_DOMAIN")'"
$	IF F$TRNLNM("DOMAINNAME") .EQS. "" THEN -
		DEFINE/NOLOG DOMAINNAME "''F$TRNLNM("MULTINET_LOCALDOMAIN")'"
$	IF F$TRNLNM("DOMAINNAME") .EQS. "" THEN -
		DEFINE/NOLOG DOMAINNAME "''F$TRNLNM("HOST")'"
$!
$!  Ready
$!
$	EXIT
