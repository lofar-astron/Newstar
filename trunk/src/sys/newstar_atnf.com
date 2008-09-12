$!  newstar_atnf.com
$!
$!  Local startup for Newstar (HjV 931007)
$!  Revision:
$!	CMV 931201  Split off newstar_env.csh
$!	HjV 940125  Change n_root
$!	WNB 940621  Make COM
$!
$!+
$!       Institute:     Australian Telescope National Facility
$!       Address:       P.O. Box 76
$!			Epping NSW2121
$!			Australia
$!	Contact person: Wim Brouw
$! 	Email address:  wbrouw@atnf.csiro.au
$!	FTP-node(s):    norma.atnf.csiro.au  (DW)
$!			venice.atnf.csiro.au (SW and many more)
$!			ateles.atnf.csiro.au (CV)
$!			robin.atnf.csiro.au  (VX)
$!	Phone:          +(61)2.3724316
$!-
$!
$!  Define the name of this site
$!
$	DEFINE/NOLOG N_SITE atnf
$	DEFINE/NOLOG N_INSTALL "dw/sw/cv/vx"
$	DEFINE/NOLOG N_HOSTS "norma,venice,ateles,robin"
$!
$!  Define the root of the Newstar directory tree
$!
$  	DEFINE/NOLOG/TRANS=CONCEAL N_ROOT "UTIL0:[BOOK.WBROUW.WNB.NSTAR.]"
$!
$!  Make sure we have the standard settings (HOSTTYPE etc)
$!
$	@n_root:[src.sys]newstar_env
$!
$!  Any non-standard environment settings should be made here
$!
$!
$!  Now do the general setup
$!
$	@N_ROOT:[SRC.SYS]NEWSTAR_INIT
$!
$!  Now we may wish to change anything we do not like
$!
$	DEFINE/NOLOG "n_src" "/n_root/src"		! For genaid
$	DEFINE/NOLOG N_LIB "N_ROOT:[LIB]"
$	DEFINE/NOLOG N_EXE "N_ROOT:[EXE]"
$	DEFINE/NOLOG N_IMPORT "N_ROOT:[TEST]"
$	DEFINE/NOLOG N_HLP "N_ROOT:[EXE.HTML]"
$       IF F$TRNLNM("MAG0") .EQS. "" THEN -
                ASSIGN/NOLOG/TRANS=CONCEAL ROBIN$MSA0: MAG0
$       IF F$TRNLNM("MAG1") .EQS. "" THEN -
                ASSIGN/NOLOG/TRANS=CONCEAL ROBIN$MUB0: MAG1
$       IF F$TRNLNM("MAG9") .EQS. "" THEN -
                ASSIGN/NOLOG/TRANS=CONCEAL ROBIN$MUC0: MAG9
$!
$!  Ready
$!
$	EXIT
