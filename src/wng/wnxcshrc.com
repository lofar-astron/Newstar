$!# wnxcshrc.ssc
$!# WNB 920911
$!#
$!# Revisions:
$!#	WNB 921015	Change use of OLBEXE
$!#	WNB 921021	Add pa3
$!#	WNB 921215	Change edt alias
$!#	WNB 921224	Make SSC
$!#	WNB 930128	Change edt alias; directory change aliases
$!#	WNB 930901	Add nod
$!#	WNG 930921	Change _COD from symbol to logical (VMS)
$!#	WNB 931123	Make back directories consistent (Unix)
$!#	WNB 940216	Change nod; add ntd
$!#
$!#  Additional general definitions WNG package
$!#
$	IF P1 .NES. "ND"
$	THEN
$	  EDT=="EDI/EDT/COMMAND=WNGEDTINI.COM"
$	  LO*GOUT=="LOGOUT/BRIEF"
$	  SHD*EF=="SHOW DEFAULT"
$      DDIR*ECTORY=="DIRECT/DATE/SIZE/OWN/PROT/WIDTH=(OWN=8,FILENAME=18,SIZE=4)"
$	  DDEL*ETE=="DELETE/CONF"
$	  ND*IR=="@WNG:WNXCSHRC ND "
$	  PA3=="WNGFEX A3 "
$	  PEPS=="WNGFEX PS "
$	  PLAS=="WNGFEX LA "
$	  PQMS=="WNGFEX QM "
$	  PVAX=="WNGFEX SP "
$	  SET CONTROL=T
$	ELSE
$	  IF P2 .NES. ""
$	  THEN
$	    SUBNAME=P2
$	  ELSE
$	    INQUIRE SUBNAME Subname
$	  ENDIF
$	  IF SUBNAME .NES. ""
$	  THEN
$	    SET DEF WNG_OLBEXE:['SUBNAME']
$	    IF F$TRNLNM("NC_COD") .NES. "" THEN DEASSIGN NC_COD
$	    IF F$TRNLNM("NL_COD") .NES. "" THEN DEASSIGN NL_COD
$	    IF F$SEARCH("LOGIN.COM") .NES. ""
$	    THEN
$	      @LOGIN
$	    ENDIF
$	  ELSE
$	    SET DEF WNG_OLBEXE:[-]
$	    IF F$TRNLNM("NC_COD") .NES. "" THEN DEASSIGN NC_COD
$	    IF F$TRNLNM("NL_COD") .NES. "" THEN DEASSIGN NL_COD
$	  ENDIF
$	ENDIF
$ !
$	EXIT
