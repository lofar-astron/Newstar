$!#  wngcshrc.ssc
$!#  WNB 920911
$!# 
$!#  Revisions:
$!#	WNB 920917	Typo WNGFEX
$!#	WNB 921012	Solve symbol conflict and typo
$!#	HJV 921001	Change N-series to Newstar
$!#	WNB 921006	Typo NGET
$!#	WNB 921224	Make SSC
$!#	WNB 930301	Move path to wnglogin.sun
$!#	WNB 930303	Add NSTAR_DIR
$!#	HjV 930414	Typo NSTAR_DIR
$!#	WNB 931217	Add NCOPY to NSTAR_DIR
$!# 
$!# 	Environment for all WN programs
$!#	Call by inserting in .cshrc as source wngcshrc.sun
$!# 
$	WNGTYP=="C''WNG_TYPE'"
$	NXEC=="@WNG:NXEC"
$	NC*OMP=="@WNG:NXEC NC"
$	NDEL=="@WNG:NXEC ND"
$	NG*ET=="@WNG:NXEC NG"
$	NL*INK=="@WNG:NXEC NL"
$	NNET=="@WNG:NXEC NN"
$	NX*REF=="@WNG:NXEC NX"
$	NN*EWS=="HELP/PAGE/LIBRARY=WNG:NNEWS NNEWS"
$ !
$	WNGFEX=="@WNG:WNGFEX"				!PROGRAM FILE HANDLING
$	NCOPY=="@NSC:NCOPY"				!FOR DATA COPY
$	DWE*XECUTE=="@WNG:DWEXE"			!SPECIAL DWARF EXECUTE
$	ASSIGN/NOLOG WNG:WNG.DEF WNG_DEF		!FOR COMPILATIONS
$	NSTAR_DIR=="WNG,DWARF,NSCAN,NCOPY,NMAP,NPLOT"	!N DIRECTORIES
$ !
$	WRITE SYS$OUTPUT "Type nnews for Newstar news"
$ !
$	EXIT
