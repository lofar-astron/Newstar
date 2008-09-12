$!#  dwarfcshrc_nfra.ssc
$!#  HjV 930226
$!# 
$!#  Revisions:
$!#	WNB 930302	Delete some
$!#	HjV 930621	Change test HOSTTYPE for HP and SUN
$!#	CMV 930721	Add test on existence of HOSTTYPE
$!# 
$!# 	Environment for DWARF programs
$!#	Call by inserting in .cshrc as source dwarfcshrc_nfra.sun
$!# 
$	ASSIGN/NOLOG/TRANS=CONCEAL USER5:[WNB.] ROOTDWARF:
$	ASSIGN/NOLOG ROOTDWARF:[EXE] SYSDWARF
$	ASSIGN/NOLOG ROOTDWARF:[EXE] EXEDWARF
$	ASSIGN/NOLOG ROOTDWARF:[DWARF] LIBDWARF
$	@ROOTDWARF:[DWARF]DWARFCSHRC
$	@ROOTDWARF:[DWARF]DWARF_ALIAS
$ !
$	EXIT
