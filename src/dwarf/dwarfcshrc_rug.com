$!# dwarfcshrc_rug.ssc
$!#  HjV 930226
$!#
$!#  Revisions:
$!#	WNB 930302	Make ssc
$!#	HjV 930706	Change name of disk (dj3 iso. dj2)
$!#
$!#	Environment for DWARF programs
$!#	Call by sourcing in .cshrc (login.com)
$!#
$       ASSIGN/NOLOG/TRANS=CONCEAL DU$GWS:[GWSX.WNB.] ROOTDWARF:
$	ASSIGN/NOLOG ROOTDWARF:[EXE] SYSDWARF
$	ASSIGN/NOLOG ROOTDWARF:[EXE] EXEDWARF
$	ASSIGN/NOLOG ROOTDWARF:[DWARF] LIBDWARF
$	@ROOTDWARF:[DWARF]DWARFCSHRC
$	@ROOTDWARF:[DWARF]DWARF_ALIAS
