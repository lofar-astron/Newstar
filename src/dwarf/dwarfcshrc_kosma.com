$!#  dwarfcshrc_kosma.ssc
$!#  HjV 930630
$!# 
$!#  Revisions:
$!#	HjV 930720	Typo's
$!# 
$!# 	Environment for DWARF programs
$!#	Call by inserting in .cshrc as source dwarfcshrc_kosma.sun
$!# 
$	ASSIGN/NOLOG/TRANS=CONCEAL APOLLO_UTILDSK:[NEWSTAR.] ROOTDWARF:
$	ASSIGN/NOLOG ROOTDWARF:[SVX.EXE] SYSDWARF
$	ASSIGN/NOLOG ROOTDWARF:[SVX.EXE] EXEDWARF
$	ASSIGN/NOLOG ROOTDWARF:[DWARF] LIBDWARF
$	@ROOTDWARF:[DWARF]DWARFCSHRC
$	@ROOTDWARF:[DWARF]DWARF_ALIAS
$ !
$	EXIT
