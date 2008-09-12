$!#  dwarfcshrc_arecb.ssc
$!#  HjV 930914
$!# 
$!#  Revisions:
$!# 
$!# 	Environment for DWARF programs
$!#	Call by inserting in .cshrc as source dwarfcshrc_arecb.sun
$!# 
$	ASSIGN/NOLOG/TRANS=CONCEAL USER5:[WNB.] ROOTDWARF:
$	ASSIGN/NOLOG ROOTDWARF:[EXE] SYSDWARF
$	ASSIGN/NOLOG ROOTDWARF:[EXE] EXEDWARF
$	ASSIGN/NOLOG ROOTDWARF:[DWARF] LIBDWARF
$	@ROOTDWARF:[DWARF]DWARFCSHRC
$	@ROOTDWARF:[DWARF]DWARF_ALIAS
$ !
$	EXIT
