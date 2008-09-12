$!#  dwarfcshrc_atnf.ssc
$!#  WNB 920915
$!# 
$!#  Revisions:
$!#	WNB 930302	Make ssc
$!#	WNB 940124	Change directories
$!#	WNB 940216	Change LIBDWARF directory
$!# 
$!# 	Environment for DWARF programs
$!#	Call by sourcing in .cshrc (login.com) dwarfcshrc_atnf.sun
$!# 
$	ASSIGN/NOLOG/TRANS=CONCEAL UTIL0:[BOOK.WBROUW.WNB.] ROOTDWARF:
$	ASSIGN/NOLOG ROOTDWARF:[EXE] SYSDWARF
$	ASSIGN/NOLOG ROOTDWARF:[EXE] EXEDWARF
$	ASSIGN/NOLOG ROOTDWARF:[DWARF] LIBDWARF
$	@ROOTDWARF:[DWARF]DWARFCSHRC
$	@ROOTDWARF:[DWARF]DWARF_ALIAS
$ !
$	EXIT
