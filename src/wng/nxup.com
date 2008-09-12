$!# nxup.ssc
$!# WNB 920909
$!#
$!# Revisions:
$!#	WNB 921016	end if typo; directory typo
$!#	WNB 921113	Preserve space
$!#	WNB 921122	Delete .udf
$!#	WNB 921208	Check EXEDWARF
$!#	WNB 921215	Change to mv
$!#	WNB 921224	Make SSC
$!#	WNB 930108	Change order strip, mv
$!#	HjV 930623	Change cp into mv for saving active executable
$!#
$!#	Update in DWARF system. Use as:
$!#	  $WNG/nxup.sun <type> <dwarflib> <full name> <file name> <logtext>
$!#			type=1	(copy full name file)
$!#			    =2	(put file name in OLB)
$!#			    =3	put file name.ppd in exedwarf
$!#			    =4	put file name.exe in exedwarf
$!#			    =5  (put full name in HLB)
$!#			    =6	(put file name.udf in exedwarf)
$!#			    =a* delete i.s.o. put
$!#
$!#		Uses environment variable EXEDWARF_UNIX
$!#
$!# Note: Only .exe, .ppd supported
$!#
$	IF F$TRNLNM("EXEDWARF") .EQS. "" THEN GOTO EXIT !CANNOT DO
$	FNM=F$PARSE(P3,,,"NAME","SYNTAX_ONLY")	!FILE NAME
$	FTP=F$PARSE(P3,,,"TYPE","SYNTAX_ONLY")	!FILE TYPE
$	FMD=F$PARSE(P3,,,"DIRECTORY","SYNTAX_ONLY") !FIRST DIRECTORY
$	FMD=FMD-"["-F$EXTRACT(F$LOCATE(".",FMD),F$LENGTH(FMD),FMD)-"]"
$	FNV=F$EXTRACT(0,F$LOCATE(";",P3),P3)	!FULL NAME, NO VERSION
$	IF P1-"A" .NES. P1 THEN GOTO DEL	!DELETE
$ !
$ ! Copy PPD
$ !
$ PPD:	IF P1-"3" .EQS. P1 THEN GOTO EXE	!NO PPD
$	COPY 'P3' EXEDWARF:'P4'.PPD
$	PURGE EXEDWARF:'P4'.PPD/KEEP=2
$	IF F$SEARCH("''P3'") .NES. "" THEN DELETE/NOLOG 'P3';* !SAVE SPACE
$ !
$ ! Copy EXE
$ !
$ EXE:	IF P1-"4" .EQS. P1 THEN GOTO HLB	!NO EXE
$	COPY 'P3' EXEDWARF:'P4'.EXE
$	PURGE EXEDWARF:'P4'.EXE/KEEP=2
$	IF F$SEARCH("''P3'") .NES. "" THEN DELETE/NOLOG 'P3';* !SAVE SPACE
$ !
$ ! Copy to .HLB
$ !
$ HLB:	IF P1-"5" .EQS. P1 THEN GOTO EXIT	!NO HLB
$	IF F$TRNLNM("LIBDWARF") .EQS. "" THEN GOTO EXIT
$	IF F$SEARCH("LIBDWARF:''P4'.HLB") .EQS. "" THEN -
		LIBRARY/CREATE/HELP LIBDWARF:'P4'.HLB !CREATE .HLB LIBRARY
$	LIBRARY/HELP LIBDWARF:'P4'.HLB 'P3'	!SET IN HLB
$ !
$ ! Ready
$ !
$ EXIT:
$ EXT1:
$	EXIT
$ !
$ ! Delete
$ !
$ DEL:
$ !
$ ! Copy PPD
$ !
$ DPD:	ON ERROR THEN EXIT
$	IF P1-"3" .EQS. P1 THEN GOTO DXE	!NO PPD
$	IF F$SEARCH("EXEDWARF:''P4'.PPD") .NES. "" THEN -
		DELETE/NOLOG EXEDWARF:'P4'.PPD;*
$ !
$ ! Copy EXE
$ !
$ DXE:	IF P1-"4" .EQS. P1 THEN GOTO HDB	!NO EXE
$	IF F$SEARCH("EXEDWARF:''P4'.EXE") .NES. "" THEN -
		DELETE/NOLOG EXEDWARF:'P4'.EXE;*
$ !
$ ! Copy to .HLB
$ !
$ HDB:	IF P1-"5" .EQS. P1 THEN GOTO EXIT	!NO HLB
$	IF F$TRNLNM("LIBDWARF") .EQS. "" THEN GOTO EXIT
$	IF F$SEARCH("LIBDWARF:''P2'.HLB") .NES. "" THEN -
		DELETE/NOLOG LIBDWARF:'P2'.HLB;*
$	GOTO EXIT
