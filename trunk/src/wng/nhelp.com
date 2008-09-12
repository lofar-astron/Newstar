$!#  nhelp.ssc
$!# 	WNB 920908
$!# 
$!#  Revisions:
$!#	WNB 920922	Add .fun
$!#	WNB 921001	Overhaul
$!#	WNB 921014	Add -a nnet
$!#	WNB 921117	Add regular expressions
$!#	WNB 921122	Delete .uin
$!#	WNB 921208	Add update note; -a switch
$!#	WNB 921211	Add QP
$!#	WNB 921216	Add ##
$!#	WNB 921218	Add FSC etc
$!#	WNB 921230	Make SSC
$!#	WNB 930303	No more DWARF share
$!#	WNB 930330	Add .A.. and .X..
$!#	WNB 930803	Add .dsf
$!# 
$!#	Help text for nxec commands. Used from nxec as:
$!#		csh -f nhelp.sun tp typ ext pcod pnam chtp	(Unix)
$!#		@WNG:NHELP tp typ				(VAX)
$!#	where:
$!#		tp=	1 help on command  2 on codes  3 on filenames
$!#		typ=	nxec given type
$!#		ext=	current machine extension (e.g. dw)
$!#		pcod=	program code (e.g. nc)
$!#		pnam=	program name (e.g. ncomp)
$!#		chtp=	list of extension to be bypassed
$!#
$!#	Also uses the environment variables WNG and *Q_D
$!#
$!# Intro
$!#
$	TYP=P2					! TYPE
$	GOTO H'P1'				! DISTRIBUTE
$!#
$!# Command level
$!#
$ H1:	TELL ""
$	TELL "Unknown NXEC type (''TYP')"
$	TELL " "
$	TELL "The @WNG:NXEC command can compile, link and maintain all sorts of"
$	TELL "files, and, optionally, update the DWARF system."
$	TELL "The general use is:"
$	TELL "    @WNG:NXEC type [-codes ...] [name[,...] ...]"
$	TELL "The type can be:"
$	TELL "    NC[ompile]    to compile"
$	TELL "    NL[ink]       to link"
$	TELL "    ND[elete]     to delete"
$	TELL "    NG[et]        to get from TLB"
$	TELL "    NN[et]	to get across net"
$	TELL "    NX[ref]       to produce crossreference between Fortran files"
$	TELL "In general there will be symbols defined"
$	L0="''F$TRNLNM("WNG")'WNGCSHRC_''WNG_SITE'.COM"
$	TELL "(in ''F$PARSE(L0,,,,"NO_CONCEAL")') to"
$	TELL "call the different modes directly as:"
$	TELL "    NXEC, NCOMP, NLINK, NDEL, NGET, NNET, NXREF."
$	TELL "More Help is available by specifying a type and no further"
$	TELL "arguments, or by specifying a ? somewhere in the argument list."
$	TELL "E.g.: $ NCOMP or $ NXEC NCOMP"
$	TELL "A file UPDyymmdd.LOG will describe the results"
$	TELL "Retry NXEC command with a type, or in short form."
$	TELL ""
$	GOTO EXEX
$!#
$!# Codes level
$!#
$ H2:	TELL ""
$	TELL "The @NXEC <type> command can have codes and (file)names as"
$	TELL "arguments. A code argument starts with a - or +. Help on filename"
$	TELL "arguments is available by not specifying a filename."
$	TELL "Codes are a single letter, optionally preceded by an N or a + to"
$	TELL "indicate negation, or followed by a single digit to subspecify"
$	TELL "information. Some codes accept an argument in <> brackets, e.g."
$	TELL "L<MYLIB>."
$	TELL "Q[ualifier] codes are special codes. They are followed by a"
$	TELL "letter to indicate the qualifier type, and an angle-bracketed"
$	TELL "string with the qualifier, e.g. QB</AFTER=TODAY> ."
$	TELL "Codes will be read left-to-write (e.g. BNB = NB, NBB = B)."
$	TELL "The codes specified will be preceded by codes specified in"
$	TELL "the possibly defined logical name ''PCOD'_COD."
$	TELL "The known codes for ''TYP' are (default given first):"
$	TELL "       ?    this help text"
$	TELL "  NB   B    execute as Batch job, without a log"
$	TELL "       B1   execute as Batch job, with a printed log"
$	TELL "       B2   execute as a spawned command, with log in SPAWN.LOG"
$	GOSUB H2'PCOD'
$	TELL ""
$	GOTO EXEX
$!#
$!# NCOMP codes
$!#
$ H2NC:	TELL "  NA   A1   alter Dwarf routines (INCLUDE '(abc)' to 'ABC' and"
$	TELL "                PROGRAM to SUBROUTINE)"
$	TELL "       A2   alter Fortran routines (INCLUDE 'xxx:abc.def' to"
$	TELL "                INCLUDE 'ABC.DEF') and"
$	TELL "                alter Fortran routines (INCLUDE 'abc.def' to"
$	TELL "                INCLUDE 'ABC_DEF') and C routines"
$	TELL "                (include abc.inc to include abc_inc)"
$	TELL "       A4   alter INTEGER*4/REAL*4 into INTEGER/REAL, REAL*8 into"
$	TELL "                DOUBLE PRECISION, LOGICAL*1 into BYTE"
$	TELL "                May combine switches (A6 = A2 + A4)"
$	TELL "   C  NC    compile .FOR, .FVX, .FSC, .MVX, .DSC, .PSC,"
$	TELL "                .HLP, .DEF, .INC, .AVX, .XVX"
$	TELL "   D  ND    use /DEBUG in compiling"
$	TELL "   L  NL    save .OBJ in WNLIB.OLB (if C), and save text of all"
$	TELL "      L1    files (except some, see filename help) in WNLIB.TLB."
$	TELL "            L1: only uses .olb, not .tlb"
$	TELL "            By specifying L<name> the default WNLIB name can be"
$	TELL "            overwritten."
$	TELL "   O  NO    optimize Fortran compilation"
$	TELL "  NP   P    print the compilation listing"
$	TELL "  NU   U    update in DWARF sytem if .PIN or .HLP"
$	TELL "		  update in WN system if .DEF, .PEF, .DSF or .INC"
$	TELL "  NX   X    produce XREF listing in Macro and Fortran compilation"
$	TELL "   Z  NZ    act on .GRP extension"
$	TELL ""
$	TELL "      QB<.> batch execution qualifier (e.g. QB</AFTER=TOMORROW>)"
$	TELL "      QF<.> Fortran qualifier (e.g. QF</NOI4>). Default:"
$	TELL "            ''FQ_D'"
$	TELL "      QI<.> Macro header files"
$	TELL "      QJ<.> Fortran header files (e.g. QJ<MYA.INC+MYB.INC>)"
$	TELL "      QM<.> Macro qualifiers"
$	TELL "      QP<.> Pure extensions (with .): only files with these"
$	TELL "            extensions will be done. Default: ''PQ_D'"
$	RETURN
$!#
$!# NNET codes
$!#
$ H2NN:	TELL "  NA   A    ask node information i.s.o. using default"
$	TELL "   Z  NZ    act on .GRP extension"
$	TELL ""
$	TELL "      QB<.> batch execution qualifier (e.g. QB</AFTER=TOMORROW>)"
$	TELL "      QP<.> Pure extensions (with .): only files with these"
$	TELL "            extensions will be done. Default: ''PQ_D'"
$	TELL ""
$	TELL "NNET uses the following symbols to find the foreign network:"
$	TELL "    WNG_NODE        the node (e.g. rzmvx4 or robin.atnf.csiro.au)"
$	TELL "    WNG_NODEDIR     the root directory (e.g. USER5:[WNB] or /usr/wnb)"
$	TELL "    WNG_NODEUSER    the user (e.g. wbrouw)
$	TELL ""
$	RETURN
$!#
$!# NDEL codes
$!#
$ H2ND:	TELL "   A  NA    ask confirmation for each deletion"
$	TELL "   C  NC    compiled data (.OBJ, .PPD etc) are deleted"
$	TELL "   L  NL    delete .OBJ in WNLIB.OLB (if C), and delete text of"
$	TELL "      L1    files (except some, see filename help) in WNLIB.TLB."
$	TELL "            L1: only uses .olb, not .tlb"
$	TELL "            By specifying L<name> the default WNLIB name can be"
$	TELL "            overwritten."
$	TELL "  NU   U    delete in DWARF sytem if .PIN or .HLP"
$	TELL "		  delete in WN system if .DEF, .PEF, .DSF or .INC"
$	TELL "   Z  NZ    acts on .GRP extension"
$	TELL ""
$	TELL "      QB<.> batch execution qualifier (e.g. QB</AFTER=TOMORROW>)"
$	TELL "      QP<.> Pure extensions (with .): only files with these"
$	TELL "            extensions will be done. Default: ''PQ_D'"
$	TELL ""
$	TELL " Note: only files present in (specified) directory can be deleted"
$	TELL "       if wildcards or no extension present."
$	RETURN
$!#
$!# NGET codes
$!#
$ H2NG:	TELL "  NA   A    always get new file from .TLB, even if present"
$	TELL "   L  NL    use WNLIB.TLB to extract files."
$	TELL "            By specifying L<name> the default WNLIB name can be"
$	TELL "            overwritten."
$	TELL "   Z  NZ    acts on .GRP extension"
$	TELL ""
$	TELL "      QB<.> batch execution qualifier (e.g. QB</AFTER=TOMORROW>)"
$	TELL "      QP<.> Pure extensions (with .): only files with these"
$	TELL "            extensions will be done. Default: ''PQ_D'"
$	RETURN
$!#
$!# NLINK codes
$!#
$ H2NL:	TELL "  NA   A1   alter startup routine to exclude logging (Dwarf)"
$	TELL "  ND   D    use /DEBUG in linking"
$	TELL "   L  NL    use as L<name> to overwrite the default object"
$	TELL "            library name WNLIB with another name."
$	TELL "  NP   P    print the link map"
$	TELL "   S  NS    use DWARF library in linking"
$	TELL "  NU   U    update in DWARF sytem."
$	TELL "  NX   X    produce XREF listing in link map"
$	TELL "   Z  NZ    acts on .GRP extension"
$	TELL ""
$	TELL "      QB<.> batch execution qualifier (e.g. QB</AFTER=TOMORROW>)"
$	TELL "      QL<.> link qualifiers"
$	TELL "      QO<.> link options (e.g. QO<MYLIB/LIB,HISLIB/LIB>)"
$	RETURN
$!#
$!# Filename level
$!#
$ H3:	TELL ""
$	TELL "Filenames can be specified in separate arguments, or separated"
$	TELL "by commas in a single argument. Filenames can contain wildcards."
$	TELL "If no extension is given .* is assumed, except for indirect"
$	TELL "files where .GRP is assumed. A filename preceded by"
$	TELL "an @ will be assumed to contain filenames, as will be files with"
$	TELL "a .GRP extension (unless NZ code specified)."
$	TELL "The current default device and directory will be assumed if"
$	TELL "none are specified."
$	TELL ".GRP and @ files will contain a filename per line, optionally"
$	TELL "followed by a comment preceded with a !, or will be a comment"
$	TELL "comment line starting with a !. The filenames must have"
$	TELL "extensions, and cannot contain wildcards, but may have device"
$	TELL "and directory informations. Optionally the filename may be"
$	TELL "followed by standard switches, indicating"
$	TELL "codes to be used for this line only, e.g. compilation or"
$	TELL "linking data. E.g. A.FOR -NO will be compiled with"
$	TELL "no optimizing."
$	TELL "If the line starts with a # the line will be a UNIX shell"
$	TELL "command. If the line starts with a $,"
$	TELL "the line will be given to DCL to be executed (e.g. to"
$	TELL "assign logical names)."
$	TELL "If the line starts with $$ or ## the next 3 characters are"
$	TELL "checked if they are pp$ (pp#) (pp any two characters). If the"
$	TELL "pp characters are identical to the program name that invoked"
$	TELL "the current .grp (e.g. NC), the command will be executed."
$	TELL "The action of the command will depend on the type of the file."
$	TELL "For ''TYP' the action will be:"
$	TELL "  .GRP          will be read as an indirect file (if not NZ code)"
$	GOSUB H3'PCOD'
$	TELL ""
$	GOTO EXEX
$!#
$!# NCOMP files
$!#
$ H3NC:	TELL "  .FOR, .FVX, .FSC will be compiled as Fortran programs (if C code)"
$	TELL "  .MVX          will be compiled as Macro program (if C)"
$	TELL "  .DSC          will be handled by WNGTAB (if C)"
$	TELL "  .HLP          will be put in .HLB with same name (if C)"
$	TELL "  .PIN, .PSC    will be compiled (if C)"
$	TELL "  .DEF, .INC    will be compiled (if C)"
$	TELL "                All of the above will be put in"
$	TELL "                .TLB (unless NL or L1 code)"
$	TELL "  .AVX          will be changed into an .OLB (if C)"
$	TELL "  .XVX          will be changed into an .EXE (if C)"
$	TELL "  .A%%, .X%%    will be put in _AX.TLB (unless NL or L1 code)"
$	TELL "  ''F$EXTRACT(0,40,CHTP)'"
$	TELL "  ''F$EXTRACT(40,-1,CHTP)' will be skipped"
$	TELL "  All other files with 3 character extensions will be put in .TLB"
$	TELL "  (unless NL or L1 code specified)."
$	TELL "  All other files will be skipped."
$	RETURN
$!#
$!# NNET files
$!#
$ H3NN:	TELL "  ''F$EXTRACT(0,40,CHTP)'"
$	TELL "  ''F$EXTRACT(40,-1,CHTP)' will be skipped"
$	TELL "  All other files with 3 character extensions will be got"
$	TELL "  All other files will be skipped."
$	RETURN
$!#
$!# NDEL files
$!#
$ H3ND:	TELL "  .FOR, .FVX, .FSC will be deleted as Fortran programs (if C code)"
$	TELL "  .MVX          will be deleted as Macro program (if C)"
$	TELL "  .DSC          will be handled by WNGTAB (if C)"
$	TELL "  .HLP          will be put in .HLB with same name (if C)"
$	TELL "  .EXE          will be deleted as rtask image"
$	TELL "  .PIN, .PSC    will be deleted as such (if C)"
$	TELL "  .DEF, .INC    will be deleted (if C)"
$	TELL "                All of the above (except .EXE) will be deleted"
$	TELL "                 from .TLB (unless NL or L1 code)"
$	TELL "  .A%%, .X%%    will be deleted, also from _AX.TLB (unless codes)"
$	TELL "  ''F$EXTRACT(0,40,CHTP)'"
$	TELL "  ''F$EXTRACT(40,-1,CHTP)' will be skipped"
$	TELL "  All other files with 3 character extensions will be deleted"
$	TELL "  from .TLB (unless NL or L1 code specified)."
$	TELL "  All other files will be skipped."
$	RETURN
$!#
$!# NGET files
$!#
$ H3NG:	TELL "  ''F$EXTRACT(0,40,CHTP)'"
$	TELL "  ''F$EXTRACT(40,-1,CHTP)' will be skipped"
$	TELL "  All other files with 3 character extensions will be got"
$	TELL "                 from .TLB (unless NL or L1 code specified)."
$	TELL "  All other files will be skipped."
$	RETURN
$!#
$!# NLINK files
$!#
$ H3NL:	TELL "  .EXE          will produce program"
$	TELL "                if no extension program will be produced"
$	RETURN
$!#
$!# Ready
$!#
$ EXEX:	EXIT
