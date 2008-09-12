C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GENUN_GETMSG
C.Keywords:	Message Facility, Get Text
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	UNIX
C.Comments:
C.Version:	900410 FMO - creation
C.Version:	900502 FMO - added GEN_LUN
C.Version:	920310 GvD - added PPD_LENGTHLON, UDF_UNIT and DBD_ messages
C.Version:	920508 GvD - updated DWC_TBNOTALL for logical data
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:	940131 CMV - all messages in dwc.def
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_GETMSG (STATID,LMSG,MSG,FLAGS,BARR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	STATID		! (i) status identification
	INTEGER*4	LMSG		! (o) sign. length of message text
	CHARACTER*(*)	MSG		! (o) message text
	INTEGER*4	FLAGS		! (i) message components to be returned
	BYTE		BARR(4)		! (o) message-specific info
C
C.Purpose:	Get message text associated with error code
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1	always
C.Notes:
C	- If STATID is associated with a message string, that string will be
C	  returned in MSG. Otherwise, STATID will be written into MSG in
C	  hexadecimal format.
C	- FLAGS and BARR are not used.
C-------------------------------------------------------------------------
C
	INTEGER		MAXPAR
	PARAMETER	(MAXPAR=238)
	INTEGER		WNCALN
C
	INTEGER*4	C(MAXPAR)
	CHARACTER*80	T(MAXPAR)
C
	DATA C(  1),T(  1)	/GEN_STMESSAG,
	1'     !AS is started at !%D !%T'/
	DATA C(  2),T(  2)	/GEN_ENDMESSAG,
	1'     !AS is ended at !%T               STATUS=!AS'/
	DATA C(  3),T(  3)	/GEN_ISNOTANM,
	1'string !AS is not alphanumeric'/
	DATA C(  4),T(  4)	/GEN_FORIOERR,
	1'Fortran-errornr !SJ on LUN !SJ in file !AS'/
	DATA C(  5),T(  5)	/GEN_SUCCESS,
	1'success'/
	DATA C(  6),T(  6)	/GEN_INVDATTYP,
	1'Invalid data type specifier: !AS'/
	DATA C(  7),T(  7)	/GEN_STROVFLO,
	1'Output string too short to hold value, !SJ characters
	2 truncated'/
	DATA C(  8),T(  8)	/GEN_SYMDEFERR,
	1'Error defining symbol !AS'/
	DATA C(  9),T(  9)	/GEN_SYMGETERR,
	1'Error getting the value of symbol !AS'/
	DATA C( 10),T( 10)	/GEN_SYMDELERR,
	1'Error deleting symbol !AS'/
C
	DATA C( 11),T( 11)	/DWC_SUCCESS,
	1'success'/
	DATA C( 12),T( 12)	/DWC_GETINPERR,
	1'IO-error on !AS'/
	DATA C( 13),T( 13)	/DWC_EOFCTRLZ,
	1'end-of-file found or CTRL/Z given on !AS'/
	DATA C( 14),T( 14)	/DWC_EXPERRMSG,
	1'!ASerror at or near position !SJ in value-string:!/	!AS'/
	DATA C( 15),T( 15)	/DWC_INVOPER,
	1'operator-symbol !AS is invalid'/
	DATA C( 16),T( 16)	/DWC_INVNONR,
	1'invalid or no number'/
	DATA C( 17),T( 17)	/DWC_TOOMANARG,
	1'too many function-arguments, max=!SJ'/
	DATA C( 18),T( 18)	/DWC_TOOLITARG,
	1'not enough function-arguments, !SJ are required'/
	DATA C( 19),T( 19)	/DWC_TOODEENES,
	1'subexpressions too deeply nested, max. depth=!SJ'/
	DATA C( 20),T( 20)	/DWC_TOOMANYNR,
	1'too many numbers in (sub)expression, max=!SJ'/
	DATA C( 21),T( 21)	/DWC_UNBPAREN,
	1'unbalanced parentheses'/
	DATA C( 22),T( 22)	/DWC_DIVBYZERO,
	1'divide by zero'/
	DATA C( 23),T( 23)	/DWC_UNKFUNC,
	1'unknown function-name'/
	DATA C( 24),T( 24)	/DWC_NOTAFTOP,
	1'nothing found after operator'/
	DATA C( 25),T( 25)	/DWC_INVFUNARG,
	1'invalid function-argument'/
	DATA C( 26),T( 26)	/DWC_UNDEFEXP,
	1'undefined exponentiation'/
	DATA C( 27),T( 27)	/DWC_SETCURNOD,
	1'Current node set to !AS for current program run'/
	DATA C( 28),T( 28)	/DWC_INTOVERFL,
	1'Integer overflow during conversion to !AS data type'/
	DATA C( 29),T( 29)	/DWC_TONOTALL,
	1'TO only allowed after start-value'/
	DATA C( 30),T( 30)	/DWC_STEPNOTAL,
	1'BY only allowed after TO'/
	DATA C( 31),T( 31)	/DWC_TSNOTALL,
	1'TO or BY not allowed'/
	DATA C( 32),T( 32)	/DWC_SAVEOVFLO,
	1'Aborting due to parameter save area overflow'/
	DATA C( 33),T( 33)	/DWC_STEPISZER,
	1'BY-value is zero'/
	DATA C( 34),T( 34)	/DWC_STEPSIGN,
	1'BY-value has wrong sign'/
	DATA C( 35),T( 35)	/DWC_TOOMANYEL,
	1'too many elements in array, max=!SJ'/
	DATA C( 36),T( 36)	/DWC_NOENDAPOS,
	1'no ending apostrophe found'/
	DATA C( 37),T( 37)	/DWC_SYMNOTDEF,
	1'symbol !AS not defined'/
	DATA C( 38),T( 38)	/DWC_MUTUALSUB,
	1'probably mutual substitution'/
	DATA C( 39),T( 39)	/DWC_NOENDQUO,
	1'no ending quotation-mark found'/
	DATA C( 40),T( 40)	/DWC_TOOMANCHR,
	1'too many characters in this element, max=!SJ'/
	DATA C( 41),T( 41)	/DWC_INVUNIT,
	1'unitcode !AS is invalid or not permitted'/
	DATA C( 42),T( 42)	/DWC_STRTOOSHO,
	1'output-string too short, max. length=!SJ'/
	DATA C( 43),T( 43)	/DWC_NOVALDEF,
	1'no value given or no default found for valuenr !SJ
	2 in this valueset'/
	DATA C( 44),T( 44)	/DWC_KEYVAHELP,
	1'value of symbol !AS is HELP'/
	DATA C( 45),T( 45)	/DWC_UNKKEYW,
	1'!AS is an unknown!ASkeyword for image !AS'/
	DATA C( 46),T( 46)	/DWC_UNKPRKEYW,
	1'!AS is an unknown!ASprogramkeyword for image !AS'/
	DATA C( 47),T( 47)	/DWC_PARTOOSML,
	1'!SJ elements are available in call-argument ARRAY;
	2 !SJ are needed'/
	DATA C( 48),T( 48)	/DWC_PARNONR,
	1'The call-argument NELEM is not present'/
	DATA C( 49),T( 49)	/DWC_PARAMERR,
	1'error occurred for programkeyword !AS'/
	DATA C( 50),T( 50)	/DWC_PARNOTFND,
	1'keyword !AS not found or not accessible for the requested
	2 operation'/
	DATA C( 51),T( 51)	/DWC_PARNOVAL,
	1'no value found for keyword !AS'/
	DATA C( 52),T( 52)	/DWC_PARWRDEF,
	1'wrong default-value for programkeyword !AS'/
	DATA C( 53),T( 53)	/DWC_PARWRANS,
	1'wrong answer given for keyword !AS, please repeat'/
	DATA C( 54),T( 54)	/DWC_PARGIVVAL,
	1'!AS has no program-default; please give a value or CTRL/Z'/
	DATA C( 55),T( 55)	/DWC_PPDNOVIRT,
	1'no virtual memory found for !SJ bytes'/
	DATA C( 56),T( 56)	/DWC_PPDFRVIRT,
	1'error in free virtual memory'/
	DATA C( 57),T( 57)	/DWC_ACTTOOMAN,
	1'too many processes active'/
	DATA C( 58),T( 58)	/DWC_CALSEPEXP,
	1'sorry, you can''t give an expression in the command-line'/
	DATA C( 59),T( 59)	/DWC_CALINVRAD,
	1'!AS is an invalid radix-option (D, O or X is valid)'/
	DATA C( 60),T( 60)	/DWC_CALINVTYP,
	1'!AS is an invalid datatype (B,I,J,L,R,D are valid)'/
	DATA C( 61),T( 61)	/DWC_INVSYMNAM,
	1'invalid symbol-name'/
	DATA C( 62),T( 62)	/DWC_RESERVSYM,
	1'invalid symbol-name, !AS is a reserved name'/
	DATA C( 63),T( 63)	/DWC_GETINPTR,
	1'Answer is too long; truncated to !SJ characters'/
	DATA C( 64),T( 64)	/DWC_LOKILLIMG,
	1'!AS is an illegal image-name'/
	DATA C( 65),T( 65)	/DWC_MULTIQUAL,
	1'no other qualifiers are allowed when !AS is given'/
	DATA C( 66),T( 66)	/DWC_BLANKSLAS,
	1'perhaps you put a blank before a slash that indicates a
	2 division'/
	DATA C( 67),T( 67)	/DWC_STRINVNR,
	1'stream-name !AS is invalid'/
	DATA C( 68),T( 68)	/DWC_STRNOTALL,
	1'stream-name !AS is not allowed with image-name !AS'/
	DATA C( 69),T( 69)	/DWC_APPTWODOT,
	1'2 succeeding dots in given name'/
	DATA C( 70),T( 70)	/DWC_APPTOOLON,
	1'resulting name longer than !SJ characters'/
	DATA C( 71),T( 71)	/DWC_APPMINUS,
	1'too many minus-signs in given name'/
	DATA C( 72),T( 72)	/DWC_SUBPRCERR,
	1'error in starting subprocess !AS'/
	DATA C( 73),T( 73)	/DWC_FILNOTFND,
	1'file !AS not found in Master or User directory'/
	DATA C( 74),T( 74)	/DWC_NOPARCOM,
	1'no parameters given in command-line'/
	DATA C( 75),T( 75)	/DWC_UNKQUAL,
	1'!AS is an unknown qualifier'/
	DATA C( 76),T( 76)	/DWC_AMBQUAL,
	1'!AS is an ambiguous qualifier-abbreviation'/
	DATA C( 77),T( 77)	/DWC_QUALNOVAL,
	1'no value given on qualifier !AS'/
	DATA C( 78),T( 78)	/DWC_QUALVALNA,
	1'no value allowed on qualifier !AS'/
	DATA C( 79),T( 79)	/DWC_INVIMGSTR,
	1'syntax-error in image$stream !AS'/
	DATA C( 80),T( 80)	/DWC_SYNERRSYM,
	1'syntax-error in symbol-name !AS'/
	DATA C( 81),T( 81)	/DWC_LOKUNKIMG,
	1'No PPD file found for image !AS in Master or User
	2 directories'/
	DATA C( 82),T( 82)	/DWC_QUALBATCH,
	1'qualifier !AS is not possible in batch-mode'/
	DATA C( 83),T( 83)	/DWC_SPECWRSYN,
	1'sorry, correct syntax is KEYWORD=VALUE'/
	DATA C( 84),T( 84)	/DWC_ERRSAVSYM,
	1'syntax-error in symbol-name in SAV-file on linenr !SJ'/
	DATA C( 85),T( 85)	/DWC_LETNOSVAL,
	1'no symbol-name, =-sign or value given'/
	DATA C( 86),T( 86)	/DWC_NOVALALL,
	1'No data-value is allowed when qualifier !AS is given'/
	DATA C( 87),T( 87)	/DWC_SYMBOLCLR,
	1'symbol is cleared'/
	DATA C( 88),T( 88)	/DWC_TWICEVAL,
	1'(Default-3)value is given both as a string and as an
	2 array argument'/
	DATA C( 89),T( 89)	/DWC_TBNOTALL,
	1'You cannot use the TOBY-flag for vectors and
	2 character/logical-data'/
	DATA C( 90),T( 90)	/DWC_TBNOMULT,
	1'Nr of elements must be a multiple of 3, when TOBY is used'/
	DATA C( 91),T( 91)	/DWC_USESAVFIL,
	1'!AS is used as SAV-file!/'/
	DATA C( 92),T( 92)	/DWC_SYMBCLEAR,
	1'symbol !AS is cleared'/
	DATA C( 93),T( 93)	/DWC_NRSYMCLR,
	1'!SJ symbols cleared'/
	DATA C( 94),T( 94)	/DWC_INVLEVEL,
	1'invalid value for qualifier /LEVEL'/
	DATA C( 95),T( 95)	/DWC_UNKDWCOM,
	1'DWARF_COMMON-symbol is undefined!/ Use SPECIFY DWARF
	2 to create it'/
	DATA C( 96),T( 96)	/DWC_EXEERRORS,
	1'errors found, image is not started'/
	DATA C( 97),T( 97)	/DWC_WAITSUBPR,
	1'waiting for image !AS in stream !AS   (started at !AS)'/
	DATA C( 98),T( 98)	/DWC_WAITREADY,
	1'stream !AS is ready;   processing will continue'/
	DATA C( 99),T( 99)	/DWC_WTNOJOB,
	1'no jobname or jobnumber given in the command'/
	DATA C(100),T(100)	/DWC_WTMOREJOB,
	1'more than 1 jobname or jobnumber given in the command'/
	DATA C(101),T(101)	/DWC_WTNOKEYW,
	1'no underscores/keywords are possible in the command'/
	DATA C(102),T(102)	/DWC_WTNOTHACT,
	1'no such image$streams active'/
	DATA C(103),T(103)	/DWC_SAVINVGLB,
	1'invalid option used for GLOBALS-qualifier (GLOBAL or IMAGE)'/
	DATA C(104),T(104)	/DWC_SAVNRSAVE,
	1'!SJ symbols saved'/
	DATA C(105),T(105)	/DWC_RESNRREST,
	1'!SJ symbols restored'/
	DATA C(106),T(106)	/DWC_CHKERRMSG,
	1'keyword !AS, data-error in value-string:!/	!AS'/
	DATA C(107),T(107)	/DWC_WILDNOTAL,
	1'no wildcard (*) allowed for keyword !AS'/
	DATA C(108),T(108)	/DWC_PARNOOUT,
	1'no output-value given for keyword !AS'/
	DATA C(109),T(109)	/DWC_PARRETBAT,
	1'RETRY_PARM call illegal in batch-mode; keyword=!AS'/
	DATA C(110),T(110)	/DWC_NOGLBSTR,
	1'$0 is a global stream-name and cannot be used'/
	DATA C(111),T(111)	/DWC_PROGSTERR,
	1'error in PROG_START, program is aborted'/
	DATA C(112),T(112)	/DWC_PARELTSML,
	1'array-elements are !SJ bytes long; !SJ bytes are needed'/
	DATA C(113),T(113)	/DWC_NODCOMERR,
	1'error in combining node-name !AS with the current
	2 node-name !AS'/
	DATA C(114),T(114)	/DWC_CLRDWARF,
	1'DWARF-symbols cannot be cleared; use SPECIFY'/
	DATA C(115),T(115)	/DWC_TOOMANSET,
	1'!SJ value-sets given in program-array, max=!SJ'/
	DATA C(116),T(116)	/DWC_ENDOFLOOP,
	1'end-of-loop for a keyword'/
	DATA C(117),T(117)	/DWC_NULLNOTAL,
	1'no null-value (2 quotation-marks) allowed for keyword !AS'/
	DATA C(118),T(118)	/DWC_NULLVALUE,
	1'null-value returned'/
	DATA C(119),T(119)	/DWC_WILDCARD,
	1'wildcard-value returned'/
	DATA C(120),T(120)	/DWC_IMGSUBPRC,
	1' '/
	DATA C(121),T(121)	/DWC_NOLOCVAL,
	1'You cannot specify a local value for keyword !AS'/
	DATA C(122),T(122)	/DWC_NODWVALUE,
	1'No value found for DWARF-programkeyword !AS'/
	DATA C(123),T(123)	/DWC_DWSERROR,
	1'Warn DWARF-manager: error during interpretation of
	2 DWARF-symbols'/
	DATA C(124),T(124)	/DWC_UNKDWKEY,
	1'DWARF-programkeyword !AS not defined in the routine'/
	DATA C(125),T(125)	/DWC_IMMNOSUBS,
	1'PPD''s IMMEDIATE switch overruled /NOSUBSTITUTE for
	2 this keyword'/
	DATA C(126),T(126)	/DWC_MANDATVAL,
	1'You have to give a value when qualifier !AS is given'/
	DATA C(127),T(127)	/DWC_NOCUPDSYM,
	1'No update-symbol for the control-area DWARF_COMMON found'/
	DATA C(128),T(128)	/DWC_KEYWMISM,
	1'Wrong keyword-name given!AS!/	Found keyword !AS, expected !AS'/
	DATA C(129),T(129)	/DWC_SUBPRCPMT,
	1'wants to prompt for input; please finish your current typing'/
	DATA C(130),T(130)	/DWC_EXEUSER,
	1'File !AS being taken from USER directory'/
	DATA C(131),T(131)	/DWC_PRESENT,
	1'Argument is present'/
	DATA C(132),T(132)	/DWC_ABSENT,
	1'Argument is absent'/
	DATA C(133),T(133)	/DWC_NEGATED,
	1'Qualifier is negated'/
	DATA C(134),T(134)	/DWC_REQUIRED,
	1'Required argument is absent'/
	DATA C(135),T(135)	/DWC_CLIBUFERR,
	1'Error in module CLI_BUF'/
	DATA C(136),T(136)	/DWC_CLISYNTAX,
	1'Syntax error in command-line definition'/
	DATA C(137),T(137)	/DWC_CLIPARUNK,
	1'Unknown parameter nr !SJ'/
	DATA C(138),T(138)	/DWC_CLINAMAMB,
	1'Ambiguously abbreviated argument name: !AS'/
	DATA C(139),T(139)	/DWC_CLINAMUNK,
	1'Unknown argument name: !AS'/
	DATA C(140),T(140)	/DWC_CLISTRINV,
	1'Programming error: invalid CLI-string ID !SJ'/
	DATA C(141),T(141)	/DWC_CLISTRMAX,
	1'More than !SJ CLI strings are requested'/
	DATA C(142),T(142)	/DWC_CLISTROVR,
	1'Total length of CLI strings exceeds !SJ characters'/
C
	DATA C(143),T(143)	/PPD_SUCCESS,
	1'success'/
	DATA C(144),T(144)	/PPD_NOSUCCESS,
	1'no success'/
	DATA C(145),T(145)	/PPD_STRNOTAN,
	1'string contains non-alphanumeric characters'/
	DATA C(146),T(146)	/PPD_ARRNOTASC,
	1'values in array are not in ascending order'/
	DATA C(147),T(147)	/PPD_ARRNOTDES,
	1'values in array are not in descending order'/
	DATA C(148),T(148)	/PPD_OPTINVAL,
	1'invalid option'/
	DATA C(149),T(149)	/PPD_OPTNOTUNI,
	1'specified part of option is not unique'/
	DATA C(150),T(150)	/PPD_STRTOOSML,
	1'string is not long enough'/
	DATA C(151),T(151)	/PPD_SEQERROR,
	1'sequence-error'/
	DATA C(152),T(152)	/PPD_ENDOFFILE,
	1'all entries processed'/
	DATA C(153),T(153)	/PPD_KEYNOTFND,
	1'specified keyword not found'/
	DATA C(154),T(154)	/PPD_ERRMINCHK,
	1'error detected in check against minimum value'/
	DATA C(155),T(155)	/PPD_ERRMAXCHK,
	1'error detected in check against maximum value'/
	DATA C(156),T(156)	/PPD_STRNOTALP,
	1'string is not alphabetic'/
	DATA C(157),T(157)	/PPD_STRNOTNUM,
	1'string is not numeric'/
	DATA C(158),T(158)	/PPD_IMTOOLONG,
	1'image-name more than 8 characters'/
	DATA C(159),T(159)	/PPD_PPDNOTFND,
	1'specified PPD-file not found'/
	DATA C(160),T(160)	/PPD_PKYNOTFND,
	1'specified P-keyword not found'/
	DATA C(161),T(161)	/PPD_NOCURENTR,
	1'no current ppd-entry'/
	DATA C(162),T(162)	/PPD_NUMVALMIN,
	1'Number of values less then minimum; specified !SJ, minimum !SJ'/
	DATA C(163),T(163)	/PPD_NUMVALMAX,
	1'Number of values exceeds maximum; specified !SJ, maximum !SJ'/
	DATA C(164),T(164)	/PPD_NUMSETMAX,
	1'Number of value-sets exceeds maximum; specified !SJ,
	2 maximum !SJ'/
	DATA C(165),T(165)	/PPD_VALLSSMIN,
	1'Specified value less than minimum !AS'/
	DATA C(166),T(166)	/PPD_VALEXCMAX,
	1'Specified value exceeds maximum !AS'/
	DATA C(167),T(167)	/PPD_ARRNOTNAS,
	1'Values in array are not in non-ascending order'/
	DATA C(168),T(168)	/PPD_ARRNOTNDE,
	1'Values in array are not in non-descending order'/
	DATA C(169),T(169)	/PPD_UNAMNOT,
	1'USER_PARAMETER not specified'/
	DATA C(170),T(170)	/PPD_MAX16,
	1'value too long; max 16 characters'/
	DATA C(171),T(171)	/PPD_CHATNUNI,
	1'one of the checks or attributes not uniquely abbreviated'/
	DATA C(172),T(172)	/PPD_CHATINV,
	1'one of the checks or attributes invalid'/
	DATA C(173),T(173)	/PPD_MUTEXCLCH,
	1'mutually exclusive checks specified'/
	DATA C(174),T(174)	/PPD_NNDNOTNOD,
	1'attribute NULL_NODE and check NODE must both be specified'/
	DATA C(175),T(175)	/PPD_UNDONLVEC,
	1'undefined values only permitted for a vector'/
	DATA C(176),T(176)	/PPD_TYPENOT,
	1'DATA_TYPE not specified'/
	DATA C(177),T(177)	/PPD_TYPCHKINV,
	1'one or more specified checks invalid for the specified
	2 DATA_TYPE'/
	DATA C(178),T(178)	/PPD_TYPEINV,
	1'DATA_TYPE invalid'/
	DATA C(179),T(179)	/PPD_IOINV,
	1'IO invalid'/
	DATA C(180),T(180)	/PPD_LENGTHNOT,
	1'LENGTH required for this DATA_TYPE'/
	DATA C(181),T(181)	/PPD_NOTPOSINT,
	1'value should be a positive integer value'/
	DATA C(182),T(182)	/PPD_LENGTHINV,
	1'LENGTH invalid'/
	DATA C(183),T(183)	/PPD_NVLINVVEC,
	1'NVALUES should be > 0 for a vector'/
	DATA C(184),T(184)	/PPD_NVLINVCHK,
	1'one of the specified checks invalid for NVALUES = 1'/
	DATA C(185),T(185)	/PPD_MNVALINV,
	1'MIN_NVALUES should be <= NVALUES'/
	DATA C(186),T(186)	/PPD_MXVALINV,
	1'MAX_NVALUES should be <= NVALUES and >= MIN_NVALUES'/
	DATA C(187),T(187)	/PPD_MMNOVAL,
	1'check MINIMUM or MAXIMUM, but no corresponding value'/
	DATA C(188),T(188)	/PPD_MMNOCHK,
	1'MINIMUM or MAXIMUM value, but no corresponding check'/
	DATA C(189),T(189)	/PPD_MMINV,
	1'MINIMUM or MAXIMUM invalid'/
	DATA C(190),T(190)	/PPD_VCINVNVL,
	1'for a vector the nr of MINIMUM/MAXIMUM values should be
	2 NVALUES'/
	DATA C(191),T(191)	/PPD_NVCINVNVL,
	1'if not a vector the nr of MINIMUM/MAXIMUM values should be 1'/
	DATA C(192),T(192)	/PPD_UNITINV,
	1'UNIT invalid'/
	DATA C(193),T(193)	/PPD_SEARCHINV,
	1'SEARCH invalid'/
	DATA C(194),T(194)	/PPD_PSEARCH,
	1'when DEFAULT specified, SEARCH = PROGRAM required'/
	DATA C(195),T(195)	/PPD_OPTNOVAL,
	1'check OPTIONS or ABBREV_OPTIONS, but no corresponding value'/
	DATA C(196),T(196)	/PPD_OPTNOCHK,
	1'OPTIONS specified but no corresponding check'/
	DATA C(197),T(197)	/PPD_NOIMAGE,
	1'no image name specified'/
	DATA C(198),T(198)	/PPD_EXEUSER,
	1'PPD-file found on n_uexe'/
	DATA C(199),T(199)	/PPD_REFEXCMAX,
	1'number of external references exceeds maximum;
	2 tell programmer'/
	DATA C(200),T(200)	/PPD_GLOFILNF,
	1'the file for global searches is not (yet) present'/
	DATA C(201),T(201)	/PPD_PARINV,
	1'parameter name invalid'/
	DATA C(202),T(202)	/PPD_PARNOTUNI,
	1'parameter name not unique'/
	DATA C(203),T(203)	/PPD_INVQUAVAL,
	1'invalid qualifier value(s)'/
	DATA C(204),T(204)	/PPD_DEFVALINV,
	1'DEFAULTS invalid'/
	DATA C(205),T(205)	/PPD_INTREF,
	1'one or more internal references; 2nd compilation pass
	2 necessary'/
	DATA C(206),T(206)	/PPD_KEYAMBIG,
	1'ambiguous keyword !AS; you could mean one of the keywords:'/
C
	DATA C(207),T(207)	/CPL_WRKFUL,
	1'not enough space in work-area; please contact programmer'/
	DATA C(208),T(208)	/CPL_ERRCNTEXC,
	1'error-count exceeded'/
	DATA C(209),T(209)	/CPL_FLDNOTUNI,
	1'field name not uniquely abbreviated'/
	DATA C(210),T(210)	/CPL_FLDINVAL,
	1'invalid field name'/
	DATA C(211),T(211)	/CPL_SRCEOF,
	1'End of source file detected'/
	DATA C(212),T(212)	/CPL_SUCCESS,
	1'Success'/
	DATA C(213),T(213)	/CPL_SRCOPNERR,
	1'Error opening source file'/
	DATA C(214),T(214)	/CPL_SRCREWERR,
	1'Error rewinding source file'/
	DATA C(215),T(215)	/CPL_SRCRDERR,
	1'Error reading source line !SJ'/
	DATA C(216),T(216)	/CPL_SRCCLOERR,
	1'Error closing source file'/
	DATA C(217),T(217)	/CPL_OBJOPNERR,
	1'Error opening object file'/
	DATA C(218),T(218)	/CPL_OBJWRTERR,
	1'Error writing object record !SJ'/
	DATA C(219),T(219)	/CPL_OBJCLOERR,
	1'Error closing object file'/
	DATA C(220),T(220)	/CPL_OBJDELERR,
	1'Error deleting object file'/
	DATA C(221),T(221)	/CPL_FLDNRINV,
	1'Invalid work-field number !SJ'/
	DATA C(222),T(222)	/CPL_STROVRFLO,
	1'Output string too short'/
	DATA C(223),T(223)	/CPL_ARROVRFLO,
	1'Output array too small'/
	DATA C(224),T(224)	/CPL_FLDUNEXP,
	1'Unexpected field in source line !SJ'/
	DATA C(225),T(225)	/CPL_EOFUNEXP,
	1'Unexpected end of source file'/
	DATA C(226),T(226)	/CPL_VALLISINV,
	1'Invalid value list: !AS'/
	DATA C(227),T(227)	/CPL_DYNFILERR,
	1'Error filling dynamic object buffer'/
	DATA C(228),T(228)	/CPL_DYNWRTERR,
	1'Error writing dynamic object buffer to object file'/
	DATA C(229),T(229)	/CPL_CLISTERR,
	1'Error writing compilation listing'/
	DATA C(230),T(230)	/CPL_DATTYPINV,
	1'Invalid data-type code: !AS'/
	DATA C(231),T(231)	/GEN_LUNNOFREE,
	1'No free LUN available'/
	DATA C(232),T(232)	/PPD_LENGTHLON,
	1'LENGTH must be <=255'/
	DATA C(233),T(233)	/UDF_UNINOTFND,
	1'unit-code !AS not found'/
	DATA C(234),T(234)	/UDF_GRPNOTFND,
	1'unit-group !AS not found'/
	DATA C(235),T(235)	/UDF_STRTOOSML,
	1'string is not long enough'/
	DATA C(236),T(236)	/DBD_BADNODE,
	1'Node name syntax error'/
	DATA C(237),T(237)	/DBD_NAMTOLNG,
	1'Junction name !AS consists of more than 8 characters'/
	DATA C(238),T(238)	/DBD_NAMTOMNY,
	1'Node name consists of more than !SL junction names'/
C
	DO I = 1,MAXPAR
		IF (STATID.EQ.C(I)) GOTO 100
	ENDDO
	WRITE (MSG,'(A15,Z8.8)') 'Status code nr ',STATID
	GOTO 900
 100	MSG = T(I)
C
 900	LMSG = WNCALN(MSG)
	GEN_GETMSG = 1
	RETURN
	END
