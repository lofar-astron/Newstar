C+ WNDSTA_X.FOR
C  JEN 930222. 
C
C  Revisions:
C	HjV 930427	Typo
C	WNB 930630	Add NGF sub-fields, remove some unused variables
C	WNB 931015	Use SSH
C	CMV 931210	Change test for LOOPS to ???_LOOPS
C
	LOGICAL FUNCTION WNDSTA_X (KW,TXT,CSET,SETS)
C
C  	Extra layer of keywords below SETS, which prompts the (beginning)
C  	user for each of the 5/6 Set indices separately.
C  	WNDSTA_X is called from WNDSTA.FOR when the user indicates the
C	wish for this extra layer by typing a certain character (>).
C	The keyword name (KW) indicates the relevant kind of Sets,
C	i.e. SCN_SETS, WMP_SETS etc.
C  	The input default string (TXT) may be analysed to give relevant 
c	default values for the extra keywords in this layer.
C
C  Result:
C
C	WNDSTA_X_L = WNDSTA ( KW_C*:I, TXT_C*:I, CSET_C*:O, SETS_J:O )
C
C	The output arguments (CSET, SETS) are the first elements CSET(1) and 
C	SETS(0,0) of arrays used in WNDSTA.FOR. They contain the answers given
C	by the user, in the same format as they would have been after the call 
C	to WNDPAR in the calling routine WNDSTA.FOR. 
C
C  PIN references:
C
C	SCN_GROUPS, _OBSS, _FIELDS, _CHANNELS, _SECTORS
C	WMP_GROUPS, _FIELDS, _CHANNELS, _POLARS, _TYPES, _MAPS
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'SSH_O_DEF'		!SET OFFSETS
C
C  Parameters:
C
	INTEGER MAXINX			! Max nr of indices (local)
	  PARAMETER (MAXINX=17)
C
	INTEGER SCN_GROUPS,SCN_OBSS,SCN_FIELDS
	INTEGER SCN_CHANNELS,SCN_SECTORS
	  PARAMETER (SCN_GROUPS=1,SCN_OBSS=2,SCN_FIELDS=3)
	  PARAMETER (SCN_CHANNELS=4,SCN_SECTORS=5)
C
	INTEGER WMP_GROUPS,WMP_FIELDS,WMP_CHANNELS
	INTEGER WMP_POLARS,WMP_TYPES,WMP_MAPS
	  PARAMETER (WMP_GROUPS=6,WMP_FIELDS=7,WMP_CHANNELS=8)
	  PARAMETER (WMP_POLARS=9,WMP_TYPES=10,WMP_MAPS=11)
C
	INTEGER NGF_GROUPS,NGF_FIELDS,NGF_CHANNELS
	INTEGER NGF_POLARS,NGF_IFRS,NGF_CUTS
	  PARAMETER (NGF_GROUPS=12,NGF_FIELDS=13,NGF_CHANNELS=14)
	  PARAMETER (NGF_POLARS=15,NGF_IFRS=16,NGF_CUTS=17)
C
C  Arguments:
C
	CHARACTER*(*) KW		! Original keyword (e.g. SCN_SETS)
	CHARACTER*(*) TXT		! Original default string (WNDSTA)
	CHARACTER*(*) CSET		! User spec string (e.g. 0.0.*.2.0)
	INTEGER SETS			! returns the nr of Set specs (1 or 0)
C
C  Function references:
C
	LOGICAL WNDPAR			! Get input from the user
	LOGICAL WNCASD			! Incr ptr if digit (0-9)
	LOGICAL WNCASC			! Incr ptr if given char
	LOGICAL WNCATC			! Test for given char
	LOGICAL WNCASB			! Incr ptr if blank
C
C  Data declarations:
C
	CHARACTER*32 KWL(MAXINX)	! Keywords of the Set group
	  DATA KWL/	'SCN_GROUPS','SCN_OBSS','SCN_FIELDS',
	1		'SCN_CHANNELS','SCN_SECTORS',
	1		'WMP_GROUPS','WMP_FIELDS','WMP_CHANNELS',
	1		'WMP_POLARS','WMP_TYPES','WMP_MAPS',
	1		'NGF_GROUPS','NGF_FIELDS','NGF_CHANNELS',
	1		'NGF_POLARS','NGF_IFRS','NGF_CUTS'/
	CHARACTER*16 DFLTXT(MAXINX)	! Default value (WNDPAR)
	INTEGER NSPEC			! Input counter (WNDPAR)
	CHARACTER*32 STRIN		! User input string (WNDPAR)
	INTEGER N,INX1,INX2		! Misc
C
C-
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C  Determine from keyword name (KW) which group of keywords is required:
C
	IF (INDEX(KW,'WMP_SET').GT.0) THEN
	  INX1 = WMP_GROUPS
	  INX2 = WMP_MAPS
	  CALL WNCTXT (F_T,'Specify indices for  g.f.c.p.t.m  separately:') 
	  CALL WNCTXT (F_T,'  g = group(s)  : output run selection (0,1,...)') 
	  CALL WNCTXT (F_T,'  f = field(s)  : mosaicking (WSRT numbering)') 
	  CALL WNCTXT (F_T,'  c = channel(s): frequency (WSRT numbering)') 
	  CALL WNCTXT (F_T,'  p = polar(s)  : polarisations (0,1,2,3)') 
	  CALL WNCTXT (F_T,'  t = type(s)   : types (0=map, 1,2,3,4,5,6') 
	  CALL WNCTXT (F_T,'  m = map(s)    : sequence nr(s)') 
C
	ELSE IF (INDEX(KW,'SCN_SET').GT.0) THEN
	  INX1 = SCN_GROUPS			! first keyword
	  INX2 = SCN_SECTORS			! last keyword
	  CALL WNCTXT (F_T,'Specify indices for  g.o.f.c.s  separately:') 
	  CALL WNCTXT (F_T,'  g = group(s)  : e.g. object or calibrator') 
	  CALL WNCTXT (F_T,'  o = obs(s)    : e.g. multiple 12h observ.') 
	  CALL WNCTXT (F_T,'  f = field(s)  : pointing centres, mosaicking') 
	  CALL WNCTXT (F_T,'  c = channel(s): frequency (0=continuum)') 
	  CALL WNCTXT (F_T,'  s = sector(s) : contiguous HA-range(s)') 
C
	ELSE IF (INDEX(KW,'MDL_SET').GT.0) THEN
	  CALL WNCTXT (F_T,
     *		'MDL-Set sub-keyword layer not yet implemented') 
	  GOTO 900				! force retry in WNDSTA
C
	ELSE IF (INDEX(KW,'NGF_SET').GT.0) THEN
	  INX1 = NGF_GROUPS
	  INX2 = NGF_CUTS
	  CALL WNCTXT (F_T,'Specify indices for  g.f.c.p.i.c  separately:') 
	  CALL WNCTXT (F_T,'  g = group(s)  : usually only one (0)') 
	  CALL WNCTXT (F_T,'  f = field(s)  : mosaicking (WSRT numbering)') 
	  CALL WNCTXT (F_T,'  c = channel(s): frequency (WSRT numbering)') 
	  CALL WNCTXT (F_T,'  p = polar(s)  : polarisations (0,1,2,3)') 
	  CALL WNCTXT (F_T,'  i = ifr/tel(s): ifrs/tels (0,1,2,....)')
	  CALL WNCTXT (F_T,'  c = cut(s)    : sequence nr(s)') 
C
	ELSE IF (KW(:5) .EQ.'LOOPS' .OR. KW(5:10).EQ.'LOOPS') THEN
	  CALL WNCTXT (F_T,'LOOPS sub-keyword layer not implemented') 
	  GOTO 900				! force retry in WNDSTA
C
	ELSE
	  CALL WNCTXT (F_T,'Keyword name not recognised: !AS ',KW) 
	  GOTO 900				! force retry in WNDSTA
	END IF
C
C  Default values:
C  If a default string (TXT) was given to WNDPAR in the calling routine WNDSTA,
C  this string will be split into defaults for the 5/6 extra keywords: 
C
	DO I=INX1,INX2
	  DFLTXT(I)='0'				! Safe default default values
	END DO
C
	IF (TXT.EQ.'""' .OR. TXT.EQ.' ') THEN	! SETS(0,0).LE.0 (see WNDSTA)
	ELSE 					! Input Set default string given
	  CALL WNCTXT (F_T,'Default Set spec is: !AS ',TXT)
	  J1=1					! pointer in TXT
	  JS=WNCASB(TXT,J1)			! Strip blanks, incr ptr J1
	  JS=WNCASC(TXT,J1,'"')			! Strip quote("), if present (?)
C
	  IF (WNCASC(TXT,J1,'#')) THEN		! Abolute Unit nr (#n)
	    IF (WNCASC(TXT,J1,'*')) THEN	! All Units (Sectors/Maps)
	      DO I=INX1,INX2
	        DFLTXT(I)='*'			! All Indices
	      END DO
	    END IF
C
	  ELSE
	    DO I=INX1,INX2
	      DFLTXT(I)='*'			! All Indices
	    END DO
	    I=INX1
 77	      CONTINUE
	      J2=J1
	      DO WHILE (WNCASC(TXT,J2,'-').OR.
     *			WNCASC(TXT,J2,':').OR.
     *			WNCASC(TXT,J2,'*').OR.
     *			WNCASD(TXT,J2))
	      END DO
C
	      IF (J2.GT.J1) THEN		! if more than zero chars, 
		DFLTXT(I) = TXT(J1:J2-1)	! then copy the default string
	      END IF
C
	      IF (WNCATC(TXT,J2,'.')) THEN
		J1=J2+1				! skip the separating dot (.)
		I=I+1				! next index nr
		IF (I.LE.INX2) GOTO 77		! next index	
	      END IF
	    END IF
	  END IF
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Ask input from the user:
C
 100	  CONTINUE
	  CSET = ' '				! will be filled with spec
	  J=0					! char pointer in CSET
C
	  DO I=INX1,INX2
 110	    CONTINUE
	    JS = WNDPAR(KWL(I),STRIN,LEN(STRIN),NSPEC,DFLTXT(I))
C
	    IF (.NOT.JS) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) THEN	! interrupt (^Z, ^D, #)
	        GOTO 900			! Force retry in WNDSTA
	      ELSE
	        CALL WNCTXT (F_T,'Invalid input: !AS ',STRIN)
	        GOTO 110			! Try again
	      END IF
	    ELSE IF (NSPEC.LT.0.OR.E_C.EQ.DWC_WILDCARD) THEN   ! input is *
	      STRIN = '*'			! Should already be there?
	    ELSE IF (NSPEC.EQ.0.OR.E_C.EQ.DWC_NULLVALUE) THEN  ! input is ""
	      STRIN = DFLTXT(I)			! use default (?)
	    END IF
C
	    J1=1				! Start character in STRIN
	    DO WHILE (WNCASB(STRIN,J1).OR.
     *			WNCASC(STRIN,J1,'"').OR.
     *			WNCASC(STRIN,J1,'.'))
	    END DO
C
	    J2=J1				! Stop character in STRIN
	    DO WHILE (WNCASC(STRIN,J2,'-').OR.
     *			WNCASC(STRIN,J2,':').OR.
     *			WNCASC(STRIN,J2,'*').OR.
     *			WNCASD(STRIN,J2))
	    END DO
C
	    IF (J2.GT.J1) THEN
	      IF (J.LT.LEN(CSET)) THEN
	        CSET(J+1:) = STRIN(J1:J2-1)	! Add input strin to CSET
	        J=J+J2-J1
	      END IF
	    ELSE
	      CALL WNCTXT (F_T,'Invalid input: !AS ',STRIN)
	      GOTO 110				! Try again
	    END IF
C
	    IF (I.LT.INX2) THEN			! After all but last index
	      IF (J.LT.LEN(CSET)) THEN
	        CSET(J+1:) = '.'		! insert separation dot (.)
	        J=J+1
	      END IF
	    END IF
	  END DO
C
	GOTO 800				! Finished successfully
C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Finished successfully:
C
 800	CONTINUE
	CALL WNCTXT (F_T,'You have specified the Set: !AS ',CSET)
	CALL WNCTXT (F_T,
     *	'  It will now be used as default in the original question.')
	CALL WNCTXT (F_T,
     *	'  Type <CR> if you are satisfied, or try again.')
	WNDSTA_X = .TRUE.			! Successful Set spec
	SETS = 1				! One Set specified by user
C
	RETURN
C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Finished un-successfully:
C
 900	CONTINUE
	WNDSTA_X = .FALSE.			! forces retry in WNDSTA
	SETS = 0				! No Sets specified
	CSET = ' '
C
	RETURN
C
C
	END
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  The call to WNDSTA_X is inserted in WNDSTA.FOR in the following way:
C  Add the following declarations:
C
C	LOGICAL WNDSTA_X				! extra keyword layer
C
C  Insert the following after line 431
C			(IF (SETS(SOF_0_NLINE,0).EQ.0) GOTO 800):
C
C	IF ((INDEX(CSET,'@').GT.0).OR.(INDEX(CSET,'>').GT.0)) THEN
C  	  JS=WNDSTA_X(KW,TXT,CSET(1),
C			SETS(SOF_0_NLINE,0))	! extra keyword layer
C	  IF (.NOT.JS) GOTO 10			! Problem: try WNDPAR again
C	  TXT=CSET(1)				! Success: new default string
C	  J=WNCAL0(TXT)+1			! Significant length
C	  GOTO 10				! call WNDPAR with new default
C	END IF
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
