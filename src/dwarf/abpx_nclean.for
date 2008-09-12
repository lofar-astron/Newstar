C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	ABPX_NCLEAN
C.Keywords:	Automatic Batch Programming
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:  HP	
C.Comments:
C.Version:	930323 Yuan Tang - creation
C.Version:	940120 CMV - Changed messenger
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE ABPX_NCLEAN
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Dry-run or run DWARF program NCLEAN
C.Returns:	Status code to command interpreter
C	success DWC_SUCCESS	for successfull dry_run
C	error	DWC_EXEERRORS	for dry-run or program-start error
C	exit status of program	if it has run
C.Notes:
C	- The program is invoked via a foreign command with the stream name as
C	  required parameter. The foreign command automatically includes the
C	  qualifier  /RUNMODE=NORMAL (batch mode), =DEFINE (definition mode)
C	  or =DRYRUN (check mode).
C#	- The available NCLEAN invocations are:
C#l	ABP_NCLEAN <standard_stream> <required_quals> [<optional_quals>]
C#l			one-line description
C#l	ABP_NCLEAN <stream>
C#l			use defaults saved in SYS$LOGIN:NCLEAN$<stream>.sav
C#l	ABP_NCLEAN always accepts the optional qualifiers
C#l		/<name>=<value>		default value:
C
C	- First, the external defaults for the program in the given stream will
C	  be cleared en restored from the file NCLEAN$<stream>.sav in ABPDIR
C	  (for the standard streams) or in the user's login directory (for
C	  other streams).
C	- Then, additional (possibly overriding) defaults will be specified
C	  according to the chosen stream and the given qualifiers.
C	- Finally, DWARF program NCLEAN will be run in the chosen stream
C	  unless the runmode is DRYRUN.
C-------------------------------------------------------------------------
C
	LOGICAL		ABP_RUN_INIT, ABP_RUN_SPEC, ABP_RUN_DO
	INTEGER		STR_SIGLEN, MSG_SET
C
	CHARACTER*(*)	PROGNAM			! this program
		PARAMETER (PROGNAM = 'NCLEAN')
	CHARACTER*(*)	DWFNAM			! DWARF program to be run
		PARAMETER (DWFNAM = 'NCLEAN')
C
C				Command-line syntax definition
C				- the first two arguments are always:
C				  parameter STREAM, default value ""
C				  qualifier RUNMODE, default value NORMAL
C				- up to 8 other qualifiers can be defined
C
	INTEGER		NRARG			! total nr of arguments [2,20]
		PARAMETER (NRARG = 15)
	CHARACTER*16	NAME(NRARG)		! argument names
	CHARACTER*6	DEFVAL(NRARG)		! default argument values
		DATA NAME(1),DEFVAL(1) /'STREAM','""'/
		DATA NAME(2),DEFVAL(2) /'RUNMODE','NORMAL'/
		DATA NAME(3),DEFVAL(3) /'INPUT_WMP_NODE'  ,'""'/
		DATA NAME(4),DEFVAL(4) /'INPUT_MDL_NODE' ,'""'/
                DATA NAME(5),DEFVAL(5) /'CLEAN_LIMIT','""'/
		DATA NAME(6),DEFVAL(6) /'AREA','""'/
		DATA NAME(7),DEFVAL(7) /'OUTPUT_MDL_NODE','""'/
		DATA NAME(8),DEFVAL(8) /'WMP_SETS','""'/
		DATA NAME(9),DEFVAL(9) /'SCN_NODE','""'/
		DATA NAME(10),DEFVAL(10) /'SCN_SETS','""'/
		DATA NAME(11),DEFVAL(11) /'SELECT_IFRS','""'/
		DATA NAME(12),DEFVAL(12) /'COMPON_LIMIT','""'/
		DATA NAME(13),DEFVAL(13) /'CLIP_AREA','""'/
		DATA NAME(14),DEFVAL(14) /'CLIP_LEVELS','""'/
		DATA NAME(15),DEFVAL(15) /'RESTORE_BEAM','""'/

C
C				You can define multiple arrays to let
C				it depend on the stream.
C
	INTEGER SWUREST(NRARG)    /0,0,1,1,0,0,0,1,0,0,0,0,0,0,1/
        INTEGER SWUVCOVER(NRARG)  /0,0,1,0,1,0,1,1,0,0,0,0,0,0,1/
	INTEGER SWBEAM(NRARG)     /0,0,1,0,1,1,1,0,0,0,0,0,0,0,0/
	INTEGER SWDATA(NRARG)     /0,0,1,1,1,0,1,1,1,1,1,1,1,1,1/
C
	CHARACTER	STREAM*12, RUNMODE*20
	CHARACTER	EXEFIL*80
	INTEGER		IS
	LOGICAL		OK
C
C
C				Initialise
C				- start DWARF control, messenger,
C				  command-line interpreter, PPD access
C				- get full executable spec
C				- get stream name and run mode
C				- refresh appropriate external defaults
C
	OK = ABP_RUN_INIT (PROGNAM,DWFNAM,NRARG,NAME,DEFVAL,
	1					EXEFIL,STREAM,RUNMODE)

	IF (.NOT.OK) GOTO 999
C
C				Read all program-specific qualifiers
C				and specify them as a symbol.
C
        IF (STREAM(1:8) .EQ. '$UVCOVER') THEN
	   OK = ABP_RUN_SPEC (DWFNAM,STREAM,SWUVCOVER,NAME,NAME,NRARG)
        ELSE IF (STREAM(1:6) .EQ. '$UREST') THEN
           OK = ABP_RUN_SPEC (DWFNAM,STREAM,SWUREST,NAME,NAME,NRARG)
        ELSE IF (STREAM(1:5) .EQ. '$DATA') THEN
           OK = ABP_RUN_SPEC (DWFNAM,STREAM,SWDATA,NAME,NAME,NRARG)
        ELSE
           OK = ABP_RUN_SPEC (DWFNAM,STREAM,SWBEAM,NAME,NAME,NRARG)
        ENDIF

	IF (.NOT.OK) GOTO 999
C
C				Run DWARF program
C

	OK = ABP_RUN_DO (DWFNAM,STREAM,EXEFIL,RUNMODE)

	IF (.NOT.OK) GOTO 999
C
C				Only get here if the program was not 
C				started (dry-run or failed startup)
C
	IS = DWC_SUCCESS
	GOTO 900
C
 999	IS = DWC_EXEERRORS
 900	E_C = MSG_SET (IS,0)
	END
