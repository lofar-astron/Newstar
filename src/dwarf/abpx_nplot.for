C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	ABPX_NPLOT
C.Keywords:	Automatic Batch Programming
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	Alliant
C.Comments:
C.Version:	911206 GvD - creation
C.Version:	940120 CMV - Changed messenger
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE ABPX_NPLOT
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Dry-run or run DWARF program NMAP
C.Returns:	Status code to command interpreter
C	success DWC_SUCCESS	for successfull dry_run
C	error	DWC_EXEERRORS	for dry-run or program-start error
C	exit status of program	if it has run
C.Notes:
C	- The program is invoked via a foreign command with the stream name as
C	  required parameter. The foreign command automatically includes the
C	  qualifier  /RUNMODE=NORMAL (batch mode), =DEFINE (definition mode)
C	  or =DRYRUN (check mode).
C#	- The available NMAP invocations are:
C#l	ABP_NMAP <standard_stream> <required_quals> [<optional_quals>]
C#l			one-line description
C#l	ABP_NMAP <stream>
C#l			use defaults saved in SYS$LOGIN:NMAP$<stream>.sav
C#l	ABP_NMAP always accepts the optional qualifiers
C#l		/<name>=<value>		default value:
C
C	- First, the external defaults for the program in the given stream will
C	  be cleared en restored from the file NMAP$<stream>.sav in ABPDIR
C	  (for the standard streams) or in the user's login directory (for
C	  other streams).
C	- Then, additional (possibly overriding) defaults will be specified
C	  according to the chosen stream and the given qualifiers.
C	- Finally, DWARF program NMAP will be run in the chosen stream
C	  unless the runmode is DRYRUN.
C-------------------------------------------------------------------------
C
	LOGICAL		ABP_RUN_INIT, ABP_RUN_SPEC, ABP_RUN_DO
	INTEGER		STR_SIGLEN, MSG_SET
C
	CHARACTER*(*)	PROGNAM			! this program
		PARAMETER (PROGNAM = 'NPLOT')
	CHARACTER*(*)	DWFNAM			! DWARF program to be run
		PARAMETER (DWFNAM = 'NPLOT')
C
C				Command-line syntax definition
C				- the first two arguments are always:
C				  parameter STREAM, default value ""
C				  qualifier RUNMODE, default value NORMAL
C				- up to 8 other qualifiers can be defined
C
	INTEGER		NRARG			! total nr of arguments [2,20]
		PARAMETER (NRARG = 8)
	CHARACTER*16	NAME(NRARG)		! argument names
	CHARACTER*6	DEFVAL(NRARG)		! default argument values
		DATA NAME(1),DEFVAL(1) /'STREAM','""'/
		DATA NAME(2),DEFVAL(2) /'RUNMODE','NORMAL'/
		DATA NAME(3),DEFVAL(3) /'MAP_NODE'  ,'""'/
		DATA NAME(4),DEFVAL(4) /'SETS' ,'""'/
		DATA NAME(5),DEFVAL(5) /'MODEL_NODE' ,'""'/
                DATA NAME(6),DEFVAL(6) /'FULL_CONT'   ,'""'/
                DATA NAME(7),DEFVAL(7) /'DOT_CONT'   ,'""'/
		DATA NAME(8),DEFVAL(8) /'COORD' ,'""'/
C
C				Define which qualifiers (ie. keywords)
C				have to be defined or are optional.
C				  0 = ignore
C				  1 = required
C				  2 = optional
C				You can define multiple arrays to let
C				it depend on the stream.
C				Note that the switches for STREAM and RUNMODE
C				are of no interest.
C
	INTEGER SWPLOT(NRARG) /0,0,1,1,1,1,1,1/
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
C				Stream $RAW does not need the MODEL_NODE.
C
	IF (STREAM.EQ.'$PLOT') THEN
	    OK = ABP_RUN_SPEC (DWFNAM,STREAM,SWPLOT,NAME,NAME,NRARG)
	ENDIF
	IF (.NOT.OK) GOTO 999
C
C				Run DWARF program
C
	OK = ABP_RUN_DO (DWFNAM,STREAM,EXEFIL,RUNMODE)
	IF (.NOT.OK) GOTO 999
C                		Only get here if the program was not 
C				started (dry-run or failed startup)
C
	IS = DWC_SUCCESS
	GOTO 900
C
 999	IS = DWC_EXEERRORS
 900	E_C = MSG_SET (IS,0)
	END
