C+ WNDSTR.FOR
C  WNB 931015
C
C  Revisions:
C	WNB 910826	Add loop definitions to #
C	WNB 910909	Minor changes
C	JPH 930513	Comments
C	WNB 931015	Split off STR, STS; use SSH_DEF
C
	LOGICAL FUNCTION WNDSTR(FCA,SETS)
C
C  Reset or save the current set search path
C
C  Result:
C
C	WNDSTR_L = WNDSTR( FCA_J:I, SETS_J(0:*,0:*):IO)
C				Reset the set list SETS to initial conditions
C				Note: FCA not used
C	WNDSTS_L = WNDSTS( FCA_J:I, SETS_J(0:*,0:*):I, SSETS_J(0:*):O)
C				Save status of SETS list into SSETS list.
C				Note: FCA not used
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'SSH_O_DEF'		!SET RELATED
C
C  Parameters:
C
C
C  Entry points:
C
	LOGICAL WNDSTS			!SAVE SET STATUS
C
C  Arguments:
C
	INTEGER FCA			!FILE TO SEARCH
	INTEGER SETS(0:SOF__N-1,0:*)	!SETS TO DO
	INTEGER SSETS(0:SOF__N-1)	!SAVE SET LIST
C
C  Function references:
C
C
C  Data declarations:
C
C-
C
C WNDSTR
C
	WNDSTR=.TRUE.
	DO I=1,SOF__N-1
	  SETS(I,0)=0					!RESET SEARCH
	END DO
C
	RETURN
C
C WNDSTS
C
	ENTRY WNDSTS(FCA,SETS,SSETS)
C
	WNDSTS=.TRUE.
	DO I=0,SOF__N-1
	  SSETS(I)=SETS(I,0)				!SAVE SET STATUS
	END DO
C
	RETURN
C
C
	END
