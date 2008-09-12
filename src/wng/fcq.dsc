!+ FCQ.DSC
!  WNB 850910
!
!  Revisions:
!
!
!	Define FCA (File Control Area) queue
!
!
!
%LOCAL=XHL=6					!LENGTH EXIT HANDLER BLOCK
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
%REVISION=WNB=890724="Original version"
%COMMENT="FCQ.DSC defines the FCA (File Control Area) queue and the"
%COMMENT="        FCA exit handler block."        	
!-
.DEFINE
.COMMON
	FCAQUE	J	/0/		!FCA QUEUE HEAD
	FCAEXH	J(XHL)	/(XHL)0/	!EXIT HANDLER BLOCK
.END					!END DEFINITION
!-
