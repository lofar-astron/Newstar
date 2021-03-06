!+ DWE.DSC
!  WNB 940304
!
!  Revisions:
!
%REVISION=WNB=940304="Original version"
!
!	DWexecute common for switches
!
%COMMENT="DWE.DEF is a common area for the use of dwe switches"
%COMMENT=" "
!
%VERSION=1
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!
%ALIGN
!
%LOCAL=CLI__PARAMETER=0				!CLI bits
%LOCAL=CLI__REQUIRED=8
%LOCAL=CLI__QUALIFIER=1
%LOCAL=CLI__DEFAULT=4
%LOCAL=CLI__VALUE=16
%LOCAL=PREQ=CLI__PARAMETER+CLI__REQUIRED
%LOCAL=Q=CLI__QUALIFIER
%LOCAL=QDEF=Q+CLI__DEFAULT
%LOCAL=QVAL=Q+CLI__VALUE
!-
.DEFINE
  .DATA
	SW	A:(1)		/PROGSTRM,WAIT,DEBUG,TEST,ASK, \ !SWITCH NAMES
					SAVE,INPUT,BATCH,LOG, \ !Note: All NGEN variables
					RUN,DATAB,INFIX,APPLY, \ !after LOG
					DE_APPLY,UFLAG,MEMORY,MODELB/
%GLOBAL=NRARG=SW__N-1					!# OF SWITCHES
						!Note: Dwarf CLI interface
						!	limits it to 20 now
	ATTR	J(NRARG)	/PREQ,  QDEF,Q,Q,Q, \		!SWITCH TYPES
					Q,QVAL,Q,QVAL, \
					Q,QVAL,QVAL,QVAL, \
					QVAL,QVAL,QVAL,QVAL/
	PROMPT	C14(NRARG)	/"Program$stream",(NRARG-1)" "/ !SWITCH PROMPS
	DEFVAL	C8(NRARG)	/(8)" ",SPOOL, \		!SWITCH DEFAULT VALUE
					" "," "," ","*", \
					NONE,NONE,"100000"," "/
	NEGDEF	C8(NRARG)	/(8)" ",NO, \		!NEGATED DEFAULTS
					NO,"""""""""","""""""""",NONE, \
					ALL,ALL,100000,""""""""""/
	VSW	C80(NRARG)			!SWITCH VALUES
	LSW	J(NRARG)			!SWITCH VALUE LENGTHS
	QSW	J(NRARG)			!SWITCH SEEN VALUES
.END
