!+ NCO.DSC
!  JPH 930317
!
!  Revisions:
%REVISION=JPH=961213="Add SCANS"
%REVISION=JPH=960220="Add SIFRS"
%REVISION=JPH=940518="Add DO_MDL and DO_IFH"
%REVISION=JPH=931101="New maintenance system"
%REVISION=JPH=930317="Clone from NSC.DSC"
%REVISION=AXC=010709="Linux Port -  TABS "
!
!	Layout of overall include file (NCO.DEF)
!
%COMMENT="NCO.DEF is an INCLUDE file for the NCOPY program"
%COMMENT=" "
!
%VERSION=1
%SYSTEM=1
%USER=JPH
%%DATE
%%NAME
!
! Get number of telescopes
!
%INCLUDE=NSTAR_DSF
!
%LOCAL=MXNSCT=64				!SETS per job
%LOCAL=NIFR=NSTAR_TEL*(NSTAR_TEL+1)/2		!# INTERFEROMETERS

			!MAX. # OF MOSAICK FIELDS
!-
.DEFINE
  .PARAMETER
       MXNSCT	J	/MXNSCT/		!MAX. # OF SETS
  .DATA
!
!  Local variables:
!
  .COMMON
        OPTION	C24				!PROGRAM OPTION
        OPT=OPTION C3
        IFILE	C80				!INPUT FILE NAME
        OFILE	C80				!OUTPUT FILE NAME
        NODE	C80				!OUTPUT NODE
        INSCTS	J(0:7,0:MXNSCT)			!sectors TO DO
        OUTSCTS	J(0:7,0:MXNSCT)			!output sectors pattern
        SCANS	J(0:1)				! scans selection
        SIFRS   B(0:NSTAR_TEL-1,0:NSTAR_TEL-1)  !SELECTED INTERFEROMETERS
        HARAN	E(0:1)	/-.49999,.49999/	!HA range
        OINT	J				!OUTPUT INTEGRATION TIME (SEC)
        CVUTST	D				!UT/ST CONVERSION FACTOR
        FCAOUT	J				!OUTPUT FCA
        NODOUT	C80				!OUTPUT NODE
        FCAIN	J				!INPUT FCA
        NODIN	C80				!INPUT NODE
        INPOL	J				!# of input 
        ONPOL	J				! & output polsns
        SGPH	J(0:7)				!SUB-GROUP POINTER
        SGNR	J(0:7)				!SUB-GROUP NUMBER
        CAP	J	/0/			!apply
        CDAP	J	/0/			! /deapply bitmasks
        DO_MDL	L				!copy model
        DO_IFH	L				!copy IF-data
.END
