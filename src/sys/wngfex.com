$! WNGFEX.COM
$! WNB 920911
$!
$! Revisions and documentation: see wngfex.csh
$!	WNB 940531	Change N_SRC usage and make for non-nfra sites
$!
$       VER=F$VERIFY(0)
$       APPEND="APPEND"                         !MAKE SURE
$       COPY="COPY"
$       DELETE="DELETE"
$       RENAME="RENAME"
$       P1=F$EDIT(P1,"UPCASE")
$       P4=F$EDIT(P4,"UPCASE")
$       IF P3 .EQS. "" THEN P3="''P2'"
$       A=F$SEARCH(P2)                          !SEE IF PRESENT
$       IF A .EQS. "" THEN GOTO EXIT
$!
$       IF F$EXTRACT(0,2,P1) .EQS. "RE" THEN GOTO REN
$       IF F$EXTRACT(0,2,P1) .EQS. "CC" THEN GOTO CAT
$       IF F$EXTRACT(0,2,P1) .EQS. "LN" THEN GOTO LNK
$       IF F$EXTRACT(0,2,P1) .EQS. "LR" THEN GOTO LRM
$       IF F$EXTRACT(0,2,P1) .EQS. "RL" THEN GOTO REM
$!
$       IF F$EXTRACT(0,2,P1) .EQS. "SP" THEN GOTO SPL
$       IF F$EXTRACT(0,2,P1) .EQS. "QM" THEN GOTO QMS
$       IF F$EXTRACT(0,2,P1) .EQS. "PS" THEN GOTO PSP
$       IF F$EXTRACT(0,2,P1) .EQS. "A3" THEN GOTO PA3
$       IF F$EXTRACT(0,2,P1) .EQS. "LA" THEN GOTO LAS
$!
$ EXIT: ON ERROR THEN EXIT
$       VER=F$VERIFY(VER)
$       EXIT
$!
$ REN:  ON ERROR THEN GOTO EXIT
$       RENAME 'A' 'P3'
$       GOTO EXIT
$!
$ CAT:  B=F$SEARCH(P3)                          !SEE IF OUTPUT PRESENT
$       ON ERROR THEN GOTO EXIT
$       IF B .NES. "" THEN APPEND 'A' 'B'
$       IF B .EQS. "" THEN COPY 'A' 'P3'
$       IF P4-"D" .NES. P4 THEN DELETE 'A'
$       GOTO EXIT
$!
$ LNK:  ON ERROR THEN GOTO EXIT
$       RENAME 'A' 'P3'
$       GOTO EXIT
$!
$ LRM:  ON ERROR THEN GOTO EXIT
$       RENAME 'A' 'P3'
$!
$ REM:  ON ERROR THEN GOTO EXIT
$       IF "0123456789"-F$EXTRACT(0,1,P4) .EQS. "0123456789" THEN P4=5 !5 DAYS
$       DELETE/NOLOG/MODIF/BEFORE="TODAY-''P4'-00:00:00" -
		*.TMP;*,*.LOG;*,*.PLT;*
$       GOTO EXIT
$!
$ SPL:  B=F$EDIT(F$GETJPI("","USERNAME"),"TRIM") !USER NAME
$       C=F$PARSE(P3,,,"NAME","SYNTAX_ONLY")    !FILE NAME
$       D=F$PARSE(P3,,,"TYPE","SYNTAX_ONLY")    !FILE TYPE
$       ON ERROR THEN GOTO EXIT
$       @N_SRC:[sys]wngfex_'F$TRNLNM("N_SITE")'.com SP 'A' 'B'_'C''D' 'P4'
$       IF P4-"D" .NES. P4 THEN DELETE 'A'
$       GOTO EXIT
$!
$ QMS:  ON ERROR THEN GOTO EXIT
$       @N_SRC:[sys]wngfex_'F$TRNLNM("N_SITE")'.com QM 'A' "" 'P4'
$       GOTO EXIT
$!
$ PSP:  ON ERROR THEN GOTO EXIT
$       @N_SRC:[sys]wngfex_'F$TRNLNM("N_SITE")'.com PS 'A' "" 'P4'
$       GOTO EXIT
$!
$ PA3:  ON ERROR THEN GOTO EXIT
$       @N_SRC:[sys]wngfex_'F$TRNLNM("N_SITE")'.com A3 'A' "" 'P4'
$       GOTO EXIT
$!
$ LAS:  ON ERROR THEN GOTO EXIT
$       @N_SRC:[sys]wngfex_'F$TRNLNM("N_SITE")'.com LA 'A' "" 'P4'
$       GOTO EXIT
