$!+wngfex_kosma.com
$!
$! File with system dependent commands for Newstar site KOSMA (VAX version)
$!
$! Called by wngfex.com with the command in P1, the name of the 
$! input file in P2 and the name of the spool-file in P3.
$! If the file has to be deleted after spooling, P4 will be equal to "D"
$!
$ if (P1 .eqs. "SP") 
$ then
$   COPY   'P2' 'P3'
$   IF P4-"D" .NES. P4
$   THEN
$     PRINT/QUEUE=MATRIX_LA100/DELETE 'P3'
$   ELSE
$     PRINT/QUEUE=MATRIX_LA100 'P3'
$   ENDIF
$ endif
$!
$ if (P1 .eqs. "PS") 
$ then
$        IF P4-"D" .NES. P4
$        THEN
$          PRINT/QUEUE=POSTSCRIPT/DELETE 'P2'
$        ELSE
$          PRINT/QUEUE=POSTSCRIPT 'P2'
$        ENDIF
$ endif
$!
$ if (P1 .eqs. "LA")
$ then
$        IF P4-"D" .NES. P4
$        THEN
$          PRINT/QUEUE=LASERJET/DELETE 'A'
$        ELSE
$          PRINT/QUEUE=LASERJET 'A'
$        ENDIF
$ endif
$!
$!-


