$!+wngfex_atnf.com
$!
$! File with system dependent commands for Newstar site ATNF (VAX version)
$!
$! Called by wngfex.com with the command in P1, the name of the 
$! input file in P2 and the name of the spool-file in P3.
$! If the file has to be deleted after spooling, P4 will be equal to "D"
$!
$ if (P1 .eqs. "SP")
$ then
$   COPY   'P2' 'P3'
$   LW132  'P3'
$   DELETE 'P3';*
$ endif
$!
$ if (P1 .eqs. "PS")  THEN LASER 'P2'
$!
$ if (P1 .eqs. "LA")
$ then
$       IF P4-"D" .NES. P4
$       THEN
$         PRINT/DELETE 'P2'
$       ELSE
$         PRINT 'P2'
$       ENDIF
$ endif
$!
$!-
