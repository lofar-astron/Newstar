c+ NCOINI.FOR
C  JPH 930317
C
C  Revisions:
C        JPH 930825        WNDDAB --> WNDDA0
C        AXC 010709        Linux ports - TABS
C
        SUBROUTINE NCOINI
C
C  Initialize NCOPY program
C
C  Result:
C
C        CALL NCOINI        will set header lines, logging, DWARF interface
C
C  Include files:
C
        INCLUDE 'WNG_DEF'
        INCLUDE 'NSC_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
        LOGICAL WNDINI                        !INIT DWARF
        LOGICAL WNDDA0                        !OPEN DATABASE
C
C  Data declarations:
C
C-
C
C SET HEADER LINES
C
        CALL WNCFHD(F_P,1,'!40C\NCOPY: Program to copy SCN files')
C
C START DWARF
C
        IF (.NOT.WNDINI(PRGNAM)) CALL WNGEX        !EXIT IF NO DWARF START
C
C LOGGING
C
        CALL WNDLOG(LOGCD)                        !PROPER LOGGING
C
C DATABASE
C
        IF (.NOT.WNDDA0()) CALL WNGEX                !OPEN DATABASE
C
        RETURN                                        !READY
C
C
        END
