c+ WNGSWB.FOR
C  WNB 900315
C
C  Revisions:
C  GvD 920402 Added entry WNGSWQ
C
	SUBROUTINE WNGSWB(N,BUF)
C
C  Swap bytes
C
C  Result:
C
C	CALL WNGSWB( N_J:I, BUF_B(0:*):IO)
C			Will swap byte pairs in BUF of length N bytes
C	CALL WNGSWI( N_J:I, BUF_B(0:*):IO)
C			Will swap Integer*2 pairs in BUF of length N bytes
C	CALL WNGSWJ( N_J:I, BUF_B(0:*):IO)
C			Will reverse byte order in Integer*4 values in BUF
C			of length N bytes
C	CALL WNGSWQ( N_J:I, BUF_B(0:*):IO)
C			Will reverse byte order in Real*8 values in BUF
C			of length N bytes
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER N			!BUFFER LENGTH IN BYTES
	BYTE BUF(0:*)			!BUFFER TO TRANSLATE
C
C  Function references:
C
C
C  Data declarations:
C
	BYTE BB,BC(0:7)
C-
C
C SWB
C
	DO I=0,2*(N/2)-1,2
	  BB=BUF(I)
	  BUF(I)=BUF(I+1)
	  BUF(I+1)=BB
	END DO
C
	RETURN
C
C SWI
C
	ENTRY WNGSWI(N,BUF)
C
	DO I=0,4*(N/4)-1,4
	  DO I1=0,3
	    BC(I1)=BUF(I+I1)
	  END DO
	  BUF(I+0)=BC(2)
	  BUF(I+1)=BC(3)
	  BUF(I+2)=BC(0)
	  BUF(I+3)=BC(1)
	END DO
C
	RETURN
C
C SWJ
C
	ENTRY WNGSWJ(N,BUF)
C
	DO I=0,4*(N/4)-1,4
	  DO I1=0,3
	    BC(I1)=BUF(I+I1)
	  END DO
	  BUF(I+0)=BC(3)
	  BUF(I+1)=BC(2)
	  BUF(I+2)=BC(1)
	  BUF(I+3)=BC(0)
	END DO
C
	RETURN
C
C SWQ
C
	ENTRY WNGSWQ(N,BUF)
C
	DO I=0,8*(N/8)-1,8
	  DO I1=0,7
	    BC(I1)=BUF(I+I1)
	  END DO
	  BUF(I+0)=BC(7)
	  BUF(I+1)=BC(6)
	  BUF(I+2)=BC(5)
	  BUF(I+3)=BC(4)
	  BUF(I+4)=BC(3)
	  BUF(I+5)=BC(2)
	  BUF(I+6)=BC(1)
	  BUF(I+7)=BC(0)
	END DO
C
	RETURN
C
C
	END
