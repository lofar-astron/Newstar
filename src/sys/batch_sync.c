/* batch_sync.c

	Reads output piped to stdin from Newstar program and echoes it on stdout
(pipe to log) and stderr (terminal).
	Newstar program affixes '~\n' to prompts. When this line ending is 
found, this program sends a 'go' signal to the named-pipe file whoise name is in
environment variable N_PSCTEST. This pipe is read by batch_ask, which responds 
by supplying one line of input to the Newstar program for each 'go' in the pipe.  

History:
   JPH 9510..
   JPH 960815	Disable control-C
   JPH 961018	Comments. Fix discarding of reply echo
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
 
int main()
{
  char *sp, text[512], *n_psctest;
  FILE *go;
  int pr;

  signal (SIGINT,SIG_IGN);
  n_psctest= getenv ("N_PSCTEST");	/* get name of pipe */
  go= fopen (n_psctest, "wu");		/* open it for unbuffered write */
  if (errno) {
    printf("ERROR	Can't open pipe '%s': ", n_psctest); perror("");
    return errno;
  }
 
  while (fgets (text, 512, stdin)) {	/* loop over input lines */
    pr= 0;
    for (sp=text; *sp; sp++);	 	/* skip to end of input */
    if (*--sp =='\n' && *--sp =='~'){	/* last chars '~\n' ?	*/
      fputs ("go\n", go);		/* yes, pipe to batch_ask */
      fflush (go);
      *sp= 0;				/* truncate before '~\n' */
      pr= 1;				/* set 'prompt' flag' */
    }
    fputs (text, stdout);		/* log output */
    fflush (stdout);
    fputs (text, stderr);		/* terminal output */
    fflush (stderr);
    if (pr) fgets (text, 512, stdin);	/* discard reply echo */
  }
  return errno;
}

