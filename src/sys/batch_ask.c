/* batch_ask.c */

/*	Transfer inputs list for Newstar program, prompting for user's input if parameter value is /ASK. 
	The list consist of lines of the form

	<blanks><parameter name><blanks>=<blanks><parameter value><comments>

Invocation:
	$n_exe/batch_ask.exe <input file>

Lines are read from $argv[1] and parameter values copied to stdout. When a parameter value /ASK is found, the parameter is prompted for on stderr and a value copied from stdin to stdout.   

History:
	JPH 951020	Adapt from include.
	JPH 960815	Disable control-C
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
 
int main (argc, argv)
  char** argv;
  int argc;
{
  FILE *in, *go;
  char l[4096], m[8];
  char *lp, *fp, *sp;
  int i;

  signal (SIGINT,SIG_IGN); 
  in= fopen(argv[1], "r");
  if (errno) {
    printf("ERROR	Can't open %s\n", argv[1]); return errno;
  }
  go = fopen(argv[2], "ru");
  if (errno) {
    printf("ERROR	Can't open %s\n", argv[2]); return errno;
  }
/*  setbuf (stdout, 0);
  setbuf (go, 0); */
 
  while (fgets (l, 4096, in)) {		/* parameter line */
    for (sp=l; *sp !='='; sp++);	/* skip over '=' and */
    for (sp++; *sp ==' '; sp++);	/* following blanks */
    if (! strncmp(sp, "/ASK", 4)) {	/* value is '/ASK' ? */
      *sp = '';
      *(sp+1) = 0;
      fputs (sp, stderr);		/* output alert (program has provided
      fflush (stderr);				the prompt)	*/
      fgets (sp, 4096, stdin);		/* get reply from stdin
    }else{
/*      fputs (l, stderr);	*/
    }
    fgets (m, 8, go);			/* wait for sync signal */
    fputs (sp, stdout);			/* reply to program	*/
    fflush (stdout);
  }
  return errno; 
}
