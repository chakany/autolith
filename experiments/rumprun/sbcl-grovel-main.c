/* Run one sb-grovel program in a rumprun guest. The program is compiled with
 * -Dmain=sbcl_grovel_main; it writes Lisp constants to a guest file, which
 * this wrapper prints to the console between markers for the host. */
#include <stdio.h>

int sbcl_grovel_main(int, char **);

int main(void)
{
    char *arguments[] = { "grovel", "/tmp/grovel.lisp", NULL };
    if (sbcl_grovel_main(2, arguments)) return 1;
    /* The program leaves its output stream open for exit to flush. */
    if (fflush(NULL)) { perror(arguments[1]); return 1; }
    FILE *stream = fopen(arguments[1], "r");
    if (!stream) { perror(arguments[1]); return 1; }
    puts("SBCL-GROVEL-BEGIN");
    for (int character; (character = getc(stream)) != EOF; ) putchar(character);
    puts("SBCL-GROVEL-END");
    return fclose(stream) ? 1 : 0;
}
