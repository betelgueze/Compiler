/*
	Interpret jazyka IFJ2011
	Autori:
		Michal Risa (xrisam01)
*/

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "main.h"

FILE *fin;
HT_INIT_STATIC(TFunc, DEFAULT_SIZE); /* tabulka funkcii */

int main(int argc, char *argv[])
{
	fin = stdin; /* defaultny vstup */
	int err = ALL_OK;

	if(argc == 2) {
		/* skusi otvorit vstupny subor */
		FILE *f = fopen(argv[1], "r");
		if(f == NULL) {
			err = errno;
			fprintf(stderr, "Error opening file '%s': %s\n",
				argv[1], strerror(err));
			return MEM_ERROR;
		}
		fin = f;
	}
	err = parse();

	if(fin != stdin)
		fclose(fin);

	/* prejde tabulku funkcii a uvolni jej obsah */
	struct htable_iter_t iter = ht_begin(TFunc);
	while(ht_iter_empty(iter) == 0) {
		free_function_info((struct Function*)ht_iter_deref(iter));
		iter = ht_iter_next(iter);
	}
	ht_clear(TFunc);
	/* tabulka funkcii je staticky alokovana takze ju netreba uvolnovat */

#ifdef LOG
	const char *err_str[] = {
		"OK",
		"Lexical error",
		"Syntax error",
		"Semantical error",
		"Runtime error",
		"Internal error",
		"Unknown error o.O"
	};
	if(err > 5)
		err = 5;
	fprintf(stderr, "STATUS: %s\n", err_str[err]);
#endif
	return err;
}

