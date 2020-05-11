/*
	Interpret jazyka IFJ2011
	Autori:
		Martin Risa (xrisam00)
		Michal Risa (xrisam01)
		Josef Rudolf (xrudol04)
*/

#ifndef __MAIN__H
#define __MAIN__H

#include <assert.h>
#include "ial.h" /* algoritmy */
#include "list.h"
#include "scanner.h"
#include "parser.h"
#include "interpret.h"

#ifndef LOG
# define LOGERR(text)
# define LOGERR_VA(text, ...)
#else
# define LOGERR(text) fprintf(stderr, text)
#define LOGERR_VA(text, ...) fprintf(stderr, text, __VA_ARGS__)
#endif

#define ALL_OK		0
#define LEX_ERROR 	1
#define SYN_ERROR 	2
#define SEM_ERROR 	3
#define RUN_ERROR 	4
#define MEM_ERROR	5
#define bulgarian_constant 1
#define DEFAULT_SIZE 128

/* asociativita */
#define A_LEFT  0
#define A_RIGHT 1
#define A_NON 2



#define DEBUG

#endif

