/*
	Interpret jazyka IFJ2011
	Autori:
		Tomas Valek (xvalek02)
*/

#ifndef __SCANNER__H
#define __SCANNER__H

#include <stdio.h>

/* Verze:	1.6*/

//PROSIM NEPISTE KOMENTARE S DIAKRITIKOU !!!

/* SEZNAM TOKENU */
enum {
	/* 1-znakove */
	T_OP_LBRACKET = 0, //	'('
	T_OP_RBRACKET, //       ')'
	T_OP_COMMA, //          ','
	T_OP_SEMIC, //          ';'
    
    T_OP_FIRST, //zarazka zacatek bloku operatoru = 4
	T_OP_ADD, // 		'+'
	T_OP_SUB, // 		'-'
	T_OP_DIV, // 		'/'
	T_OP_MULT, // 		'*'
	T_OP_POW, // 		'^'
	T_OP_SM, // 		'<'
	T_OP_BG, // 		'>'
	T_OP_ASSIGN, // 	'='
	T_OP_STRLEN , // 	'#'		13

	/* 2-znakove */
	T_OP_EQSM, // 		'<='
	T_OP_EQBG, // 		'>='
	T_OP_CAT, // 		'..'	konkatenace pouze pro string
	T_OP_EQ, // 		'=='
	T_OP_NOTEQ, // 		'~='
	T_OP_LAST, // zarazka konec bloku operatoru
	
	/* vice-znakove*/
	T_ID, 			// identifikator 20

	/* datove typy */
	T_VAL_INT,
	T_VAL_DOUBLE,
	T_VAL_STRING,	//23

	/* klicove slova */
	T_K_DO = 128,	//od 128, kvuli rychlejsimu bitovemu soucinu
	T_K_ELSE,
	T_K_END,
	T_K_FALSE,
	T_K_FUNCTION,
	T_K_IF,
	T_K_LOCAL,
	T_K_NIL,
	T_K_READ,
	T_K_RETURN,
	T_K_THEN, //138
	T_K_TRUE,
	T_K_WHILE,
	T_K_WRITE,
//	T_K_AND,		logicke operatory jsou docasne vypnuty
	T_K_BREAK,
	T_K_ELSEIF,
	T_K_FOR,
	T_K_IN,
//	T_K_NOT,
//	T_K_OR,
	T_K_REPEAT,
	T_K_UNTIL,
	/* funkce*/
	T_BF_TYPE,//151, 148 pokud jsou log. op. vypnuty
	T_BF_SUBSTR,
	T_BF_FIND,
	T_BF_SORT,

	/* chyby */
	T_EOF = EOF, // konec souboru
	T_ERROR = -2, // systemova chyba - out of memory, ...
	T_INVALID = -3,	// chyba lex. analyzatoru, spatny vstup
	T_START = -4
};

struct token_t {
	int type;
	union {
		void *p;
		int i;
		double d;
	}val;
};

struct token_t get_token(void);

#endif
