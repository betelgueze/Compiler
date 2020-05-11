/*
	Interpret jazyka IFJ2011
	Autori:
		Martin Risa (xrisam00)
*/

#ifndef __INTERPRET__H
#define __INTERPRET__H

#include "main.h"
#define TRUE 1
#define FALSE 0
#define bulgarian_constant 1

/**
 * Instruction Data
 */
typedef struct
{
  int I_Type;	  		// instruction type
  void * addr1; 		// address 1
  void * addr2; 		// address 2
  void * addr3; 		// address 3
  void * addr4; 		// address 4
}tInstr;

int interpret(void);

//list of instructions

//D is void * and represents data
//R is return value and is same as D
//I is tListItem *

//jump instructions
#define I_NOP			0		//(NULL,NULL,NULL,NULL)
#define I_CALL_START	1		//(NULL,NULL,NULL,D) D in this and next 2 cases represents char * ID of calling function
#define I_CALL_STOP		2		//(NULL,NULL,NULL,D)
#define I_CALL			3		//(NULL,NULL,NULL,D)
#define I_SETPARAM		4		//(D1,NULL,D2,R) D1  je char * a bere sa z aktualnej TS D2 je identifikator funkcie zktrej sa taha R ako char *
#define I_COPY			5		//(D,D,D,R)
#define I_COPY1			6		//(D,D,D,R)
#define I_STOP			7		//(NULL,NULL,NULL,NULL) no operation
#define I_GOTO 			8		//(NULL, NULL,NULL, I)
#define I_CONDGOTO		9		//(D, NULL,NULL, I)
#define I_CONDGOTO_INV	10		//(D, NULL,NULL, I)
#define I_READ			11		//(D,NULL,NULL,R) format is -1 for INT,-2 for DOUBLE, -3 for LINE, -4 for file positive, number for n chars
#define I_WRITE			12		//(NULL,NULL,NULL,D)
#define I_TYPE 			13		//(D,NULL,NULL,R)
#define I_SUBSTR		14		//(D,D,D,R)
#define I_FIND			15		//(D,D,NULL,R)
#define I_SORT			16		//(D,NULL,NULL,NULL)

//instructions in expressions
#define I_SET			17		//(D,NULL,NULL,R) // result is char * , ID of variable where store D
#define I_PLUS			18		//(D,D,NULL,R)
#define I_MINUS			19		//(D,D,NULL,R)
#define I_MULTIPLY		20		//(D,D,NULL,R)
#define I_DIVIDE		21		//(D,D,NULL,R)
#define I_EXPONENT		22		//(D,D,NULL,R)
#define I_CONCAT		23		//(D,D,NULL,R)
#define I_EQUALS		24		//(D,D,NULL,R)
#define I_NOTEQUALS		25		//(D,D,NULL,R)
#define I_GREATER		26		//(D,D,NULL,R)
#define I_SMALLER		27		//(D,D,NULL,R)
#define I_GREATEREQU	28		//(D,D,NULL,R)
#define I_SMALLEREQU	29		//(D,D,NULL,R)

#define I_STRLEN		30		//addr1 = str ktory pojde do strlen


/**s
 * structure for any variable
 * 
 * */
#define NIL 0
#define INT 1
#define DBL 2
#define BOL 3
#define STR 4

enum {
	RM_NUM = -'n',
	RM_LINE = -'l',
	RM_FILE = -'a'
};

/*
Popis instrukcii:
I_CALL_START: vytvori novy stack frame pre funkciu 'name' a vlozi ho na vrchol
		zasobniku
	@1,2,3: nezalezi
	@4: nazov funkcie 'name'

I_CALL_STOP: zmaze stack frame z vrcholu zasobniku funkcie 'name' a uvolni ho
	@1,2,3: nezalezi
	@4: nazov funkcie 'name

I_CALL: vlozi instrukcie funkcie 'name' do zoznamu vykonavanych instrukcii
	@1,2,3: nezalezi
	@4: nazov funkcie 'name'

I_SET: mov
	@1: vyhladavaci kluc pod kt. sa v stack frame beziacej funkcie najde zdroj
	@2,3: nezalezi
	@4: vyhladavaci kluc pod kt. sa v stack frame beziacej funkcie najde ciel

I_COPY: mov medzi rozdnymi stack frames
	@1: vyhladavaci kluc premennej z ktorej sa ide presuvat (stack frame na
		vrchole zasobniku)
	@2: nazov funkcie z ktorej sa ide presuvat
	@3: nazov funkcie do ktorej sa ide presuvat
	@4: vyhladavaci kluc premennej do ktorej sa ide presuvat (stack frame na
		vrchole zasobniku)
*/

#endif

