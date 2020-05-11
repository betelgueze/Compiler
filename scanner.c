/*
	Interpret jazyka IFJ2011
	Autori:
		Tomas Valek (xvalek02)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h> //strcmp
#include <ctype.h>	//isdigit(c),..
#include <errno.h>
#include "scanner.h" 

#define DAVKA			32
#define POCET_KEYWORDS	24
#define ASCII			(48)
#define MAX_ESC			255
#define NUMBER_OF_ESC	3
#define VYCHOZI_STAV	1000

#define ASCII_TEST (c > 1 && c < 32) && (c != 9 && c != 10 && c != 12 && c != 13 )

#define VYPIS 0	//aktivace/deaktivace kontrolnich vypisu

/* SEZNAM STAVU */
enum {
	S_START = 0,
	S_ONE_SUB,	// -
	S_TWO_SUB,	// --
	S_TWO_SUB_SQ_BRACKET,	// --[
	S_TWO_SUB_TWO_SQ_BRACKET,	// --[[
	S_TWO_SUB_THREE_SQ_BRACKET, // --[[..]
	S_SMALLER,	// <
	S_GREATER,	// >
	S_EQUAL,	// =
	S_POINT,	// .
	S_TILDA,	// ~
	S_ID_KEYW,	// identifikator nebo klicove slovo
	S_NUM,		// cislo~=
	S_NUM_POINT,		//napr 8.
	S_NUM_POINT_NUM,	//napr 8.8
	S_NUM_E,			//napr 8e
	S_NUM_E_ADD,		//napr 8e+ neboplus 8.8e+
	S_NUM_E_SUB,		//napr 8e- nebo 8.8e-
	S_NUM_E_ADD_NUM,	//napr 8e+10
	S_S,			//retezec
	S_S_S,			//retezec2
	S_S_ESC,		//escape '\'
	S_S_ESC_NUM,	//escape cislo napr \002
};

//Klicova a rezervovana slova
char *key_word[POCET_KEYWORDS]={"do","else","end","false","function","if",
"local","nil","read","return","then","true","while","write"/*,"and"*/,"break",
"elseif","for","in"/*,"not","or"*/,"repeat","until","type","substr","find","sort"};

/* struktura pro retezec
 s je misto pro retezec
 alloc_size je alokovana velikost
 lenght je delka retezce
*/
typedef struct {
	char* string;
	int alloc_size;
	int lenght;
}T_STRING;


//GLOBALNI PROMENNA
extern FILE *fin;
static unsigned number_line = 1;	//cislo radku
static unsigned counter_esc = 0;	//citac poctu cisel v esc sekvenci napr \123 == counter_esc = 3
static unsigned number_esc = 0;		//citac escape cisel
static unsigned position = 0;		//pozice ve stringu

/*******************************************************************************
***********************************STRING***************************************
*******************************************************************************/

int init_string(T_STRING* s)
{/*Funkce inicializuje string. Tzn. vytvori pro nej pamet velikosti 
DAVKA(32bajtů).Funkce vraci chybu v pripade neprideleni pameti nebo 0 jako
 uspech.
 "s" je promenna struktury STRING
*/

	if ( (s->string = (char*)malloc(DAVKA)) == NULL )
	{//pokud se alokace nepodarila
		return 1;
	}

	s->string[0] = '\0';
	s->alloc_size = DAVKA;
	s->lenght = 0;

	return 0;
}

int add_char( char character, T_STRING* s )
{/*Funkce zkontroluje zda nedosla pamet a prida na konec retezce 1 znak.
 Pokud dosla prialokuje. Funkce vraci chybu malloc nebo konci uspechem 0.
 "character" je znak
 "s" je promenna struktury STRING
*/

	if ( s->lenght + 1 >= s->alloc_size  )
	{//dosla pamet pro string, musime reallokovat

		char* realloc_address;

		if ( (realloc_address = (char*)realloc(s->string,s->alloc_size*2 )) == NULL )
		//if ( realloc_address == NULL )
		{
			free((void*)s->string);
			s->string = NULL;
			return 1;
		}

		s->alloc_size = s->alloc_size*2;	//upravime delku stringu
		s->string = realloc_address;		//predame adresu
	}

	//pridavame znak do stringu:
	s->string[s->lenght] = character;
	s->lenght++;
	s->string[s->lenght] = '\0';

	return 0;
}
/*******************************************************************************
*******************************KONEC STRINGU************************************
*******************************************************************************/

/*******************************************************************************
***********************************FUNKCE***************************************
*******************************************************************************/

struct token_t get_token(void)
{/*Hlavni funkce lexikalniho analyzatoru. Funkce prechazi mezi stavy. Pokud
 nenastane konecny stav, jedna se o chybu. Chyba se vypise uvolni se pamet a 
vraci se token s chybou. Pokud konecny stav nastane funkce vraci token ve kterem
 je:
* type - typ tokenu
* a unie ve ktere je:
	* p - ukazatel bud na: string nebo identifikator
	* i - cislo intiger
	* d - cislo double
 U identifikatoru a stringu je nutno uvolnit pamet po zavolani
 funkce(free(token.val.p)).
*/

	struct token_t token;

	int state = S_START;	//stav automatu
	int c;					//nacitany znak

	token.type = VYCHOZI_STAV;		//vychozi stav tokenu
	token.val.p = NULL;

	T_STRING s;
	s.string = NULL;

	while ( 1 )
	{//nacitame znak po znaku dokud neskoncime v konecnem stavu

		c = getc(fin);
#if VYPIS
		printf("Znak:\t%c\n", c);
#endif
		if ( c == '\n' )
			++number_line;
		else if ( ASCII_TEST )
		{/*pokud je na vstupu znak s ASCII < 32 && > 1 vyjma znaku jako
		 tabulator, mezera, atd., tak vypiseme warning a ignorujeme ho. Viz.
		 makro ASCII_TEST*/
			fprintf(stderr,"Lex: Line %u, warning '%c'\n",number_line, c);
			continue;
		}
		else if ( c == EOF )
		{//konec souboru
			token.type = T_EOF;

			if ( state != S_START )
			{//Pokud je pocatecni stav, chyba.
				fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
				goto error;
			}
			free(s.string);
			return token;
		}

		switch (state)
		{//Case stavoveho automatu.

			case S_START:	//vychozi stav automatu(pocatecni)
				
				if ( isspace(c) != 0)// bily znak-preskocit
					state = S_START;
				else if ( (isalpha(c) != 0 ) || c == '_' )
				{//znak nebo _

					if ( init_string(&s) == 1 )
						goto system_error;

					if ( add_char(c, &s) == 1 )
						goto system_error;

					state = S_ID_KEYW;
				}
				else if ( isdigit(c) != 0 )
				{// cislo

					if ( init_string(&s) == 1 )
						goto system_error;

					if ( add_char(c, &s) == 1 )
						goto system_error;

					state = S_NUM;
				}
				else if ( c == '"' )
				{//retezec

					if ( init_string(&s) == 1 )
						goto system_error;

					state = S_S;
				}
				else if ( c == '+' )
					token.type = T_OP_ADD;
				else if ( c == '/' )
					token.type = T_OP_DIV;
				else if ( c == '*' )
					token.type = T_OP_MULT;
				else if ( c == '^' )
					token.type = T_OP_POW;
				else if ( c == '#' )//delka stringu
					token.type = T_OP_STRLEN;
				else if ( c == '(' )
					token.type = T_OP_LBRACKET;
				else if ( c == ')' )
					token.type = T_OP_RBRACKET;
				else if ( c == ',' )
					token.type = T_OP_COMMA;
				else if ( c == ';' )
					token.type = T_OP_SEMIC;
				else if ( c == '-' )
				{//minus
					if ( init_string(&s) == 1 )
						goto system_error;

					state = S_ONE_SUB;
					if ( add_char(c, &s) == 1 )
						goto system_error;
				}
				else if ( c == '<' )
					state = S_SMALLER;
				else if ( c == '>' )
					state = S_GREATER;
				else if ( c == '=' )
					state = S_EQUAL;
				else if ( c == '.' )
					state = S_POINT;
				else if ( c == '~' )
					state = S_TILDA;
				else
				{
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				break; //vyskoci z case a pokracuje while
/*==============================================================================
===========================================================================MINUS
==============================================================================*/
			case S_ONE_SUB:	// -
				if ( c == '-' )
				{// --
					state = S_TWO_SUB;
					if ( add_char(c, &s) == 1 )
						goto system_error;
				}
				else
				{
					if ( c == '\n' )
						--number_line;
					token.type = T_OP_SUB;
					ungetc(c, fin);
					free(s.string);
					s.string = NULL;
				}
				break;
/*==============================================================================
=======================================================================KOMENTARE
==============================================================================*/
			case S_TWO_SUB:	//--
				if ( c == '[' )
					state = S_TWO_SUB_SQ_BRACKET;
				else
				{	//jedna se o radkovy komentar
					do
					{
						if ( c == '\n' || c == EOF )
							break;
						c = getc(fin);
					}while(c != EOF && c != '\n');
					free(s.string);
					s.string = NULL;
#if VYPIS
					printf("STRING VYMAZAN \n");
#endif
					++number_line;
					state = S_START;
				}
				break;

			case S_TWO_SUB_SQ_BRACKET:	// --[
				if ( c == '[' )
					state = S_TWO_SUB_TWO_SQ_BRACKET;
				else
				{
					do
					{
						c = getc(fin);
					}while(c != EOF && c != '\n');
					free(s.string);
					s.string = NULL;
#if VYPIS
					printf("STRING VYMAZAN \n");
#endif
					++number_line;
					state = S_START;
				}
				break;

			case S_TWO_SUB_TWO_SQ_BRACKET:	//--[[
				while ( 1 )
				{//nacitame znaky v nekonecnem cyklu
					c = getc(fin);
					if ( c == ']' )
					{/*pokud se znak == ] otestujeme, zda za nim neni zase znak
					 ], pokud ano konec cyklu, uvolnime string, jdeme na
					 pocatecni stav.*/
						c = getc(fin);
						if ( c == ']' )
							break;
					}
					else if ( c == '\n' )
						++number_line;
					else if ( c == EOF )
					{
						fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
						goto error;
					}
				}
				free(s.string);
				s.string = NULL;
#if VYPIS
				printf("STRING VYMAZAN \n");
#endif
				state = S_START;
				break;
/*==============================================================================
===========================================================================MENSI
==============================================================================*/
			case S_SMALLER:	// <
				if ( c == '=' )
					token.type = T_OP_EQSM;
				else
				{
					if ( c == '\n' )
						--number_line;
					token.type = T_OP_SM;
					ungetc(c, fin);
				}
				break;
/*==============================================================================
===========================================================================VETSI
==============================================================================*/
			case S_GREATER:	// >
				if ( c == '=' )
					token.type = T_OP_EQBG;
				else
				{
					if ( c == '\n' )
						--number_line;
					token.type = T_OP_BG;
					ungetc(c, fin);
				}
				break;
/*==============================================================================
========================================================================ROVNA SE
==============================================================================*/
			case S_EQUAL:	// =
				if ( c == '=' )
					token.type = T_OP_EQ;
				else
				{
					if ( c == '\n' )
						--number_line;
					token.type = T_OP_ASSIGN;
					ungetc(c, fin);
				}
				break;
/*==============================================================================
===========================================================================TECKA
==============================================================================*/
			case S_POINT:	// .
				if ( c == '.' )
					token.type = T_OP_CAT;
				else
				{
					if ( c == '\n' )
						--number_line;
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				} 
				break;
/*==============================================================================
=========================================================================VLNOVKA
==============================================================================*/
			case S_TILDA:	// ~
				if ( c == '=' )
					token.type = T_OP_NOTEQ;
				else
				{
					if ( c == '\n' )
						--number_line;
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				} 
				break;
/*==============================================================================
//===============================================================ID nebo KEYWORD
==============================================================================*/
			case S_ID_KEYW:	// ID nebo klicove slovo zacina A-Za-z_
				if ( (isalnum(c) != 0) || c == '_' ) //cislo, pismeno nebo _
				{// pridame do retezce
					if ( add_char(c, &s) == 1 )
						goto system_error;
				}
				else if ( c == ';' || c == ',' || c == '*' || c == '+' ||\
						 c == '-' || (isspace(c) != 0) || c == '/' || c == '<'\
						 || c == '>' || c == '.' || c == '~' || c == '('\
						 || c == ')' || c == '^' || c == '=' )
				{//neni cislo ani pismeno ani _ takze ID konci
					if ( c == '\n' )
						--number_line;
					ungetc(c, fin);
#if VYPIS
					printf("KONTROLA: string = %s\n", s.string);
#endif

					for ( int i = 0; i < POCET_KEYWORDS; i++ )
					{//Zkontrolujeme, zda se jedna o klicove slovo
						if ( (strcmp(s.string, key_word[i])) == 0 )
						{//jedna se o klicove slovo
							token.type = T_K_DO + i;
							free(s.string);
							s.string = NULL;
							return token;	//zde se vraci KEY_WORD
						}
					}

					token.type = T_ID;
					token.val.p = s.string;
					//uvolneni pameti nastane az vne funkce
				}
				else
				{
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				break;
/*==============================================================================
===========================================================================CISLO
==============================================================================*/
			case S_NUM: // cislo
				if ( isdigit(c) != 0 )
				{//pridame do retezce
					if ( add_char(c, &s) == 1 )
						goto system_error;
				}
				else if( (c == 'e') || (c == 'E') )
				{
					if ( add_char(c, &s) == 1 )
						goto system_error;
					state = S_NUM_E;
				}
				else if ( c == '.' )
				{
					if ( add_char(c, &s) == 1 )
						goto system_error;
					state = S_NUM_POINT;
				}
				else if ( isspace(c) != 0 || c == '+' || c == '-' || c == '*' \
						|| c == '/' || c == '^' || c == '=' || c == ';' || c ==\
						 ',' || c == ')' || c == '~' || c == '<' || c == '>' )
				{/*to jsou znaky, ktere se mohou vyskytnout za cislem, ale uz
					 nejsou soucasti cisla*/
					if ( c == '\n' )
						--number_line;
					ungetc(c, fin);
					token.type = T_VAL_INT;
					//string na int:
					if ( sscanf(s.string, "%d", &token.val.i) != 1 )
					{//pokud nastala chyba pri sscanf
						int err = errno;
						fprintf(stderr, "Error converting '%s' to double: %s\n", s.string, strerror(err));
						goto system_error;
					}
					free(s.string);
					s.string = NULL;
				}
				else
				{
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				break;

			case S_NUM_POINT:	// napr 8.
				if ( isdigit(c) != 0 )
				{//pridame do retezce
					if ( add_char(c, &s) == 1 )
						goto system_error;
					state = S_NUM_POINT_NUM;
				}
				else
				{
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				break;

			case S_NUM_POINT_NUM:	//napr 8.8
				if ( isdigit(c) != 0 )
				{
					if ( add_char(c, &s) == 1 )
						goto system_error;
				}
				else if( (c == 'e') || (c == 'E') )
				{
					if ( add_char(c, &s) == 1 )
						goto system_error;
					state = S_NUM_E;
				}
				else if ( isspace(c) != 0 || c == '+' || c == '-' || c == '*' \
						|| c == '/' || c == '^' || c == '=' || c == ';' || c ==\
						 ',' || c == ')' || c == '~' || c == '<' || c == '>' )
				{/*to jsou znaky, ktere se mohou vyskytnout za cislem, ale uz
					 nejsou soucasti cisla*/
					if ( c == '\n' )
						--number_line;
					ungetc(c, fin);
					token.type = T_VAL_DOUBLE;
					//string na double:
					if ( sscanf(s.string, "%lf", &token.val.d) != 1 )
					{//pokud nastala chyba pri sscanf
						int err = errno;
						fprintf(stderr, "Error converting '%s' to double: %s\n", s.string, strerror(err));
						goto system_error;
					}
					free(s.string);
					s.string = NULL;
				}
				else
				{
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				break;

			case S_NUM_E:	//napr 8e nebo 8.8e
				if ( c == '+' || c == '-' )
				{
					state = S_NUM_E_ADD;
					if ( add_char(c, &s) == 1 )
						goto system_error;
				}
				else if ( isdigit(c) != 0 )
				{
					state = S_NUM_E_ADD_NUM;
					if ( add_char(c, &s) == 1 )
						goto system_error;
				}
				else
				{
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				break;

			case S_NUM_E_ADD:	// napr. 8e+- nebo 8e
				if ( isdigit(c) != 0 )
				{
					state = S_NUM_E_ADD_NUM;
					if ( add_char(c, &s) == 1 )
						goto system_error;
				}
				else
				{
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				break;

			case S_NUM_E_ADD_NUM:	// napr. 8e+-8
				if ( isdigit(c) != 0 )
				{
					state = S_NUM_E_ADD_NUM;
					if ( add_char(c, &s) == 1 )
						goto system_error;
				}
				else if ( isspace(c) != 0 || c == '+' || c == '-' || c == '*' \
						|| c == '/' || c == '^' || c == '=' || c == ';' || c ==\
						 ',' || c == ')' || c == '~' || c == '<' || c == '>' )
				{/*to jsou znaky, ktere se mohou vyskytnout za cislem, ale uz
					 nejsou soucasti cisla*/
					if ( c == '\n' )
						--number_line;
					ungetc(c, fin);
					token.type = T_VAL_DOUBLE;
					//string na double:
					if ( sscanf(s.string, "%lf", &token.val.d) != 1 )
					{//pokud nastala chyba pri sscanf
						int err = errno;
						fprintf(stderr, "Error converting '%s' to double: %s\n", s.string, strerror(err));
						goto system_error;
					}
					free(s.string);
					s.string = NULL;
				}
				else
				{
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				break;
/*==============================================================================
==========================================================================STRING
==============================================================================*/
			case S_S:	// "
				if ( c == '"' )
				{
					token.type = T_VAL_STRING;
					token.val.p = s.string;
					//uvolneni funkce nastane az vne programu
				}
				else if ( c == '\\' )
				{//tento znak se nepridava, protoze napr \n bude nahrazeno: '\n'
					state = S_S_ESC;
				}
				else if ( c == 0 || c == EOF )//0 je ascii hodnota
				{
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				else
				{
					if ( add_char(c, &s) == 1 )
						goto system_error;
					state = S_S;
				}
				break;
/*==============================================================================
==========================================================================ESCAPE
==============================================================================*/
			case S_S_ESC:	/*napr. "..\ 		*/
				if ( isdigit(c) != 0 )
				{
					position = s.lenght;	//ulozim si misto kde bylo '\' abych nahradil esc. sekvenci
					state = S_S_ESC_NUM;
					counter_esc++;	//prictu citac zadanych cisel za '\'
					number_esc = number_esc*10+(c-ASCII);
				}
				else if ( c == '\n' )
				{
					state = S_S;
				}
				else if ( c == 'n' )
				{
					if ( add_char('\n', &s) == 1 )
						goto system_error;
					state = S_S;
				}
				else if ( c == 't' )
				{
					if ( add_char('\t', &s) == 1 )
						goto system_error;
					state = S_S;
				}
				else if ( c == '\\' )
				{
					if ( add_char('\\', &s) == 1 )
						goto system_error;
					state = S_S;
				}
				else if ( c == '\"' )
				{
					if ( add_char('\"', &s) == 1 )
						goto system_error;
					state = S_S;
				}
				else
				{
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				break;

			case S_S_ESC_NUM:	// napr "..\0-9			
				if ( counter_esc == NUMBER_OF_ESC && number_esc > 0 && number_esc <= MAX_ESC )
				{//atoi
					//pridame tri cislice, ktere jsme vytahli z esc sekvence:
					s.string[position] = number_esc;
					number_esc = 0;	//vynulujeme cislo pro dalsi escape sekvenci
					counter_esc = 0;	//vynulujeme pocet cisel v escape
					s.lenght++;	//posuneme misto ve stringu o 1
					s.string[s.lenght] = '\0'; //za esc sekvenci musime vlozit '\0'
					ungetc(c, fin);
					state = S_S;
				}
				else if ( isdigit(c) != 0 )
				{
					counter_esc++;
					number_esc = number_esc*10+(c-ASCII);
					state = S_S_ESC_NUM;
				}
				else
				{
					if ( c == '\n' )
						--number_line;
					fprintf(stderr,"Lex: Line %u, invalid token '%c'\n",number_line, c);
					goto error;
				}
				break;

			default:
				goto error;
		}//konec case

		if ( token.type != VYCHOZI_STAV )
		{
#if VYPIS
			printf("radek c. %u\n", number_line);
#endif
			return token;
		}

	}//konec while
	printf("radek c. %u\n", number_line);
	return token;

error:	//pri chybe
	token.type = T_INVALID;
	free((void*)s.string);
	return token;

system_error:
	token.type = T_ERROR;
	free((void*)s.string);
	return token;

}
