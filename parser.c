/*
	Interpret jazyka IFJ2011
	Autori:
		Martin Risa (xrisam00)
		Michal Risa (xrisam01)
		Josef Rudolf (xrudol04)
*/

#include <assert.h>
#include <string.h>
#include "main.h"

/**
 * function generates structure of instruction and fills it with data
 * */
tInstr * GenerateInstruction(int type ,void *O1,void *O2,void *O3,void *O4,
	tList * list)
{
	tInstr * new = malloc(sizeof(tInstr));
	if(new == NULL)
		return NULL;

	new->I_Type = type;
	new->addr1 = O1;
	new->addr2 = O2;
	new->addr3 = O3;
	new->addr4 = O4;

	if(ListInsertLast(list, new) == MEM_ERROR) {
		free(new);
		return NULL;
	}

	return new;
}

extern FILE *fin; /* vstupny subor */
extern struct htable_t *TFunc; /* tabulka funkcii */
//premenna na predikovanie kompletizovania instrukcii skoku
struct Function *pf_info = NULL; /* momentalne spracovavana funckia v TFunc */
//aktuakne spracuvavany token
struct token_t token;

/* generovanie nazvov pomocnych premennych */
unsigned id_len = 0;
char *generate_id(void)
{
	char buf[32];
	int len = sprintf(buf, "@%u", id_len++);
	if(len < 0)
		return NULL;

	char *ret = malloc(len + 1);
 	if(ret == NULL)
		return NULL;

	strcpy(ret, buf);
	ret[len] = '\0';
	return ret;
}

/* vytvori novy token + kluc cez generate_id, vlozi ich do tabulky ht, vrati
	kluc + ulozi referenciu na token do t */
char *gen_and_insert_token(struct token_t **t, struct htable_t *ht)
{
	assert(ht != NULL);
	assert(t != NULL);
	char *id = generate_id();
	if(id == NULL)
		return NULL;

	struct token_t token;
	token.type = NIL;
        token.val.p = NULL;
	if((*t = ht_insert(ht, id, &token, sizeof token)) == NULL) {
		free(id);
		return NULL;
	}
	return id;
}
#if 0
int throw_params(int stat)
{
	int state = 0;
	assert(state == 0 || state == 1);
	while((token = get_token()).type != T_OP_RBRACKET);/** {
		if(token.type == T_OP_RBRACKET){break;}
		if(token.type == T_OP_COMMA)state=1;
		else if(token.type == T_ID || token.type == T_K_NIL || token.type == T_VAL_INT ||
				token.type == T_VAL_DOUBLE || token.type == T_VAL_STRING){
			if(token.type == T_ID) {
				if(state !=0){
					if(ht_lookup((struct htable_t*)pf_info->SymbolTable.Last->data,
						(char*)token.val.p) == NULL) {
						LOGERR_VA("Undefined parameter '%s'\n", (char*)token.val.p);
						free_token(token);
						return SEM_ERROR;
					}	
				}
				else state =1;
			}
		}
		else return SYN_ERROR;
		free_token(token);
		token = get_token();
		if(token.type == T_EOF)
			return SYN_ERROR;
		if(token.type == T_INVALID)
			return LEX_ERROR;*/
	
	return ALL_OK;
}
#endif
/* 
 * tabulka vlastnosti operatoru 
 * (priorita,asociativita,pocet operandu)
 * xrudol04
 */
const top_info op_table[] = {
  [T_OP_ADD] = {3,A_LEFT,2},
  [T_OP_SUB] = {3,A_LEFT,2},
  [T_OP_DIV] = {2,A_LEFT,2},
  [T_OP_MULT] = {2,A_LEFT,2},
  [T_OP_POW] = {1,A_RIGHT,2},
  [T_OP_SM] = {5,A_LEFT,2},
  [T_OP_BG] = {5,A_LEFT,2},
  [T_OP_STRLEN] = {0,A_NON,1},
  [T_OP_EQSM] = {5,A_LEFT,2},
  [T_OP_EQBG] = {5,A_LEFT,2},
  [T_OP_CAT] = {4,A_LEFT,2},
  [T_OP_EQ] = {5,A_LEFT,2},
  [T_OP_NOTEQ] = {5,A_LEFT,2}
};

const int MAX_OP_ARGS = 2;


enum {
	DS_START, /* poc. stav, ocakava "function" */
	DS_FID, /* identifikator funkcie */
	DS_FLB, /* ( */
	DS_FOPT_PAR,
	DS_PAR,
	DS_PARID,
	DS_ENDPAR,
	DS_VLOCAL,
	DS_VID, /* identifikator premennej */
	DS_VOPT_VAL,
	DS_VVAL, /* inicializator premennej */
	DS_VEND,
};

void print_instr_list(tList *l)
{
	assert(l != NULL);

	static const char *i_names[] = {
		"I_NOP",
		"I_CALL_START",
		"I_CALL_STOP",
		"I_CALL",
		"I_SETPARAM",
		"I_COPY",
		"I_COPY1",
		"I_STOP",
		"I_GOTO",
		"I_CONDGOTO",
		"I_CONDGOTO_INV",
		"I_READ",
		"I_WRITE",
		"I_TYPE",
		"I_SUBSTR",
		"I_FIND",
		"I_SORT",
		"I_SET",
		"I_PLUS",
		"I_MINUS",
		"I_MULTIPLY",
		"I_DIVIDE",
		"I_EXPONENT",
		"I_CONCAT",
		"I_EQUALS",
		"I_NOTEQUALS",
		"I_GREATER",
		"I_SMALLER",
		"I_GREATEREQU",
		"I_SMALLEREQU",
		"I_STRLEN",
	};

	char *empty = "(nil)";
	tListItem *item;
	printf("\tInstruction   [ addr1ptr  ' addr1str ';  addr2ptr  ' addr2str ';"
			"  addr3ptr  ' addr3str ';  addr4ptr  ' addr4str ']\n");
	for(item = l->First; item != NULL; item = item->NextItem) {
		assert(((tInstr*)item->data)->I_Type >= 0 &&
			((tInstr*)item->data)->I_Type <= I_STRLEN);

		printf("\t %12s [%10p '%10s'; %10p '%10s'; %10p '%10s'; %10p '%10s']\n",
			i_names[((tInstr*)item->data)->I_Type],
				((tInstr*)item->data)->addr1,
				((tInstr*)item->data)->addr1 == NULL ?
					empty : ((tInstr*)item->data)->I_Type == I_READ ? "0xCAFED00D" : (char*)((tInstr*)item->data)->addr1,
				((tInstr*)item->data)->addr2,
				((tInstr*)item->data)->addr2 == NULL ?
					empty : (char*)((tInstr*)item->data)->addr2,
				((tInstr*)item->data)->addr3,
				((tInstr*)item->data)->addr3 == NULL ?
					empty : (char*)((tInstr*)item->data)->addr3,
				((tInstr*)item->data)->addr4,
				((tInstr*)item->data)->addr4 == NULL ?
					empty : (char*)((tInstr*)item->data)->addr4);
	}
}

void print_stack(struct Function *func)
{
	const char *var_types[] = {
		"Nil",
		"Integer",
		"Double",
		"Bool",
		"String",
		"Unknown"
	};

	int frame;
	struct listItem *item;
	struct listItem *last = func->SymbolTable.Last;
	struct htable_iter_t iter;
	struct token_t *t;

	for(item = func->SymbolTable.First, frame = 0; item != NULL;
		item = item->NextItem, frame++) {

		item != last ?
			printf("\t\tFrame %i:\n", frame) : printf("\tVar table %p:\n",
				item->data);
		iter = ht_begin((struct htable_t*)item->data);

		printf("\t\t[ '    name    ' ;    ptr    ; '    type     ' ; '   value   ']\n");
		while(ht_iter_empty(iter) == 0) {
			t = ht_iter_deref(iter);
			int t_idx = t->type < 0 || t->type > STR ? STR + 1 : t->type;
/*			printf("\t\tVar '%s', type: %s, val: ", iter.entry->key,
				var_types[t_idx]);*/
			printf("\t\t[ '%12s' ; %p ; '%12s ' ;", iter.entry->key, (void*)t,
				var_types[t_idx]);
			if(strcmp(func->ID, iter.entry->key) == 0)
				printf("'  {retval}  ']\n");
			switch(t_idx) {
				case NIL:
					printf("'         NIL']\n");
					break;
				case INT:
					printf("'%12i']\n", t->val.i);
					break;
				case DBL:
					printf("'%12g']\n", t->val.d);
					break;
				case STR:
					printf("'%12s']\n", (char*)t->val.p);
					break;
				case BOL:
					printf("'%12s']\n", t->val.i ? "true" : "false");
					break;
				default:
					printf("==Error!!! o.O Unknown variable type d(#_#)b==\n");
					assert(0);
			}
			iter = ht_iter_next(iter);
		}
	}
}

void print_function_info(struct Function *func)
{
	assert(func != NULL);

	struct listItem *item;

	printf("\nFunction '%s' [%p] info:\n\tStack frames:\n", func->ID,
		(void*)func);

	print_stack(func);

	printf("\tParameters:\n");
	for(item = func->Parameters.First; item != NULL; item = item->NextItem)
		printf("\t\t%s\n", (char*)item->data);

	printf("\tInstructions:\n");
	print_instr_list(&func->Instruction);
}

void free_token(struct token_t token)
{
	if(token.type == T_ID || token.type == T_VAL_STRING)
		free(token.val.p);
}

void free_function_info(struct Function *func_info) /* xrisam01 */
{
	assert(func_info != NULL);
	LOGERR_VA("Freeing function '%s'\n", func_info->ID);
#ifdef LOG
	print_function_info(func_info);
#endif
	/* odstrani zoznam parametrov */
	ListDispose(&func_info->Parameters);

	/* odstrani stack frame-y */
	struct listItem *item;
	/* vzorova tabulka premennych */
	struct listItem *last = func_info->SymbolTable.Last; 
	for(item = func_info->SymbolTable.First; item != NULL;
		item = item->NextItem) {
		/* uvolni stringy */
		struct htable_iter_t iter = ht_begin((struct htable_t*)item->data);
		while(ht_iter_empty(iter) == 0) {
			if(((struct token_t*)ht_iter_deref(iter))->type == STR)
				free(((struct token_t*)ht_iter_deref(iter))->val.p);
			if(item != last)
	/* len vzorova tabulka (ktora je v last) moze uvolnovat hashovacie kluce */
				iter.entry->key = NULL;

			iter = ht_iter_next(iter);
		}
		ht_free((struct htable_t*)item->data);
	}
	/* odstrani zoznam stack frame-ov */
	ListDispose(&func_info->SymbolTable);
#if 1
	/* odstrani instrukcie */
	for(item = func_info->Instruction.First; item != NULL;
		item = item->NextItem) {
		free(item->data);
	}
#endif
	/* odstrani zoznam instrukcii */
	ListDispose(&func_info->Instruction);
}

static int decl_insert_param(struct htable_t *var_table)
{
	assert(var_table != NULL);

	if(ht_lookup(TFunc, (char*)token.val.p) != NULL) {
		/* parameter rovnakeho mena ako funkcia */
		LOGERR_VA("Invalid parameter identifier '%s', same function "
			"already exists\n", (char*)token.val.p);
		return SYN_ERROR;
	}
	if(ht_lookup(var_table, (char*)token.val.p) != NULL) {
		/* duplicitne parametre */
		LOGERR_VA("Multiple definitions of parameter '%s'\n",
			(char*)token.val.p);
		return SYN_ERROR;
	}
	/* vlozi parameter do zoznamu a tabulky */
	if(ListInsertLast(&pf_info->Parameters, token.val.p) != ALL_OK)
		return MEM_ERROR;

	token.type = NIL;
	if(ht_insert(var_table, (char*)token.val.p, (void*)&token, sizeof token)
		== NULL) {
		token.val.p = NULL;
		return MEM_ERROR;
	}
	token.val.p = NULL;
	return ALL_OK;
}

int declaration(void) /* spracovanie deklaracii */ /* xrisam01 */
{
	struct htable_t *var_table = ht_init(DEFAULT_SIZE); /* tabulka premennych */
	struct Function *f_rec_ht = NULL;
	if(var_table == NULL)
		return MEM_ERROR;

	id_len = 0U; /* zakladny stav generatora nazvov docasnych premennych */
	char *var_name = NULL;
	struct token_t *var_info = NULL;
	int ret = SYN_ERROR;
	struct Function f_info;
	memset((void*)&f_info, 0, sizeof f_info);
	/* zoznamy aj ich aktivita su nastavene na zakladne hodnoty */

	/* vzorova tabulka je aktivny prvok zoznamu */
	ListFirst(&f_info.SymbolTable);

	int state = DS_START;
	while(1) {
		token = get_token();
		if(token.type == T_EOF) {
			if(state == DS_START) {
				/* ziadna dalsia funkcia - uvolnit zdroje */
				ht_free(var_table);
				ListDispose(&f_info.SymbolTable);
				return ALL_OK;
			}
			LOGERR("Unexpected End Of File\n");
			break;
		}
		if(token.type == T_ERROR || token.type == T_INVALID) {
			/* chyba lexikalnej analyzy */
			ret = token.type == T_ERROR ? MEM_ERROR : LEX_ERROR;
			break;
		}

		switch(state) {
			case DS_START: /* ocakava "function" */
				if(token.type != T_K_FUNCTION) {
					LOGERR("'function' expected\n");
					goto end;
				}

				state = DS_FID;
				break;

			case DS_FID: /* ocakava idenfitikator funkcie */
				if(token.type != T_ID) {
					LOGERR("Identifier expected\n");
					goto end;
				}
				if(ht_lookup(TFunc, (char*)token.val.p) != NULL) {
					/* funkcia uz bola definovana */
					LOGERR_VA("Multiple definitions of function '%s'\n",
						(char*)token.val.p);
					goto end;
				}
				f_info.ID = (char*)token.val.p; /* identifikator funkcie */

				/* vlozi funkciu do tabulky funkcii */
				f_rec_ht = pf_info = (struct Function *)ht_insert(TFunc, f_info.ID,
					(void*)&f_info, sizeof(f_info));
				if(pf_info == NULL) {
					ret = MEM_ERROR;
					goto end;
				}
				token.val.p = NULL;

				/* vlozi vzorovu tabulku premennych do stack frame-ov funkcie */
				if(ListInsertLast(&pf_info->SymbolTable, (void*)var_table)
					!= ALL_OK) {
//					ht_free(var_table);
					ret = MEM_ERROR;
					goto end;
				}
#if 1
				/* vlozi implicitne premenne */
				char *def_vars[] = {
					"type",
					"substr",
					"find",
					"sort",
					f_info.ID
				};
				char *key;
				struct token_t tmp;

				int pos;
				for(pos = 0; pos < 5; pos++) {
					tmp.type = NIL;
					key = malloc(strlen(def_vars[pos]) + 1);
					if(key == NULL) {
						ret = MEM_ERROR;
						goto end;
					}
					strcpy(key, def_vars[pos]);
					if(ht_insert(var_table, key, (void*)&tmp, sizeof tmp)
						== NULL) {
						free(key);
						ret = MEM_ERROR;
						goto end;
					}
				}
#endif
				state = DS_FLB;
				break;

			case DS_FLB: /* ocakava '(' */
				if(token.type != T_OP_LBRACKET) {
					LOGERR("'(' expected\n");
					goto end;
				}
				state = DS_FOPT_PAR;
				break;

			case DS_FOPT_PAR: /* ocakava identifikator parametra alebo ')' */
				if(token.type == T_OP_RBRACKET) {
					state = DS_VLOCAL;
					/* deklaracia spracovana */
					break;
				}
				if(token.type == T_ID) {
					int p_ret = decl_insert_param(var_table);
					if(p_ret != ALL_OK) {
						ret = p_ret;
						goto end;
					}
					state = DS_ENDPAR;
					break;
				}
				/* chyba */
				LOGERR("')' or identifier expected\n");
				goto end;

			case DS_ENDPAR: /* ocakava ukoncenie parametra = ',' alebo ')' */
				if(token.type == T_OP_COMMA) {
					state = DS_PARID;
					break;
				}
				if(token.type == T_OP_RBRACKET) {
					state = DS_VLOCAL;
					/* deklaracia spracovana */
					break;
				}
				LOGERR("',' or ')' expected\n");
				goto end;

			case DS_PARID: /* ocakava identifikator parametra */
				if(token.type == T_ID) {
					int p_ret = decl_insert_param(var_table);
					if(p_ret != ALL_OK) {
						ret = p_ret;
						goto end;
					}
					state = DS_ENDPAR;
					break;
				}
				LOGERR("Identifier expected\n");
				goto end;

			case DS_VLOCAL: /* ocakava "local" */
				if(token.type != T_K_LOCAL) {
					/* koniec lokalnych premennych */
					ret = ALL_OK;
					goto end;
				}
				state = DS_VID;
				break;

			case DS_VID: // ocekava ID promenne
				if(token.type == T_ID) {
					if(ht_lookup(var_table, (char*)token.val.p) != NULL) {
						/* premenna uz existuje */
						LOGERR_VA("Multiple declarations of variable"
							" '%s'\n", (char*)token.val.p);
						goto end;
					}
					if(ht_lookup(TFunc, (char*)token.val.p) != NULL) {
						/* premenna rovnakeho mena ako funkcia */
						LOGERR_VA("Invalid variable identifier '%s', "
							"same function already exists\n",
							(char*)token.val.p);
						goto end;
					}
					var_name = (char*)token.val.p;
					/* upravi typ tokenu */
					token.type = NIL;
					/* vlozi do tabulky */
					var_info = ht_insert(var_table, (char*)token.val.p,
						(void*)&token, sizeof token);
					if(var_info == NULL) {
						ret = MEM_ERROR;
						goto end;
					}
					token.val.p = NULL;
					state = DS_VOPT_VAL;
					break;
				}
	
				LOGERR("Identifier expected\n");
				/* chyba */
				goto end;

			case DS_VOPT_VAL:
				if(token.type == T_OP_SEMIC) {
					/* koniec jednej lokalnej premennej */
					state = DS_VLOCAL;
					break;
				}
				if(token.type == T_OP_ASSIGN) {
					state = DS_VVAL;
					break;
				}
				LOGERR("'=' or ';' expected\n");
				/* chyba */
				goto end;

			case DS_VVAL:
				assert(var_info != NULL);
				/* konstanty */
				if(token.type == T_VAL_INT) {
					var_info->type = INT;
					var_info->val.i = token.val.i;
					state = DS_VEND;
					break;
				}
				if(token.type == T_VAL_DOUBLE) {
					var_info->type = DBL;
					var_info->val.d = token.val.d;
					state = DS_VEND;
					break;
				}
				if(token.type == T_VAL_STRING) {
					var_info->type = STR;
					var_info->val.p = token.val.p;
					state = DS_VEND;
					break;
				}
				if(token.type == T_K_TRUE || token.type == T_K_FALSE) {
					var_info->type = BOL;
					/* nemozno dat: token.type == T_K_TRUE */
					var_info->val.i = token.type == T_K_TRUE ? TRUE : FALSE;
					state = DS_VEND;
					break;
				}
				LOGERR("Constant expected\n");
				/* chyba */
				goto end;

			case DS_VEND:
				if(token.type == T_OP_SEMIC) {
					/* koniec jednej lokalnej premennej */
					state = DS_VLOCAL;
					break;
				}
				LOGERR("';' expected\n");
				/* chyba */
				goto end;
#ifndef NDEBUG
			default:
				assert(0);
#endif
		}
	}
end:
	/* cleanup */
	if(ret != ALL_OK) {
		free_token(token);
		if(f_rec_ht == NULL) {
			/* lokalny neporiadok */
			ht_free(var_table);
			ListDispose(&f_info.SymbolTable);
		}
	}
	return ret;
}

/*
 * 
 * 
 * 
 * 
 * 
 * SEKCE ZPRACOVÁNÍ VÝRAZŮ
 * 
 * 
 * 
 * 
 * 
 * 
 */


/*
 * Pomocná funkce pro výpis obsahu seznamu tokenů.
 * Na vstupu tList obsahující ukazatele na dynamicky alokované
 * tokeny.
 * Tato funkce je jen testovací a bude odstraněna.
 * xrudol04
 */
// void TokenListPrint(tList *L) {
//   struct token_t *acttoken = NULL;
//   int i = 0;
//   ListFirst(L);
//   while (L->Act != NULL) {
//     printf("Token %d: ",i);
//     i++;
//     acttoken = (struct token_t *)L->Act->data;
//     switch (acttoken->type) {
//       case T_OP_ADD:
//         printf("T_OP_ADD");
//         break;
//       case T_OP_SUB:
//         printf("T_OP_SUB");
//         break;
//       case T_OP_MULT:
//         printf("T_OP_MULT");
//         break;
//       case T_OP_POW:
//         printf("T_OP_POW");
//         break;
//       case T_OP_SM:
//         printf("T_OP_SM");
//         break;
//       case T_OP_BG:
//         printf("T_OP_BG");
//         break;
//       case T_OP_ASSIGN:
//         printf("T_OP_ASSIGN");
//         break;
//       case T_OP_STRLEN:
//         printf("T_OP_STRLEN");
//         break;
//       case T_OP_LBRACKET:
//         printf("T_OP_LBRACKET");
//         break;
//       case T_OP_RBRACKET:
//         printf("T_OP_RBRACKET");
//         break;
//       case T_OP_COMMA:
//         printf("T_OP_COMMA");
//         break;
//       case T_OP_SEMIC:
//         printf("T_OP_SEMIC");
//         break;
//       case T_OP_EQSM:
//         printf("T_OP_EQSM");
//         break;
//       case T_OP_EQBG:
//         printf("T_OP_EQBG");
//         break;
//       case T_OP_CAT:
//         printf("T_OP_CAT");
//         break;
//       case T_OP_EQ:
//         printf("T_OP_EQ");
//         break;
//       case T_OP_NOTEQ:
//         printf("T_OP_NOTEQ");
//         break;
//       case T_ID:
//         printf("T_ID: %s",(char *)acttoken->val.p);
//         break;
//       case T_VAL_INT:
//         printf("T_VAL_INT: %d",acttoken->val.i);
//         break;
//       case T_VAL_DOUBLE:
//         printf("T_VAL_DOUBLE: %lf",acttoken->val.d);
//         break;
//       case T_VAL_STRING:
//         printf("T_VAL_STRING: %s",(char *)acttoken->val.p);
//         break;
//       case T_BF_TYPE:
//         printf("T_BF_TYPE");
//         break;
//       case T_BF_SUBSTR:
//         printf("T_BF_SUBSTR");
//         break;
//       case T_BF_FIND:
//         printf("T_BF_FIND");
//         break;
//       case T_BF_SORT:
//         printf("T_BF_SORT");
//         break;
//       case T_K_DO:
//         printf("T_K_DO");
//         break;
//       case T_K_ELSE:
//         printf("T_K_ELSE");
//         break;
//       case T_K_END:
//         printf("T_K_END");
//         break;
//       case T_K_FALSE:
//         printf("T_K_FALSE");
//         break;
//       case T_K_FUNCTION:
//         printf("T_K_FUNCTION");
//         break;
//       case T_K_IF:
//         printf("T_K_IF");
//         break;
//       case T_K_LOCAL:
//         printf("T_K_LOCAL");
//         break;
//       case T_K_NIL:
//         printf("T_K_NIL");
//         break;
//       case T_K_READ:
//         printf("T_K_READ");
//         break;
//       case T_K_RETURN:
//         printf("T_K_RETURN");
//         break;
//       case T_K_THEN:
//         printf("T_K_THEN");
//         break;
//       case T_K_TRUE:
//         printf("T_K_TRUE");
//         break;
//       case T_K_WHILE:
//         printf("T_K_WHILE");
//         break;
//       case T_K_WRITE:
//         printf("T_K_WRITE");
//         break;
//      /* case T_K_AND:			//v pripade implementace odkomentovat v scanner.h
//         printf("T_K_AND");*/
//         break;
//       case T_K_BREAK:
//         printf("T_K_BREAK");
//         break;
//       case T_K_ELSEIF:
//         printf("T_K_ELSEIF");
//         break;
//       case T_K_FOR:
//         printf("T_K_FOR");
//         break;
//       case T_K_IN:
//         printf("T_K_IN");
//         break;
//    /*   case T_K_NOT:			//v pripade implementace odkomentovat v scanner.h
//         printf("T_K_NOT");*/
//         break;
//    /*   case T_K_OR:
//         printf("T_K_OR");*/		//v pripade implementace odkomentovat v scanner.h
//         break;
//       case T_K_REPEAT:
//         printf("T_K_REPEAT");
//         break;
//       case T_K_UNTIL:
//         printf("T_K_UNTIL");
//         break;
//       case T_EOF:
//         printf("T_EOF");
//         break;
//       case T_ERROR:
//         printf("T_ERROR");
//         break;
//       case T_INVALID:
//         printf("T_INVALID");
//         break;
//       case T_START:
//         printf("T_START");
//         break;
//     }
//     printf("\n");
//     ListSucc(L);
//   }
// }



/*
 * Pomocná funkce pro výpis postfix notace.
 * Na vstupu tList obsahující ukazatele na dynamicky alokované
 * tokeny.
 * Tato funkce je jen testovací a bude odstraněna.
 * xrudol04
 */
// void TokenListPostfixPrint(tList *L) {
//   struct token_t *acttoken = NULL;
//   ListFirst(L);
//   while (L->Act != NULL) {
//     acttoken = (struct token_t *)L->Act->data;
//     switch (acttoken->type) {
//       case T_OP_ADD:
//         printf("+");
//         break;
//       case T_OP_SUB:
//         printf("-");
//         break;
//       case T_OP_MULT:
//         printf("*");
//         break;
//       case T_OP_POW:
//         printf("^");
//         break;
//       case T_OP_SM:
//         printf("<");
//         break;
//       case T_OP_BG:
//         printf(">");
//         break;
//       case T_OP_ASSIGN:
//         printf("=");
//         break;
//       case T_OP_STRLEN:
//         printf("#");
//         break;
//       case T_OP_LBRACKET:
//         printf("(");
//         break;
//       case T_OP_RBRACKET:
//         printf(")");
//         break;
//       case T_OP_COMMA:
//         printf(",");
//         break;
//       case T_OP_SEMIC:
//         printf(";");
//         break;
//       case T_OP_EQSM:
//         printf("<=");
//         break;
//       case T_OP_EQBG:
//         printf(">=");
//         break;
//       case T_OP_CAT:
//         printf("..");
//         break;
//       case T_OP_EQ:
//         printf("==");
//         break;
//       case T_OP_NOTEQ:
//         printf("~=");
//         break;
//       case T_ID:
//         printf("%s",(char *)acttoken->val.p);
//         break;
//       case T_VAL_INT:
//         printf("%d",acttoken->val.i);
//         break;
//       case T_VAL_DOUBLE:
//         printf("%lf",acttoken->val.d);
//         break;
//       case T_VAL_STRING:
//         printf("%s",(char *)acttoken->val.p);
//         break;
//       case T_K_FALSE:
//         printf("false");
//         break;
//       case T_K_TRUE:
//         printf("true");
//         break;
//     }
//     printf(" ");
//     ListSucc(L);
//   }
// }


/*
 * Funkce, která dynamicky alokuje nový token a zkopíruje
 * do něho data z tokenu na vstupu.
 * Vstupní proměnná je ukazatel na strukturu token_t.
 * Funkce vrací ukazatel na nově alokovanou strukturu token_t.
 * xrudol04
 */
struct token_t *itp_copytoken(struct token_t *input) {
  struct token_t *output = NULL;
  output = malloc(sizeof(struct token_t));
  if (output != NULL) {
    output->val.p = NULL;
    output->type = input->type;
    if (input->type == T_VAL_STRING || input->type == T_ID) {
      output->val.p = malloc((strlen(input->val.p)+1)*sizeof(char));
      if (output->val.p == NULL) {
        free(output);
        return NULL;
      }
      strcpy(output->val.p,input->val.p);
    }
    else {
      output->val = input->val;
    }

  }
  return output;
}

/*
 * Uvolní token a jeho string. Na svtupu ukazatel na token.
 * xrudol04
 */
void free_inputtoken(struct token_t *inputtoken) {
  if (inputtoken->type == T_VAL_STRING || inputtoken->type == T_ID) {
    free(inputtoken->val.p);
  }
  free(inputtoken);
}


 /**
  * funckia spracuvava volanie funkcii
  */
char * parse_calling(int * error)
{
	//pocitadlo parametrov funkcie
	int params_count = 0;
	//celkovy pocet arametrov
	int params_total = 0;
	//identifikator ci sa jedna o vstavanu funkciu
	int builtin_id = 0;

	//tu sa nam vrati vysledok
	//char * F_result;
	char * F_name = NULL;
	//struct * aha;
	TFunction * F = NULL;
	if(!(token.type == T_K_READ || (token.type <= T_BF_SORT && token.type>= T_BF_TYPE))){
			//F_result = token.val.p;
		//nazov funkcie ktoru chceme volat
		F_name = token.val.p;
		//data k funkcii ktoru volame
		if(F_name)
			F = ht_lookup(TFunc,F_name);
		//smeanticka kontrola definovania voalnej funkcie
	}

	if(!F){
		//najprv zkontrolujeme ci sa nejedna o vstavanu funkciu a
		if(token.type == T_K_READ){
			builtin_id = T_K_READ;
			params_total = 1;
		}
		else if(token.type == T_BF_TYPE){
			//nasavime identifikator builtin funkie
			builtin_id = T_BF_TYPE;
			//zaznamenanie poctu vstupnyhc parametrov
			params_total = 1;
		}
		else if(token.type == T_BF_FIND){
			builtin_id = T_BF_FIND;
			params_total = 2;
		}
		else if(token.type == T_BF_SUBSTR){
			builtin_id = T_BF_SUBSTR;
			params_total = 3;
		}
		else if(token.type == T_BF_SORT){
			builtin_id = T_BF_SORT;
			params_total = 1;
		}
		//inak sa jedna o nedefinovanu funkciu -> SEM_ERROR
		else {
			*error = SEM_ERROR;
			return NULL;
		}
	}
	else {
          //uvolnění stringu z paměti a získání ukazatele na název funkce z hashtable
          //opravuje memory leak
          free(F_name);
          F_name = (char *)(list_entry(F, struct htable_entry_t, data)->key);
	}
	//aktualny zoznam instrukcii
	//tList * list = &(pf_info->Instruction);
	//zoznam nazvov vstupnych arametrov
	tListItem * lp_item = F == NULL ? NULL : F->Parameters.First;//pf_info->Parameters.First;
	//ak nespracuvavame vstavanu funkciu
	if(builtin_id ==0){
	//zistenie poctu vstupnych parametrov
		tListItem * tmp = lp_item;
		//dokym niesme na konci zoznamu
		while(tmp){
			params_total++;
			tmp = tmp->NextItem;
			//iteruj
		}
		/* stack frame uzivatelom definovanej funkcie */
		if(GenerateInstruction(I_CALL_START, NULL, NULL,
			NULL, F->ID, &pf_info->Instruction) == NULL)
			return NULL;
	}
	//v tomto kuse mame zistene aku funkciu ideme volat aj pocet parametrov ktore potrebujeme
	//definovanie premennych ktore ukladaju parametre
	char * first_p = NULL;
	char * second_p = NULL;
	char * third_p = NULL;
	while(params_count < params_total) {
		//zvys pocet parametrov
		params_count++;
		int err;
		//spracuj parameter ktory moze byt tiez funckia
		char * result = parse_expression(&err);
		if(err != ALL_OK){
			*error = err;
			return NULL;
		}
		//ak spracuvavam parameter vstavanej funkcie
		if(builtin_id != 0) {
			switch(builtin_id) {
				/* read */
				case T_K_READ: {
					if(token.type != T_OP_RBRACKET){
						*error = SYN_ERROR;
						return NULL;
					}
					/* parameter read-u by mal byt v result */
					struct token_t *format = ht_lookup
						(pf_info->SymbolTable.First->data, result);
					assert(format != NULL);
					unsigned long read_mode;
					char *fmt = format->val.p;
					if(format->type == STR && fmt[0] == '*') {
						/* '*a', '*l', '*n' */
						if(strlen(fmt) != 2) {
							/* vstup spatnej dlzky */
							*error = SEM_ERROR;
							return NULL;
						}
						read_mode = -(char)fmt[1];
						if(read_mode != (unsigned long)RM_FILE &&
							read_mode != (unsigned long)RM_LINE &&
							read_mode != (unsigned long)RM_NUM) {
							/* spatny modifikator - iny ako a, l, n */
							*error = SEM_ERROR;
							return NULL;
						}
					}
					else if(format->type == INT)
						read_mode = (unsigned long)format->val.i;
					else {
					/* spatny parameter read-u = semanticka chyba */
						*error = SEM_ERROR;
						return NULL;
					}

					/* vlozi ulozisko returnu readu */
					struct token_t *t;
					char *dest_key = gen_and_insert_token(&t,
						pf_info->SymbolTable.First->data);
					if(dest_key == NULL) {
						*error = MEM_ERROR;
						return NULL;
					}

					/* instrukcia read... addr1 = mode, addr4 = dest */
					if(GenerateInstruction(I_READ, (void*)read_mode, NULL,
						NULL, dest_key, &pf_info->Instruction) == NULL) {
						*error = MEM_ERROR;
						return NULL;
					}
/*					*error = throw_params(0);
					if(*error != ALL_OK)
						return NULL;*/
					return dest_key;
				}
				/* type */
				case T_BF_TYPE: {
					if(token.type != T_OP_RBRACKET){
						*error = SYN_ERROR;
						return NULL;
					}
					/* ulozisko navratovej hodnoty */
					struct token_t *t;
					char *dest_key = gen_and_insert_token(&t,
						pf_info->SymbolTable.First->data);
					if(dest_key == NULL) {
						*error = MEM_ERROR;
						return NULL;
					}
					t->type = NIL;
					if(GenerateInstruction(I_TYPE, result, NULL, NULL, dest_key,
						&pf_info->Instruction)==NULL){
						*error = MEM_ERROR;
						return NULL;
					}
/*					*error = throw_params(0);
					if(*error != ALL_OK)
						return NULL;*/
					return dest_key;
				}
				/* find */
				case T_BF_FIND:{
					if(params_count == 1) {
						if(token.type != T_OP_COMMA) {
							*error = SYN_ERROR;
							return NULL;
						}
						first_p = result;
						continue;
					}
					if(token.type != T_OP_RBRACKET){
						*error = SYN_ERROR;
						return NULL;
					}
					/* ulozisko navratovej hodnoty */
					struct token_t *t;
					char *dest_key = gen_and_insert_token(&t,
						pf_info->SymbolTable.First->data);
					if(dest_key == NULL) {
						*error = MEM_ERROR;
						return NULL;
					}
					t->type = INT;
					second_p = result;
					if(GenerateInstruction(I_FIND, first_p,second_p, third_p,
						dest_key, &pf_info->Instruction)==NULL){
						*error = MEM_ERROR;
						return NULL;
					}
/*					*error = throw_params(0);
					if(*error != ALL_OK)
						return NULL;*/
					return dest_key;
				}
				case T_BF_SUBSTR:{
					if(params_count == 1){
						if(token.type != T_OP_COMMA){
							*error = SYN_ERROR;
						if(builtin_id != 0)	return NULL;
						}
						first_p = result;
						continue;
					}
					//2.parameter
					else if(params_count == 2){
						if(token.type != T_OP_COMMA){
							*error = SYN_ERROR;
							return NULL;
						}
						second_p = result;
						continue;
					}
					if(token.type != T_OP_RBRACKET){
						*error = SYN_ERROR;
						return NULL;
					}
					/* ulozisko navratovej hodnoty */
					struct token_t *t;
					char *dest_key = gen_and_insert_token(&t,
						pf_info->SymbolTable.First->data);
					if(dest_key == NULL) {
						*error = MEM_ERROR;
						return NULL;
					}
					t->type = STR;
					third_p = result;
					//generovanie instrukcie
					if(GenerateInstruction(I_SUBSTR, first_p, second_p,third_p,
						dest_key, &pf_info->Instruction)==NULL){
						*error = MEM_ERROR;
						return NULL;
					}
					//kompletizovanie skokov
/*					*error = throw_params(0);
					if(*error != ALL_OK)
						return NULL;*/
					return dest_key;
				}
				case T_BF_SORT: {
					if(token.type != T_OP_RBRACKET){
						*error = SYN_ERROR;
						return NULL;
					}
					/* ulozisko navratovej hodnoty */
					struct token_t *t;
					char *dest_key = gen_and_insert_token(&t,
						pf_info->SymbolTable.First->data);
					if(dest_key == NULL) {
						*error = MEM_ERROR;
						return NULL;
					}
					t->type = STR;
					//staci mi jeden parameter
					first_p = result;
					if(GenerateInstruction(I_SORT,first_p,second_p,third_p,
						NULL, &pf_info->Instruction) == NULL){
						*error = MEM_ERROR;
						return NULL;
					}
/*					*error = throw_params(0);
					if(*error != ALL_OK)
						return NULL;*/
					return first_p;
				} /* case T_BF_SORT */
			} /* switch(builtin_id) */
		} /* if(builtin_id != 0) */
		else {
			/* vlozi parameter */
			assert(lp_item != NULL);
			/* (source var,source func,dest func, dest var) */
			if(GenerateInstruction(I_COPY1, result, pf_info->ID, F_name,
				lp_item->data, &pf_info->Instruction) == NULL) {
				*error = MEM_ERROR;
				return NULL;
			}
			lp_item = lp_item->NextItem;

			//postup: 1. vygeneruj I_CALL_START 2.na prislusne miesta v novej tabulke symbolov generuj I_SET 3.generuj I_CALL 4. generuj I_CALL_STOP
			//poznamka: parse expression do globalnej premennej token dava ciarky prave zatvorky popripade rozne voloviny :D ale je to vlastne prvy token co neparti do vyrazu
			// ziadne tokeny sa tu nenacitavajuvsetko zateba spravi parse_exp ktore uz som zavolal ja takze mozes nato kuknut :)
		}
	}
	/* volanie funkcie */
	if(GenerateInstruction(I_CALL, NULL, NULL,NULL, F_name,
		&pf_info->Instruction) == NULL) {
		*error = MEM_ERROR;
		return NULL;
	}

	/* navratova hodnota */
	char *ret_id = generate_id();
	if(ret_id == NULL) {
		*error = MEM_ERROR;
		return NULL;
	}
	/* vlozi navratovu hodnotu do nasej tabulky premennych */
	struct token_t tmp;
	tmp.type = NIL;
	if(ht_insert((struct htable_t*)pf_info->SymbolTable.Last->data, ret_id,
		(void*)&tmp, sizeof tmp) == NULL) {
		free(ret_id);
		*error = MEM_ERROR;
		return NULL;
	}
	/* ulozi navratovu hodnotu */
	/* (source var,source func,dest func, dest var) */
	if(GenerateInstruction(I_COPY, F_name, F_name, pf_info->ID, ret_id,
		&pf_info->Instruction) == NULL) {
		*error = MEM_ERROR;
		return NULL;
	}
	/* zmaze staru tabulku premennych */
	if(GenerateInstruction(I_CALL_STOP, NULL, NULL, NULL, F_name,
		&pf_info->Instruction) == NULL) {
		*error = MEM_ERROR;
		return NULL;
	}
/*	*error = throw_params(params_total == 0);
	if(*error != ALL_OK)
		return NULL;*/
	return ret_id;
}

/*
 * Funkce pro převod výrazu z infixové notace na
 * inverzní polskou notaci (postfix).
 * Vrací ukazatel na nový seznam tokenů.
 * Pokud dojde k chybě při alokaci paměti, vrací NULL.
 * 
 * Funkce na vstupu očekává ukazatel na integer,
 * kam se uloží kód chyby, pokud k nějaké dojde.
 * xrudol04
 */

char *infix_to_postfix(tList *out, int *err) {
  //seznam musí být prázdný
  assert(out != NULL && out->First == NULL && out->Last == NULL && out->Act == NULL);

  //pomocná proměnná, která dostane hodnotu 1, pokud funkce narazí na konec výrazu.
  int end = 0;
  int retvalue = ALL_OK;
  char *retstring = NULL;
  int prev_token; //typ předchozího tokenu
  //ukazatel použitý pokud váraz je pouze jedna proměnná
  void *res;

  tList stack; //zásobník implementovaný seznamem
  //staticky alokovaná struktura token
  //ukazatel na dynamicky alokovaný vstupní token
  struct token_t *inputtoken = NULL;
  //ukazatel na dočasný dynamický token pro přesun stack->výstup
  struct token_t *movetoken = NULL;
  
  //token pro vložení do SymbolTable, pokud výraz je pouze literál
  struct token_t tmptoken;
  
  //inicializace tokenu pro vložení do SymbolTable
  tmptoken.val.p = NULL;
  
  //inicalizace zásobníku
  ListInit(&stack);
  
  //test prvního tokenu
  //free_token(token);
  token = get_token();
  switch(token.type) {
    //tokeny, které na začátku výrazu nemají co dělat
    case T_ERROR:
      *err = MEM_ERROR;
      return NULL;
    case T_INVALID:
      *err = LEX_ERROR;
      return NULL;
    case T_EOF:
    case T_OP_RBRACKET:
    case T_OP_ADD:
    case T_OP_SUB:
    case T_OP_DIV:
    case T_OP_MULT:
    case T_OP_POW:
    case T_OP_SM:
    case T_OP_BG:
    case T_OP_EQSM:
    case T_OP_EQBG:
    case T_OP_CAT:
    case T_OP_EQ:
    case T_OP_NOTEQ:
      *err = SYN_ERROR;
      return NULL;
      
    //první token je literál
    case T_VAL_INT:
    case T_VAL_DOUBLE:
    case T_VAL_STRING:
    case T_K_FALSE:
    case T_K_TRUE:
    case T_K_NIL:
     
      //záloha prvního tokenu
      if ((inputtoken = itp_copytoken(&token)) == NULL) {
        *err = MEM_ERROR;
        return NULL;
      }
      free_token(token);
      token = get_token();
      //test typu druhého tokenu
      switch(token.type) {
        
        //chyby
        case T_ERROR:
          free_inputtoken(inputtoken);
          *err = MEM_ERROR;
          return NULL;
        case T_INVALID:
          free_inputtoken(inputtoken);
          *err = LEX_ERROR;
          return NULL;
        case T_EOF:
          free_inputtoken(inputtoken);
          *err = SYN_ERROR;
          return NULL;
      
        //operátor
        case T_OP_ADD:
        case T_OP_SUB:
        case T_OP_DIV:
        case T_OP_MULT:
        case T_OP_POW:
        case T_OP_SM:
        case T_OP_BG:
        case T_OP_EQSM:
        case T_OP_EQBG:
        case T_OP_CAT:
        case T_OP_EQ:
        case T_OP_NOTEQ:

          //vloží první operand na výstup
          retvalue = ListInsertLast(out,(void *)inputtoken);
          if (retvalue != ALL_OK) {
            free_inputtoken(inputtoken);
            *err = retvalue;
            return NULL;
          }
          //vloží druhý token (operátor) na stack
          if ((inputtoken = itp_copytoken(&token)) == NULL) {
            *err = MEM_ERROR;
            return NULL;
          }
          retvalue = ListInsertFirst(&stack,(void *)inputtoken);
          break;
        default:
          //vytvořit jednorázovou proměnnou pro literál a vrátit její char
          
          retstring = generate_id();
          if (retstring == NULL) {
            retvalue = MEM_ERROR;
            free_inputtoken(inputtoken);
            break;
          }
          
          //vyplneni tokenu pro vlozeni do SymbolTable
          switch(inputtoken->type) {
            case T_VAL_INT:
              tmptoken.type = INT;
              tmptoken.val.i = inputtoken->val.i;
              break;
            case T_VAL_DOUBLE:
              tmptoken.type = DBL;
              tmptoken.val.d = inputtoken->val.d;
              break;
            case T_VAL_STRING:
              tmptoken.type = STR;
              tmptoken.val.p = inputtoken->val.p;
              inputtoken->val.p = NULL;
              break;
            case T_K_FALSE:
              tmptoken.type = BOL;
              tmptoken.val.i = 0;
              break;
            case T_K_TRUE:
              tmptoken.type = BOL;
              tmptoken.val.i = 1;
              break;
            case T_K_NIL:
              tmptoken.type = NIL;
              tmptoken.val.p = NULL;
              break;
          }

          //vlozeni promenne do SymbolTable
          if (ht_insert(pf_info->SymbolTable.Last->data,retstring,(void *)&tmptoken,sizeof(struct token_t)) == NULL) {
            *err = MEM_ERROR;
            free(retstring);
            free_inputtoken(inputtoken);
            return NULL;
          }
          free_inputtoken(inputtoken);
          return retstring;
          
      } //end switch(token.type) druhý token
      break;
    
    //první token je levá závorka nebo mřížka
    case T_OP_LBRACKET:
    case T_OP_STRLEN:
      if ((inputtoken = itp_copytoken(&token)) == NULL) {
        *err = MEM_ERROR;
        return NULL;
      }
      //vloží operátor nebo levou závorku na stack
      retvalue = ListInsertFirst(&stack,(void *)inputtoken);
      break;
    case T_BF_SORT:
    case T_BF_SUBSTR:
    case T_BF_FIND:
    case T_K_READ:
    case T_BF_TYPE:
    //první token je ID. možnost funkce
    case T_ID:
      //záloha prvního tokenu
      if ((inputtoken = itp_copytoken(&token)) == NULL) {
        *err = MEM_ERROR;
        return NULL;
      }
      free_token(token);
      token = get_token();
      switch(token.type) {
        //chyby
        case T_ERROR:
          free_inputtoken(inputtoken);
          *err = MEM_ERROR;
          return NULL;
        case T_INVALID:
          free_inputtoken(inputtoken);
          *err = LEX_ERROR;
          return NULL;
        case T_EOF:
          free_inputtoken(inputtoken);
          *err = SYN_ERROR;
          return NULL;
        
        //levá závorka, pokus o volání funkce
        case T_OP_LBRACKET:{
            if (inputtoken->type == T_K_READ || inputtoken->type == T_BF_SORT || inputtoken->type == T_BF_FIND || inputtoken->type == T_BF_SUBSTR || inputtoken->type == T_BF_TYPE ||ht_lookup(TFunc,inputtoken->val.p) != NULL) {
            //je to funkce a je deklarovaná alebo voalnie definovaych funkcii
            //přesun T_ID funkce do token
                    token.type = inputtoken->type;
                    token.val.p = inputtoken->val.p;

                    free(inputtoken);
                    //volání zpracování funkce
                    retstring = parse_calling(&retvalue);
                    *err = retvalue;
            }
            else {
            //ID se snaží tvářit jako funkce, ale není deklarovaná
        //    printf("%s není deklarovaná funkce\n",(char *)inputtoken->val.p);
                    free_inputtoken(inputtoken);
                    retvalue = SEM_ERROR;
            }
        }
        break;
        
        case T_OP_ADD:
        case T_OP_SUB:
        case T_OP_DIV:
        case T_OP_MULT:
        case T_OP_POW:
        case T_OP_SM:
        case T_OP_BG:
        case T_OP_EQSM:
        case T_OP_EQBG:
        case T_OP_CAT:
        case T_OP_EQ:
        case T_OP_NOTEQ:
          if (ht_lookup((struct htable_t *)pf_info->SymbolTable.Last->data,inputtoken->val.p) != NULL) {
            //je to lokální proměnná aktuální funkce, začne výraz
//             printf("%s je lokální proměnná funkce\n",(char *)inputtoken->val.p);
            //vloží první operand na výstup
            retvalue = ListInsertLast(out,(void *)inputtoken);
            if (retvalue != ALL_OK) {
              free_inputtoken(inputtoken);
              *err = retvalue;
              return NULL;
            }
            //vloží druhý token (operátor) na stack
            if ((inputtoken = itp_copytoken(&token)) == NULL) {
              *err = MEM_ERROR;
              return NULL;
            }
            retvalue = ListInsertFirst(&stack,(void *)inputtoken);
          }
          else {
            //proměnná není deklarovaná
//             printf("%s není jako proměnná deklarovaná\n",(char *)inputtoken->val.p);
            free_inputtoken(inputtoken);
            *err = SEM_ERROR;
            return NULL;
          }
          break;
        default:
          //ověření deklarace proměnné
          if ((res = ht_lookup((struct htable_t *)pf_info->SymbolTable.Last->data,inputtoken->val.p)) == NULL) {
            *err = SEM_ERROR;
            free_inputtoken(inputtoken);
            return NULL;
          }
          retstring = (char *)(list_entry(res, struct htable_entry_t, data)->key);
          free_inputtoken(inputtoken);
          return retstring;
      } //end switch(token.type) druhý token
      break;

    default:
        //printf("%d není deklarovaná funkce\n",token.type);
      *err = SYN_ERROR;
      return NULL;
  } //end switch(token.type) první token
  prev_token = token.type;

  //cyklus načítání tokenů
  while ((retvalue == ALL_OK) && (retstring == NULL)) {
    
    free_token(token);
    token = get_token();
    switch(token.type) {
      case T_ERROR:
        retvalue = MEM_ERROR;
        break;
      case T_INVALID:
        retvalue = LEX_ERROR;
        break;
      case T_EOF:
        retvalue = SYN_ERROR;
        break;
      
      //operand
      case T_ID:
      case T_VAL_INT:
      case T_VAL_DOUBLE:
      case T_VAL_STRING:
      case T_K_FALSE:
      case T_K_TRUE:
      case T_K_NIL:
        //kontrola kompatibility s předchozím typem tokenu
        switch(prev_token) {
          case T_OP_LBRACKET:
          case T_OP_ADD:
          case T_OP_SUB:
          case T_OP_DIV:
          case T_OP_MULT:
          case T_OP_POW:
          case T_OP_SM:
          case T_OP_BG:
          case T_OP_STRLEN:
          case T_OP_EQSM:
          case T_OP_EQBG:
          case T_OP_CAT:
          case T_OP_EQ:
          case T_OP_NOTEQ:
            break;
          default:
            retvalue = SYN_ERROR;
            break;
        }
        if (retvalue != ALL_OK) {
          break;
        }
        
        //pokud proměnná není deklarovaná, vyvolat sémantickou chybu
        if (token.type == T_ID && ht_lookup((struct htable_t *)pf_info->SymbolTable.Last->data,token.val.p) == NULL) {
//           printf("Promenna %s neni deklarovana\n",(char *)token.val.p);
          retvalue = SEM_ERROR;
          break;
        }
        
        //hodit operand na výstup
        if ((inputtoken = itp_copytoken(&token)) == NULL) {
          retvalue = MEM_ERROR;
          break;
        }
        retvalue = ListInsertLast(out,(void *)inputtoken);
        break;
      
      //levá závorka
      case T_OP_LBRACKET:
        //kontrola kompatibility s předchozím typem tokenu
        switch(prev_token) {
          case T_OP_LBRACKET:
          case T_OP_ADD:
          case T_OP_SUB:
          case T_OP_DIV:
          case T_OP_MULT:
          case T_OP_POW:
          case T_OP_SM:
          case T_OP_BG:
          case T_OP_STRLEN:
          case T_OP_EQSM:
          case T_OP_EQBG:
          case T_OP_CAT:
          case T_OP_EQ:
          case T_OP_NOTEQ:
            break;
          default:
            retvalue = SYN_ERROR;
            break;
        }
        
        if (retvalue != ALL_OK) {
          break;
        }
        //hodit levou závorku na zásobník
        if ((inputtoken = itp_copytoken(&token)) == NULL) {
          retvalue = MEM_ERROR;
          break;
        }
        retvalue = ListInsertFirst(&stack,(void *)inputtoken);
        break;
        
      //operátor
      case T_OP_ADD:
      case T_OP_SUB:
      case T_OP_DIV:
      case T_OP_MULT:
      case T_OP_POW:
      case T_OP_SM:
      case T_OP_BG:
      case T_OP_STRLEN:
      case T_OP_EQSM:
      case T_OP_EQBG:
      case T_OP_CAT:
      case T_OP_EQ:
      case T_OP_NOTEQ:
        //kontrola kompatibility strlen s předchozím typem tokenu
        if (token.type == T_OP_STRLEN) {
          switch(prev_token) {
            case T_OP_LBRACKET:
            case T_OP_ADD:
            case T_OP_SUB:
            case T_OP_DIV:
            case T_OP_MULT:
            case T_OP_POW:
            case T_OP_SM:
            case T_OP_BG:
            case T_OP_EQSM:
            case T_OP_EQBG:
            case T_OP_CAT:
            case T_OP_EQ:
            case T_OP_NOTEQ:
              break;
            default:
              retvalue = SYN_ERROR;
              break;
          }
        }
        //kontrola kompatibility operátoru s předchozím typem tokenu
        else {
          switch(prev_token) {
            case T_ID:
            case T_VAL_INT:
            case T_VAL_DOUBLE:
            case T_VAL_STRING:
            case T_K_FALSE:
            case T_K_TRUE:
            case T_K_NIL:
            case T_OP_RBRACKET:
              break;
            default:
              retvalue = SYN_ERROR;
              break;
          }
        }
        if (retvalue != ALL_OK) {
          break;
        }
        
        //udělá kopii aktuálního tokenu do inputtoken
        if ((inputtoken = itp_copytoken(&token)) == NULL) {
          retvalue = MEM_ERROR;
          break;
        }
        do {
          //zásobník je prázdný
          if (stack.First == NULL) {
            retvalue = ListInsertFirst(&stack,(void *)inputtoken);
            break;
          }
          
          //na zásobníku je levá závorka
          if (((struct token_t *)stack.First->data)->type == T_OP_LBRACKET) {
            retvalue = ListInsertFirst(&stack,(void *)inputtoken);
            break;
          }
          //na zásobníku je operátor
          else if ((((struct token_t *)stack.First->data)->type >= T_OP_FIRST) || 
          (((struct token_t *)stack.First->data)->type <= T_OP_LAST)) {
            //priority operátorů se rovnají a operátor má asociativitu doprava
            if ((op_table[((struct token_t *)stack.First->data)->type].prio == op_table[inputtoken->type].prio) ||
              (op_table[inputtoken->type].assoc == A_RIGHT)) {
              retvalue = ListInsertFirst(&stack,(void *)inputtoken);
              break;
            }
            //operátor na zásobníku má nižší prioritu
            else if (op_table[((struct token_t *)stack.First->data)->type].prio > op_table[inputtoken->type].prio) {
              retvalue = ListInsertFirst(&stack,(void *)inputtoken);
              break;
            }
            //operátor na zásobníku má vyšší prioritu nebo mají oba prioritu stejnou a asociativitu doleva
            else {
              //přesune vrchol zásobníku na konec výstupního výrazu
              if ((movetoken = itp_copytoken((struct token_t *)stack.First->data)) == NULL) {
                retvalue = MEM_ERROR;
                break;
              }
              ListDeleteTokenFirst(&stack);
              retvalue = ListInsertLast(out,(void *)movetoken);
            }
          }
        } while(retvalue == ALL_OK);
        //(op_table[stack->First->data->type].prio <= op_table[inputtoken->type].prio) 
        break;
      case T_OP_RBRACKET:
        //kontrola kompatibility pravé závorky s předchozím typem tokenu
        switch(prev_token) {
          case T_OP_RBRACKET:
          case T_ID:
          case T_VAL_INT:
          case T_VAL_DOUBLE:
          case T_VAL_STRING:
          case T_K_FALSE:
          case T_K_TRUE:
          case T_K_NIL:
            break;
          default:
            retvalue = SYN_ERROR;
        }
        if (retvalue != ALL_OK) {
          break;
        }
        
        
        do {
          //zásobník je prázdný, došlo ke konci výrazu
          if (stack.First == NULL) {
            end = 1;
            break;
          }

          //na vrcholu zásobníku je levá závorka, bude odstraněna a cyklus končí
          if (((struct token_t *)stack.First->data)->type == T_OP_LBRACKET){
            ListDeleteTokenFirst(&stack);
            break;
          }
          //token z vrcholu zásobníku bude přidán na konec výstupního výrazu
          else {
            if ((movetoken = itp_copytoken((struct token_t *)stack.First->data)) == NULL) {
              retvalue = MEM_ERROR;
              break;
            }
            ListDeleteTokenFirst(&stack);
            retvalue = ListInsertLast(out,(void *)movetoken);
          }
        } while (retvalue == ALL_OK);
        break;
      
      //pokud byl nalezen jiný token, výraz končí
      default:
        //kontrola posledního prvku výrazu
        switch(prev_token) {
          case T_OP_RBRACKET:
          case T_ID:
          case T_VAL_INT:
          case T_VAL_DOUBLE:
          case T_VAL_STRING:
          case T_K_FALSE:
          case T_K_TRUE:
          case T_K_NIL:
            break;
          default:
            retvalue = SYN_ERROR;
        }
        if (retvalue != ALL_OK) {
          break;
        }
        
        //cyklus vyprázdnění zásobníku
        do {
          //pokud je zásobník prázdný, výraz končí
          if (stack.First == NULL) {
            end = 1;
            break;
          }

          //pokud v zásobníku zbyla levá závorka, došlo k syntaktické chybě
          if (((struct token_t *)stack.First->data)->type == T_OP_LBRACKET) {
            retvalue = SYN_ERROR;
          }
          //ostatní tokeny ze zásobníku budou přesunuty do výstupního výrazu
          else {
            if ((movetoken = itp_copytoken((struct token_t *)stack.First->data)) == NULL) {
              retvalue = MEM_ERROR;
              break;
            }
            ListDeleteTokenFirst(&stack);
            retvalue = ListInsertLast(out,(void *)movetoken);
          }
        } while (retvalue == ALL_OK);
    }
    //pokud došlo k chybě, uvolnit alokovanou paměť
    if (retvalue != ALL_OK) {
          free_token(token);
          ListTokenDispose(&stack);
          *err = retvalue;
          return NULL;
    }
    //pokud výraz skončil, hlavní cyklus skončí
    if (end != 0) {
      break;
    }
    prev_token = token.type;
  }
  ListTokenDispose(&stack);
  return retstring;
}

/*
 * Funkce, která z tokenu na vstupu vezme jeho
 * string ve val.p a zkopíruje ho do nového dynamicky alokovaného prostoru.
 * Vrací ukazatel na začátek stringu.
 * xrudol04
 */
char *tokenname(struct token_t *inputtoken) {
  char *retvalue = NULL;
  if (inputtoken->type == T_VAL_STRING || inputtoken->type == T_ID) {
    retvalue = malloc((strlen(inputtoken->val.p)+1)*sizeof(char));
    if (retvalue != NULL) {
      strcpy(retvalue,inputtoken->val.p);
    }
  }
  return retvalue;
}

/*
 * TODO testovací funkce, vypisující název typu tokenu.
 */
// void print_operator_name(int input) {
//   switch (input) {
//     case T_OP_ADD:
//       printf("+");
//       break;
//     case T_OP_SUB:
//       printf("-");
//       break;
//     case T_OP_DIV:
//       printf("/");
//       break;
//     case T_OP_MULT:
//       printf("*");
//       break;
//     case T_OP_POW:
//       printf("^");
//       break;
//     case T_OP_SM:
//       printf("<");
//       break;
//     case T_OP_BG:
//       printf(">");
//       break;
//     case T_OP_STRLEN:
//       printf("#");
//       break;
//     case T_OP_EQSM:
//       printf("<=");
//       break;
//     case T_OP_EQBG:
//       printf(">=");
//       break;
//     case T_OP_CAT:
//       printf("..");
//       break;
//     case T_OP_EQ:
//       printf("==");
//       break;
//     case T_OP_NOTEQ:
//       printf("~=");
//       break;
//     default:
//       printf("blbost");
//       break;
//   }
// }

/*
 * TODO pomocná testovací funkce, která vypisuje typ operandu
 */
// void print_operand_type(int type) {
//   switch(type) {
//     case NIL:
//       printf("NIL");
//       break;
//     case INT:
//       printf("INT");
//       break;
//     case DBL:
//       printf("DBL");
//       break;
//     case BOL:
//       printf("BOL");
//       break;
//     case STR:
//       printf("STR");
//       break;
//     default:
//       printf("blbost");
//       break;
//   }
// }

/*
 * Funkce, která zpracuje řetězec výrazu v posfixové notaci
 * a vytvoří instrukce.
 * Na vstupu očekává seznam tokenů, seřazených podle postfixové notace (výstup infix_to_postfix)
 * na výstupu vrací ukazatel na string, který slouží jako klíč k proměnné,
 * do které se uloží výsledek poslední instrukce výrazu.
 * xrudol04
 */
char *postfix_parse(tList *input,int *err) {
  int retvalue = ALL_OK;
  char *retstring = NULL;
  char *actstring = NULL;
  char *instruction_args[MAX_OP_ARGS];
  int instruction_types[MAX_OP_ARGS];
  int instruction;
  void *res;
  struct token_t tmptoken;
  struct token_t *stacktoken = NULL;
  tList stack;
  *err = ALL_OK; //TODO smazat
  ListInit(&stack);
  ListFirst(input);

  
  //hlavni cyklus
  while ((retvalue == ALL_OK) && (input->Act != NULL)) {
    actstring = NULL;
    stacktoken = NULL;
    tmptoken.val.p = NULL;
    //testuje se typ tokenu na aktualni pozici vstupniho seznamu
    switch(((struct token_t *)input->Act->data)->type) {
      //identifikator
      case T_ID:
        //získat název proměnné z vstupního tokenu
        
        //ověření deklarace proměnné
        if ((res = ht_lookup((struct htable_t *)pf_info->SymbolTable.Last->data,((struct token_t *)input->Act->data)->val.p)) == NULL) {
          retvalue = SEM_ERROR;
          break;
        }
        actstring = (char *)(list_entry(res, struct htable_entry_t, data)->key);
        
        //vytvořit zásobníkový token typu NIL
        if ((stacktoken = malloc(sizeof(struct token_t))) == NULL) {
          retvalue = MEM_ERROR;
          free(actstring);
          break;
        }
        stacktoken->type = NIL;
        stacktoken->val.p = actstring;
        
        //vložit zásobníkový token na zásobník
        retvalue = ListInsertFirst(&stack,(void *)stacktoken);
        if (retvalue != ALL_OK) {
          free(actstring);
          free(stacktoken);
        }
        else {
          stacktoken = NULL;
        }
        break;
        
      //literal - vytvori se proměnná
      case T_VAL_INT:
      case T_VAL_DOUBLE:
      case T_VAL_STRING:
      case T_K_FALSE:
      case T_K_TRUE:
      case T_K_NIL:
        //generovani nazvu jednorazove promenne
        actstring = generate_id();
        if (actstring == NULL) {
          retvalue = MEM_ERROR;
          break;
        }
        
        //vyplneni tokenu pro vlozeni do SymbolTable
        switch(((struct token_t *)input->Act->data)->type) {
          case T_VAL_INT:
            tmptoken.type = INT;
            tmptoken.val.i = ((struct token_t *)input->Act->data)->val.i;
            break;
          case T_VAL_DOUBLE:
            tmptoken.type = DBL;
            tmptoken.val.d = ((struct token_t *)input->Act->data)->val.d;
            break;
          case T_VAL_STRING:
            tmptoken.type = STR;
            tmptoken.val.p = tokenname((struct token_t *)input->Act->data);
            if (tmptoken.val.p == NULL) {
              retvalue = MEM_ERROR;
            }
            break;
          case T_K_FALSE:
            tmptoken.type = BOL;
            tmptoken.val.i = 0;
            break;
          case T_K_TRUE:
            tmptoken.type = BOL;
            tmptoken.val.i = 1;
            break;
          case T_K_NIL:
            tmptoken.type = NIL;
            tmptoken.val.p = NULL;
            break;
        }
        if (retvalue != ALL_OK) {
          free(actstring);
          break;
        }

        //vlozeni promenne do SymbolTable
        if (ht_insert(pf_info->SymbolTable.Last->data,actstring,(void *)&tmptoken,sizeof(struct token_t)) == NULL) {
          retvalue = MEM_ERROR;
          free(actstring);
          break;
        }
        
        //vytvoření zásobníkového tokenu
        if ((stacktoken = malloc(sizeof(struct token_t))) == NULL) {
          retvalue = MEM_ERROR;
          break;
        }
        stacktoken->type = tmptoken.type;
        stacktoken->val.p = actstring;
        
        //vlozeni zásobníkového tokenu na stack
        retvalue = ListInsertFirst(&stack,(void *)stacktoken);
        if (retvalue != ALL_OK) {
          free(stacktoken);
        }
        else {
          stacktoken = NULL;
        }
        break;
      
      //operátor
      case T_OP_ADD:
      case T_OP_SUB:
      case T_OP_DIV:
      case T_OP_MULT:
      case T_OP_POW:
      case T_OP_SM:
      case T_OP_BG:
      case T_OP_STRLEN:
      case T_OP_EQSM:
      case T_OP_EQBG:
      case T_OP_CAT:
      case T_OP_EQ:
      case T_OP_NOTEQ:
        //vygeneruje se ID dočasné proměnné
        actstring = generate_id();
        if (actstring == NULL) {
          retvalue = MEM_ERROR;
          break;
        }
        retstring = actstring;
        
        //vyplneni tokenu pro vlozeni do SymbolTable
        tmptoken.type = NIL;
        tmptoken.val.p = NULL;
        
        //vlozeni promenne do SymbolTable
        if (ht_insert(pf_info->SymbolTable.Last->data,actstring,(void *)&tmptoken,sizeof(struct token_t)) == NULL) {
          retvalue = MEM_ERROR;
          free(actstring);
          break;
        }
        
        //vyjmutí operandů ze zásobníku a vložení do pole instruction_args
        int i;
        for(i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
          assert(stack.First != NULL);
          assert(i<MAX_OP_ARGS);
          
          instruction_args[i] = (char *)((struct token_t *)stack.First->data)->val.p;
          instruction_types[i] = ((struct token_t *)stack.First->data)->type;
          ((struct token_t *)stack.First->data)->val.p = NULL;
          ListDeleteStackTokenFirst(&stack);
        }
        
        //TODO smazat testovací printf
//         printf("%s = ",actstring);
//         print_operator_name(((struct token_t *)input->Act->data)->type);
//         printf("(");
//         for(int i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
//           printf("%s[",instruction_args[i]);
//           print_operand_type(instruction_types[i]);
//           printf("]");
//           if (i != (op_table[((struct token_t *)input->Act->data)->type].args - 1)) {
//             printf(", ");
//           }
//         }
//         printf(")\n");
        // konec testovacího printf
        
        //vygenerovat instrukci, operandy jsou v instruction_args[]
        switch(((struct token_t *)input->Act->data)->type) {
          case T_OP_ADD:
          case T_OP_SUB:
          case T_OP_DIV:
          case T_OP_MULT:
          case T_OP_POW:
            //kontrola kompatibility prvního operandu
            switch(instruction_types[1]) {
              case NIL:
              case INT:
              case DBL:
                break;
              default:
                retvalue = SEM_ERROR;
                break;
            }
            //kontrola kompatibility druhého operandu
            switch(instruction_types[0]) {
              case NIL:
              case INT:
              case DBL:
                break;
              default:
                retvalue = SEM_ERROR;
                break;
            }
            if (retvalue != ALL_OK) {
//               for(int i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
//                 assert(i<MAX_OP_ARGS);
//                 free(instruction_args[i]);
//               }
              break;
            }
            
            //vybrání typu instrukce pro vygenerování
            switch(((struct token_t *)input->Act->data)->type) {
              case T_OP_ADD:
                instruction = I_PLUS;
                break;
              case T_OP_SUB:
                instruction = I_MINUS;
                break;
              case T_OP_DIV:
                instruction = I_DIVIDE;
                break;
              case T_OP_MULT:
                instruction = I_MULTIPLY;
                break;
              case T_OP_POW:
                instruction = I_EXPONENT;
                break;
            }
            //generování instrukce
            if (GenerateInstruction(instruction,(void *)instruction_args[1],(void *)instruction_args[0],NULL,actstring,&(pf_info->Instruction)) == NULL) {
              retvalue = MEM_ERROR;
//               for(int i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
//                 assert(i<MAX_OP_ARGS);
//                 free(instruction_args[i]);
//               }
            }
            break;
            
          case T_OP_CAT:
            //kontrola kompatibility prvního operandu
            switch(instruction_types[1]) {
              case NIL:
              case STR:
                break;
              default:
                retvalue = SEM_ERROR;
                break;
            }
            //kontrola kompatibility druhého operandu
            switch(instruction_types[0]) {
              case NIL:
              case STR:
                break;
              default:
                retvalue = SEM_ERROR;
                break;
            }
            if (retvalue != ALL_OK) {
//               for(int i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
//                 assert(i<MAX_OP_ARGS);
//                 free(instruction_args[i]);
//               }
              break;
            }
            
            instruction = I_CONCAT;
            //generování instrukce
            if (GenerateInstruction(instruction,(void *)instruction_args[1],(void *)instruction_args[0],NULL,actstring,&(pf_info->Instruction)) == NULL) {
              retvalue = MEM_ERROR;
//               for(int i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
//                 assert(i<MAX_OP_ARGS);
//                 free(instruction_args[i]);
//               }
            }
            break;

          case T_OP_EQ:
          case T_OP_NOTEQ:
            //operandy mohou mít jakýkoliv typ
            
            //vybrání typu instrukce pro vygenerování
            switch(((struct token_t *)input->Act->data)->type) {
              case T_OP_EQ:
                instruction = I_EQUALS;
                break;
              case T_OP_NOTEQ:
                instruction = I_NOTEQUALS;
                break;
            }
            //generování instrukce
            if (GenerateInstruction(instruction,(void *)instruction_args[1],(void *)instruction_args[0],NULL,actstring,&(pf_info->Instruction)) == NULL) {
              retvalue = MEM_ERROR;
//               for(int i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
//                 assert(i<MAX_OP_ARGS);
//                 free(instruction_args[i]);
//               }
            }
            break;
            
          case T_OP_SM:
          case T_OP_BG:
          case T_OP_EQSM:
          case T_OP_EQBG:
            //kontrola typů operandů
            switch(instruction_types[1]) {
              case NIL:
                break;
              case STR:
                switch(instruction_types[0]) {
                  case NIL:
                  case STR:
                    break;
                  default:
                    retvalue = SEM_ERROR;
                    break;
                }
                break;
              case INT:
              case DBL:
                switch(instruction_types[0]) {
                  case NIL:
                  case INT:
                  case DBL:
                    break;
                  default:
                    retvalue = SEM_ERROR;
                    break;
                }
                break;
              default:
                retvalue = SEM_ERROR;
                break;
            }
            if (retvalue != ALL_OK) {
//               for(int i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
//                 assert(i<MAX_OP_ARGS);
//                 free(instruction_args[i]);
//               }
              break;
            }
            
            //vybrání typu instrukce pro vygenerování
            switch(((struct token_t *)input->Act->data)->type) {
              case T_OP_SM:
                instruction = I_SMALLER;
                break;
              case T_OP_BG:
                instruction = I_GREATER;
                break;
              case T_OP_EQSM:
                instruction = I_SMALLEREQU;
                break;
              case T_OP_EQBG:
                instruction = I_GREATEREQU;
                break;
            }
            //generování instrukce
            if (GenerateInstruction(instruction,(void *)instruction_args[1],(void *)instruction_args[0],NULL,actstring,&(pf_info->Instruction)) == NULL) {
              retvalue = MEM_ERROR;
//               for(int i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
//                 assert(i<MAX_OP_ARGS);
//                 free(instruction_args[i]);
//               }
            }
           break;
            
          case T_OP_STRLEN:
            switch(instruction_types[0]) {
              case NIL:
              case STR:
                break;
              default:
                retvalue = SEM_ERROR;
                break;
            }
            if (retvalue != ALL_OK) {
//               for(int i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
//                 assert(i<MAX_OP_ARGS);
//                 free(instruction_args[i]);
//               }
              break;
            }
            instruction = I_STRLEN;
            //generování instrukce
            if (GenerateInstruction(instruction,(void *)instruction_args[0],NULL,NULL,actstring,&(pf_info->Instruction)) == NULL) {
              retvalue = MEM_ERROR;
//               for(int i=0;i<op_table[((struct token_t *)input->Act->data)->type].args;i++) {
//                 assert(i<MAX_OP_ARGS);
//                 free(instruction_args[i]);
//               }
            }
            break;
            
        }
        if (retvalue != ALL_OK) {
          break;
        }
        
        //vytvořit zásobníkový token typu NIL
        if ((stacktoken = malloc(sizeof(struct token_t))) == NULL) {
          retvalue = MEM_ERROR;
          break;
        }
        stacktoken->type = NIL;
        stacktoken->val.p = actstring;
        
        retvalue = ListInsertFirst(&stack,(void *)stacktoken);
        if (retvalue != ALL_OK) {
          free(stacktoken);
        }
        else {
          stacktoken = NULL;
        }
        break;
    }
    ListSucc(input);
  }
  
  //pokud nedošlo k chybě a zásobník není prázdný, výraz byl operand v závorkách
  //funkce vrátí string klíč k tomuto operandu
  if ((retvalue == ALL_OK) && (stack.First != NULL)) {
    retstring = ((struct token_t *)stack.First->data)->val.p;
  }

  //úklid
  if (stacktoken != NULL) {
    free(stacktoken);
  }
//   for (ListFirst(&strids);strids.Act != NULL;ListSucc(&strids)) {
//     if (strids.Act->data != NULL) {
//       free(strids.Act->data);
//     }
//   }
//   ListDispose(&strids);
  ListStackTokenDispose(&stack);

  //pokud došlo k chybě...
  if (retvalue != ALL_OK) {
    *err = retvalue;
    return NULL;
  }
  
  return retstring;
}



/*
 * 
 * 
 * 
 * KONEC SEKCE ZPRACOVÁNÍ VÝRAZŮ
 * 
 * 
 * 
 * 
 */



int statement(void)
{
	int wasnot_else = 0;
	int last_token_type = T_OP_SEMIC;
        void *res = NULL;
	//zasobnik na kompletizovanie skokov
	tList statement_stack;
	tList return_stack;
	ListInit (&statement_stack);
	ListInit (&return_stack);
	char * statement_result;
	int statement_context_type = T_OP_SEMIC;
	//int ret = SYN_ERROR;
	while(1){
		switch(token.type){
			case T_K_DO:{
				if(statement_context_type != T_K_WHILE){TokenListDispose(&statement_stack);ListDispose(&return_stack);return SYN_ERROR;}
				else {statement_context_type = T_OP_SEMIC;token.type=T_OP_SEMIC;}
			}break;
			//bodkociarka nic sa nedeje
			case T_OP_SEMIC:
				{
					if(last_token_type == T_K_END)return SYN_ERROR;
					if(statement_context_type == T_K_WRITE && last_token_type != T_OP_RBRACKET){TokenListDispose(&statement_stack);ListDispose(&return_stack);return SYN_ERROR;}
					else {
						if(statement_context_type != T_K_END)
							statement_context_type = T_OP_SEMIC;
					}
				}break;
			// lava zatvroka -> spracuvavam Write
			case T_OP_LBRACKET:{
				if(last_token_type != T_K_WRITE)return SYN_ERROR;
				int error;
				char * result = parse_expression(&error);
				if(error!= ALL_OK){TokenListDispose(&statement_stack);ListDispose(&return_stack);return error;}
				//generate write operaand
				if(GenerateInstruction(I_WRITE, NULL, NULL,	NULL,result, &pf_info->Instruction) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
				//ak mame zaznamenat instrukciu pre kompletizovanie skokov
				continue;
			}break;
			//carka -> spracuvavam Write a nasleduje vyraz
			case T_OP_COMMA:{
				if(statement_context_type != T_K_WRITE)return SYN_ERROR;
				int error;
				char * resu = parse_expression(&error);
				if(error!= ALL_OK){TokenListDispose(&statement_stack);ListDispose(&return_stack);return error;}
				//generate write operaand
				if(GenerateInstruction(I_WRITE, NULL, NULL,	NULL,resu, &pf_info->Instruction) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
				//ak mame zaznamenat instrukciu pre kompletizovanie skokov
				continue;
			}break;
			//prava zatvorka -> spracuvavam write predchadza jej
			case T_OP_RBRACKET:{
				if(!(last_token_type == T_K_WRITE || last_token_type == T_K_IF ||last_token_type == T_K_WHILE || last_token_type == T_K_UNTIL || last_token_type ==T_OP_ASSIGN|| last_token_type == T_ID)){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return SYN_ERROR;
				}
			}break;
			case T_OP_ASSIGN:
			{
				//T_ID = -> neskaceme, sprecujeme vyraz,vyhladame vysledok,vygenerujeme instrujciu
				int error;
				if(last_token_type != T_ID){TokenListDispose(&statement_stack);ListDispose(&return_stack); return SYN_ERROR;}
				char * result = parse_expression(&error);
				if(error!= ALL_OK){TokenListDispose(&statement_stack);ListDispose(&return_stack);return error;}
				if(GenerateInstruction(I_SET,result , NULL, NULL,statement_result, &pf_info->Instruction) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
			}break;
			case T_K_WRITE:
			{
				if(!(last_token_type == T_K_END || last_token_type == T_OP_SEMIC || last_token_type == T_K_DO || last_token_type == T_K_ELSE || last_token_type == T_K_REPEAT || last_token_type == T_K_THEN )){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return SYN_ERROR;
				}
				statement_context_type = T_K_WRITE;
			}break;
			case T_K_IF:{
				if(!(last_token_type == T_K_END ||last_token_type == T_OP_SEMIC || last_token_type == T_K_DO || last_token_type == T_K_ELSE || last_token_type == T_K_REPEAT|| last_token_type == T_K_THEN )){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return SYN_ERROR;
				}
				wasnot_else = 1;
				int error;
				char * result = parse_expression(&error);
				if(error!= ALL_OK){TokenListDispose(&statement_stack);ListDispose(&return_stack);return error;}
				//result = ht_lookup(pf_info->SymbolTable.First->data,result);
				tInstr * first;
				//este nekomlpetny skok
				if((first = GenerateInstruction(I_CONDGOTO, result, NULL,	NULL,NULL, &pf_info->Instruction)) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
				//vkladanie na statement_stack
				struct token_t * new_ptr = malloc(sizeof(struct token_t));
				if(!new_ptr){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
				new_ptr->type = T_K_IF;
				new_ptr->val.p = first;
				if(ListInsertFirst (&statement_stack,new_ptr)== MEM_ERROR){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
			}break;
			case T_K_THEN:{
				if(!(last_token_type == T_K_IF)){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return SYN_ERROR;
				}
				token.type = T_OP_SEMIC;
			}break;
			case T_K_ELSE:{
				wasnot_else =0;
				if(!(last_token_type == T_K_END ||last_token_type == T_OP_SEMIC || last_token_type == T_K_THEN )){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return SYN_ERROR;
				}
				tInstr * first;
				//este nekomlpetny skok
				if((first = GenerateInstruction(I_GOTO, NULL, NULL,	NULL,NULL, &pf_info->Instruction)) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
				tInstr * I;
				//este nekomlpetny skok
				if((I = GenerateInstruction(I_NOP, NULL, NULL,	NULL,NULL, &pf_info->Instruction)) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
				if(statement_stack.First!=NULL){tInstr * __tmp = ((struct token_t *)statement_stack.First->data)->val.p;
				__tmp->addr4 = I;
				free(statement_stack.First->data);
				ListDeleteFirst(&statement_stack);
				struct token_t * ahojvole = malloc(sizeof(struct token_t));
				if(!ahojvole){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
				ahojvole->type = T_K_ELSE;
				ahojvole->val.p = first;
				if(ListInsertFirst (&statement_stack,ahojvole)== MEM_ERROR){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
				}
				else
					return SYN_ERROR;
			}break;

			case T_K_WHILE:{
				if(!(last_token_type == T_K_END ||last_token_type == T_OP_SEMIC || last_token_type == T_K_THEN || last_token_type == T_K_ELSE || last_token_type == T_K_REPEAT || last_token_type == T_K_DO)){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return SYN_ERROR;
				}
				tInstr * hello;
				//generovanie nop
				if((hello = GenerateInstruction(I_NOP, NULL, NULL,	NULL,NULL, &pf_info->Instruction)) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
				//ak sme mali kompletizovat kompletizujme
				//inak budem vkladat do statement_stack
				struct token_t * new_mem = malloc(sizeof(struct token_t));
				if(!new_mem){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
				new_mem->type = T_K_LOCAL;
				new_mem->val.p = hello;
				if(ListInsertFirst (&statement_stack,new_mem)== MEM_ERROR){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}


				//parsovanie vyrazu
				int error;
				char * result = parse_expression(&error);
				if(error!= ALL_OK){TokenListDispose(&statement_stack);ListDispose(&return_stack);return error;}

				//generovanie podmieneneho skoku
				tInstr * first;
				//este nekomlpetny skok
				if((first = GenerateInstruction(I_CONDGOTO,  result, NULL,	NULL,NULL, &pf_info->Instruction)) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
				//vlozenie na statement_stack
				new_mem = malloc(sizeof(struct token_t));
				if(!new_mem){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
				new_mem->type = T_K_WHILE;
				new_mem->val.p = first;
				if(ListInsertFirst (&statement_stack,new_mem)== MEM_ERROR){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
				statement_context_type = T_K_WHILE;
				continue;
			}break;
			case T_K_END:{
				statement_context_type = T_K_END;
				if(!(last_token_type==T_OP_SEMIC || last_token_type == T_K_ELSE || last_token_type == T_K_END)){TokenListDispose(&statement_stack);ListDispose(&return_stack);return SYN_ERROR;}
				if(wasnot_else){TokenListDispose(&statement_stack);ListDispose(&return_stack);return SYN_ERROR;}
				//ak je zoznam prazdny konicm
				if(statement_stack.First== NULL){
					tInstr * last;
					if((last = GenerateInstruction(I_NOP, NULL, NULL,	NULL,NULL, &pf_info->Instruction)) == NULL){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
					//kompletizuj vsetky returny z return_stack
					tListItem * nojo = return_stack.First;
					tInstr * I;
					while(nojo!=NULL){
						I = return_stack.First->data;
						I->addr4 = last;
						nojo=nojo->NextItem;
						ListDeleteFirst(&return_stack);
					}

					if(strcmp(pf_info->ID,"main")==0){token = get_token();if(token.type == T_ERROR || token.type == T_INVALID)return (T_ERROR)?MEM_ERROR:LEX_ERROR;if(token.type!=T_OP_SEMIC)return SYN_ERROR;}
					return ALL_OK;
				}
				else{
					tInstr * I;
					//inak budeme musiet na nasledujucu instrukciu odniekadial skakat
					if(((struct token_t *)statement_stack.First->data)->type == T_K_WHILE){
						//este nekomlpetny skok
						if((I = GenerateInstruction(I_GOTO, NULL, NULL,	NULL,((struct token_t *)statement_stack.First->NextItem->data)->val.p, &pf_info->Instruction)) == NULL){
							TokenListDispose(&statement_stack);ListDispose(&return_stack);
							return MEM_ERROR;
						}
						//napojili sme ho na nop generovany whilom

						//mazem druhy v zozname
						tListItem * temp = statement_stack.First->NextItem;
						statement_stack.First->NextItem = temp->NextItem;
						free((struct token_t *)temp->data);
					}

					//generovanie NOP
					if((I = GenerateInstruction(I_NOP, NULL, NULL,	NULL,NULL, &pf_info->Instruction)) == NULL){
						TokenListDispose(&statement_stack);ListDispose(&return_stack);
						return MEM_ERROR;
					}
					//kompetizovanie skoku posledeho v statement_stack
					struct token_t * ehm = statement_stack.First->data;
					tInstr * I1 = ehm->val.p;
					I1->addr4 = I;
					free((struct token_t *)statement_stack.First->data);
					ListDeleteFirst(&statement_stack);
					if(statement_stack.First != NULL &&
						((struct token_t *)statement_stack.First->data)->type
						== T_K_IF)wasnot_else = 1;
					token = get_token();
					if(token.type == T_EOF) {
						TokenListDispose(&statement_stack);ListDispose(&return_stack);
						//ak bola predtym bodkociarka mozme skoncit
						return SYN_ERROR;
					}
					else if(token.type == T_ERROR || token.type == T_INVALID) {
						TokenListDispose(&statement_stack);ListDispose(&return_stack);
						if(token.type == T_ERROR)return MEM_ERROR;
						else return LEX_ERROR;
					}
					else if(token.type== T_OP_SEMIC);
					else {
						TokenListDispose(&statement_stack);ListDispose(&return_stack);
						return SYN_ERROR;
					}
				}
			}break;
			case T_K_REPEAT:
			{	
				if(!(last_token_type == T_K_END ||last_token_type == T_OP_SEMIC))return SYN_ERROR;
				tInstr * I;
				if((I=GenerateInstruction(I_NOP, NULL, NULL,	NULL,NULL, &pf_info->Instruction)) == NULL){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
				struct token_t * new_mem = malloc(sizeof(struct token_t));
				if(!new_mem){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
				new_mem->type = T_K_REPEAT;
				new_mem->val.p = I;
				if(ListInsertFirst (&statement_stack,new_mem)== MEM_ERROR){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
			}break;
			case T_K_UNTIL:
			{
				if(!(last_token_type == T_K_END ||last_token_type == T_OP_SEMIC)){TokenListDispose(&statement_stack);ListDispose(&return_stack);return SYN_ERROR;}
				//parsovanie vyrazu
				int error;
				char * result = parse_expression(&error);
				if(error!= ALL_OK){TokenListDispose(&statement_stack);ListDispose(&return_stack);return error;}
				if(GenerateInstruction(I_CONDGOTO_INV,  result, NULL,	NULL,((struct token_t *)statement_stack.First->data)->val.p, &pf_info->Instruction)== NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
				free((struct token_t *)statement_stack.First->data);
				ListDeleteFirst(&statement_stack);
			}break;

			case T_ID:
			{
				if(statement_context_type == T_K_WRITE){TokenListDispose(&statement_stack);ListDispose(&return_stack);return SEM_ERROR;}
				if(!(last_token_type == T_K_END ||last_token_type == T_OP_SEMIC|| last_token_type  == T_K_THEN || last_token_type  == T_K_ELSE || last_token_type  == T_K_DO || last_token_type == T_K_REPEAT)){TokenListDispose(&statement_stack);ListDispose(&return_stack);return SYN_ERROR;}
				//set context

				if((res = ht_lookup(pf_info->SymbolTable.First->data,token.val.p)) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					free(token.val.p);
					return SYN_ERROR;
				}

                statement_result = (char *)(list_entry(res, struct htable_entry_t, data)->key);
                free(token.val.p);
                token.val.p = NULL;
				statement_context_type = T_ID;
			}break;
			case T_K_RETURN:{
				if(!(last_token_type == T_K_END || last_token_type == T_K_REPEAT ||last_token_type == T_OP_SEMIC|| last_token_type  == T_K_THEN)){TokenListDispose(&statement_stack);ListDispose(&return_stack);return SYN_ERROR;}
				int error;
				char * result = parse_expression(&error);
				if(error!= ALL_OK){TokenListDispose(&statement_stack);ListDispose(&return_stack);return error;}
				tInstr * first;
				if((first = GenerateInstruction(I_SET, result, NULL,	NULL,pf_info->ID, &pf_info->Instruction)) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
				if((first = GenerateInstruction(I_GOTO, NULL, NULL,	NULL, NULL, &pf_info->Instruction)) == NULL){
					TokenListDispose(&statement_stack);ListDispose(&return_stack);
					return MEM_ERROR;
				}
				if(ListInsertFirst(&return_stack,first)==MEM_ERROR){TokenListDispose(&statement_stack);ListDispose(&return_stack);return MEM_ERROR;}
			}break;
			default:{
				TokenListDispose(&statement_stack);ListDispose(&return_stack);return SYN_ERROR;}
		}
		last_token_type = token.type;
		token = get_token();
		if(token.type == T_EOF) {
			TokenListDispose(&statement_stack);ListDispose(&return_stack);
			//ak bola predtym bodkociarka mozme skoncit
			return SYN_ERROR;
		}
		if(token.type == T_ERROR || token.type == T_INVALID) {
			TokenListDispose(&statement_stack);ListDispose(&return_stack);
			if(token.type == T_ERROR)return MEM_ERROR;
			else return LEX_ERROR;

		}
	}
}

enum {
	PS_START,
};

char *parse_expression(int *err) {
  assert(err != NULL);
  tList postfixlist;
  char *ret = NULL;
  *err = ALL_OK;
  
  ListInit(&postfixlist);
  ret = infix_to_postfix(&postfixlist,err);
  if (*err == ALL_OK && ret == NULL) {
    ret = postfix_parse(&postfixlist,err);
  }
//   printf("Postfix: ");
//   TokenListPostfixPrint(&postfixlist);
//   printf("\n");
  ListTokenDispose(&postfixlist);
  return ret;
}

/**
 * Main function calling syntactic and semantic analyzing
 * */
int parse(void)
{
	token.type = ~0U;
	int ret = ALL_OK;
	while(1) {
		ret = declaration();
		if(ret != ALL_OK)
			return ret;
		if(token.type == T_EOF)
			/* koniec zdrojaku */
			break;

		/*
			v tomto momente:
			token = posledny token na ktorom skoncilo parsovanie declaration
			pf_info = odkaz to tabulky funkcii na posledne spracovanu funkciu
		*/
#if 1
		ret = statement();
		if(ret != ALL_OK)
			return ret;
#endif
	}

	/* zdrojak mame naparsovany */
	struct Function *fmain_info = ht_lookup(TFunc, "main");
	if(fmain_info == NULL) {
		/* nedeklarovany main */
		LOGERR("Function 'main' undeclared\n");
		return SYN_ERROR;
	}
#if 0
	if(fmain_info->Parameters.First != NULL) {
		/* parametre v maine ? :D */
		LOGERR("Function 'main' has to contain zero parameters\n");
		return SYN_ERROR;
	}
#endif
	if(pf_info == NULL || strcmp(pf_info->ID, "main") != 0) {
		LOGERR("Function 'main' has to be declared as last function\n");
		/* posledna funkcia nebola main */
		return SYN_ERROR;
	}

	//insert I_STOP nstruction to Instruction list
	if(GenerateInstruction(I_STOP,NULL,NULL,NULL,NULL,
		&fmain_info->Instruction) == NULL)
		return MEM_ERROR;
	return interpret();
}

