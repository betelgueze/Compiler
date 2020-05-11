/*
	Interpret jazyka IFJ2011
	Autori:
		Martin Risa (xrisam00)
		Michal Risa (xrisam01)
		Josef Rudolf (xrudol04)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "main.h"

tList list;						//Instruction list

extern struct htable_t * TFunc;		//Function table
extern FILE *fin;
struct htable_t * new_table;
tListItem * find_in_Ilist(tInstr * I)
{
	tListItem * temp = list.First;
	while((temp) &&(tInstr * )temp->data != I)temp = temp->NextItem;
	return temp;
}

void * insert_new_stack(struct Function *func)
{
	assert(func != NULL);

	/* kopia hashovacej tabulky */	
	struct htable_t *new =  ht_copy(
		(struct htable_t*)func->SymbolTable.Last->data);
	if(new == NULL)
		return NULL;
	new_table = new;
	/* vytvori kopie defaultnych stringov */
	struct token_t *t;
	struct htable_iter_t iter = ht_begin(new);
	while(ht_iter_empty(iter) == 0) {
		t = (struct token_t*)iter.entry->data;
		if(t->type == STR) {
			if(t->val.p != NULL) {
				char *res = malloc(strlen(t->val.p) + 1);
				if(res == NULL) {
					ht_free(new);
					return NULL;
				}
				strcpy(res, t->val.p);
				t->val.p = res;
			}
			else
				t->val.p = NULL;
		}
		iter = ht_iter_next(iter);
	}


	return new;
}

/**
 * returns RUN_ERROR, SEM_ERROR, MEM_ERROR, ALL_OK
 * */
int interpret(void)
{

	tList ActFunc,Ilist_stack,I_stack;
	list = ((TFunction *)ht_lookup(TFunc,"main"))->Instruction;
	ListFirst (&list);
	//zachytava meno funkcii 1. je aktualne spracuvavana
	ListInit (&ActFunc);
	//zachytava ukazatele na instrukcie ktorymi sa zacina v danom instrukcnom zozname
	ListInit (&Ilist_stack);
	//zachytava ukazatele na instrukcie ktorymi sa pokracuje po volani
	ListInit (&I_stack);
	if(ListInsertFirst (&ActFunc,"main")!= ALL_OK)return MEM_ERROR;
	tInstr * I;
	//zkopiruj do i aktulanu instrukciu
	I = list.Act->data;
	while(1)
	{
		//podla typu instruckie
		switch(I->I_Type){
		case I_NOP: {
			LOGERR("I_NOP\n");
		}break;
		//ak sa jedna o skok
		case I_GOTO:{
			LOGERR_VA("I_NOP: @4=%p\n", I->addr4);
			//skoc
			I = I->addr4;
			list.Act = find_in_Ilist(I);
			//pokracuj nasavenou instrukciou
			continue;
		}break;
		//zaciatok volania funkcie, zaloz tabulku symbolov a vloz funkciu na zasobnik spracovavanych instrukcii
		case I_CALL_START:{
			LOGERR_VA("I_CALL_START: @4=%p->'%s'\n", I->addr4, (char*)I->addr4);
			//nazov funkcie
			char * func_name = I->addr4;

			//pointer to actual interpreting function
			TFunction * func =  (TFunction *)ht_lookup(TFunc,func_name);
			//copiyng table of variables
#if 0
			struct htable_t * temp = ht_copy(func->SymbolTable.Last->data);
			if(temp == NULL) {
				NameListDispose(ActFunc);
				ListDeleteFirst(ActFunc);
				return MEM_ERROR;
			}
			//inserting new table of variables to heap
			if(ListInsertFirst (&func->SymbolTable, temp)!= ALL_OK){
				NameListDispose(ActFunc);
				ListDeleteFirst(ActFunc);
				return MEM_ERROR;
			}
#endif
			if(insert_new_stack(func) == NULL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_CALL_START: %i\n", __LINE__);
				return MEM_ERROR;
			}
		}break;
		//volanie funkcie obnasa vlozenie zoznamu instrukcii do aktualneho zoznamu instruckii
		case I_CALL:{
			LOGERR_VA("I_CALL: @4=%p->'%s'\n", I->addr4, (char*)I->addr4);

			char * func_name = I->addr4;
			//updating global list which represents actual interpreting function
			if(ListInsertFirst (&ActFunc,func_name)!= ALL_OK) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_CALL_START: %i\n", __LINE__);
				return MEM_ERROR;
			}
			/* vlozi do zasobniku stackov */
			if(ListInsertFirst(&(((TFunction * )ht_lookup(TFunc,func_name))->SymbolTable), new_table)!= ALL_OK){
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				return MEM_ERROR;
			}
			TFunction * func =  (TFunction *)ht_lookup(TFunc,func_name);
			//vlozi instrukciu zo zaciatku zoznamu
			if(ListInsertFirst (&Ilist_stack,list.First)!= ALL_OK) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_CALL: %i\n", __LINE__);
				return MEM_ERROR;
			}
			list.First = func->Instruction.First;
			//vlozi instrukciu ku ktorej sa vraciame
			if(ListInsertFirst (&I_stack,list.Act->NextItem)!= ALL_OK) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_CALL: %i\n", __LINE__);
				return MEM_ERROR;
			}
			list.Act = func->Instruction.First;
			I = list.Act->data;
			continue;
			//pokracuje sa nastavenou instrkciou
		}break;
		//navrat z funkcie obnasa mazanie tabulky symbolov a zo zasobniku aktulne spracuvavanych funkcii
		case I_CALL_STOP:{
			LOGERR_VA("I_CALL_STOP: @4=%p->'%s'\n", I->addr4, (char*)I->addr4);
			char * func_name = I->addr4;
			TFunction * func =  (TFunction *)ht_lookup(TFunc,func_name);
#ifdef LOG
			printf("RUN TIME STACK DUMP, function '%s'\n", func_name);
			print_stack(func);
			printf("\n\n");
#endif
			//free hash table of called function
			tList temp = func->SymbolTable;
			struct htable_t* ht_to_delete = temp.First->data;
			  struct htable_iter_t iter = ht_begin(ht_to_delete);
			  while(ht_iter_empty(iter) == 0) {
			   if(((struct token_t*)ht_iter_deref(iter))->type == STR)
			    free(((struct token_t*)ht_iter_deref(iter))->val.p);
			 /* len vzorova tabulka (ktora je v last) moze uvolnovat hashovacie kluce */
			    iter.entry->key = NULL;
			   iter = ht_iter_next(iter);
			  }
			  ht_free(ht_to_delete);
			list.First = Ilist_stack.First->data;
			ListDeleteFirst (&Ilist_stack);
			//deleting function from function heap
			ListDeleteFirst (&ActFunc);
			//deleting new table of variables from heap
			ListDeleteFirst (&func->SymbolTable);
		}break;
		//podmieneny skok
		case I_CONDGOTO:{
			LOGERR_VA("I_CONDGOTO: @1=%p->'%s'\n", I->addr1, (char*)I->addr1);
			char * de = I->addr1;
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * condition = ht_lookup(ht_to_search, de);
			//ak je vysledok vyrazu nerovny nule
			if(condition->val.i == 0){
				I = I->addr4;
				list.Act = find_in_Ilist(I);
				continue;
			}
		}break;
		case I_CONDGOTO_INV:{
			LOGERR_VA("I_CONDGOTO_INV: @1=%p->'%s'\n", I->addr1, (char*)I->addr1);
			char * de = I->addr1;
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * condition = ht_lookup(ht_to_search, de);
			//ak je vysledok vyrazu nerovny nule
			if(condition->val.i != 0){
				I = I->addr4;
				list.Act = find_in_Ilist(I);
				continue;
			}

		}break;
#if 1
		case I_SET:{// '='
			LOGERR_VA("I_SET: %s<-%s\n", (char*)I->addr4, (char*)I->addr1);
			/*
				addr1 = zdrojova premenna
				addr2, addr3
				addr4 = cielova premenna
			*/
			/* vyhlada zdrojovu a cielovu funkciu */
			struct Function *f_src = ht_lookup(TFunc, ActFunc.First->data);
			assert(f_src != NULL);
			/* vyhlada zdrojovu a cielovu premennu */
			struct token_t *v_src, *v_dst;
			v_src = ht_lookup(
				(struct htable_t*)f_src->SymbolTable.First->data, I->addr1);
			v_dst = ht_lookup(
				(struct htable_t*)f_src->SymbolTable.First->data, I->addr4);
			assert(v_src != NULL);
			assert(v_dst != NULL);
			/* mame zdrojovu aj cielovu premennu, presun */
			if(v_dst->type == STR)
				/* uvolni obsah stringu ktory by sa inak stratil */
				free(v_dst->val.p);
			/* ciel dostava typ zdroja */
			v_dst->type = v_src->type;
			if(v_src->type == STR) {
				v_dst->val.p = malloc(strlen(v_src->val.p) + 1);
				if(v_dst->val.p == NULL) {
					ListDispose(&Ilist_stack);
					NameListDispose(&ActFunc);
					ListDeleteFirst(&ActFunc);
					LOGERR_VA("I_SET: %i\n", __LINE__);
					return MEM_ERROR;
				}
				strcpy(v_dst->val.p, v_src->val.p);
			}
			else
				memcpy(v_dst, v_src, sizeof(struct token_t));
		}break;
#endif
		case I_SETPARAM:{// '='
			LOGERR("I_SET\n");
			char * de = I->addr4;
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			TFunction * F1 = ht_lookup(TFunc, I->addr3);
			struct htable_t* ht_to_search = temp.First->data;
			struct htable_t* ht_to_search1 = F1->SymbolTable.First->data;
			struct token_t * destination = ht_lookup(ht_to_search1, de);
			char * so = I->addr1;
			struct token_t * source = ht_lookup(ht_to_search, so);
			if(destination->type== STR && destination->val.p != NULL)free(destination->val.p);
			destination->type = source->type;
			if(source->type == STR){//if variable is string we need to copy it
				destination->val.p = malloc(sizeof(char)*strlen(source->val.p)+bulgarian_constant);
				if(!destination->val.p) {
					ListDispose(&Ilist_stack);
					NameListDispose(&ActFunc);
					ListDeleteFirst(&ActFunc);
					LOGERR_VA("I_SETPARAM: %i\n", __LINE__);
					return MEM_ERROR;
				}
				char * dest = destination->val.p;
				char * sour = source->val.p;
				strcpy(dest,sour);
			}
			else if(source->type == INT)//set for other data types
				destination->val.i = source->val.i;
			else if(source->type == DBL)//set for other data types
				destination->val.d = source->val.d;
			else
				destination->val.i = source->val.i;
			}break;
		case I_COPY:{// '='
					LOGERR_VA("I_COPY: %s[%s]<-%s[%s]\n", (char*)I->addr4,
						(char*)I->addr3, (char*)I->addr1, (char*)I->addr2);
					/*
						addr1 = zdrojova premenna
						addr2 = zdrojova funkcia
						addr3 = cielova funkcia
						addr4 = cielova premenna
					*/
					/* vyhlada zdrojovu a cielovu funkciu */
					struct Function *f_src, *f_dst;
					f_src = ht_lookup(TFunc, I->addr2);
					f_dst = ht_lookup(TFunc, I->addr3);
					assert(f_src != NULL);
					assert(f_dst != NULL);
					/* vyhlada zdrojovu a cielovu premennu */
					struct token_t *v_src, *v_dst;
					v_src = ht_lookup(
						(struct htable_t*)f_src->SymbolTable.First->data, I->addr1);

					if(strcmp((char*)I->addr2, (char*)I->addr3) == 0) {
						assert(f_dst->SymbolTable.First->NextItem != NULL);
						v_dst = ht_lookup(
							(struct htable_t*)f_dst->SymbolTable.First->NextItem->data, I->addr4);
					}
					else
						v_dst = ht_lookup(
							(struct htable_t*)f_dst->SymbolTable.First->data, I->addr4);
			
					assert(v_src != NULL);
					assert(v_dst != NULL);
					/* mame zdrojovu aj cielovu premennu, presun */
					if(v_dst->type == STR)
						/* uvolni obsah stringu ktory by sa inak stratil */
						free(v_dst->val.p);
					/* ciel dostava typ zdroja */
					v_dst->type = v_src->type;
					if(v_src->type == STR) {
						v_dst->val.p = malloc(strlen(v_src->val.p) + 1);
						if(v_dst->val.p == NULL) {
							ListDispose(&Ilist_stack);
							NameListDispose(&ActFunc);
							ListDeleteFirst(&ActFunc);
							LOGERR_VA("I_COPY: %i\n", __LINE__);
							return MEM_ERROR;
						}
						strcpy(v_dst->val.p, v_src->val.p);
					}
					else {
						memcpy(v_dst, v_src, sizeof(struct token_t));
					}

					}break;
		case I_COPY1:{// '='
			LOGERR_VA("I_COPY: %s[%s]<-%s[%s]\n", (char*)I->addr4,
				(char*)I->addr3, (char*)I->addr1, (char*)I->addr2);
			/*
				addr1 = zdrojova premenna
				addr2 = zdrojova funkcia
				addr3 = cielova funkcia
				addr4 = cielova premenna
			*/
			/* vyhlada zdrojovu a cielovu funkciu */
			struct Function *f_src;
			f_src = ht_lookup(TFunc, I->addr2);
			assert(f_src != NULL);
			/* vyhlada zdrojovu a cielovu premennu */
			struct token_t *v_src, *v_dst;
			v_src = ht_lookup(
				(struct htable_t*)f_src->SymbolTable.First->data, I->addr1);
			v_dst = ht_lookup(
				new_table, I->addr4);
			assert(v_src != NULL);
			assert(v_dst != NULL);
			/* mame zdrojovu aj cielovu premennu, presun */
			if(v_dst->type == STR)
				/* uvolni obsah stringu ktory by sa inak stratil */
				free(v_dst->val.p);
			/* ciel dostava typ zdroja */
			v_dst->type = v_src->type;
			if(v_src->type == STR) {
				v_dst->val.p = malloc(strlen(v_src->val.p) + 1);
				if(v_dst->val.p == NULL) {
					ListDispose(&Ilist_stack);
					NameListDispose(&ActFunc);
					ListDeleteFirst(&ActFunc);
					LOGERR_VA("I_COPY: %i\n", __LINE__);
					return MEM_ERROR;
				}
				strcpy(v_dst->val.p, v_src->val.p);
			}
			else {
				memcpy(v_dst, v_src, sizeof(struct token_t));
			}

			}break;
		case I_READ: {
			LOGERR("I_READ\n");
			/*
				@addr1 = mod nacitavania: RM_NUM, RM_FILE, RM_LINE, inak pocet
					znakov na nacitanie
				@addr2, @addr3 = nezalezi
				@addr4 = cielova premenna
			*/
			/*
				ad riesenie:
					1) read(2) nad vstupom "a" vrati retazec "a\0" typu STR
					2) read("*a") nad prazdnym vstupom vrati prazdny retazec (STR)
					3) read("*l") nad prazdnym vstupom vrati nil (NIL)
					4) pri chybe pocas nacitavania program konci s kodom MEM_ERROR
			*/
			/* vyhlada cielovu premennu */
			struct Function *func = ht_lookup(TFunc, ActFunc.First->data);
			assert(func != NULL);
			struct token_t *v_dst = ht_lookup(
				(struct htable_t*)func->SymbolTable.First->data, (char*)I->addr4);
			assert(v_dst != NULL);
			/* uvolni obsah cielovej premennej */
			if(v_dst->type == STR)
				free(v_dst->val.p);
			v_dst->val.p = NULL;
			/* defaultny typ premennej - RM_FILE, RM_LINE, pocet znakov */
			v_dst->type = STR;
			int ret;

			if((unsigned long)I->addr1 == (unsigned long)RM_NUM) {
				/* nacitnie cisla */	
				FILE *f = fin;
				fin = stdin;
				struct token_t token = get_token();
				fin = f;
				if(token.type != T_VAL_INT && token.type != T_VAL_DOUBLE) {
					if(token.type == T_ERROR) {
						ListDispose(&Ilist_stack);
						NameListDispose(&ActFunc);
						ListDeleteFirst(&ActFunc);
						/* interna chyba programu */
						return MEM_ERROR;
					}
					/* chybny token */
					free_token(token);
					/* koniec riadku alebo spatny token */
					v_dst->type = NIL;
					/* spracovanie cisel hotovo */
					break;
				}
				/* vlozi do tabulky */
				if(token.type == T_VAL_INT) {
					v_dst->type = INT;
					v_dst->val.i = token.val.i;
				}
				else /* token.type == T_VAL_DOUBLE */ {
					v_dst->type = DBL;
					v_dst->val.d = token.val.d;
				}
				break;
			}

			int buf_size = 32;
			int buf_pos = 0;
			char *buf = malloc(buf_size);
			if(buf == NULL){
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				return MEM_ERROR;
			}
			/* nacita znaky */
			while((ret = fgetc(stdin)) != EOF) {
				/* pripadna realokacia buffera */
				if(buf_pos == buf_size - 1) {
					buf_size <<= 1; /* buf_size * 2 */
					char *new_buf = realloc(buf, buf_size);
					if(new_buf == NULL) {
						ListDispose(&Ilist_stack);
						NameListDispose(&ActFunc);
						ListDeleteFirst(&ActFunc);
						free(buf);
						return MEM_ERROR;
					}
					buf = new_buf;
				}

				buf[buf_pos] = ret;
				buf_pos++;
				/* konecny automat podla toho, co chceme nacitat */
				/* ukoncovacie podmienky */
				switch((unsigned long)I->addr1) {
					case RM_FILE:
						/* cely subor, vzdy prejde */
						break;
					case RM_LINE:
						/* riadok */
						if(ret == '\n') {
							/* prepise '\n' za '\0' */
							buf_pos--;
							goto read_end;
						}
						break;

					default:
						/* num znakov */
						if((unsigned long)buf_pos == (unsigned long)I->addr1)
							goto read_end;
						break;
				}
			}
read_end:
			/* ukoncovaci znak + zapisanie premennej do tabulky */
			assert(buf_pos < buf_size);
			buf[buf_pos] = '\0';
			v_dst->type = STR;
			v_dst->val.p = buf;

			switch((unsigned long)I->addr1) {
				case RM_FILE:
					/* ad riesenie 2) */
					break;

				case RM_LINE:
					/* ad riesenie 3) */
					if(buf_pos == 0) {
						free(buf);
						/* prazdny vstup */
						v_dst->type = NIL;
					}
					break;

				case RM_NUM:
					/* nesmie sa stat, zachytene vyssie */
					assert(0);
					break;

				default:
#if 0				
					if((unsigned long)buf_pos < (unsigned long)I->addr1) {
						/* nacitalo sa menej znakov ako sa malo */
					}
#endif
					break;
			}
			break;
		}
		case I_WRITE:{
			LOGERR_VA("I_WRITE: @4=%p->%s -> stdout\n", I->addr4,
				(char*)I->addr4);
			/* addr4 = meno vypisovanej premennej */
			/* aktualne beziaca funkcia */
			struct Function *f = ht_lookup(TFunc, ActFunc.First->data);
			assert(f != NULL);
			/* premenna ktora ide na vypis */
			struct token_t *t = ht_lookup(
				(struct htable_t*)f->SymbolTable.First->data, (char*)I->addr4);
			assert(t != NULL);
			/* nesmie byt typu NIL ani BOL */
			if(t->type == NIL || t->type == BOL) {

				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_WRITE: %i\n", __LINE__);
				return RUN_ERROR;
			}
			/* vypis ! */
			switch(t->type) {
				case INT: printf("%g", (double)t->val.i); break;
				case DBL: printf("%g", t->val.d); break;
				case STR: printf("%s", (char*)t->val.p); break;
#ifndef NDEBUG
				default: assert(0);
#endif
			}
/*
			char * de = I->addr4;
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * source = ht_lookup(ht_to_search, de);
			if(source->type == NIL)return RUN_ERROR;
			if(source->type == INT)printf("%g",(double)source->val.i);
			if(source->type == DBL)printf("%g",source->val.d);
			if(source->type == STR)printf("%s",(char*)source->val.p);
*/
		}break;

		case I_TYPE:{
			LOGERR_VA("I_TYPE: %s<-type(%s)\n", (char*)I->addr4,
				(char*)I->addr1);
			char * de = I->addr4;
			char * so = I->addr1;
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * destination = ht_lookup(ht_to_search, de);
			struct token_t * source = ht_lookup(ht_to_search, so);
			if(destination->type == STR && destination->val.p != NULL)
				free(destination->val.p);
			destination->val.p = malloc(sizeof(char)*8);
			if(destination->val.p == NULL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_TYPE: %i\n", __LINE__);
				return MEM_ERROR;
			}
			destination->type = STR;
			if(source->type == NIL)strcpy(destination->val.p,"nil");
			else if(source->type == INT)strcpy(destination->val.p,"number");
			else if(source->type == DBL)strcpy(destination->val.p,"number");
			else if(source->type == STR)strcpy(destination->val.p,"string");
			else /*if(source->type == BOL)*/strcpy(destination->val.p,"boolean");
		}break;
		case I_SUBSTR:{
			char * in = I->addr1;
			char * st = I->addr2;
			char * fi = I->addr3;
			char * ou = I->addr4;
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * input = ht_lookup(ht_to_search, in);
			struct token_t * start = ht_lookup(ht_to_search, st);
			struct token_t * finish = ht_lookup(ht_to_search, fi);
			struct token_t * output = ht_lookup(ht_to_search, ou);
			char * first;
			if(input->type != STR || start->type != INT ||
				finish->type != INT) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_SUBSTR: %i\n", __LINE__);
				return RUN_ERROR;
			}
			if(output->type == STR) {
                          if (output->val.p != NULL) {
                            free(output->val.p);
                          }
			}
			int count,i,size = strlen(input->val.p);
			//goto start
			if(start->val.i < 0 ){
//				char * tmp = (char *)input->val.p;
//				first = tmp[size+start->val.i-1];
                                int presah = 0;
                                if (-(start->val.i) > size) {
                                  first = (char *)input->val.p;
                                  presah = -(start->val.i) - size;
                                }
                                else {
                                  first = (char *)input->val.p + size + start->val.i;
                                }
				
				
				
				if(finish->val.i < 0){
					if(finish->val.i >= start->val.i){// - - ->
						count = finish->val.i - start->val.i+1-presah;
                                                count = (count<0)?0:count;
					}
					else{// - - <-
						//nothing to sub
						count = 0;
					}
				}
				else{
                                      
					if((size+1 + start->val.i) > finish->val.i)// - + <-
						count = 0;
					else// - + ->
						count  = finish->val.i - (size + start->val.i) - presah;
                                                count = (count<0)?0:count;
				}
			}
			else{
//				char * tmp = (char *)input->val.p;
//				first = tmp[start->val.i-1];
                                first = (char *)input->val.p + start->val.i - 1;
                                if (start->val.i > size) {
                                  count = 0;
                                }
				else if(finish->val.i < 0){
					if((size+1) + finish->val.i >= start->val.i) {// + - ->
						count = (size+1) + finish->val.i - start->val.i + 1;
                                        }
					else// + - <-
						count = 0;
				}
				else{
					if(finish->val.i >= start->val.i)// + + ->
                                          if(finish->val.i >= size) {
                                            count = size - start->val.i + 1;
                                          }
                                          else {
                                            count = finish->val.i - start->val.i + 1;
                                          }
					else count  = 0;// + + <-
				}
			}
			if ((output->val.p = malloc((count+1)*sizeof(char))) == NULL) {
                          LOGERR_VA("I_SUBSTR: %i\n", __LINE__);
                          return MEM_ERROR;
			}
			if(count == 0)//strcpy(output->val.p,"");
				((char*)output->val.p)[0] = '\0';
			else{
				char * tmp = (char *)output->val.p;
				for(i=0;i<count;i++) {
                                  tmp[i]=first[i];
                                }
                                tmp[count] = '\0';
			}
			output->type = STR;
		}break;
		case I_MINUS: {
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);

			LOGERR_VA("I_MINUS: %s[%p] <- %s[%p] - %s[%p]\n", de, result, fo, first_op, so, second_op);

			if(first_op->type == STR || second_op->type == STR ||
				first_op->type == NIL || second_op->type == NIL) {
				LOGERR_VA("I_MINUS: %i\n", __LINE__);
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				return RUN_ERROR;
			}
			if(first_op->type == DBL ){//interpreting add
				if(second_op->type == DBL)
					result->val.d = first_op->val.d - second_op->val.d;
				else
					result->val.d = first_op->val.d - (double)second_op->val.i;
				result->type = DBL;
			}
			else{
				if(second_op->type == INT ){
					result->type = INT;
					result->val.i = first_op->val.i - second_op->val.i;
				}
				else{
					result->type = DBL;
					result->val.d = (double)first_op->val.i - second_op->val.d;
				}
			}
			break;
		}
		case I_PLUS:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_PLUS: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			if(first_op->type == STR || second_op->type == STR ||
				first_op->type == NIL || second_op->type == NIL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_PLUS: %i\n", __LINE__);
				return RUN_ERROR;
			}
			if(first_op->type == DBL ){//interpreting add
				if(second_op->type == DBL)
					result->val.d = first_op->val.d + second_op->val.d;
				else
					result->val.d = first_op->val.d + (double)second_op->val.i;
				result->type = DBL;
			}
			else{
				if(second_op->type == INT ){
					result->type = INT;
					result->val.i = first_op->val.i + second_op->val.i;
				}
				else{
					result->type = DBL;
					result->val.d = (double)first_op->val.i + second_op->val.d;
				}
			}
		}break;
		case I_MULTIPLY:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_MULTIPLY: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			if(first_op->type == STR || second_op->type == STR ||
				first_op->type == NIL || second_op->type == NIL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_MULTIPLY: %i\n", __LINE__);
				return RUN_ERROR;
			}
			if(first_op->type == DBL ){//interpreting add
				if(second_op->type == DBL)
					result->val.d = first_op->val.d * second_op->val.d;
				else
					result->val.d = first_op->val.d * (double)second_op->val.i;
				result->type = DBL;
			}
			else{
				if(second_op->type == INT ){
					result->type = INT;
					result->val.i = first_op->val.i * second_op->val.i;
				}
				else{
					result->type = DBL;
					result->val.d = (double)first_op->val.i * second_op->val.d;
				}
			}
		}break;
		case I_DIVIDE:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_DIVIDE: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			if(first_op->type == STR || second_op->type == STR ||
				first_op->type == NIL || second_op->type == NIL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_DIVIDE: %i\n", __LINE__);
				return RUN_ERROR;
			}
			if(first_op->type == DBL ){//interpreting add
				if(second_op->type == DBL)
					result->val.d = first_op->val.d / second_op->val.d;
				else
					result->val.d = first_op->val.d / (double)second_op->val.i;
				result->type = DBL;
			}
			else{
				if(second_op->type == INT ){
					result->type = INT;
					result->val.i = first_op->val.i / second_op->val.i;
				}
				else{
					result->type = DBL;
					result->val.d = (double)first_op->val.i / second_op->val.d;
				}
			}
		}break;

		case I_EXPONENT:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_EXPONENT: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			result->type = DBL;
			if(first_op->type == DBL && second_op->type == DBL )
				result->val.d = pow(first_op->val.d,second_op->val.d);
			else if(first_op->type == INT && second_op->type == DBL )
				result->val.d = pow((double)first_op->val.i,second_op->val.d);
			else if(first_op->type == DBL && second_op->type == INT )
				result->val.d = pow(first_op->val.d,(double)second_op->val.i);
			else if(first_op->type == INT && second_op->type == INT )
				result->val.d = pow((double)first_op->val.d,(double)second_op->val.i);
			else {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_EXPONENT: %i\n", __LINE__);
				return RUN_ERROR;
			}
		}break;


		case I_CONCAT:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_CONCAT: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			if(first_op->type != STR || second_op->type != STR) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_CONCAT: %i\n", __LINE__);
				return RUN_ERROR;
			}
			result->val.p = realloc(result->val.p,strlen(first_op->val.p)+strlen(second_op->val.p)+bulgarian_constant);
			if(result->val.p == NULL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_CONCAT: %i\n", __LINE__);
				return MEM_ERROR;
			}
			char * tmp = result->val.p;//copiing
			strcpy(tmp,first_op->val.p);
			tmp += strlen(first_op->val.p);
			strcpy(tmp,second_op->val.p);
			result->type = STR;
		}break;
		case I_EQUALS:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_EQUALS: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			result->type = BOL;
#if 0
			if(first_op->type == NIL || second_op->type == NIL) {
				LOGERR_VA("I_EQUALS: %i\n", __LINE__);
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				return RUN_ERROR;
			}
#endif
			if((first_op->type == second_op->type) ||((first_op->type == INT && second_op->type == DBL)||(first_op->type == DBL && second_op->type == INT))){
				switch(first_op->type){//interpreting equals
					case BOL:{
						if(second_op->val.i == first_op->val.i)
							result->val.i = TRUE;

						else
							result->val.i = FALSE;
					}break;
					case INT:{
						if(second_op->type == INT)
							result->val.i = (second_op->val.i == first_op->val.i);
						else
							result->val.i = ((int)second_op->val.d == first_op->val.i);
					}break;
					case DBL:{
						if(second_op->type == DBL)
							result->val.i = (second_op->val.d == first_op->val.d);
						else
							result->val.i = ((int)first_op->val.d == second_op->val.i);
					}break;
					case STR:{
						if(strcmp(first_op->val.p,second_op->val.p)== 0 )
							result->val.i = TRUE;
						else
							result->val.i = FALSE;
					}break;
					case NIL: {
						result->val.i = second_op->type == first_op->type;
						break;
					}
				}
			}
			else
				result->val.i=FALSE;
		}break;
		case I_NOTEQUALS:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_NOTEQUALS: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			result->type = BOL;
#if 0
			if(first_op->type == NIL || second_op->type == NIL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_NOTEQUALS: %i\n", __LINE__);
				return RUN_ERROR;
			}
#endif
			if((first_op->type == second_op->type) ||((first_op->type == INT && second_op->type == DBL)||(first_op->type == DBL && second_op->type == INT))){
				switch(first_op->type){//interpreting not equals
					case BOL:{
						if(second_op->val.i != first_op->val.i)
							result->val.i = TRUE;
						else
							result->val.i = FALSE;
					}break;
					case INT:{
						if(second_op->type == INT)
							result->val.i = (second_op->val.i != first_op->val.i);
						else
							result->val.i = ((int)second_op->val.d != first_op->val.i);
					}break;
					case DBL:{
						if(second_op->type == DBL)
							result->val.i = (second_op->val.d != first_op->val.d);
						else
							result->val.i = ((int)first_op->val.d != second_op->val.i);
					}break;
					case STR:{
						if(strcmp(first_op->val.p,second_op->val.p) != 0 )
							result->val.i = TRUE;
						else
							result->val.i = FALSE;
					}break;
					case NIL: {
						result->val.i = second_op->type != first_op->type;
						break;
					}
				}
			}
			else
				result->val.i=FALSE;
		}break;
		case I_GREATER:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_GREATER: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			result->type = BOL;
			if(first_op->type == NIL || second_op->type == NIL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_GREATER: %i\n", __LINE__);
				return RUN_ERROR;
			}
			if((first_op->type == second_op->type) ||((first_op->type == INT && second_op->type == DBL)||(first_op->type == DBL && second_op->type == INT))){
				switch(first_op->type){//interpreting not equals
					case INT:{
						if(second_op->type == INT)
							result->val.i = (second_op->val.i < first_op->val.i);
						else
							result->val.i = ((int)second_op->val.d < first_op->val.i);
					}break;
					case DBL:{
						if(second_op->type == DBL)
							result->val.i = (second_op->val.d < first_op->val.d);
						else
							result->val.i = ((int)first_op->val.d > second_op->val.i);
					}break;
					case STR:{
						if(strcmp(first_op->val.p,second_op->val.p) > 0 )
							result->val.i = TRUE;
						else
							result->val.i = FALSE;
					}break;
				}
			}
			else
				result->val.i=FALSE;
		}break;
		case I_SMALLER:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_SMALLER: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			result->type = BOL;
			if(first_op->type == NIL || second_op->type == NIL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_SMALLER: %i\n", __LINE__);
				return RUN_ERROR;
			}
			if((first_op->type == second_op->type) ||((first_op->type == INT && second_op->type == DBL)||(first_op->type == DBL && second_op->type == INT))){
				switch(first_op->type){//interpreting not equals
					case INT:{
						if(second_op->type == INT)
							result->val.i = (second_op->val.i > first_op->val.i);
						else
							result->val.i = ((int)second_op->val.d > first_op->val.i);
					}break;
					case DBL:{
						if(second_op->type == DBL)
							result->val.i = (second_op->val.d > first_op->val.d);
						else
							result->val.i = ((int)first_op->val.d < second_op->val.i);
					}break;
					case STR:{
						if(strcmp(first_op->val.p,second_op->val.p) < 0 )
							result->val.i = TRUE;
						else
							result->val.i = FALSE;
					}break;
				}
			}
			else
				result->val.i=FALSE;
		}break;
		case I_GREATEREQU:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_GREATEREQU: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			result->type = BOL;
			if(first_op->type == NIL || second_op->type == NIL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_GREATEREQU: %i\n", __LINE__);
				return RUN_ERROR;
			}
			if((first_op->type == second_op->type) ||((first_op->type == INT && second_op->type == DBL)||(first_op->type == DBL && second_op->type == INT))){
				switch(first_op->type){//interpreting not equals
					case INT:{
						if(second_op->type == INT)
							result->val.i = (second_op->val.i <= first_op->val.i);
						else
							result->val.i = ((int)second_op->val.d <= first_op->val.i);
					}break;
					case DBL:{
						if(second_op->type == DBL)
							result->val.i = (second_op->val.d <= first_op->val.d);
						else
							result->val.i = ((int)first_op->val.d >= second_op->val.i);
					}break;
					case STR:{
						if(strcmp(first_op->val.p,second_op->val.p) >= 0 )
							result->val.i = TRUE;
						else
							result->val.i = FALSE;
					}break;
				}
			}
			else
				result->val.i=FALSE;
		}break;
		case I_SMALLEREQU:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_SMALLEREQU: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			result->type = BOL;
			if(first_op->type == NIL || second_op->type == NIL) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_SMALLEREQU: %i\n", __LINE__);
				return RUN_ERROR;
			}
			if((first_op->type == second_op->type) ||((first_op->type == INT && second_op->type == DBL)||(first_op->type == DBL && second_op->type == INT))){
				switch(first_op->type){//interpreting not equals
					case INT:{
						if(second_op->type == INT)
							result->val.i = (second_op->val.i >= first_op->val.i);
						else
							result->val.i = ((int)second_op->val.d >= first_op->val.i);
					}break;
					case DBL:{
						if(second_op->type == DBL)
							result->val.i = (second_op->val.d >= first_op->val.d);
						else
							result->val.i = ((int)first_op->val.d <= second_op->val.i);
					}break;
					case STR:{
						if(strcmp((char *)first_op->val.p,second_op->val.p) <= 0 )
							result->val.i = TRUE;
						else
							result->val.i = FALSE;
					}break;
				}
			}
			else
				result->val.i=FALSE;
		}break;
		case I_FIND:{
			char * de = I->addr4;
			char * so = I->addr2;
			char * fo = I->addr1;
			LOGERR_VA("I_FIND: %s <- %s - %s\n", de, fo, so);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * result = ht_lookup(ht_to_search, de);
			struct token_t * second_op = ht_lookup(ht_to_search, so);
			struct token_t * first_op = ht_lookup(ht_to_search, fo);
			if(first_op->type != STR || second_op->type != STR)result->type = NIL;
			else{
				int ret = kmp_find((char *)first_op->val.p, (char *)second_op->val.p);
				if(ret == -2) {
					ListDispose(&Ilist_stack);
					NameListDispose(&ActFunc);
					ListDeleteFirst(&ActFunc);
					LOGERR_VA("I_FIND: %i\n", __LINE__);
					return MEM_ERROR;
				}	
				else if(ret == -1){
					result->type = BOL;
					result->val.i = 0;
				}
				else {
					result->val.i = ret;
					result->type = INT;
				}
			}
		}break;
		case I_SORT:{
			char * fo = I->addr1;
			LOGERR_VA("I_SORT: @1=%s\n", fo);
			TFunction * F = ht_lookup(TFunc, ActFunc.First->data);
			tList temp = F->SymbolTable;
			struct htable_t* ht_to_search = temp.First->data;
			struct token_t * string = ht_lookup(ht_to_search, fo);
			if(string->type != STR)string->type=NIL;
			else
				heap_sort(string->val.p);
		}break;

		case I_STRLEN: {
			LOGERR_VA("I_STRLEN: %s <- strlen(%s)\n", (char*)I->addr4,
				(char*)I->addr1);
			/*
				addr1 = nazov zdrojovej premennej (stringu)
				addr4 = nazov cielovej premennej
				#addr4 = strlen(addr1)
			*/
			/* vyhlada zdrojovu funkciu */
			struct Function *f_src = ht_lookup(TFunc, ActFunc.First->data);
			assert(f_src != NULL);
			/* vyhlada zdrojovu premennu */
			struct token_t *v_src, *v_dst;
			v_src = ht_lookup(
				(struct htable_t*)f_src->SymbolTable.First->data, I->addr1);
			v_dst = ht_lookup(
				(struct htable_t*)f_src->SymbolTable.First->data, I->addr4);
			assert(v_src != NULL);
			assert(v_dst != NULL);

			if(v_src->type != STR) {
				ListDispose(&Ilist_stack);
				NameListDispose(&ActFunc);
				ListDeleteFirst(&ActFunc);
				LOGERR_VA("I_STRLEN: %i\n", __LINE__);
				return RUN_ERROR;
			}
			v_dst->type = INT;

			if(v_dst->type == STR)
				free(v_dst->val.p);
			v_dst->val.i = strlen((char*)v_src->val.p);
			break;
		}

		case I_STOP:
			//last instruction should be I_STOP
			ListDeleteFirst (&ActFunc);
			return ALL_OK;
		break;
		}
		//ak sme na konci instrukcneho zoznamu
		if(list.Act->NextItem == NULL){
			list.Act = I_stack.First->data;
			ListDeleteFirst (&I_stack);
		}
		else
			//iterrating to next instruction
			ListSucc (&list);
		//zkopiruj do i aktulanu instrukciu
		I = list.Act->data;
	}
	/* not reached */
}
