/*
	Interpret jazyka IFJ2011
	Autori:
		Martin Risa (xrisam00)
		Michal Risa (xrisam01)
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "main.h"

void List2List (tList *L1, tList *L2)
{
	assert(L1 != NULL);
	assert(L2 != NULL);
	assert(L1->Act != NULL);
	if(L2->First == NULL)
		/* vkladany zoznam je prazdny */
		return;

	tListItem * tmp = L1->Act->NextItem ;
	L1->Act->NextItem = L2->First;
	L2->Last->NextItem = tmp;
}
void ListInit (tList *L)
{
	L->First = L->Last = L->Act = NULL;
}

//returns 0 if all succeeded
int ListInsertLast (tList * L,void * P)
{
	struct listItem *new = malloc(sizeof(struct listItem));
	if(new == NULL)
		return MEM_ERROR;

	if(L->Last == NULL)
		L->First = new;
	else
		L->Last->NextItem = new;

	L->Last = new;
	new->NextItem = NULL;
	new->data = P;
	return ALL_OK;
}
//this function expects list full of tokens
//and is called on initialized list
struct token_t * ListTokenFind(tList * L, int token_type){
	tListItem * act = L->First;
	struct token_t * ret_token = NULL;
	if(act)ret_token = act->data;
	if(act)
		while(act->NextItem!=NULL || ret_token->type != token_type){
			act=act->NextItem;
			ret_token = act->data;
		}
	return ret_token;
}

void ListDispose (tList *L)
{
	tListItem * temp = L->First;
	tListItem * temp1;
	while(temp != NULL) {	//cyklus az na koniec
		temp1 = temp;
		temp = temp->NextItem; 	//next
		free(temp1);
	}
	//nulovanie
	ListInit (L);
}

/*
 * Upravená funkce ListDispose pouze pro seznamy, kde ukazatel 'data'
 * v položce seznamu ukazuje na token.
 * Před uvolnění­m položky seznamu bude uvolněna
 * datová část tokenu.
 * xrudol04
 */
void ListTokenDispose (tList *L)
{
	assert(L != NULL);
	tListItem * temp = L->First;
	tListItem * temp1;
	struct token_t *tok;
	while(temp != NULL) {   //cyklus az na koniec
		temp1 = temp;
		temp = temp->NextItem;  //next
		tok = (struct token_t *)temp1->data;
		if (tok->type == T_VAL_STRING || tok->type == T_ID) {
		  if (tok->val.p != NULL) {
		    free(tok->val.p);
		  }
		}
		free(temp1->data);
		free(temp1);
	}
	//nulovanie
	ListInit (L);
}
void NameListDispose (tList *L)
{
	assert(L != NULL);
	if(L->First != NULL){
		tListItem * temp = L->First;
		tListItem * temp1;
		char *tok;
		while(temp->NextItem != NULL) {   //cyklus az na koniec
			temp1 = temp;
			temp = temp->NextItem;  //next
			tok = (char *)temp1->data;
			free(tok);
			free(temp1);
		}
	}
	//nulovanie
	ListInit (L);
}
void TokenListDispose (tList *L)
{
	assert(L != NULL);
	if(L->First != NULL){
		tListItem * temp = L->First;
		tListItem * temp1;
		struct token_t *tok;
		while(temp != NULL) {   //cyklus az na koniec
			temp1 = temp;
			temp = temp->NextItem;  //next
			tok = (struct token_t *)temp1->data;
			free(tok);
			free(temp1);
		}
	}
	//nulovanie
	ListInit (L);
}

void ListStackTokenDispose (tList *L)
{
	assert(L != NULL);
	tListItem * temp = L->First;
	tListItem * temp1;
	struct token_t *tok;
	while(temp != NULL) {   //cyklus az na koniec
		temp1 = temp;
		temp = temp->NextItem;  //next
		tok = (struct token_t *)temp1->data;
//		 if (tok->val.p != NULL) {
//		   free(tok->val.p);
//		 }
		free(tok);
		free(temp1);
	}
	//nulovanie
	ListInit (L);
}


//vracia MEM_EROR v pripade nedostatku pamete
int ListInsertFirst (tList *L, void * val)
{
	struct listItem *new = malloc(sizeof(struct listItem));
	if(new == NULL)
		return MEM_ERROR;

	if(L->First == NULL)
		L->Last = new;

	new->NextItem = L->First;
	L->First = new;

	new->data = val;
	return ALL_OK;
}

void ListFirst (tList *L)
{
	L->Act = L->First;
}

void ListCopyFirst (tList *L, void * val)
{
	assert(L->First != NULL);
	val = L->First->data;
}

void ListDeleteFirst (tList *L)
{
	if(L->First == NULL)
		return;

	struct listItem *this = L->First;
	L->First = L->First->NextItem;
	free((void*)this);

	if(L->First == NULL)
		ListInit(L);
}


void ListDeleteTokenFirst (tList *L)
{
	struct token_t *tok;
	if(L->First == NULL)
		return;

	struct listItem *this = L->First;
	L->First = L->First->NextItem;
	tok = (struct token_t *)this->data;
	if(tok->type == T_VAL_STRING || tok->type == T_ID) {
	  if(tok->val.p != NULL) {
	    free(tok->val.p);
	  }
	}
	free(this->data);
	free((void*)this);

	if(L->First == NULL)
		ListInit(L);
}

void ListDeleteStackTokenFirst (tList *L)
{
	struct token_t *tok;
	if(L->First == NULL)
		return;

	struct listItem *this = L->First;
	L->First = L->First->NextItem;
	tok = (struct token_t *)this->data;
//	 if (tok->val.p !=NULL) {
//	   free(tok->val.p);
//	 }
	free(tok);
	free((void*)this);

	if(L->First == NULL)
		ListInit(L);
}

void ListCopy (tList *L, void * val)
{
	assert(L->Act != NULL);
	val = L->Act->data;
}

void ListActualize (tList *L, void * val)
{
	assert(L->Act != NULL);
	L->Act->data = val;
}

void ListSucc (tList *L)
{
	if(L->Act != NULL)
		L->Act = L->Act->NextItem;
}

int ListActive (tList *L) 
{
	return L->Act != NULL;
}
