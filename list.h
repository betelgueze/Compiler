/*
	Interpret jazyka IFJ2011
	Autori:
		Martin Risa (xrisam00)
*/

#ifndef __ILIST__H
#define __ILIST__H

typedef struct listItem
{
	void * data;
	struct listItem * NextItem;
}tListItem;

/**
 * List Header
 * */

typedef struct {
    tListItem * Act;
    tListItem * First;
	tListItem * Last;
} tList;
/**
 * TODO
 * funkcia zkopiruje poslednu tabulku symbolov do prvej vrati MEM_ERROR ak sa kopirovanie nepodarilo
 * */
void TokenListDispose (tList *L);
void NameListDispose (tList *L);
int ht_copyLast2First(tList * );
struct token_t * ListTokenFind(tList * L, int token_type);
int ListInsertLast (tList *,void *);
void ListInit (tList *);
void ListTokenDispose (tList *);
void ListStackTokenDispose (tList *);
void ListDispose (tList *);
int ListInsertFirst (tList *, void *);
void ListFirst (tList *);
void ListCopyFirst (tList *, void *);
void ListDeleteFirst (tList *);
void ListDeleteTokenFirst (tList *);
void ListDeleteStackTokenFirst (tList *);
void ListPostDelete (tList *);
int ListPostInsert (tList *, void *);
void ListSucc (tList *);
void ListCopy (tList *, void *);
void List2List (tList *, tList *);
void ListActualize (tList *, void *);
int  ListActive (tList *);
#endif /* have __ILIST__H */

