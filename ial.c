/*
	Interpret jazyka IFJ2011
	Autori:
		Michal Risa (xrisam01)
		Josef Rudolf (xrudol04)
*/

#include <assert.h>
#include <stdlib.h> /* malloc, ... */
#include <string.h> /* memcpy, ... */
#include <stdio.h>
#include "ial.h"

/*
	Hashovacia tabulka
	Autor: xrisam01
*/

struct htable_t *ht_init(unsigned long size)
{
	assert(size != 0UL);
	struct htable_t *ht = malloc(sizeof(struct htable_t) +
		size * sizeof(struct htable_entry_t*));
	if(ht != NULL) {
		/* inicializuje */
		ht->size = size;
		memset(ht->table, 0, size * sizeof(struct htable_entry_t*));
	}
	return ht;
}

void ht_free(struct htable_t *ht)
{
	assert(ht != NULL);
	ht_clear(ht);
	free(ht);
}

void *ht_insert(struct htable_t *ht, const char *key, void *data, size_t size)
{
	assert(ht != NULL);
	assert(key != NULL);
	assert(data != NULL);
	assert(size > 0);

	/* alokuje novy prvok */
	struct htable_entry_t *this = malloc(sizeof(struct htable_entry_t) + size);
	if(this == NULL)
		return NULL;

	/* skopiruje data */
	memcpy(this->data, data, size);
	this->key = key;
	this->size = size;

	/* index do tabulky */
	unsigned long idx = ht_hash((const unsigned char*)key) % ht->size;

	/* vlozi novy prvok pred vsetky ostatne */
	this->next = ht->table[idx];
	ht->table[idx] = this;
	return this->data;
}

void *ht_lookup(struct htable_t *ht, const char *key)
{
	assert(ht != NULL);
	assert(key != NULL);

	/* index do tabulky */
	unsigned long idx = ht_hash((const unsigned char*)key) % ht->size;
	struct htable_entry_t *entry = ht->table[idx];

	/* prejde zoznam */
	for(; entry != NULL; entry = entry->next) {
		if(strcmp(entry->key, key) == 0)
			/* zhoda klucov */
			return (void*)entry->data;
	}

	/* data nenajdene */
	return NULL;
}

void ht_clear(struct htable_t *ht)
{
	assert(ht != NULL);
	unsigned long idx;
	struct htable_entry_t *entry;
	struct htable_entry_t *tmp;

	/* prejde tabulku */
	for(idx = 0; idx < ht->size; idx++) {
		if(ht->table[idx] == NULL)
			continue;

		/* uvolni zoznamy */
		for(entry = ht->table[idx]; entry != NULL; entry = tmp) {
			tmp = entry->next;
			free((void*)entry->key);
			free((void*)entry);
		}
	}

	/* inicializuje */
	memset(ht->table, 0, ht->size * sizeof(struct htable_entry_t*));
}

struct htable_iter_t ht_begin(struct htable_t *ht)
{
	assert(ht != NULL);

	struct htable_iter_t iter;
	iter.ht = ht;
	iter.entry = NULL;

	/* prejde tabulku */
	for(iter.idx = 0UL; iter.idx < ht->size; iter.idx++) {
		if(ht->table[iter.idx] == NULL)
			continue;

		/* prvy pouzity prvok */
		iter.entry = ht->table[iter.idx];
		break;
	}

	return iter;
}

struct htable_iter_t ht_iter_next(struct htable_iter_t iter)
{
	assert(iter.entry != NULL);
#if 1
	for(iter.entry = iter.entry->next; iter.entry != NULL;
		iter.entry = iter.entry->next)
		/* prejde zoznam */
		return iter;
#endif
	/* zoznam dosiel, prejde zvysok tabulky */
	for(++iter.idx; iter.idx < iter.ht->size; iter.idx++) {
		if(iter.ht->table[iter.idx] == NULL)
			continue;

		/* dalsi pouzity prvok */
		iter.entry = iter.ht->table[iter.idx];
		return iter;
	}

	iter.entry = NULL;
	return iter;
}
//int ht_copy(struct htable_t * dest , struct htable_t *src);
struct htable_t *ht_copy(struct htable_t *src) /* ZATIAL NEOTESTOVANE!!! */
{
	assert(src != NULL);
	/* vytvori novu tabulku o rovnakej velkosti */
	struct htable_t *new = ht_init(src->size);
	if(new == NULL)
		return NULL;

	/* vytvori kopiu obsahu starej */
	struct htable_entry_t *this;
	struct htable_iter_t iter = ht_begin(src);
	while(ht_iter_empty(iter) == 0) {
		this = malloc(sizeof(struct htable_entry_t) + iter.entry->size);
		if(this == NULL) {
			ht_free(new);
			return NULL;
		}

		/* skopiruje data */
		this->key = iter.entry->key;
		this->size = iter.entry->size;
		memcpy(this->data, iter.entry->data, this->size);

		/* konzistencia zoznamu */
		this->next = new->table[iter.idx];
		new->table[iter.idx] = this;

		iter = ht_iter_next(iter);
	}

	return new;
}

/*
 * Heap sort
 * Autor: xrudol04
 */

/*
 * Pomocna funkce k funkci g(), ktera prohodi dva znaky ve stringu.
 * Na vstupu funkce ocekava ukazatel na string a dva integery.
 * Integery jsou pouzity jako indexy pole znaku (stringu) a
 * s vyuzitim pomocne promenne c jsou znaky na obou pozicich
 * prohozeny.
 * Funkce nema moznost zjistit, jestli integery nejsou vetsi nez
 * (delka stringu - 1). Toto musi byt osetreno ve funkci, ktera tuto funkci vola.
 * Funkce hs_swap() nic nevraci.
 */
static void hs_swap(char *string, int a, int b) {
  char c = string[a];
  string[a] = string[b];
  string[b] = c;
}

/*
 * Pomocna funkce k funkci heap_sort(), ktera na vstupu ocekava ukazatel na string,
 * integer indexujici v poli stringu koren binarniho stromu, jehoz dva potomci
 * jsou 2*i a 2*i+1. Treti vstupni hodnota je aktualni delka razene casti stringu,
 * podle ktere se zjistuje, jestli potomci lezi uvnitr razene casti stringu.
 * funkce hs_heapify premeni binarni strom na haldu tak, ze ze vsech trech prvku
 * (koren a dva potomci) vybere prvek s nejvetsi hodnotou, a pokud tento prvek neni
 * koren, prohodi jeho hodnotu s hodnotou korene s pomoci funkce hs_swap()
 * Funkce nic nevraci.
 */
static void hs_heapify(char *string, int i, int len) {
  int swapnode = 0;
  char temp = string[i-1];
  if ((i<<1) <= len) {
    if ((unsigned char)temp < (unsigned char)string[(i<<1)-1]) {
      temp = string[(i<<1)-1];
      swapnode = i<<1;
    }
  }
  if (((i<<1) + 1) <= len) {
    if ((unsigned char)temp < (unsigned char)string[i<<1]) {
      temp = string[i<<1];
      swapnode = (i<<1) + 1;
    }
  }
  if (swapnode != 0) {
    hs_swap(string,i-1,swapnode-1);
  }
}

/*
 * Pomocna funkce k funkci heap_sort(), ktera vola funkci hs_heapify()
 * nad vsemi koreny aktualne razene casti stringu.
 * Na vstupu ocekava ukazatel na razeny string a aktualni delku razene
 * casti stringu.
 * Funkce nic nevraci.
 */
static void hs_heapify_all(char *string, int len) {
	int i;
  for (i=len>>1;i>=1;i--) {
    hs_heapify(string,i,len);
  }
}

/*
 * Funkce heapsort. Na vstupu ocekava ukazatel na string, ktery seradi.
 * Funkce nic nevraci, radi na miste.
 */
void heap_sort(char *string) {
  int len = strlen(string);
  while (len>1) {
    hs_heapify_all(string,len);
    hs_swap(string,0,len-1);
    len--;
  }
}


/*
 * Vyhledavaci Knuth-Morris-Pratt algoritmus
 * Autor: xrudol04
 */

/*
 * Funkce, ktera vytvori tabulku castecnych shod
 * Na vstupu ocekava ukazatel na vyhledavany string
 * a ukazatel na predem alokovanou tabulku integeru
 * o stejne delce jako je pocet znaku stringu
 * (bez zakoncovaci \000).
 * Funkce nic nevraci, jejim ukolem je menit hodnoty
 * v tabulce.
 */
static void kmp_table(char *word, int *table) {
  int position = 2;
  int candidate = 0;
  int wordlen = strlen(word);
  //inicializace prvnich hodnot tabulky
  table[0] = -1;
  if (wordlen > 1) {
    table[1] = 0;
  }
  while (position < wordlen) {
    /*
     * Pri kazde iteraci, pri ktere se porovnavane
     * znaky rovnaji, se zvysi hodnota, ktera bude zapsana
     * na aktualni pozici v tabulce.
     */
    if (word[position-1] == word[candidate]) {
      candidate++;
      table[position] = candidate;
      position++;
    }
    /*
     * Pri neshode znaku, pokud hodnota candidate neni nulova,
     * vraci se tak, aby byla schopna navazat na dalsi vlastni
     * prefix hledaneho retezce. Pokud se v useku od zacatku
     * soucasne nalezeneho vlastniho prefixu po aktualni pozici
     * v tabulce nenachazi zadny dalsi vlastni prefix stringu,
     * hodnota candidate postupne spadne na 0.
     */
    else if (candidate > 0) {
      candidate = table[candidate];
    }
    /*
     * Pokud porovnane znaky nebyly stejne a hodnota candidate je
     * nulova, porovnavany znak neni zacatkem vlastniho prefixu
     * stringu, a proto hodnota zapsana do tabulky bude nula.
     */
    else {
      table[position] = 0;
      position++;
    }
  }
}



/*
 * Funkce find pouzivajici algoritmus Knuth-Morris-Pratt.
 * Na vstupu ocekava dva stringy
 * 'string' je text, ve kterem se bude hledat,
 * 'word' je hledany usek textu.
 * Funkce vraci hodnotu int:
 * Funkce pouziva dynamickou alokaci pole integeru. Pokud dojde k chybe pri alokaci,
 * funkce vrati hodnotu -2.
 * Pokud word nebyl ve stringu nalezen, vraci hodnotu -1 (bude zpracovana jako false).
 * Pokud hledany string byl prazdny retezec, vraci hodnotu 0
 * Pokud hledany retezec byl nalezen, vraci pozici prvniho znaku prvniho vyskytu
 * 'word' v 'string'. Pozice je pocitana od 1.
 */
int kmp_find(char *string, char *word) {
  int retvalue = 0;
  int *table = NULL;
  int wordlen = strlen(word);
  int stringlen = strlen(string);
  int match = 0;
  int i = 0;

  if (wordlen > 0) {
    /*
     * alokace pole integeru o stejne delce jako je
     * delka hledaneho stringu
     */
    table = malloc((wordlen)*sizeof(int));
    if (table == NULL) {
      retvalue = -2;
    }
    else {
      kmp_table(word,table); //vytvorit KMP tabulku
      while ((match+i) < stringlen && retvalue == 0) {
        if (word[i] == string[match+i]) {
          /*
           * kdyz se porovnavane rovnaji a algoritmus dosel
           * na konec hledaneho stringu, retvalue se nastavi
           * na zacatek vyskytu
           */
          if (i == (wordlen-1)) {
            retvalue = match+1;
          }
          i++;
        }
        else {
          /*
           * zacatek dalsiho porovnavani se presune na prvni
           * pismeno dalsiho kandidata na vyskyt hledaneho retezce
           */
          match = match+i-table[i];
          /*
           * index prave porovnavaneho znaku se presune na pozici
           * udavanou tabulkou, aby se nemusely porovnavat jiz
           * porovnane znaky
           */

          i = (table[i] > -1)?(table[i]):0;
        }
      }
      /*
       * retezec nebyl nalezen, retvalue bude nastaven na
       * hodnotu -1 znacici, ze retezec nebyl nalezen
       */
      retvalue = (retvalue==0)?(-1):(retvalue);
      free(table);
    }
  }
  return retvalue;
}
