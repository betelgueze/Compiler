/*
	Interpret jazyka IFJ2011
	Autori:
		Michal Risa (xrisam01)
		Josef Rudolf (xrudol04)
*/

#ifndef __IAL__H
#define __IAL__H

#include <stdlib.h>

/*
        Hashovacia tabulka
        Autor: xrisam01
                (ht_hash: djb2 hash, zdroj: http://www.cse.yorku.ca/~oz/hash.html)
                (list_entry: From Linux Kernel (include/linux/list.h))
*/

/**
 * list_entry - get the struct for this entry
 * @ptr:        the &struct list_head pointer.
 * @type:       the type of the struct this is embedded in.
 * @member:     the name of the list_struct within the struct.
 */
#define list_entry(ptr, type, member) \
        ((type *)((char *)(ptr)-(char *)(&((type *)0)->member)))
        
struct htable_entry_t {
	struct htable_entry_t *next;
	const char *key; /* vyhladavaci kluc */
	size_t size; /* velkost dat */
	char data[]; /* uzivatelove data */
};

struct htable_t {
	unsigned long size; /* velkost tabulky */
	struct htable_entry_t *table[]; /* tabulka */
};

struct htable_iter_t {
	struct htable_t *ht;
	unsigned long idx;
	struct htable_entry_t *entry;
};

/*
ht_hash: djb2 hash, zdroj: http://www.cse.yorku.ca/~oz/hash.html
*/
static inline unsigned long ht_hash(const unsigned char *str)
{
	unsigned long hash = 5381LU;
	int c;
	while((c = *str++) != '\0')
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
	return hash;
}

/*
HT_INIT_STATIC: staticky alokuje inicializovanu tabulku
	@name: nazov tabulky
	@size: velkost tabulky
	#return: void

	Vystraha:
		1.)	Parametre vlozene do HT_INIT_STATIC su NEKONTROLOVANE. Preto je
			nevyhnutne zadat SPRAVNE parametre.

		2.)	Ucinok makra je ekvivalentny nasledovnemu:
			HT_INIT_STATIC(mytable, 128); <=>
			struct htable_t *mytable;
			kde mytable je pointer na inicializovanu tabulku o 128 prvkoch.
*/
#define HT_INIT_STATIC(name,table_size) \
	union ht_ht_##name { /* */ \
		struct htable_t ht; /* hlavicka tabulky */ \
		char table[sizeof(struct htable_t) + table_size * sizeof(struct htable_entry_t*)]; /* tabulka - pole pointerov */ \
	}ht_##name = {.ht = {table_size}}; \
	struct htable_t *name = &ht_##name.ht /* vysledny pointer na staticku tabulku */

/*
ht_init: Dynamicky alokuje tabulku
	@size: velkost tabulky
	#return: NULL = nedostatok pamete, inak pointer na inicializovanu tabulku
*/
struct htable_t *ht_init(unsigned long size);

/*
ht_free: Zmaze obsah a uvolni dynamicky alokovanu tabulku
	@ht: tabulka
	#return: void

	Vystaha:
		Nevolat na staticku tabulku!!!
*/
void ht_free(struct htable_t *ht);

/*
ht_insert: Vlozi prvok do tabulky
	@ht: cielova tabulka
	@key: vyhladavaci kluc
	@data: data na vlozenie
	@size: velkost dat na vlozenie
	#return: 0 = OK, -1 = !malloc

	Vystraha:

	1.)	Key musi byt dynamicky alokovany. Nevytvara vlastnu
		kopiu obsahu kluca, preto tento kluc po vlozeni do tabulky
		neuvolnovat! Tabulka si ho uvolni sama prostrednictvom ht_clear.
	2.)	Nekontroluje duplicitne vkladanie dat, resp. zhodu vyhladavacich
		klucov! Na kontrou duplicity pouzit ht_lookup.

*/
void *ht_insert(struct htable_t *ht, const char *key, void *data, size_t size);

/*
ht_lookup: Vyhlada prvok v hashovacej tabulke
	@ht: prehladavana tabulka
	@key: vyladavacai kluc
	#return: NULL = data nenajdene, inak pointer na data.

	Vystraha:

	1.)	Vracia pointer na vlastnu pokiu dat, preto sa nesmu uvolnovat!
		Tabulka ich uvolni pri volani ht_clear sama.
*/
void *ht_lookup(struct htable_t *ht, const char *key);

/*
ht_clear: Uvolni OBSAH tabulky (nie celu tabulku)
	@ht: tabulka na uvolnenie obsahu
	#return: void
*/
void ht_clear(struct htable_t *ht);

/*
ht_copy: Kopia hashovacej tabulky vratane obsahu
	@src: zdrojova tabulka
	#return: nova tabulka, NULL pri nedostatku pamete

	Vystraha: Nekopiruje hashovacie kluce (entry->key) !!
*/
struct htable_t *ht_copy(struct htable_t *src); /* ZATIAL NEOTESTOVANE!!! */

/*
ht_begin: Ziska iterator na prvu polozku v tabulke
	@ht: tabulka
	#return: iterator
*/
struct htable_iter_t ht_begin(struct htable_t *ht);

/*
ht_iter_next: Ziska iterator na nasledujucu polozku v tabulke
	@iter: momentalny iterator
	#return: iterator na dalsiu polozku
*/
struct htable_iter_t ht_iter_next(struct htable_iter_t iter);

/*
ht_iter_empty: Zistenie prazdnosti iteratora
	@iter: iterator
	#return: 1 = prazdny, 0 = pouzity
*/
static inline int ht_iter_empty(struct htable_iter_t iter)
{
	return iter.entry == NULL;
}

/*
ht_iter_deref: Dereferencia iteratora
	@iter: iterator
	#return: pointer na ulozene data v polozke
*/
static inline void *ht_iter_deref(struct htable_iter_t iter)
{
	assert(iter.entry != NULL);
	return iter.entry->data;
}

/*
 * Heap sort
 * Autor: xrudol04
 */

/*
 * Funkce heapsort. Na vstupu ocekava ukazatel na string, ktery seradi.
 * Funkce nic nevraci, radi na miste.
 */
void heap_sort(char *string);

/*
 * Vyhledavaci Knuth-Morris-Pratt algoritmus
 * Autor: xrudol04
 */

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
int kmp_find(char *string, char *word);

#endif

