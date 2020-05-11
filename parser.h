/*
	Interpret jazyka IFJ2011
	Autori:
		Martin Risa (xrisam00)
		Michal Risa (xrisam01)
*/

#ifndef __PARSER__H
#define __PARSER__H

int parse(void);

/**
 * structure for any function
 * */
typedef struct Function{
	char * ID;
	tList Parameters;//typu char * (ID)
	tList SymbolTable;//list of table containing information about data
	tList Instruction;//list of instructions
}TFunction;

typedef struct op_info{
  int prio;
  int assoc;
  int args;
}top_info;

void free_token(struct token_t token);
void free_function_info(struct Function *func_info);
char * parse_expression(int *);

void print_stack(struct Function *func);

#endif /* have __PARSER__H */


