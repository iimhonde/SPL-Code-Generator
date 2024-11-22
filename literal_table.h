// $Id: literal_table.h 2024/11/13$
#ifndef _LITERAL_TABLE_H
#define _LITERAL_TABLE_H
#include <stdbool.h>
#include "machine_types.h"


extern unsigned int literal_table_size();

extern bool literal_table_empty();

extern bool literal_table_full();

extern void literal_table_initialize();

extern int literal_table_find_offset(const char *sought, word_type value);

extern bool literal_table_present(const char *sought, word_type value);

extern unsigned int literal_table_lookup(const char *val_string, word_type value);

// iteration helpers

extern void literal_table_start_iteration();

extern void literal_table_end_iteration();

extern bool literal_table_iteration_has_next();

extern word_type literal_table_iteration_next();

#endif
