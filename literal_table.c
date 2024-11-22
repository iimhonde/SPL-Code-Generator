// $Id: literal_table.c 2024/11/13$
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "literal_table.h"
#include "utilities.h"


typedef struct literal_table_entry_s 
{
    struct literal_table_entry_s *next;
    const char *text;
    word_type value;
    unsigned int offset;
} literal_table_entry_t;

static literal_table_entry_t *first;
static literal_table_entry_t *last;
static unsigned int next_word_offset;

static bool iterating;
static literal_table_entry_t *iteration_next;


unsigned int literal_table_size()
{
    return next_word_offset;
}

bool literal_table_empty()
{
    return next_word_offset == 0;
}

bool literal_table_full()
{
    return false;
}

void literal_table_initialize()
{
    first = NULL;
    last = NULL;
    next_word_offset = 0;
    literal_table_okay();
    iterating = false;
    iteration_next = NULL;
    literal_table_okay();
}

int literal_table_find_offset(const char *sought, word_type value)
{
    literal_table_okay();
    literal_table_entry_t *entry = first;
    while (entry != NULL) 
    {
	    if (strcmp(entry->text, sought) == 0) 
        {
	        return entry->offset;
	    }
	    entry = entry->next;
    }
    return -1;
}

bool literal_table_present(const char *sought, word_type value)
{
    literal_table_okay();
    return literal_table_find_offset(sought, value) >= 0;    
}

unsigned int literal_table_lookup(const char *val_string, word_type value)
{
    int ret = literal_table_find_offset(val_string, value);
    if (ret >= 0) 
    {
	    // don't insert if it's already present
	    return ret;
    }

    // it's not already present, so insert it
    literal_table_entry_t *new_entry = (literal_table_entry_t *)malloc(sizeof(literal_table_entry_t));
    new_entry->text = val_string;
    new_entry->value = value;
    new_entry->next = NULL;
    ret = next_word_offset;
    new_entry->offset = next_word_offset++;

    if (new_entry == NULL) 
    {
	    bail_with_error("No space to allocate new literal table entry!");
    }
    if (first == NULL) 
    {
	    first = new_entry;
	    last = new_entry;
    } 
    else 
    {
	    last->next = new_entry;
	    last = new_entry;
    }
    literal_table_okay();
    return ret; 
}

void literal_table_start_iteration()
{
    if (iterating) 
    {
	    bail_with_error("Attempt to start literal_table iterating when already iterating!");
    }
    literal_table_okay();
    iterating = true;
    iteration_next = first;
}

void literal_table_end_iteration()
{
    iterating = false;
}

bool literal_table_iteration_has_next()
{
    literal_table_okay();
    bool ret = (iteration_next != NULL);
    if (!ret) 
    {
	    iterating = false;
    }
    return ret;
}

word_type literal_table_iteration_next()
{
    assert(iteration_next != NULL);
    float ret = iteration_next->value;
    iteration_next = iteration_next->next;
    return ret;
}
