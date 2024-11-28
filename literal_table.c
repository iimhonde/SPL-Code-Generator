// $Id: literal_table.c 2024/11/13$
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "literal_table.h"
#include "utilities.h"

// *** literal table ***
typedef struct literal_table_entry_s 
{
    struct literal_table_entry_s *next; // next entry pointer
    const char *text; // literal text
    word_type value; // literal value
    unsigned int offset; // literal offset
} literal_table_entry_t;

// pointers to first & last entries
static literal_table_entry_t *first;
static literal_table_entry_t *last;
static unsigned int next_word_offset;

// *** iteration state *** 
static bool iterating;
static literal_table_entry_t *iteration_next;

// initialize literal table to empty
void literal_table_initialize()
{
    first = NULL;
    last = NULL;
    next_word_offset = 0;
    literal_table_okay(); // check invariant
    iterating = false;
    iteration_next = NULL;
    literal_table_okay(); // check again
}

// check invariants
void literal_table_okay()
{
    bool emp = literal_table_empty();
    assert(emp == (next_word_offset == 0));
    assert(emp == (first == NULL));
    assert(emp == (last == NULL));
}

// check empty
bool literal_table_empty()
{
    return next_word_offset == 0;
}

// lookup literal in table
unsigned int literal_table_lookup(const char *val_string, word_type value)
{
    // check if already exists
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
        // initialize new entry
	    first = new_entry;
	    last = new_entry;
    } 
    else 
    {
        // append new entry
	    last->next = new_entry;
	    last = new_entry;
    }
    literal_table_okay();
    return ret; 
}

// check if literal present
bool literal_table_present(const char *sought, word_type value)
{
    literal_table_okay();
    return literal_table_find_offset(sought, value) >= 0;    
}

// find offset
int literal_table_find_offset(const char *sought, word_type value)
{
    literal_table_okay();
    literal_table_entry_t *entry = first;

    // traverse table to find literal
    while (entry != NULL) 
    {
	    if (strcmp(entry->text, sought) == 0) 
        {
	        return entry->offset; // found
	    }
	    entry = entry->next;
    }
    return -1; // not found
}

// return num literals
unsigned int literal_table_size()
{
    return next_word_offset;
}

// never full
bool literal_table_full()
{
    return false;
}

// *** iteration state ***

// start iterating over table
void literal_table_start_iteration()
{
    if (iterating) 
    {
	    bail_with_error("Attempt to start literal_table iterating when already iterating!");
    }
    literal_table_okay();
    iterating = true;
    iteration_next = first; // set starting point
}

// check for more entries
bool literal_table_iteration_has_next()
{
    literal_table_okay();
    bool ret = (iteration_next != NULL);
    if (!ret) 
    {
	    iterating = false; // no more entries
    }
    return ret;
}

// get next value
word_type literal_table_iteration_next()
{
    assert(iteration_next != NULL); // check valid state
    float ret = iteration_next->value; // get value
    iteration_next = iteration_next->next; // move to next
    return ret;
}

// end interation
void literal_table_end_iteration()
{
    iterating = false;
}
