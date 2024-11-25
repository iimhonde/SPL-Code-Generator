// label.h
#ifndef LABEL_H
#define LABEL_H

#include <stdbool.h>
#include "machine_types.h"

typedef struct {
    bool is_set;                 // Indicates if the label has been set
    unsigned int word_offset;    // Offset in words where the label is set
} label;

// Return a fresh label that is not set
extern label *label_create();

// Requires: lab != NULL
// Set the address in the label
extern void label_set(label *lab, unsigned int word_offset);

// Is the given label set?
extern bool label_is_set(label *lab);

// Requires: label_is_set(lab)
// Return the word offset in lab
extern unsigned int label_read(label *lab);

#endif // LABEL_H
