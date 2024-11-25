// label.c
#include "label.h"
#include <stdlib.h>
#include <assert.h>

// Return a fresh label that is not set
label *label_create() {
    label *new_label = (label *)malloc(sizeof(label));
    if (new_label == NULL) {
        exit(EXIT_FAILURE);  // Handle memory allocation failure
    }
    new_label->is_set = false;
    new_label->word_offset = 0;
    return new_label;
}

// Set the address in the label
void label_set(label *lab, unsigned int word_offset) {
    assert(lab != NULL);
    lab->is_set = true;
    lab->word_offset = word_offset;
}

// Is the given label set?
bool label_is_set(const label *lab) {
    assert(lab != NULL);
    return lab->is_set;
}

// Return the word offset in lab
unsigned int label_read(const label *lab) {
    assert(lab != NULL && lab->is_set);
    return lab->word_offset;
}
