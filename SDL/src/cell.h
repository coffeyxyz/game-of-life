#ifndef CELL_H_
#define CELL_H_

#include "crl/single_link.h"

/* Struct representing a cell. */
typedef struct cell {
    int live;           // Is the cell alive
    int next;           // Should the cell be alive next turn
    sl_node_t *nb_head; // Head of the list of neighbouring cells
    sl_node_t *nb_tail; // Tail of the list of neighbouring cells
} cell_t;

void cell_add_neighbour(cell_t* target, cell_t *neighbour);
void cell_check(cell_t *target);
void cell_update(cell_t *target);

#endif
