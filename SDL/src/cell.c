// Copyright (C) 2020 Robert Coffey
// Released under the GPLv2 license

#include "crl/crl_list.h"

#include "cell.h"

static int get_living_neighbours(cell_t *cell);

void cell_add_neighbour(cell_t *target, cell_t *neighbour)
{
    sl_node_t *new_node = sl_create_node();
    new_node->val = neighbour;

    sl_add_node(&target->nb_head, &target->nb_tail, new_node);
}

void cell_check(cell_t *target)
{
    // TODO: Implement Game of Life cell rules
    target->next = get_living_neighbours(target);
}

void cell_update(cell_t *target)
{
    target->live = target->next;
}

static int get_living_neighbours(cell_t *target)
{
    int count = 0;
    for (sl_node_t *walk = target->nb_head; walk; walk = walk->next)
        if (((cell_t *)walk->val)->live)
            ++count;
    return count;
}
