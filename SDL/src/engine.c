// Copyright (C) 2020 Robert Coffey
// Released under the GPLv2 license

#include <string.h>

#include "cell.h"
#include "engine.h"

static cell_t cell_arr[CELL_COUNT];

void engine_setup(void)
{
    /* Add neighbours to each cell */
    for (int i = 0, ni; i < CELL_COUNT; ++i) {
        cell_t *targ = &cell_arr[i]; // Target cell

        /* Cell above */
        if (i >= CELL_ARRAY_WIDTH) {
            ni = i - CELL_ARRAY_WIDTH;
            cell_add_neighbour(targ, &cell_arr[ni]);
        }

        /* Cell below */
        else if (i < CELL_ARRAY_WIDTH * (CELL_ARRAY_HEIGHT-1)) {
            ni = i + CELL_ARRAY_WIDTH;
            cell_add_neighbour(targ, &cell_arr[ni]);
        }

        /* Column to the left */
        else if (i%CELL_ARRAY_WIDTH > 0) {
            /* Cell above to the left */
            if (i >= CELL_ARRAY_WIDTH) {
                ni = i - CELL_ARRAY_WIDTH - 1;
                cell_add_neighbour(targ, &cell_arr[ni]);
            }

            /* Cell directly to the left */
            ni = i - 1;
            cell_add_neighbour(targ, &cell_arr[ni]);

            /* Cell below to the left */
            if (i < CELL_ARRAY_WIDTH * (CELL_ARRAY_HEIGHT-1)) {
                ni = i + CELL_ARRAY_WIDTH - 1;
                cell_add_neighbour(targ, &cell_arr[ni]);
            }
        }

        /* Column to the right */
        else if (i%CELL_ARRAY_WIDTH < CELL_ARRAY_WIDTH-1) {
            /* Cell above to the right */
            if (i >= CELL_ARRAY_WIDTH) {
                ni = i - CELL_ARRAY_WIDTH + 1;
                cell_add_neighbour(targ, &cell_arr[ni]);
            }

            /* Cell directly to the right */
            ni = i + 1;
            cell_add_neighbour(targ, &cell_arr[ni]);

            /* Cell below to the right */
            if (i < CELL_ARRAY_WIDTH * (CELL_ARRAY_HEIGHT-1)) {
                ni = i + CELL_ARRAY_WIDTH + 1;
                cell_add_neighbour(targ, &cell_arr[ni]);
            }
        }
    }
}

void engine_step(void)
{

}
