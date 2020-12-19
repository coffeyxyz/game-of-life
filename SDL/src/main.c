/* --- game-of-life/SDL ---
 *
 * Conway's Game of Life implemented using the SDL library.
 *
 * Copyright (C) 2020 Robert Coffey
 * Released under the GPLv2 license */

#include <stdlib.h>

#define CRL_DEFINE
#include "crl/crl_list.h"

#include "screen.h"

#define INITIAL_MONITOR 1

int main(void)
{
    if (screen_init(INITIAL_MONITOR))
        return EXIT_FAILURE;

    int quit = 0;
    while (!quit) {
        if (screen_step())
            quit = 1;
    }

    screen_destroy();

    return EXIT_SUCCESS;
}
