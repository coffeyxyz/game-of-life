#ifndef ENGINE_H_
#define ENGINE_H_

#define CELL_ARRAY_WIDTH  640
#define CELL_ARRAY_HEIGHT 480
#define CELL_COUNT (CELL_ARRAY_WIDTH * CELL_ARRAY_HEIGHT)

void engine_setup(void);
void engine_step(void);

#endif
