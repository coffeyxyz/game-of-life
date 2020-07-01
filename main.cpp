#include <vector>

#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"

#define SHEIGHT	100
#define	SWIDTH	100
#define	PHEIGHT	5
#define	PWIDTH	5
#define PCOUNT SHEIGHT * SWIDTH

int get_neighbors(std::array<int, PCOUNT> arr, int index)
{
	int count = 0;

	/* Top - Right - Bottom - Left */
	count += (index < SWIDTH) ? 0 : arr[index - SWIDTH];
	count += (index%SWIDTH < SWIDTH-1) ? arr[index + 1] : 0;
	count += (index < SWIDTH * (SHEIGHT-1)) ? arr[index + SWIDTH] : 0;
	count += (index%SWIDTH > 0) ? arr[index - 1] : 0;

	/* TR - BR - BL - TL */
	count += (index >= SWIDTH && index%SWIDTH < SWIDTH-1) 
		? arr[index - SWIDTH + 1] : 0;
	count += (index < SWIDTH * (SHEIGHT-1) && index%SWIDTH < SWIDTH-1)
		? arr[index + SWIDTH + 1] : 0;
	count += (index < SWIDTH * (SHEIGHT-1) && index%SWIDTH > 0)
		? arr[index + SWIDTH - 1]: 0;
	count += (index >= SWIDTH && index%SWIDTH < SWIDTH-1)
		? arr[index - SWIDTH -1]: 0;

	return count;
}

class GoL : public olc::PixelGameEngine
{
private:
	float timer;
	std::array<int, PCOUNT> main_arr;
	std::array<int, PCOUNT> snap_arr;

public:
	GoL()
	{
		sAppName = "Jon Conway's: Game of Life";
	}

public:
	bool OnUserCreate() override
	{
		for (int i = 0; i < PCOUNT; ++i)
		{
			main_arr[i] = 0;
			snap_arr[i] = 0;
		}

		// load starting grid
		for (int i=0; i<SWIDTH; ++i)
			main_arr[i] = 1;

		timer = 0;

		return true;
	}

	bool OnUserUpdate(float fElapsedTime) override
	{
		// if space not pressed: continue
		if (!GetKey(olc::Key::SPACE).bHeld)
			return true;

		if ((timer += fElapsedTime) < 0.2f)
			return true;
		timer = 0;

		snap_arr = main_arr;
		for (int i = 0; i < PCOUNT; ++i)
		{
			int n_count = get_neighbors(snap_arr, i);

			if (n_count < 2)
				main_arr[i] = 0;
			else if (n_count > 3)
				main_arr[i] = 0;
			else if (n_count == 3)
				main_arr[i] = 1;
		}

		for (int i = 0; i < PCOUNT; ++i)
		{
			Draw(i%SWIDTH, i/SWIDTH, main_arr[i] ? olc::WHITE : olc::BLACK);
		}

		return true;
	}

	bool OnUserDestroy() override
	{
		return true;
	}
};

int main()
{
	GoL game;
	if (game.Construct(SWIDTH, SHEIGHT, PWIDTH, PHEIGHT))
		game.Start();

	return 0;
}
