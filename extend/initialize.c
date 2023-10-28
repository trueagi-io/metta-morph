#include <chicken.h>
#include <assert.h>

void CHICKEN_INIT(void)
{
	CHICKEN_run(C_toplevel);
}
