#ifndef RANDOM_H
#define RANDOM_H
#include "type.h"
typedef struct _GRand GRand;
extern GRand* random_generator;
extern GRand* g_rand_new();
extern void g_rand_free(GRand* rand);
extern guint32 g_rand_int(GRand* rand);
extern gint32 g_rand_int_range(GRand* rand, gint32 begin, gint32 end);
extern gdouble g_rand_double(GRand   *rand);
extern gdouble g_rand_double_range(GRand   *rand_, gdouble  begin, gdouble  end);
#endif
