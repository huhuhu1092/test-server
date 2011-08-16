#include "random.h"
GRand* random_generator = NULL;
#define G_GINT64_CONSTANT(val) (val##L)
#define N 624
#define M 397
#define MATRIX_A 0x9908b0df   /* constant vector a */
#define UPPER_MASK 0x80000000 /* most significant w-r bits */
#define LOWER_MASK 0x7fffffff /* least significant r bits */

/* Tempering parameters */   
#define TEMPERING_MASK_B 0x9d2c5680
#define TEMPERING_MASK_C 0xefc60000
#define TEMPERING_SHIFT_U(y)  (y >> 11)
#define TEMPERING_SHIFT_S(y)  (y << 7)
#define TEMPERING_SHIFT_T(y)  (y << 15)
#define TEMPERING_SHIFT_L(y)  (y >> 18)
struct _GRand
{
  guint32 mt[N]; /* the array for the state vector  */
  guint mti; 
};
typedef struct _GTimeVal                GTimeVal;

struct _GTimeVal
{
  glong tv_sec;
  glong tv_usec;
};

void g_get_current_time (GTimeVal *result)
{
#ifndef WIN32
  struct timeval r;
  if(result == NULL)
      return;
  /*this is required on alpha, there the timeval structs are int's
    not longs and a cast only would fail horribly*/
  gettimeofday (&r, NULL);
  result->tv_sec = r.tv_sec;
  result->tv_usec = r.tv_usec;
#else
  FILETIME ft;
  guint64 time64;
  if(result == NULL)
      return;

  GetSystemTimeAsFileTime (&ft);
  memmove (&time64, &ft, sizeof (FILETIME));

  /* Convert from 100s of nanoseconds since 1601-01-01
   * to Unix epoch. Yes, this is Y2038 unsafe.
   */
  time64 -= G_GINT64_CONSTANT (116444736000000000);
  time64 /= 10;

  result->tv_sec = time64 / 1000000;
  result->tv_usec = time64 % 1000000;
#endif
}
static guint get_random_version (void)
{
  return 22;
}
void g_rand_set_seed (GRand* rand, guint32 seed)
{
  if(rand == NULL)
      return;

  switch (get_random_version ())
    {
    case 20:
      /* setting initial seeds to mt[N] using         */
      /* the generator Line 25 of Table 1 in          */
      /* [KNUTH 1981, The Art of Computer Programming */
      /*    Vol. 2 (2nd Ed.), pp102]                  */
      
      if (seed == 0) /* This would make the PRNG procude only zeros */
	     seed = 0x6b842128; /* Just set it to another number */
      
      rand->mt[0]= seed;
      for (rand->mti=1; rand->mti<N; rand->mti++)
	rand->mt[rand->mti] = (69069 * rand->mt[rand->mti-1]);
      
      break;
    case 22:
      /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
      /* In the previous version (see above), MSBs of the    */
      /* seed affect only MSBs of the array mt[].            */
      
      rand->mt[0]= seed;
      for (rand->mti=1; rand->mti<N; rand->mti++)
	rand->mt[rand->mti] = 1812433253UL * 
	  (rand->mt[rand->mti-1] ^ (rand->mt[rand->mti-1] >> 30)) + rand->mti; 
      break;
    default:
		break;
    }
}
void g_rand_set_seed_array (GRand* rand, const guint32 *seed, guint seed_length)
{
  int i, j, k;
  if(rand == NULL)
      return;
  if(seed_length < 1)
      return;

  g_rand_set_seed (rand, 19650218UL);

  i=1; j=0;
  k = (N>seed_length ? N : seed_length);
  for (; k; k--)
    {
      rand->mt[i] = (rand->mt[i] ^
		     ((rand->mt[i-1] ^ (rand->mt[i-1] >> 30)) * 1664525UL))
	      + seed[j] + j; /* non linear */
      rand->mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
      i++; j++;
      if (i>=N)
        {
	  rand->mt[0] = rand->mt[N-1];
	  i=1;
	}
      if (j>=seed_length)
	j=0;
    }
  for (k=N-1; k; k--)
    {
      rand->mt[i] = (rand->mt[i] ^
		     ((rand->mt[i-1] ^ (rand->mt[i-1] >> 30)) * 1566083941UL))
	      - i; /* non linear */
      rand->mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
      i++;
      if (i>=N)
        {
	  rand->mt[0] = rand->mt[N-1];
	  i=1;
	}
    }

  rand->mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
}
GRand* g_rand_new_with_seed_array (const guint32 *seed, guint seed_length)
{
  GRand *rand = (GRand*)g_malloc(sizeof(GRand));
  g_rand_set_seed_array (rand, seed, seed_length);
  return rand;
}
GRand* g_rand_new()
{
  guint32 seed[4];
  GTimeVal now;
  static gboolean dev_urandom_exists = FALSE;

  if (!dev_urandom_exists)
    {  
      g_get_current_time (&now);
      seed[0] = now.tv_sec;
      seed[1] = now.tv_usec;
      seed[2] = getpid ();
      seed[3] = 0;
    }

  return g_rand_new_with_seed_array (seed, 4);
}
void g_rand_free(GRand* rand)
{
  if(rand == NULL)
      return;
  g_free (rand);
}
guint32 g_rand_int(GRand* rand)
{
  guint32 y;
  static const guint32 mag01[2]={0x0, MATRIX_A};
  /* mag01[x] = x * MATRIX_A  for x=0,1 */

  if(rand == NULL)
      return 0;

  if (rand->mti >= N) { /* generate N words at one time */
    int kk;
    
    for (kk=0;kk<N-M;kk++) {
      y = (rand->mt[kk]&UPPER_MASK)|(rand->mt[kk+1]&LOWER_MASK);
      rand->mt[kk] = rand->mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
    }
    for (;kk<N-1;kk++) {
      y = (rand->mt[kk]&UPPER_MASK)|(rand->mt[kk+1]&LOWER_MASK);
      rand->mt[kk] = rand->mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
    }
    y = (rand->mt[N-1]&UPPER_MASK)|(rand->mt[0]&LOWER_MASK);
    rand->mt[N-1] = rand->mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];
    
    rand->mti = 0;
  }
  
  y = rand->mt[rand->mti++];
  y ^= TEMPERING_SHIFT_U(y);
  y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
  y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
  y ^= TEMPERING_SHIFT_L(y);
  
  return y; 
}
#define G_RAND_DOUBLE_TRANSFORM 2.3283064365386962890625e-10
gint32 g_rand_int_range(GRand* rand, gint32 begin, gint32 end)
{
  guint32 dist = end - begin;
  guint32 random;

  if(rand == NULL)
      return begin;
  if(end <= begin)
      return begin;
  switch (get_random_version ())
    {
    case 20:
      if (dist <= 0x10000L) /* 2^16 */
	{
	  /* This method, which only calls g_rand_int once is only good
	   * for (end - begin) <= 2^16, because we only have 32 bits set
	   * from the one call to g_rand_int (). */
	  
	  /* we are using (trans + trans * trans), because g_rand_int only
	   * covers [0..2^32-1] and thus g_rand_int * trans only covers
	   * [0..1-2^-32], but the biggest double < 1 is 1-2^-52. 
	   */
	  
	  gdouble double_rand = g_rand_int (rand) * 
	    (G_RAND_DOUBLE_TRANSFORM +
	     G_RAND_DOUBLE_TRANSFORM * G_RAND_DOUBLE_TRANSFORM);
	  
	  random = (gint32) (double_rand * dist);
	}
      else
	{
	  /* Now we use g_rand_double_range (), which will set 52 bits for
	     us, so that it is safe to round and still get a decent
	     distribution */
	  random = (gint32) g_rand_double_range (rand, 0, dist);
	}
      break;
    case 22:
      if (dist == 0)
	random = 0;
      else 
	{
	  /* maxvalue is set to the predecessor of the greatest
	   * multiple of dist less or equal 2^32. */
	  guint32 maxvalue;
	  if (dist <= 0x80000000u) /* 2^31 */
	    {
	      /* maxvalue = 2^32 - 1 - (2^32 % dist) */
	      guint32 leftover = (0x80000000u % dist) * 2;
	      if (leftover >= dist) leftover -= dist;
	      maxvalue = 0xffffffffu - leftover;
	    }
	  else
	    maxvalue = dist - 1;
	  
	  do
	    random = g_rand_int (rand);
	  while (random > maxvalue);
	  
	  random %= dist;
	}
      break;
    default:
      random = 0;		/* Quiet GCC */
    }      
 
  return begin + random;
}
gdouble g_rand_double(GRand   *rand)
{
  /* We set all 52 bits after the point for this, not only the first
     32. Thats why we need two calls to g_rand_int */
  gdouble retval = g_rand_int (rand) * G_RAND_DOUBLE_TRANSFORM;
  retval = (retval + g_rand_int (rand)) * G_RAND_DOUBLE_TRANSFORM;

  /* The following might happen due to very bad rounding luck, but
   * actually this should be more than rare, we just try again then */
  if (retval >= 1.0) 
    return g_rand_double (rand);

  return retval;    
}
gdouble g_rand_double_range(GRand   *rand, gdouble  begin, gdouble  end)
{
  gdouble r;

  r = g_rand_double (rand);

  return r * end - (r - 1) * begin;
}
