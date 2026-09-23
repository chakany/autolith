/* SBCL elapsed/wall clocks use the full-width rumprun clock. The kernel's
 * 32-bit nanosecond timecounter can reverse without scheduler ticks. */
#include <time.h>
#include <sys/time.h>
#include <bmk-core/platform.h>

extern int __real___clock_gettime50(clockid_t, struct timespec *);
int __wrap___clock_gettime50(clockid_t clock, struct timespec *value)
{
    if (clock != CLOCK_MONOTONIC && clock != CLOCK_REALTIME)
        return __real___clock_gettime50(clock, value);
    bmk_time_t time = bmk_platform_cpu_clock_monotonic();
    if (clock == CLOCK_REALTIME)
        time += bmk_platform_cpu_clock_epochoffset();
    value->tv_sec = time / 1000000000;
    value->tv_nsec = time % 1000000000;
    return 0;
}

int __wrap___gettimeofday50(struct timeval *value, void *zone)
{
    (void)zone;
    if (value) {
        struct timespec time;
        __wrap___clock_gettime50(CLOCK_REALTIME, &time);
        value->tv_sec = time.tv_sec;
        value->tv_usec = time.tv_nsec / 1000;
    }
    return 0;
}
