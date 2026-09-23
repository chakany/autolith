/* Compare NetBSD timecounter clocks with the full-width bare-metal clock
 * while the single application thread does not yield to the scheduler. */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <bmk-core/platform.h>

int main(void)
{
    struct timespec previous, current;
    struct timeval wall_previous, wall_current;
    assert(!clock_gettime(CLOCK_MONOTONIC, &previous));
    assert(!gettimeofday(&wall_previous, NULL));
    int64_t start = bmk_platform_cpu_clock_monotonic(), last = start;
    unsigned monotonic_reversals = 0, wall_reversals = 0;
    do {
        int64_t now = bmk_platform_cpu_clock_monotonic();
        assert(now >= last);
        last = now;
        assert(!clock_gettime(CLOCK_MONOTONIC, &current));
        assert(!gettimeofday(&wall_current, NULL));
        if (current.tv_sec < previous.tv_sec ||
            (current.tv_sec == previous.tv_sec && current.tv_nsec < previous.tv_nsec))
            ++monotonic_reversals;
        if (wall_current.tv_sec < wall_previous.tv_sec ||
            (wall_current.tv_sec == wall_previous.tv_sec && wall_current.tv_usec < wall_previous.tv_usec))
            ++wall_reversals;
        previous = current;
        wall_previous = wall_current;
    } while (last - start < INT64_C(12000000000));
    printf("CLOCK-PROBE: monotonic-reversals=%u wall-reversals=%u bmk-elapsed=%lld\n",
           monotonic_reversals, wall_reversals, (long long)(last - start));
#ifdef REQUIRE_MONOTONIC
    assert(!monotonic_reversals && !wall_reversals);
#endif
    return 0;
}
