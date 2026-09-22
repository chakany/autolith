/* Characterize the guest ABI before attempting an SBCL port.
 * Run each mode separately: protection may fault and signals may abort.
 */
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static __thread volatile int thread_value = 11;
static volatile sig_atomic_t signal_seen;

static void
handle_signal(int number)
{
  signal_seen = number;
}

static void *
thread_main(void *argument)
{
  int *result = argument;
  result[0] = thread_value;
  thread_value = 29;
  result[1] = thread_value;
  return NULL;
}

static void
probe_threads(void)
{
  pthread_t thread;
  int child_values[2] = {-1, -1};
  int status = pthread_create(&thread, NULL, thread_main, child_values);
  printf("pthread_create=%d\n", status);
  if (status == 0) {
    status = pthread_join(thread, NULL);
    if (status != 0) {
      printf("pthread_join_failed=%d\n", status);
      exit(1);
    }
    printf("pthread_join=%d child_tls_initial=%d child_tls_after=%d parent_tls=%d\n",
           status, child_values[0], child_values[1], thread_value);
  }
}

static void
probe_signals(void)
{
  struct sigaction action;
  memset(&action, 0, sizeof(action));
  action.sa_handler = handle_signal;
  sigemptyset(&action.sa_mask);
  errno = 0;
  int status = sigaction(SIGUSR1, &action, NULL);
  printf("sigaction=%d errno=%d\n", status, errno);
  if (status == 0) {
    errno = 0;
    status = raise(SIGUSR1);
    printf("raise=%d errno=%d handler_seen=%d\n", status, errno,
           (int)signal_seen);
  }
}

static void
probe_mappings(size_t page_size)
{
  errno = 0;
  void *memory = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANON, -1, 0);
  printf("anonymous_map=%s errno=%d\n",
         memory == MAP_FAILED ? "failed" : "ok", errno);
  if (memory == MAP_FAILED)
    return;
  memset(memory, 0x5a, page_size);

  /* Replace only our own mapping, never an arbitrary fixed address. */
  errno = 0;
  void *fixed = mmap(memory, page_size, PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
  printf("fixed_map=%s errno=%d same_address=%d\n",
         fixed == MAP_FAILED ? "failed" : "ok", errno, fixed == memory);
  munmap(memory, page_size);

#if defined(__x86_64__)
  errno = 0;
  memory = mmap(NULL, page_size, PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_ANON, -1, 0);
  printf("executable_map=%s errno=%d\n",
         memory == MAP_FAILED ? "failed" : "ok", errno);
  if (memory != MAP_FAILED) {
    /* mov $42, %eax; ret, sufficient for an x86-64 instruction-fetch probe. */
    const unsigned char code[] = {0xb8, 42, 0, 0, 0, 0xc3};
    memcpy(memory, code, sizeof(code));
    int (*function)(void) = (int (*)(void))memory;
    printf("generated_code_result=%d\n", function());
    ((unsigned char *)memory)[1] = 43;
    __builtin___clear_cache((char *)memory, (char *)memory + sizeof(code));
    printf("modified_code_result=%d\n", function());
    munmap(memory, page_size);
  }
#endif
}

static void
probe_core_file(size_t page_size)
{
  FILE *file = tmpfile();
  if (file == NULL) {
    printf("core_file_setup_failed errno=%d\n", errno);
    return;
  }
  if (ftruncate(fileno(file), (off_t)page_size) != 0 ||
      fputc(23, file) == EOF || fflush(file) != 0) {
    printf("core_file_setup_failed errno=%d\n", errno);
    fclose(file);
    return;
  }
  errno = 0;
  void *memory = mmap(NULL, page_size, PROT_READ, MAP_PRIVATE, fileno(file), 0);
  printf("readonly_file_map=%s errno=%d\n",
         memory == MAP_FAILED ? "failed" : "ok", errno);
  if (memory != MAP_FAILED) {
    printf("file_content_verified=%d\n", *(unsigned char *)memory == 23);
    munmap(memory, page_size);
  }
  /* SBCL's mutable core spaces need more than a read-only file mapping. */
  errno = 0;
  memory = mmap(NULL, page_size, PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE, fileno(file), 0);
  printf("mutable_core_file_map=%s errno=%d\n",
         memory == MAP_FAILED ? "failed" : "ok", errno);
  if (memory != MAP_FAILED)
    munmap(memory, page_size);
  fclose(file);
}

static int
probe_protection(size_t page_size)
{
  void *memory = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANON, -1, 0);
  if (memory == MAP_FAILED) {
    printf("protection_setup_failed errno=%d\n", errno);
    return 1;
  }
  *(volatile unsigned char *)memory = 1;
  errno = 0;
  int status = mprotect(memory, page_size, PROT_READ);
  printf("mprotect_readonly=%d errno=%d\n", status, errno);
  fflush(stdout);
  if (status != 0) {
    munmap(memory, page_size);
    return 1;
  }
  puts("about_to_write_readonly_page");
  fflush(stdout);
  *(volatile unsigned char *)memory = 2;
  puts("write_to_readonly_page_succeeded");
  munmap(memory, page_size);
  return 0;
}

int
main(int argc, char **argv)
{
  long page_size = sysconf(_SC_PAGESIZE);
  setvbuf(stdout, NULL, _IONBF, 0);
  printf("RUMPLITH_PROBE_BEGIN mode=%s page_size=%ld\n",
         argc > 1 ? argv[1] : "basic", page_size);
  if (page_size <= 0)
    return 1;
  if (argc > 1 && strcmp(argv[1], "protection") == 0)
    return probe_protection((size_t)page_size);
  if (argc > 1 && strcmp(argv[1], "signals") == 0) {
    probe_signals();
    puts("RUMPLITH_PROBE_END");
    return 0;
  }
  if (argc > 1 && strcmp(argv[1], "basic") != 0)
    return 2;
  probe_threads();
  probe_mappings((size_t)page_size);
  probe_core_file((size_t)page_size);
  errno = 0;
  pid_t child = fork();
  if (child == 0)
    _exit(37);
  printf("fork_return=%ld errno=%d\n", (long)child, errno);
  if (child > 0) {
    int child_status = -1;
    errno = 0;
    pid_t waited = waitpid(child, &child_status, 0);
    int verified = waited == child && WIFEXITED(child_status)
                   && WEXITSTATUS(child_status) == 37;
    printf("wait_return=%ld errno=%d child_status=%d child_verified=%d\n",
           (long)waited, errno, child_status, verified);
  }
  puts("RUMPLITH_PROBE_END");
  return 0;
}
