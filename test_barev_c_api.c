// test_barev_capi.c
// build: gcc -O2 -Wall test_barev_capi.c -L. -lbarev_capi -Wl,-rpath,'$ORIGIN' -o test_barev
// run:   ./test_barev inky fd00::1234 5555 bob fd00::abcd 5555 "hello from C"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif

// prototypes

typedef void* BarevClient;
typedef void* BarevBuddy;

typedef void (*barev_log_cb)(const char* level, const char* msg, void* userdata);
typedef void (*barev_typing_cb)(BarevBuddy buddy, int isTyping, void* userdata);
typedef void (*barev_buddy_status_cb)(BarevBuddy buddy, int oldStatus, int newStatus, void* userdata);
typedef void (*barev_message_cb)(BarevBuddy buddy, const char* msg, void* userdata);
typedef void (*barev_conn_state_cb)(BarevBuddy buddy, int state, void* userdata);

void  barev_strfree(char* p);

BarevClient barev_client_new(const char* nick, const char* myipv6, uint16_t port);
void        barev_client_free(BarevClient c);
int         barev_client_start(BarevClient c);
void        barev_client_stop(BarevClient c);
void        barev_client_process(BarevClient c);
char*       barev_client_myjid(BarevClient c);

BarevBuddy  barev_client_add_buddy(BarevClient c, const char* buddynick, const char* buddyipv6, uint16_t port);
int         barev_client_remove_buddy(BarevClient c, const char* buddyjid);
BarevBuddy  barev_client_find_buddy(BarevClient c, const char* buddyjid);

int         barev_client_connect(BarevClient c, const char* buddyjid);
int         barev_client_send_message(BarevClient c, const char* buddyjid, const char* msg);
int         barev_client_send_presence(BarevClient c, int status, const char* statusMsg);
int         barev_client_send_typing(BarevClient c, const char* buddyjid);
int         barev_client_send_paused(BarevClient c, const char* buddyjid);

void        barev_set_userdata(BarevClient c, void* userdata);
void        barev_set_log_cb(BarevClient c, barev_log_cb cb);
void        barev_set_typing_cb(BarevClient c, barev_typing_cb cb);
void        barev_set_buddy_status_cb(BarevClient c, barev_buddy_status_cb cb);
void        barev_set_message_cb(BarevClient c, barev_message_cb cb);
void        barev_set_conn_state_cb(BarevClient c, barev_conn_state_cb cb);

char*       barev_buddy_get_jid(BarevBuddy b);
char*       barev_buddy_get_nick(BarevBuddy b);
char*       barev_buddy_get_ipv6(BarevBuddy b);
uint16_t    barev_buddy_get_port(BarevBuddy b);
int         barev_buddy_get_status(BarevBuddy b);
char*       barev_buddy_get_status_message(BarevBuddy b);

#ifdef __cplusplus
}
#endif

// end of prototypes

struct Ctx {
  volatile int last_state;
  volatile int saw_online;
};

static void on_log(const char* level, const char* msg, void* userdata) {
  (void)userdata;
  fprintf(stderr, "[%s] %s\n", level ? level : "?", msg ? msg : "");
}

static void on_conn_state(BarevBuddy buddy, int state, void* userdata) {
  struct Ctx* ctx = (struct Ctx*)userdata;
  ctx->last_state = state;

  char* jid = barev_buddy_get_jid(buddy);
  fprintf(stderr, "[conn] buddy=%s state=%d\n", jid ? jid : "(null)", state);
  if (jid) barev_strfree(jid);

  // treat state >= 3 as "ready" (authenticated/online-ish).
  if (state >= 3) ctx->saw_online = 1;
}

static void on_message(BarevBuddy buddy, const char* msg, void* userdata) {
  (void)userdata;
  char* jid = barev_buddy_get_jid(buddy);
  fprintf(stderr, "[msg] from=%s text=%s\n", jid ? jid : "(null)", msg ? msg : "");
  if (jid) barev_strfree(jid);
}

int main(int argc, char** argv) {
  if (argc < 8) {
    fprintf(stderr,
      "usage:\n"
      "  %s <mynick> <myipv6> <myport> <buddynick> <buddyipv6> <buddyport> <message>\n"
      "example:\n"
      "  %s inky fd00::1234 5555 bob fd00::abcd 5555 \"hello\"\n",
      argv[0], argv[0]);
    return 2;
  }

  const char* mynick = argv[1];
  const char* myip   = argv[2];
  uint16_t myport    = (uint16_t)atoi(argv[3]);

  const char* buddynick = argv[4];
  const char* buddyip   = argv[5];
  uint16_t buddyport    = (uint16_t)atoi(argv[6]);

  const char* text = argv[7];

  struct Ctx ctx;
  memset(&ctx, 0, sizeof(ctx));

  BarevClient c = barev_client_new(mynick, myip, myport);
  if (!c) {
    fprintf(stderr, "barev_client_new failed\n");
    return 1;
  }

  barev_set_userdata(c, &ctx);
  barev_set_log_cb(c, on_log);
  barev_set_conn_state_cb(c, on_conn_state);
  barev_set_message_cb(c, on_message);

  int ok = barev_client_start(c);
  fprintf(stderr, "start=%d\n", ok);
  if (!ok) {
    fprintf(stderr, "start failed\n");
    barev_client_free(c);
    return 1;
  }

  // announce presence
  // status=1 is a guess for "available" (depends on TBuddyStatus order)
  barev_client_send_presence(c, 1, "hi");

  // run several times after start for protocol to do initial work
  for (int i = 0; i < 50; i++) {
    barev_client_process(c);
    usleep(20 * 1000);
  }

  BarevBuddy b = barev_client_add_buddy(c, buddynick, buddyip, buddyport);
  if (!b) {
    fprintf(stderr, "add_buddy failed\n");
    barev_client_free(c);
    return 1;
  }

  char* buddyjid = barev_buddy_get_jid(b);
  if (!buddyjid) {
    fprintf(stderr, "buddy has no jid?\n");
    barev_client_free(c);
    return 1;
  }
  fprintf(stderr, "buddy jid=%s\n", buddyjid);

  ok = barev_client_connect(c, buddyjid);
  fprintf(stderr, "connect=%d\n", ok);

  // wait until connection is ready
  ctx.saw_online = 0;
  for (int i = 0; i < 500; i++) { // ~10 seconds at 20ms
    barev_client_process(c);
    if (ctx.saw_online) break;
    usleep(20 * 1000);
  }

  fprintf(stderr, "ready=%d last_state=%d\n", ctx.saw_online, ctx.last_state);

  ok = barev_client_send_message(c, buddyjid, text);
  fprintf(stderr, "send_message=%d\n", ok);

  // keep repeating so it actually transmits
  for (int i = 0; i < 200; i++) {
    barev_client_process(c);
    usleep(20 * 1000);
  }

  barev_strfree(buddyjid);
  barev_client_free(c);
  return 0;
}
