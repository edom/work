// cc -std=c99 -Wall -Werror -o tcp tcp.c

#define _BSD_SOURCE // for inet_aton

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>

int main (int argc, char* argv[]) {

    if (argc != 5) {
        printf("Usage: %s ip_address port report_period max_count\n", argv[0]);
        goto end;
    }

    const char* s_addr = argv[1];
    const char* s_port = argv[2];
    const int report_period = atoi(argv[3]);
    if (report_period <= 0) { printf("report_period must be positive; given %d\n", report_period); goto end; }
    const int max_count = atoi(argv[4]);

    const int port = atoi(s_port);

    char s_target[32];
    (void)snprintf(s_target, sizeof(s_target), "%s:%d", s_addr, port);

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    if (inet_aton(s_addr, &(addr.sin_addr)) == 0) { perror("inet_aton"); goto end; }

    int count[2] = {0}; // ok, fail

    // If you encounter the "Cannot assign requested address" error, you have these options:
    // - Reduce the max_count program argument.
    // - Assign more IP addresses to the machine running this program.
    // Don't fight TIME_WAIT.
    // The proper solution is to assign more IP addresses.
    // See https://vincent.bernat.ch/en/blog/2014-tcp-time-wait-state-linux

    for (int i = 0; i < max_count; ++i) {

        if (i % report_period == 0) {
            const int total = count[0] + count[1];
            printf("%s %d/%d ok %d error %d total %d\n", s_target, i, max_count, count[0], count[1], total);
        }

        int has_error = 0;

        int sfd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (sfd == -1) { perror("socket"); has_error = 1; goto next; }

        if (connect(sfd, (struct sockaddr*)(&addr), sizeof(addr)) != 0) { perror("connect"); has_error = 1; goto next; }

next:

        if (sfd != -1) {
            if (close(sfd) != 0) { perror("close"); has_error = 1; }
        }

        ++(count[has_error]);

    }

end:

    return EXIT_SUCCESS;

}
