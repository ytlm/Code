#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <netdb.h>
#include <errno.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/epoll.h>

#define MAXEVENTS 1024

int create_and_bind(char *port) {

    struct addrinfo hints;
    struct addrinfo *result, *rp;

    int s, sfd;

    memset(&hints, 0, sizeof(struct addrinfo));

    hints.ai_flags    = AI_PASSIVE;
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    s = getaddrinfo(NULL, port, &hints, &result);
    if (s) {
        fprintf(stderr, "getaddrinfo failed, err: %s\n", gai_strerror(s));
        return -1;
    }

    for (rp = result; rp != NULL; rp = rp->ai_next) {

        sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if ( sfd == -1 ) {
            continue;
        }

        s = bind(sfd, rp->ai_addr, rp->ai_addrlen);
        if (s == 0) {
            break;
        }

        close(sfd);
    }

    if (rp == NULL) {
        fprintf(stderr, "bind failed, err: %s\n", strerror(errno));
        return -1;
    }

    freeaddrinfo(result);

    return sfd;
}

int make_socket_non_blocking(int sfd) {
    int flags, s;

    flags = fcntl(sfd, F_GETFL, 0);
    if (flags == -1) {
        fprintf(stderr, "fcntl get flags failed, err: %s\n", strerror(errno));
        return -1;
    }

    flags |= O_NONBLOCK;

    s = fcntl(sfd, F_SETFL, flags);
    if (s == -1) {
        fprintf(stderr, "fcntl set flags failed, err: %s\n", strerror(errno));
        return -1;
    }

    return 0;
}

int main(int argc, char *argv[])
{
    if ( argc != 2 ) {
        fprintf(stderr, "Usage %s [port]\n", argv[0]);
        return 1;
    }

    int sfd;

    sfd = create_and_bind(argv[1]);
    if (sfd == -1) {
        fprintf(stderr, "create_and_bind failed\n");
        return -1;
    }

    int ret;

    ret = make_socket_non_blocking(sfd);
    if (ret == -1) {
        fprintf(stderr, "make_socket_non_blocking failed\n");
        return -1;
    }

    ret = listen(sfd, SOMAXCONN);
    if (ret == -1) {
        fprintf(stderr, "listen failed, err: %s\n", strerror(errno));
        return -1;
    }

    int epoll_fd;
    struct epoll_event e_event;

    epoll_fd = epoll_create1(0);
    if ( epoll_fd == -1 ) {
        fprintf(stderr, "epoll_create failed, err: %s\n", strerror(errno));
        return 1;
    }

    e_event.events  = EPOLLIN | EPOLLET;
    e_event.data.fd = sfd;                    // sockfd

    if ( epoll_ctl( epoll_fd, EPOLL_CTL_ADD, sfd, &e_event) ) {
        fprintf(stderr, "epoll_fd close failed, err: %s\n", strerror(errno));
        goto FAILED;
    }

    struct epoll_event *events;

    events = calloc(MAXEVENTS, sizeof(struct epoll_event));

    while(1) {
        int n, i;

        n = epoll_wait(epoll_fd, events, MAXEVENTS, -1);

        for (i = 0; i < n; i++) {
            if((events[i].events & EPOLLERR) ||
                    (events[i].events & EPOLLHUP) ||
                    (!(events[i].events & EPOLLIN))) {

                fprintf(stderr, "epoll waite error\n");

                close(events[i].data.fd);

                continue;
            } else if (sfd == events[i].data.fd) {

                while(1) {
                    struct sockaddr in_addr;
                    socklen_t in_len;
                    int infd;
                    char hbuf[NI_MAXHOST], sbuf[NI_MAXSERV];

                    in_len = sizeof(struct sockaddr);

                    infd = accept(sfd, &in_addr, &in_len);
                    if (infd == -1) {
                        fprintf(stderr, "accept failed, err: %s\n", strerror(errno));
                        break;
                    }

                    ret = getnameinfo(&in_addr, in_len, hbuf, sizeof(hbuf), sbuf, sizeof(sbuf), NI_NUMERICHOST | NI_NUMERICHOST);
                    if (ret == 0) {
                        printf("Accepted connection fd:%d, (host=%s, port=%s)\n", infd, hbuf, sbuf);
                    }

                    if (make_socket_non_blocking(infd)) {
                        fprintf(stderr, "make_socket_non_blocking failed\n");
                        return -1;
                    }

                    e_event.events = EPOLLIN | EPOLLET;
                    e_event.data.fd = infd;

                    if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, infd, &e_event)){
                        fprintf(stderr, "epoll_fd close failed, err: %s\n", strerror(errno));
                        return -1;
                    }
                }

                continue;
            } else {
                int done = 0;

                while(1) {

                    ssize_t count;
                    char buf[512];

                    count = read(events[i].data.fd, buf, sizeof(buf));
                    if (count == -1) {
                        if (errno != EAGAIN) {
                            fprintf(stderr, "read\n");
                            done = 1;
                        }
                        break;
                    } else if (count == 0) {
                        done = 1;
                        break;
                    }

                    buf[count] = '\0';

                    fprintf(stdout, "fd:%d, receive buf: %s\n", events[i].data.fd, buf);
                }

                if (done) {
                    printf("Closed connection on fd: %d\n", events[i].data.fd);
                    close(events[i].data.fd);
                }
            }
        }
    }

    free(events);

    close( sfd );

FAILED:

    close( epoll_fd );

    return 0;
}
