
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

#ifndef _HASH_H_
#define _HASH_H_

typedef struct hash_s    hash_t;
typedef struct bucket_s  bucket_t;

struct bucket_s {

    size_t   k_len;
    size_t   v_len;

    char       *key;
    void       *value;

    bucket_t   *next;
};

struct hash_s {
    int32_t     size;

    bucket_t  **bucket;
};

#define hash_method(key, c)   ((uint32_t) key * 31 + c)

#define hash_index(ht, key) ((key) % (ht)->size)

int32_t hash_init(hash_t *hash, int32_t max);

void* hash_lookup(hash_t *hash, char *key, size_t k_len, size_t *v_len);

int32_t hash_insert(hash_t *hash, char *key, size_t k_len, void *value, size_t v_len);
int32_t hash_delete(hash_t *hash, char *key, size_t k_len);

int32_t hash_destory(hash_t *hash);

#endif
