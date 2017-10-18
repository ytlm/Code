#include "hash.h"

uint32_t hash_key(char *data, size_t k_len)
{
    uint32_t     key;
    size_t       i;

    key = 0;

    for(i = 0; i < k_len; i++) {
        key = hash_method(key, data[i]);
    }

    return key;
}

int32_t hash_init(hash_t *hash, int32_t max)
{
    int32_t i;

    hash->size = max;

    hash->bucket = (bucket_t **)calloc(hash->size, sizeof(bucket_t));

    if (hash->bucket == NULL) {
        return -1;
    }

    for (i = 0; i < hash->size; i++) {
        hash->bucket[i] = NULL;
    }

    return 0;
}

int32_t hash_insert(hash_t *hash, char *key, size_t k_len, void *value, size_t v_len)
{
    int32_t index;

    bucket_t *bucket, *temp_bucket, **bt;

    index = hash_index(hash, (hash_key(key, k_len)));

    bucket = hash->bucket[index];

    bt = &(hash->bucket[index]);


    while(bucket) {
        if (k_len == bucket->k_len && strncmp(key, bucket->key, k_len) == 0) {
            return -2; // alweady exists
        }
        bt = &(bucket->next);
        bucket = bucket->next;
    }

    temp_bucket = (bucket_t *)calloc(1, sizeof(bucket_t));
    if (temp_bucket == NULL) {
        return -1; // failed;
    }

    temp_bucket->key   = key;
    temp_bucket->k_len = k_len;
    temp_bucket->value = value;
    temp_bucket->v_len = v_len;
    temp_bucket->next  = NULL;

    *bt = temp_bucket;

    return 0;
}

void* hash_lookup(hash_t *hash, char *key, size_t k_len, size_t *v_len)
{
    *v_len = 0;

    int32_t index;

    bucket_t *bucket;

    index = hash_index(hash, (hash_key(key, k_len)));

    bucket = hash->bucket[index];

    if (bucket == NULL) {
        return NULL;
    }

    while(bucket) {
        if (k_len == bucket->k_len && strncmp(key, bucket->key, k_len) == 0) {
            *v_len = bucket->v_len;
            return bucket->value;
        }
        bucket = bucket->next;
    }

    return NULL;
}

void hash_free_bucket(bucket_t *bucket)
{
    bucket->next = NULL;

    free(bucket->key);
    free(bucket->value);

    free(bucket);
}


int32_t hash_delete(hash_t *hash, char *key, size_t k_len)
{
    int32_t index;

    bucket_t *bucket, **bt;

    index = hash_index(hash, (hash_key(key, k_len)));

    bucket = hash->bucket[index];

    bt = &(hash->bucket[index]);

    while(bucket) {
        if (k_len == bucket->k_len && strncmp(key, bucket->key, k_len) == 0) {
            *bt = bucket->next;
            // hash_free_bucket(bucket);
            return 0;
        }
        bt = &(bucket->next);

        bucket = bucket->next;
    }

    return -1; // not found
}
