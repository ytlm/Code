#include "hash.h"

void print_hash(hash_t *hash)
{
    printf(" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * \n");
    int32_t i;

    char *val;

    bucket_t *bt;

    for(i = 0; i < hash->size; i++) {
        bt = hash->bucket[i];
        while(bt) {
            val = (char *)bt->value;
            printf("key:%s, value:%s\n", bt->key, val);
            bt = bt->next;
        }
    }
    printf(" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * \n");
}

int main(int argc, char *argv[])
{
    int i;
    int32_t ok = -1;

    size_t v_len;

    char *val;

    hash_t hash;

    hash_init(&hash, 5);

    char *key[10] = {
        "test1",
        "test2",
        "test3",
        "test4",
        "test5",
        "test6",
        "test7",
        "test8",
        "test9",
        "test10"
    };

    char *value[10] = {
        "value1",
        "valeu2",
        "value3",
        "value4",
        "value5",
        "value6",
        "value7",
        "value8",
        "value9",
        "value10"
    };

    for (i = 0; i < 10; i++) {
        hash_insert(&hash, key[i], sizeof(key[i]), value[i], sizeof(value[i]));
    }
    print_hash(&hash);

    val = hash_lookup(&hash, key[3], sizeof(key[3]), &v_len);
    if (val) {
        printf("found key:%s, value:%s len:%d\n", key[3], val, v_len);
    } else {
        printf("not found key:%s\n", key[3]);
    }

    ok = hash_delete(&hash, key[8], sizeof(key[8]));
    if (ok >= 0 ) {
        printf("delete success\n");
    } else {
        printf("delete failed\n");
    }
    print_hash(&hash);

    val = hash_lookup(&hash, key[8], sizeof(key[8]), &v_len);
    if (val) {
        printf("found key:%s, value:%s len:%d\n", key[3], val, v_len);
    } else {
        printf("not found key:%s\n", key[8]);
    }

    for (i = 0; i < 10; i++) {
        ok = hash_delete(&hash, key[i], sizeof(key[i]));
        if (ok >= 0 ) {
            printf("delete success, key:%s\n", key[i]);
        } else {
            printf("delete failed, key:%s\n", key[i]);
        }
    }

    print_hash(&hash);

    val = hash_lookup(&hash, key[3], sizeof(key[3]), &v_len);
    if (val) {
        printf("found key:%s, value:%s len:%d\n", key[3], val, v_len);
    } else {
        printf("not found key:%s\n", key[3]);
    }

    return 0;
}
