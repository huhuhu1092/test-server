/*
 * Copyright (C) 2007 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "SE_HashMap.h"
#include "SE_Memory.h"
#include "SE_Log.h"
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

SE_Result SE_HashMap_Init(int initialCapacity,
        int (*hash)(SE_Element key), SE_HashMap* map) 
{
	int minimumBucketCount;
    SE_ASSERT(map != NULL);    
    SE_Object_Clear(map, sizeof(SE_HashMap));
    // 0.75 load factor.
    minimumBucketCount = initialCapacity * 4 / 3;
    map->bucketCount = 1;
    while (map->bucketCount <= minimumBucketCount) {
        // Bucket count must be power of 2.
        map->bucketCount <<= 1; 
    }

    map->buckets = (SE_HashMap_Entry**)SE_Calloc(map->bucketCount, sizeof(SE_HashMap_Entry*));
    if (map->buckets == NULL) {
        SE_Object_Clear(map, sizeof(SE_HashMap));
        return SE_INVALID;
    }
    map->size = 0;
    map->hash = hash;
    
    return SE_VALID;
}
int SE_HashMap_Hash(SE_Element key) {
    char* data = NULL;
    int keysize = 0;
	int h, i;
    switch(key.type)
    {
    case SE_INT:
        data = (char*)&key.i;
    case SE_UINT:
        data = (char*)&key.ui;
        keysize = sizeof(int);
        break;
    case SE_SHORT:
        data = (char*)&key.s;
    case SE_USHORT:
        data = (char*)&key.us;
        keysize = sizeof(short);
        break;
    case SE_LONG:
        data = (char*)&key.l;
    case SE_ULONG:
        data = (char*)&key.ul;
        keysize = sizeof(long);
        break;
    case SE_CHAR:
        data = (char*)&key.c;
    case SE_UCHAR:
        data = (char*)&key.uc;
        keysize = sizeof(char);
        break;
    case SE_STRING:
        keysize = SE_String_Length(&key.str);
        data = SE_String_GetData(&key.str);
        break;
    case SE_DATA:
        keysize = sizeof(void*);
        data = (char*)key.dp.data;
        break;
    }
    h = keysize;
    for (i = 0; i < keysize; i++) {
        h = h * 31 + *data;
        data++;
    }
    return h;
}


/**
 * Hashes the given key.
 */
static int hashKey(SE_HashMap* map, SE_Element key)
{
    int h;
    if(map->hash)
        h = map->hash(key);
    else
        h = SE_HashMap_Hash(key);

    // We apply this secondary hashing discovered by Doug Lea to defend
    // against bad hashes.
    h += ~(h << 9);
    h ^= (((unsigned int) h) >> 14);
    h += (h << 4);
    h ^= (((unsigned int) h) >> 10);
       
    return h;
}

int SE_HashMap_Size(SE_HashMap* map) {
    return map->size;
}

static int calculateIndex(int bucketCount, int hash) {
    return (hash) & (bucketCount - 1);
}

static void releaseBucket(SE_HashMap_Entry** buckets, int bucketCount)
{
    int i;
    for(i = 0 ; i < bucketCount ; i++)
    {
        SE_HashMap_Entry* entry = buckets[i];
        while(entry != NULL)
        {
            SE_HashMap_Entry* next = entry->next;
            SE_Element_Release(&entry->key);
            SE_Element_Release(&entry->value);
            SE_Free(entry);
            entry = next;
        }
    }
}
static void expandIfNecessary(SE_HashMap* map) {
    // If the load factor exceeds 0.75...
    if (map->size > (map->bucketCount * 3 / 4)) {
        // Start off with a 0.33 load factor.
		int i;
        int newBucketCount = map->bucketCount << 1;
        SE_HashMap_Entry** newBuckets = (SE_HashMap_Entry**)SE_Calloc(newBucketCount, sizeof(SE_HashMap_Entry*));
        LOGI("#### expand hashmap ####");
        if (newBuckets == NULL) {
            // Abort expansion.
            return;
        }
        memset(newBuckets, 0, newBucketCount * sizeof(SE_HashMap_Entry*)); 
        // Move over existing entries.
        for (i = 0; i < map->bucketCount; i++) {
            SE_HashMap_Entry* entry = map->buckets[i];
            while (entry != NULL) {
                SE_HashMap_Entry* next = entry->next;
                int index = calculateIndex(newBucketCount, entry->hash);
                entry->next = newBuckets[index];
                newBuckets[index] = entry;
                entry = next;
            }
        }

        // Copy over internals.
        //releaseBucket(map->buckets, map->bucketCount);
        SE_Free(map->buckets);
        map->buckets = newBuckets;
        map->bucketCount = newBucketCount;
    }
}
void SE_HashMap_Release(void* mapData) {
    SE_HashMap* map = (SE_HashMap*)mapData;
    releaseBucket(map->buckets, map->bucketCount);
    SE_Free(map->buckets);
}

static SE_HashMap_Entry* createEntry(SE_Element key, int hash, SE_Element value) {
    SE_HashMap_Entry* entry = (SE_HashMap_Entry*)SE_Malloc(sizeof(SE_HashMap_Entry));
    if (entry == NULL) {
        return NULL;
    }
    entry->key = key;
    entry->hash = hash;
    entry->value = value;
    entry->next = NULL;
    return entry;
}

static int equalKeys(SE_Element keyA, SE_Element keyB) {
    if (SE_Element_Compare(keyA, keyB) == 0) 
    {
        return 1;
    }
    else
        return 0;
}

SE_Result SE_HashMap_Put(SE_HashMap* map, SE_Element key, SE_Element value) {
    int hash = hashKey(map, key);
    size_t index = calculateIndex(map->bucketCount, hash);

    SE_HashMap_Entry** p = &(map->buckets[index]);
    while (1) {
        SE_HashMap_Entry* current = *p;

        // Add a new entry.
        if (current == NULL) {
            *p = createEntry(key, hash, value);
            if (*p == NULL) {
                return SE_INVALID;
            }
            map->size++;
            expandIfNecessary(map);
            return SE_VALID;
        }

        // Replace existing entry.
        if (equalKeys(current->key, key)) {
            SE_Element oldValue = current->value;
            SE_Element oldKey = current->key;
            current->value = value;
            current->key = key;
            SE_Element_Release(&oldKey);
            SE_Element_Release(&oldValue);
            return SE_VALID;
        }

        // Move to next entry.
        p = &current->next;
    }
}

SE_Element* SE_HashMap_Get(SE_HashMap* map, SE_Element key) {
    int hash = hashKey(map, key);
    size_t index = calculateIndex(map->bucketCount, hash);

    SE_HashMap_Entry* entry = map->buckets[index];
    while (entry != NULL) {
        if (equalKeys(entry->key, key)) {
            return &entry->value;
        }
        entry = entry->next;
    }

    return NULL;
}

int SE_HashMap_ContainsKey(SE_HashMap* map, SE_Element key) {
    int hash = hashKey(map, key);
    size_t index = calculateIndex(map->bucketCount, hash);

    SE_HashMap_Entry* entry = map->buckets[index];
    while (entry != NULL) {
        if (equalKeys(entry->key, key)) {
            return 1;
        }
        entry = entry->next;
    }

    return 0;
}
SE_Result SE_HashMap_Remove(SE_HashMap* map, SE_Element key) {
    int hash = hashKey(map, key);
    size_t index = calculateIndex(map->bucketCount, hash);

    // Pointer to the current entry.
    SE_HashMap_Entry** p = &(map->buckets[index]);
    SE_HashMap_Entry* current;
    while ((current = *p) != NULL) {
        if (equalKeys(current->key, key)) {
            SE_Element value = current->value;
            SE_Element_Release(&value);
            *p = current->next;
            SE_Element_Release(&current->key);
            SE_Free(current);
            map->size--;
            return SE_VALID;
        }

        p = &current->next;
    }

    return SE_VALID;
}

void SE_HashMap_ForEach(SE_HashMap* map, 
        int (*callback)(SE_Element key, SE_Element value, void* context),
        void* context) {
    int i;
    for (i = 0; i < map->bucketCount; i++) {
        SE_HashMap_Entry* entry = map->buckets[i];
        while (entry != NULL) {
            if (!callback(entry->key, entry->value, context)) {
                return;
            }
            entry = entry->next;
        }
    }
}

int SE_HashMap_CurrentCapacity(SE_HashMap* map) {
    int bucketCount = map->bucketCount;
    return bucketCount * 3 / 4;
}

int SE_HashMap_CountCollisions(SE_HashMap* map) {
    int collisions = 0;
    int i;
    for (i = 0; i < map->bucketCount; i++) {
        SE_HashMap_Entry* entry = map->buckets[i];
        while (entry != NULL) {
            if (entry->next != NULL) {
                collisions++;
            }
            entry = entry->next;
        }
    }
    return collisions;
}
