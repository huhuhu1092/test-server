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

/**
 * Hash map.
 */

#ifndef SE_HASHMAP_H
#define SE_HASHMAP_H

#include "SE_Common.h"
#include "SE_Element.h"
#ifdef __cplusplus
extern "C" {
#endif
typedef struct SE_HashMap_Entry_tag {
    SE_Element key;
    int hash;
    SE_Element value;
    struct SE_HashMap_Entry_tag* next;
} SE_HashMap_Entry;

typedef struct SE_HashMap_tag {
    SE_HashMap_Entry** buckets;
    int bucketCount;
    int (*hash)(SE_Element key);
    int size;
} SE_HashMap;

/**
 * Creates a new hash map. Returns NULL if memory allocation fails.
 *
 * @param initialCapacity number of expected entries
 * @param hash function which hashes keys
 * @param equals function which compares keys for equality
 */
extern SE_Result SE_HashMap_Init(int initialCapacity, int (*hash)(SE_Element key), SE_HashMap* map);

/**
 * Frees the hash map. Does not free the keys or values themselves.
 */
extern void SE_HashMap_Release(void* mapData);

/**
 * Hashes the memory pointed to by key with the given size. Useful for
 * implementing hash functions.
 */
extern int SE_HashMap_Hash(SE_Element key);

/**
 * Puts value for the given key in the map. Returns pre-existing value if
 * any.
 *
 * If memory allocation fails, this function returns NULL, the map's size
 * does not increase, and errno is set to ENOMEM.
 */
extern SE_Result SE_HashMap_Put(SE_HashMap* map, SE_Element key, SE_Element value);

/**
 * Gets a value from the map. Returns NULL if no entry for the given key is
 * found or if the value itself is NULL.
 */
extern SE_Element* SE_HashMap_Get(SE_HashMap* map, SE_Element key);

/**
 * Returns true if the map contains an entry for the given key.
 */
extern int SE_HashMap_ContainsKey(SE_HashMap* map, SE_Element key);

/**
 * Removes an entry from the map. Returns the removed value or NULL if no
 * entry was present.
 * if has entry user need to release the return value of SE_Element
 */
extern SE_Result SE_HashMap_Remove(SE_HashMap* map, SE_Element key);

/**
 * Gets the number of entries in this map.
 */
extern int SE_HashMap_Size(SE_HashMap* map);

/**
 * Invokes the given callback on each entry in the map. Stops iterating if
 * the callback returns false.
 */
extern void SE_HashMap_ForEach(SE_HashMap* map, 
        int (*SE_Callback)(SE_Element key, SE_Element value, void* context),
        void* context);


/**
 * For debugging.
 */

/**
 * Gets current capacity.
 */
extern int SE_HashMap_CurrentCapacity(SE_HashMap* map);

/**
 * Counts the number of entry collisions.
 */
extern int SE_HashMap_CountCollisions(SE_HashMap* map);

#ifdef __cplusplus
}
#endif

#endif /* __HASHMAP_H */ 
