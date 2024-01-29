#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

static Entry* findEntry(Entry* entries, uint32_t capacity, ObjString* key) {
    uint32_t index = key->hash & (capacity - 1);
    Entry* tombstone = NULL;
    // This loop can never be infinite because of our load factor. There will
    // always be at least 25% of the underlying array empty, so we will always
    // have at least some empty spots to fill.
    for (;;) {
        // Get a reference to the space in the hash table where we can put data.
        Entry* entry = &entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) { // An actually empty entry.
                // If we have ever passed a tombstone we return that entry instead.
                return tombstone != NULL ? tombstone : entry;
            } else {
                // A tombstone entry.
                if (tombstone == NULL) {
                    tombstone = entry;
                }
            }
        } else if (entry->key == key) {
            // Found the key.
            return entry;
        }
        // If we couldn't return the handle to the empty space, we keep searching
        // to the space directly after the initial one we calculated, making sure
        // to wrap around based on the length of our underlying container.
        //
        // The capacity is always a power of two because of the way we grow it
        // so we can use bitwise AND instead of modulo for a performance gain
        index = (index + 1) & (capacity - 1);
    }
}

static void adjustCapacity(Table* table, uint32_t capacity) {
    // Allocate the backing array.
    Entry* entries = ALLOCATE(Entry, capacity);
    // Initialize all of the entries in the array.
    for (uint32_t i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL();
    }

    // Reset the count so we can discard the tombstones in the previous array.
    table->count = 0;
    // Loop over all the old values in the table.
    for (uint32_t i = 0; i < table->capacity; i++) {
        // Get the entry.
        Entry* entry = &table->entries[i];
        // If the entry is empty skip over it, there is nothing to be done.
        if (entry->key == NULL) {
            continue;
        }

        // Insert the entry from the old table array into the new table array.
        // This we have to do this because of the wrapping when inserting a key,
        // a table of a different size will have different collisions and we need
        // to account for that.
        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

/// If an entry with the key is found, returns `true` and sets the `value` parameter
/// to point to the corresponding value. Otherwise returns false.
bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) {
        return false;
    }

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) {
        return false;
    }

    *value = entry->value;
    return true;
}

/// Sets the matching `key` to the `value` passed in. Returns true when a new
/// key is created. Returns false if the key already existed.
bool tableSet(Table* table, ObjString* key, Value value) {
    if (table->count >= table->capacity * TABLE_MAX_LOAD) {
        uint32_t capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }
    Entry* entry = findEntry(table->entries, table->capacity, key);
    // We will get a NULL key back from `findEntry` when the space for the key
    // we looked up is empty.
    bool isNewKey = entry->key == NULL;
    if (isNewKey && IS_NIL(entry->value)) {
        // Only increment the count when we add an entry to an empty bucket.
        // This avoids counting tombstones twice, as we consider them to be
        // full for the purpose of the load factor.
        table->count++;
    }

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) {
        return false;
    }

    // Find the entry.
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) {
        return false;
    }

    // Place a tombstone in the entry.
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

void tableAddAll(Table* from, Table* to) {
    for (uint32_t i = 0; i < from->capacity; i++) {
        Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

ObjString* tableFindString(Table* table, const char* chars, uint32_t length,
                           uint32_t hash) {
    if (table->count == 0) {
        return NULL;
    }

    uint32_t index = hash & (table->capacity - 1);
    for (;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            // Stop if we find an empty non-tombstone entry.
            if (IS_NIL(entry->value)) {
                return NULL;
            }
        } else if (entry->key->length == length && entry->key->hash == hash &&
                   memcmp(entry->key->chars, chars, length) == 0) {
            // We found the entry.
            return entry->key;
        }

        index = (index + 1) & (table->capacity - 1);
    }
}

void tableRemoveWhite(Table* table) {
    for (uint32_t i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key != NULL && !entry->key->obj.is_marked) {
            tableDelete(table, entry->key);
        }
    }
}

void markTable(Table* table) {
    for (uint32_t i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        markObject((Obj*)entry->key);
        markValue(entry->value);
    }
}
