// Copyright 2000-2005 the Contributors, as shown in the revision logs.
// Licensed under the Apache License 2.0 ("the License").
// You may not use this file except in compliance with the License.

package org.ibex.nestedvm.util;

// Based on the various org.xwt.util.* classes by Adam Megacz

public class InodeCache {
    private static final Object PLACEHOLDER = new Object();
    private static final short SHORT_PLACEHOLDER = -2;
    private static final short SHORT_NULL = -1;
    private static final int LOAD_FACTOR = 2;
    
    /** Maximum size of the cache */
    private final int maxSize;
    
    /** Total number of slots in the cache */
    private final int totalSlots;
    
    /** Maximum number of used slots in the cache */
    private final int maxUsedSlots;
    
    // Arrays to store keys, next and previous elements, inodes, and reverse lookups
    private final Object[] keys;
    private final short[] next;
    private final short[] prev;
    private final short[] inodes;
    private final short[] reverse;
    
    // Current size and number of used slots in the cache
    private int size, usedSlots;
    
    // Most recently used and least recently used elements
    private short mru, lru;
    
    /**
     * Construct the inode cache with default size  
     */
    public InodeCache() { 
        this(1024);
    }
    
    /**
     * Construct the inode cache with given max size
     * 
     * @param maxSize max size to use
     */
    public InodeCache(int maxSize) {
        this.maxSize = maxSize;
        totalSlots = maxSize*LOAD_FACTOR*2 + 3;
        maxUsedSlots = totalSlots / LOAD_FACTOR;
        if(totalSlots > Short.MAX_VALUE) throw new IllegalArgumentException("cache size too large");
        keys = new Object[totalSlots];
        next = new short[totalSlots];
        prev = new short[totalSlots];
        inodes = new short[totalSlots];
        reverse = new short[totalSlots];
        clear();
    }
    
    /**
     * Fillethe array of object with the given object
     * 
     * @param a array to fill
     * @param o object to use
     */
    private static void fill(Object[] a,Object o) { 
        for(int i=0;i<a.length;i++) a[i] = o; 
    }
     
    /**
     * Fill the array of short with the given short
     * 
     * @param a array to fill
     * @param s short to use
     */
    private static void fill(short[] a, short s)  { 
        for(int i=0;i<a.length;i++) a[i] = s; 
    }
        
    /**
     * Clear the cache
     */
    public final void clear() {
        size = usedSlots = 0;
        mru = lru = -1;
        fill(keys,null);
        fill(inodes,SHORT_NULL);
        fill(reverse,SHORT_NULL);
    }
    
    /**
     * Get the value with the given key
     * 
     * @param key the key to use
     * @return the value of the key 
     */
    public final short get(Object key) {
        int hc = key.hashCode() & 0x7fffffff;
        int dest = hc % totalSlots;
        int odest = dest;
        int tries = 1;
        boolean plus = true;
        Object k;
        int placeholder = -1;
        
        while((k = keys[dest]) != null) {
            if(k == PLACEHOLDER) {
                if(placeholder == -1) placeholder = dest;
            } else if(k.equals(key)) {
                short inode = inodes[dest];
                if(dest == mru) return inode;
                if(lru == dest) {
                    lru = next[lru];
                } else {
                    short p = prev[dest];
                    short n = next[dest];
                    next[p] = n;
                    prev[n] = p;
                }
                prev[dest] = mru;
                next[mru] = (short) dest;
                mru = (short) dest;
                return inode;
            }
            dest = Math.abs((odest + (plus ? 1 : -1) * tries * tries) % totalSlots);
            if(!plus) tries++;
            plus = !plus;
        }
        
        // not found
        int slot;
        if(placeholder == -1) {
            // new slot
            slot = dest;
            if(usedSlots == maxUsedSlots) {
                clear();
                return get(key);
            }
            usedSlots++;
        } else {
            // reuse a placeholder
            slot = placeholder;
        }
        
        if(size == maxSize) {
            // cache is full
            keys[lru] = PLACEHOLDER;
            inodes[lru] = SHORT_PLACEHOLDER;
            lru = next[lru];
        } else {
            if(size == 0) lru = (short) slot;
            size++;
        }
        
        int inode;
        OUTER: for(inode = hc & 0x7fff;;inode++) {
            dest = inode % totalSlots;
            odest = dest;
            tries = 1;
            plus = true;
            placeholder = -1;
            int r;
            while((r = reverse[dest]) != SHORT_NULL) {
                int i = inodes[r];
                if(i == SHORT_PLACEHOLDER) {
                    if(placeholder == -1) placeholder = dest;
                } else if(i == inode) {
                    continue OUTER;
                }
                dest = Math.abs((odest + (plus ? 1 : -1) * tries * tries) % totalSlots);
                if(!plus) tries++;
                plus = !plus;
            }
            // found a free inode
            if(placeholder != -1) dest = placeholder;
            break OUTER;
        }
        keys[slot] = key;
        reverse[dest] = (short) slot;
        inodes[slot] = (short) inode;
        if(mru != -1) {
            prev[slot] = mru;
            next[mru] = (short) slot;
        }
        mru = (short) slot;
        return (short) inode;
    }
    
    /**
     * Reverse lookup of an inode
     * 
     * @param inode inode to search
     * @return the value
     */
    public Object reverse(short inode) {
        int dest = inode % totalSlots;
        int odest = dest;
        int tries = 1;
        boolean plus = true;
        int r;
        while((r = reverse[dest]) != SHORT_NULL) {
            if(inodes[r] == inode) return keys[r];
            dest = Math.abs((odest + (plus ? 1 : -1) * tries * tries) % totalSlots);
            if(!plus) tries++;
            plus = !plus;
        }        
        return null;
    }   
}
