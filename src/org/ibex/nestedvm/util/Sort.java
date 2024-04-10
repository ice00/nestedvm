package org.ibex.nestedvm.util;

/**
 * Sort with a QuickSort with Insertion Sort ont short list (6 elements)
 */
public final class Sort {
    private Sort() { }

    /**
     * Comparable interface
     */
    public interface Comparable { 
        /**
         * Compare to the given object
         * 
         * @param o the object to compare
         * @return the compare value
         */
        public int compareTo(Object o); 
    }
    
    /**
     * Compare function interface
     */
    public interface CompareFunc { 
        /**
         * Compare two objects
         * 
         * @param a first object
         * @param b second object
         * @return the compare value
         */
        public int compare(Object a, Object b);
    }

    /** Compare function */
    private static final CompareFunc comparableCompareFunc = new CompareFunc() {        
        @Override
        public int compare(Object a,Object b) { 
            return ((Comparable)a).compareTo(b); 
        }
    };

    /**
     * Sort the given objects with a standard comparable function
     * 
     * @param a the objects to sort
     */
    public static void sort(Comparable[] a) { sort(a,comparableCompareFunc); }
    
    /**
     * Sort the objects with the given comparable function
     * 
     * @param a the objects to sort
     * @param c the comparable function
     */
    public static void sort(Object[] a, CompareFunc c) { 
        sort(a,c,0,a.length-1); 
    }

    /**
     * Ricorsive QuicoSort/Insertion Sort
     * 
     * @param a objects to sort
     * @param c the comparable function
     * @param start starting position of sort
     * @param end ending position of sort
     */
    private static void sort(Object[] a, CompareFunc c, int start, int end) {
        Object tmp;
        if(start >= end) return;
        if(end-start <= 6) {
            for(int i=start+1;i<=end;i++) {
                tmp = a[i];
                int j;
                for(j=i-1;j>=start;j--) {
                    if(c.compare(a[j],tmp) <= 0) break;
                    a[j+1] = a[j];
                }
                a[j+1] = tmp;
            }
            return;
        }

        Object pivot = a[end];
        int lo = start - 1;
        int hi = end;

        do {
            while((lo < hi) && c.compare(a[++lo],pivot) < 0) { }
            while((hi > lo) && c.compare(a[--hi],pivot) > 0) { }
            tmp = a[lo]; a[lo] = a[hi]; a[hi] = tmp;
        } while(lo < hi);

        tmp = a[lo]; a[lo] = a[end]; a[end] = tmp;

        sort(a, c, start, lo-1);
        sort(a, c, lo+1, end);
    }
}
