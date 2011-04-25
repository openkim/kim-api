/*                                                                      */
/* Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna */
/* All rights reserved.                                                 */
/*                                                                      */
/* Author: Valeriu Smirichinski                                         */
/*                                                                      */

#include <stdio.h> 
#include <stdlib.h> 
#include <string.h>

void qsort_(void *base, int ne, int wid, int (*compar)(const void *, const void *));
int bsearch_( const void * key, const void * base, int ne, int wid, int ( * comparator ) ( const void *, const void * ) );


void qsort_(void *base, int ne, int wid, int (*compar)(const void *, const void *)){
	size_t nel,width;
        nel =ne;
        width = wid;
	qsort(base,nel,width,compar);	
}; 

int bsearch_( const void * key, const void * base, int ne, int wid, int ( * comparator ) ( const void *, const void * ) ){
       char* pf; char* pb; char* pinitial;
       size_t nel,width;
       int index;
       nel =ne;
       width = wid;
       pb = (char *) base;
       pf =(char *) bsearch(key, base, nel,width, comparator );
       if(pf == NULL) return -1;
       pinitial = pf;
       while ((*comparator)( (void *)(pinitial - wid),(void *)pf)==0) pinitial = pinitial - wid;
       return (int) (pinitial-pb)/width + 1;
};

