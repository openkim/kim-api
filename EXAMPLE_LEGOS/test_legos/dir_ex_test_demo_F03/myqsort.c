/*
*
* CDDL HEADER START
*
* The contents of this file are subject to the terms of the Common Development
* and Distribution License Version 1.0 (the "License").
*
* You can obtain a copy of the license at
* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
* specific language governing permissions and limitations under the License.
*
* When distributing Covered Code, include this CDDL HEADER in each file and
* include the License file in a prominent location with the name LICENSE.CDDL.
* If applicable, add the following below this CDDL HEADER, with the fields
* enclosed by brackets "[]" replaced with your own identifying information:
*
* Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
*
* CDDL HEADER END
*

*
* Copyright (c) 2013, Regents of the University of Minnesota.
* All rights reserved.
*
* Contributors:
*    Valeriu Smirichinski
* 
*/

/*                                                                      */
/* Release: This file is part of the openkim-api.git repository.        */
/*                                                                      */


#include <stdio.h> 
#include <stdlib.h> 
#include <string.h>
/*
void qsort_(void *base, int ne, int wid, int (*compar)(const void *, const void *)){
	size_t nel,width;
        nel =ne;
        width = wid;
	qsort(base,nel,width,compar);
	
}; 
*/
void qsort_(void **base, int * ne, int * wid, int (**compar)(const void *, const void *)){
	size_t nel,width;
        nel =*ne;
        width = *wid;
	qsort(*base,nel,width,*compar);
	
};
/*
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

*/
int bsearch_( const void ** key, const void ** base, int *ne, int *wid, int ( ** comparator2 ) ( const void *, const void * ) ){
       char* pf; char* pb; char* pinitial;
       int ( * comparator ) ( const void *, const void * );
       size_t nel,width;
       int index;
       nel =*ne;
       width = *wid;
       pb = (char *) *base;
       comparator= *comparator2;

       pf =(char *) bsearch(*key, *base, nel,width, comparator );
       if(pf == NULL) return -1;
       pinitial = pf;
       while ((*comparator)( (void *)(pinitial - *wid),(void *)pf)==0) pinitial = pinitial - *wid;
       return (int) (pinitial-pb)/width + 1;
};


