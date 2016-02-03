/* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2011, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
 *
 * This file is part of Prelude
 *
 * Prelude is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation ; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Prelude is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY ; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program ; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *---------------------------------------------------------------------------- */

#ifndef DWORD_H
#define DWORD_H
#include <stdlib.h>
// A deadline word is an ultimately periodic word (ie prefix.(periodic
// pattern)) that describes the sequence of deadlines of the successive
// instances of a task. This allows to specify different deadlines for
// different instances of the same task.

/**
 * The deadline word structure: prefix.(periodic pattern)
 */
struct dword_t {
  int* pref;
  int pref_size;
  int* pat;
  int pat_size;
};

/**
 * Returns the deadline for nth task instance.
 */
static inline int get_nth_deadline(const struct dword_t* dword, int n)
{
  if(n < dword->pref_size)
    return dword->pref[n];
  else
    return dword->pat[(n-dword->pref_size)%dword->pat_size];
}

static inline void free_dword(struct dword_t* dword)
{
  free(dword->pref);
  free(dword->pat);
}

#endif
