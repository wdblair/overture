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

#ifndef _encoded_task_params_H
#define _encoded_task_params_H
#include <stdlib.h>
#include "dword.h"
// Description of a real time task, with precedence encoding.

struct encoded_task_params {
  char* e_t_name;
  int e_t_period;
  int e_t_initial_release;
  int e_t_wcet;
  struct dword_t e_t_dword;
  int (*e_t_body)(void*); // This is the code to execute at each
                             // instance of the task.
};

static inline void free_task(struct encoded_task_params* t)
{
  free_dword(&(t->e_t_dword));
}

#endif
