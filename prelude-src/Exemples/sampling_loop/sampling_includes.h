#ifndef _sampling_includes_H
#define _sampling_includes_H
#include <stdio.h>
#include "sampling.h"

/* This file is user-defined, not generated */

int id(int i) {
  return i;
}

int input_i() {
  static int i=0;
  
  return i++;
}

void output_o(int i) {
  printf("o=%d\n",i);
}

void swap(int i, int j, struct swap_outs_t* outs) {
  outs->o=j;
  outs->p=i;
}

#endif
