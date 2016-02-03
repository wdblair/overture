# The only things you should have to change when building a new program are:
# o PLU_SOURCE: the name of the .plu file
# o MAIN_NODE: the main node in the .plu file
# o possibly the LIB DIRS path

DEFAULT: all
PRELUDE_LIB=../../lib/prelude
EDF_LIB=../../lib/marte
CFLAGS=-g -I$(EDF_LIB) -I$(PRELUDE_LIB) -I.
PLU_SOURCE=sampling
MAIN_NODE=sampling
GENERATED=$(PLU_SOURCE)_c/$(MAIN_NODE)
TARGET_EXE=sampling

all: $(EDF_LIB)/edf_word_sched.o $(GENERATED).o main_marte.c
	mgcc $(CFLAGS) main_marte.c $(EDF_LIB)/dword_ts.o $(EDF_LIB)/edf_word_sched.o $(GENERATED).o -o $(TARGET_EXE)

$(GENERATED).o: $(GENERATED).h $(PLU_SOURCE)_includes.h $(GENERATED).c
	gcc $(CFLAGS) -c $(GENERATED).c -o $(GENERATED).o

$(EDF_LIB)/edf_word_sched.o: $(EDF_LIB)/dword_ts.o $(EDF_LIB)/edf_word_sched.h $(EDF_LIB)/edf_word_sched.c
	mgcc $(CFLAGS) -c $(EDF_LIB)/edf_word_sched.c -o $(EDF_LIB)/edf_word_sched.o

$(EDF_LIB)/dword_ts.o: $(EDF_LIB)/dword_ts.h $(EDF_LIB)/dword_ts.c
	mgcc $(CFLAGS) -c $(EDF_LIB)/dword_ts.c -o $(EDF_LIB)/dword_ts.o

$(GENERATED).c $(GENERATED).h: $(PLU_SOURCE).plu
	preludec -node $(MAIN_NODE) $(PLU_SOURCE).plu
