CC=gcc
CFLAGS  = -g -DDEBUG -I. -Wextra -Wall -Wfloat-equal -Wundef -Wshadow -Wpointer-arith -Wcast-align
CFLAGS += -Wstrict-prototypes -Wstrict-overflow=5 -Wwrite-strings -Wformat=2 -Wcast-qual
CFLAGS += -Wconversion -Wno-unused-parameter -Wno-float-conversion -Wno-sign-conversion -Wno-conversion
LIBS = -lncurses -lm -lpush

ODIR = .obj

DEP = $(OBJ:%.o=%.d)

C = $(wildcard *.c)
OBJ = $(C:%.c=$(ODIR)/%.o)

fpsheet: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

-include $(DEP)

$(ODIR)/%.o: %.c
	$(CC) -MMD -c -o $@ $< $(CFLAGS)

all: fpsheet

.PHONY: clean

clean:
	rm -f $(OBJ) $(DEP) fpsheet
