CC=gcc
CFLAGS  = -I. -Wextra -Wall -Wfloat-equal -Wundef -Wshadow -Wpointer-arith -Wcast-align
CFLAGS += -Wstrict-prototypes -Wstrict-overflow=5 -Wwrite-strings -Wformat=2 -Wcast-qual
CFLAGS += -Wconversion -Wno-unused-parameter
LIBS = -lncurses

ODIR = .obj

DEP = $(OBJ:%.o=%.d)

C = $(wildcard *.c)
OBJ = $(C:%.c=$(ODIR)/%.o)

fpsheet: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

-include $(DEP)

$(ODIR)/%.o: %.c
	$(CC) -MMD -c -o $@ $< $(CFLAGS)


.PHONY: clean

clean:
	rm -f $(OBJ) $(DEP) fpsheet
