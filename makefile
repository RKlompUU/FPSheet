CC=gcc
CFLAGS=-I.
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
