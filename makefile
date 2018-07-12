CC=gcc
CZ=zig
CFLAGS  = -g -DDEBUG -I. -Wextra -Wall -Wfloat-equal -Wundef -Wshadow -Wpointer-arith -Wcast-align
CFLAGS += -Wstrict-prototypes -Wstrict-overflow=5 -Wwrite-strings -Wformat=2 -Wcast-qual
CFLAGS += -Wconversion -Wno-unused-parameter -Wno-float-conversion -Wno-sign-conversion -Wno-conversion
LIBS = -lncurses -lm

CZFLAGS =

ODIR = .obj
ZODIR = .zobj

DEP = $(OBJ:%.o=%.d)

C = $(wildcard *.c)
Z = $(wildcard *.zig)
OBJ = $(C:%.c=$(ODIR)/%.o)
OBJ += $(Z:%.zig=$(ZODIR)/%.o)

fpsheet: $(OBJ)
	$(CZ) build-exe $(addprefix --object ,$^) --name $@ # $(LIBS)

-include $(DEP)

$(ODIR)/%.o: %.c
	$(CC) -MMD -c -o $@ $< $(CFLAGS)

$(ZODIR)/%.o: %.zig
	$(CZ) build-obj $< --output $@ --output-h zheaders/$(*F).h $(CZFLAGS)

all: fpsheet

.PHONY: clean

clean:
	rm -f $(OBJ) $(ZOBJ) $(DEP) fpsheet
