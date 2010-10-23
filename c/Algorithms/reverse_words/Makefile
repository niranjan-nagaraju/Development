CFLAGS += -g -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast
INCLUDE += -I../../common/include
LIBS += -lcommon -L../../common/out

EXE = reverse_words

$(EXE): $(EXE).c
	mkdir -p out/
	$(CC) $(CFLAGS) $< -o out/$(EXE) $(INCLUDE) $(LIBS) 

clean: 
	rm -f out/$(EXE) 
