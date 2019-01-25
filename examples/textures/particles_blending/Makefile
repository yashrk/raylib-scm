.PHONY: clean

TARGET = particles_blending

all: $(TARGET).scm
	csc -include-path `csi -p '(repository-path)'` ${TARGET}.scm -o ${TARGET}
clean:
	rm -f *~ *.o ${TARGET}

