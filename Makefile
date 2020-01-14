all: test_fortrangl

clean:
	rm test_fortrangl
	rm *.o

fortrangl.o: fortrangl.c
	gcc -g -O0 -c fortrangl.c -I /opt/intel/include

test_fortrangl: test_fortrangl.f90 fortrangl.o
	ifort -m64 -g -O0 -o test_fortrangl test_fortrangl.f90 fortrangl.o -lglfw -lGL -lGLEW
