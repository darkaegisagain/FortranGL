all: cube

clean:
	rm cube
	rm *.o
	rm *.mod

glf.o: glf.c
	gcc -g -c glf.c -I /opt/intel/include

glm.o: glm.cpp
	g++ -g -c glm.cpp -I ./glm/glm -I /opt/intel/include

fortrangl.o: fortrangl.f90
	ifort -m64 -g -c fortrangl.f90

cube: cube.f90 fortrangl.o glf.o glm.o
	ifort -m64 -g -o cube cube.f90 fortrangl.o glf.o glm.o -lglfw -lGL -lGLEW -lGLU
