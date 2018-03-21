OBJ=main.o sk.o
EXEC=nuts.exe
GFORTRAN=gfortran -O3

$(EXEC): $(OBJ)
	$(GFORTRAN) -o $(EXEC) $(OBJ)

main.o: main.f
	$(GFORTRAN) -c main.f

sk.o: sk.f
	$(GFORTRAN) -c sk.f

clean:
	rm $(EXEC) $(OBJ)
