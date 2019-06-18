#.DEFAULT_GOAL := generate #choose the default goal instead of it being the first one
.PHONY: all clean #tells make that these goals are not files but some other thing

# compiler options, debug
coarray_compiler = caf -std=f2018 -Wextra -Wall -pedantic -fimplicit-none -fcheck=all -Og -g -fbacktrace

# list of binaries to clean as well
binaries = tally test_sort


all:
	@echo "This does nothing. See Makefile"


tally: tally.f90
	${coarray_compiler} $< -o $@


sort: sort_module.o merge_module.o test_sort.o
	${coarray_compiler} sort_module.o merge_module.o test_sort.o -o test_sort


# Create any object files needed
%.o: %.f90
	${coarray_compiler} -c $<


clean:
	@echo "Cleaning up..."
	@rm -f $(binaries)
	@rm -f *.mod
	@rm -f *o
	@rm -f *.exe
	@rm -rf *.dSYM
	@rm -f a.out
	@echo "Done"
