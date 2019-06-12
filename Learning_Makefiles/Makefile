#.DEFAULT_GOAL := generate #choose the default goal instead of it being the first one
.PHONY: all clean #tells make that these goals are not files but some other thing

CC = gfortran# compiler to use

SRCS := $(wildcard *.f95)
BINS := $(SRCS:%.f95=%)

all: ${BINS}

hi: %.o ${BINS}
	@echo "Checking.."
	${CC} -o %<

%.o: %.f95
	@echo "Creating object.."
	${CC} -c $<

clean:
	@echo "Cleaning up..."
	rm -f *.mod
	rm -f *.o
