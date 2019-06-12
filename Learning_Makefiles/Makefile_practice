#.DEFAULT_GOAL := generate #choose the default goal instead of it being the first one
.PHONY: all say_hello generate clean #tells make that these goals are not files but some other thing


all: say_hello generate

say_hello:
	@echo "Hello World"

generate:
	@echo "Creating empty text files..."
	touch file-{1..10}.txt

clean:
	@echo "Cleaning up..."
	rm *.txt
