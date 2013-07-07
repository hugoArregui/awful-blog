example: install

	cd example; awful --development-mode example.scm

install: clean

	chicken-install -s

clean:

	rm -f *.so awful-blog.import.scm

