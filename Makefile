default:
	dune build --profile release

install: default
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

doc:
	dune build @doc --profile release

test:
	dune runtest --profile release
