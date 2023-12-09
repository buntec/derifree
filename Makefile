format:
	scala-cli fmt .

compile:
	scala-cli --power compile .

watch-compile:
	scala-cli --power compile . --watch

markdown:
	scala-cli --power compile . --enable-markdown

run:
	scala-cli run .

clean:
	git clean -xdf
