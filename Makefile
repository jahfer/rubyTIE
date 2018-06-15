format = exe
file = data/test_kitchen_sink.rb

build:
	jbuilder build bin/cli.$(format)

run:
	jbuilder exec bin/cli.$(format) $(file)

clean:
	jbuilder clean

.PHONY: build clean run
