#
# Overture Build
#

all:
	$(MAKE) -C src

test: all
	tests/run_tests.sh

release:
	share/mkrelease.sh

clean:
	$(MAKE) -C src cleanall
	rm -rf dist

.PHONY: all test release clean
