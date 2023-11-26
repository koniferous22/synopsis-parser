ensure-bin-dir-exists:
	test -d "$(CURDIR)/bin" || mkdir "$(CURDIR)/bin"
build: ensure-bin-dir-exists
	stack build --exec "cp \"$(CURDIR)/$$(stack path --dist-dir)/build/synopsis-parser-exe/synopsis-parser-exe\" \"$(CURDIR)/bin/synopsis-parser\""
build-watch: ensure-bin-dir-exists
	mkdir "$(CURDIR)/bin"
	stack build --file-watch --fast --exec "cp \"$(CURDIR)/$$(stack path --dist-dir)/build/synopsis-parser-exe/synopsis-parser-exe\" \"$(CURDIR)/bin/synopsis-parser\""
runtests: ensure-bin-dir-exists
	stack test --fast
runtests-watch: ensure-bin-dir-exists
	stack test --fast --file-watch
fmt:
	find ./app ./src ./test -type f | xargs stack exec hindent -- 

test-data:
	$(CURDIR)/scripts/generate-test-data
regenerate-snapshots: build
	$(CURDIR)/scripts/regenerate-snapshots
