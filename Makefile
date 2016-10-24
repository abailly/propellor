CABAL?=cabal
DATE := $(shell dpkg-parsechangelog 2>/dev/null | grep Date | cut -d " " -f2-)

# this target is provided (and is first) to keep old versions of the
# propellor cron job working, and will eventually be removed
run: build
	./propellor

build: tags propellor.1 dist/setup-config
	$(CABAL) build
	ln -sf dist/build/propellor-config/propellor-config propellor

install:
	install -d $(DESTDIR)/usr/bin $(DESTDIR)/usr/src/propellor
	install -s dist/build/propellor/propellor $(DESTDIR)/usr/bin/propellor
	mkdir -p dist/gittmp
	$(CABAL) sdist
	cat dist/propellor-*.tar.gz | (cd dist/gittmp && tar zx --strip-components=1)
	# cabal sdist does not preserve symlinks, so copy over file
	cd dist/gittmp && for f in $$(find -type f); do rm -f $$f; cp -a ../../$$f $$f; done
	# reset mtime on files in git bundle so bundle is reproducible
	find dist/gittmp -print0 | xargs -0r touch --no-dereference --date="$(DATE)"
	export GIT_AUTHOR_NAME=build \
	&& export GIT_AUTHOR_EMAIL=build@buildhost \
	&& export GIT_AUTHOR_DATE="$(DATE)" \
	&& export GIT_COMMITTER_NAME=build \
	&& export GIT_COMMITTER_EMAIL=build@buildhost \
	&& export GIT_COMMITTER_DATE="$(DATE)" \
	&& cd dist/gittmp && git init \
	&& git add . \
	&& git commit -q -m "distributed version of propellor" \
	&& git bundle create $(DESTDIR)/usr/src/propellor/propellor.git master HEAD \
	&& git show-ref master --hash > $(DESTDIR)/usr/src/propellor/head
	rm -rf dist/gittmp

clean:
	rm -rf dist Setup tags propellor propellor.1 privdata/local
	find -name \*.o -exec rm {} \;
	find -name \*.hi -exec rm {} \;

# hothasktags chokes on some template haskell etc, so ignore errors
# duplicate tags with Propellor.Property. removed from the start, as we
# often import qualified by just the module base name.
tags:
	find . | grep -v /.git/ | grep -v /tmp/ | grep -v /dist/ | grep -v /doc/ | egrep '\.hs$$' | xargs hothasktags 2>/dev/null | perl -ne 'print; s/Propellor\.Property\.//; print' | sort > tags || true

dist/setup-config: propellor.cabal
	@if [ "$(CABAL)" = ./Setup ]; then ghc --make Setup; fi
	@$(CABAL) configure

propellor.1: doc/usage.mdwn doc/mdwn2man
	doc/mdwn2man propellor 1 < doc/usage.mdwn > propellor.1

.PHONY: tags
