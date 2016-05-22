WITH_GHC_IMAGE=with-ghc
WITHOUT_GHC_IMAGE=without-ghc
WITH_GHC_CONTAINER=build-with-ghc
WITHOUT_GHC_CONTAINER=run-without-ghc

all: test
.PHONY: all list_dependencies build test clean clobber

list_dependencies: proofs/$(WITH_GHC_IMAGE)
	docker rm -vf $(WITH_GHC_CONTAINER) &> /dev/null || true
	docker run -it --name $(WITH_GHC_CONTAINER) $(WITH_GHC_IMAGE) ldd /root/my-program/my-program

build: proofs/$(WITHOUT_GHC_IMAGE)

test: build
	docker rm -vf $(WITHOUT_GHC_CONTAINER) &> /dev/null || true
	echo '()' | docker run -i --name $(WITHOUT_GHC_CONTAINER) $(WITHOUT_GHC_IMAGE) /root/my-program/my-program


customized-hint/hint.cabal:
	git clone --branch=libdir-arg git@github.com:mvdan/hint.git customized-hint

proofs/$(WITH_GHC_IMAGE): with-ghc.docker customized-hint/hint.cabal my-program.cabal src/Main.hs
	mkdir -p proofs
	docker rmi $(WITH_GHC_IMAGE):latest &> /dev/null || true # prevent orphans
	docker build -f $< -t $(WITH_GHC_IMAGE):latest . # fast if :cache contains shared work
	docker rmi $(WITH_GHC_IMAGE):cache &> /dev/null || true # drop cached work which is no longer used
	docker tag $(WITH_GHC_IMAGE):latest $(WITH_GHC_IMAGE):cache # cache for next time
	touch $@

proofs/$(WITHOUT_GHC_IMAGE): without-ghc.docker my-program.tar.gz
	mkdir -p proofs
	docker build -f $< -t $(WITHOUT_GHC_IMAGE) .
	touch $@

proofs/$(WITH_GHC_CONTAINER): proofs/$(WITH_GHC_IMAGE)
	docker rm -vf $(WITH_GHC_CONTAINER) &> /dev/null || true
	docker run --name $(WITH_GHC_CONTAINER) $(WITH_GHC_IMAGE) echo "built."
	touch $@

my-program.tar.gz: proofs/$(WITH_GHC_CONTAINER)
	docker cp $(WITH_GHC_CONTAINER):/root/my-program.tar.gz $@


clean:
	rm -rf my-program/
	docker rm -vf $(WITH_GHC_CONTAINER) $(WITHOUT_GHC_CONTAINER) || true
	docker rmi $(WITH_GHC_IMAGE):latest || true
	rm -rf proofs/

clobber: clean
	docker rmi $(WITH_GHC_IMAGE):cache || true
	docker rmi $(WITHOUT_GHC_IMAGE) || true
	rm -rf my-program.tar.gz
