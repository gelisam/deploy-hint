WITH_GHC_IMAGE=with-ghc
WITH_GHC_CONTAINER=build-with-ghc
PROGRAM=my-program

all: test
.PHONY: all build test clean clobber

build: $(PROGRAM)/$(PROGRAM)

test: build
	./$(PROGRAM)/$(PROGRAM)


proofs/$(WITH_GHC_IMAGE): with-ghc/Dockerfile $(PROGRAM).cabal src/Main.hs
	mkdir -p proofs
	docker build -f $< -t $(WITH_GHC_IMAGE) .
	touch $@

proofs/$(WITH_GHC_CONTAINER): proofs/$(WITH_GHC_IMAGE)
	docker run --name $(WITH_GHC_CONTAINER) $(WITH_GHC_IMAGE) echo "built."
	touch $@

$(PROGRAM).tar.gz: proofs/$(WITH_GHC_CONTAINER)
	docker cp $(WITH_GHC_CONTAINER):/root/$(PROGRAM).tar.gz $@

$(PROGRAM)/$(PROGRAM): $(PROGRAM).tar.gz
	tar xvf $<


clean:
	rm -rf $(PROGRAM)/
	docker rm -vf $(WITH_GHC_CONTAINER) || true
	docker rmi $(WITH_GHC_IMAGE) || true
	rm -rf proofs/

clobber: clean
	rm -rf $(PROGRAM).tar.gz
