IMAGE=with-ghc
CONTAINER=build-with-ghc
PROGRAM=my-program

all: test
.PHONY: all build test clean clobber

build: $(PROGRAM)/$(PROGRAM)

test: build
	./$(PROGRAM)/$(PROGRAM)


proofs/$(IMAGE): Dockerfile $(PROGRAM).cabal src/Main.hs
	mkdir -p proofs
	docker build -t $(IMAGE) .
	touch $@

proofs/$(CONTAINER): proofs/$(IMAGE)
	docker run --name $(CONTAINER) $(IMAGE) echo "built."
	touch $@

$(PROGRAM).tar.gz: proofs/$(CONTAINER)
	docker cp $(CONTAINER):/root/$(PROGRAM).tar.gz $@

$(PROGRAM)/$(PROGRAM): $(PROGRAM).tar.gz
	tar xvf $<


clean:
	rm -rf $(PROGRAM)/
	docker rm -vf $(CONTAINER) || true
	docker rmi $(IMAGE) || true
	rm -rf proofs/

clobber: clean
	rm -rf $(PROGRAM).tar.gz
