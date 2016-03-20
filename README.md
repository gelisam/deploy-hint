Deploying programs which use Hint
===

Haskell programs can be deployed on machines which don't have GHC installed, it's [not much more complicated](http://gelisam.blogspot.ca/2014/12/how-to-package-up-binaries-for.html) than deploying C programs. The [hint](http://hackage.haskell.org/package/hint) library, however, evaluates Haskell expressions at runtime, using the [ghc library](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc/index.html), which makes things a bit more complicated. This repository is a proof of concept demonstrating how to deploy a Haskell program which uses the hint library.

Usage
---

To demonstrate that it's possible to deploy a program on a machine on which GHC is not installed, we'll need access to such a machine. The easiest way to do this is to create a virtual machine, and the easiest way to do that is to use [docker](https://www.docker.com/products/docker-engine). If the `docker` executable is in your PATH, just type `make` and the appropriate docker images and containers will be created for you.

    $ make
    ...
    please type '()':
    (\x -> (x,x)) () is:
    Right ((),())
    and now, let's try the Prelude...
    id () is:
    Right ()
    and finally, a library from hackage.
    don't (return ()) is:
    ()

It worked! We just evaluated a few trivial Haskell expressions (`(\x -> (x,x)) ()`, `id ()` and `don't (return ())`) in a container which doesn't have GHC installed. Sweet!

Limitations
---

The proof of concept has quite a few limitations. First, while GHC doesn't need to be installed, GCC does. We also need to install the libraries on which the evaluated expressions depend, such as [base](http://hackage.haskell.org/package/base) and, in this case, [acme-dont](https://hackage.haskell.org/package/acme-dont). In addition, we need to install the C libraries on which those Haskell libraries depend, such as [libgmp](https://hackage.haskell.org/package/integer-gmp).

Those dependencies should be packaged up and installed along with the Haskell program we're deploying. Ideally, we'd like to be able to install those at a location chosen by the user, or even better, in a subfolder near the deployed Haskell program. Doing so for C libraries is easy, but since Haskell's package databases have configuration files which point to their library files using absolute paths, there's still a bit of work to be done on that front. To demonstrate that relocating package databases is possible, the proof of concept moves the global and local databases to a folder next to the deployed Haskell program. Ideally this relocation would be performed on the target machine, once we know where the program is installed, but for now the target location needs to be known in advance because the relocation uses [`ghc-pkg`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#package-management), which isn't available on the target machine.

There's also a hardcoded path in hint itself, which we're [trying to remove](https://github.com/mvdan/hint/issues/3), and one step in that process is the creation of this proof of concept.
