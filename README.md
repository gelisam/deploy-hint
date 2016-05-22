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

The proof of concept packages and then installs the libraries on which the evaluated expressions depend, in this case [base](http://hackage.haskell.org/package/base) and a few other core libraries, [acme-dont](https://hackage.haskell.org/package/acme-dont) as an example of an Haskell library from hackage, and [libgmp](https://hackage.haskell.org/package/integer-gmp) as an example of a C library. Everything is installed in a subfolder near the deployed Haskell program, nothing needs to be installed globally. The major limitation of this proof of concept is that this target location needs to be known in advance, because part of the relocation uses [`ghc-pkg`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#package-management), which isn't available on the target machine.

We also use an unreleased [version of hint](https://github.com/mvdan/hint/tree/libdir-arg) that doesn't expect the core libraries to be installed in the same folder on the build machine as on the target machine. In fact, this proof of concept originated from a [discussion](https://github.com/mvdan/hint/issues/3) about allowing hint users to override this behaviour.
