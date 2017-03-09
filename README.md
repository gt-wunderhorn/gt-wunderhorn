## Setting up the Dev Environment

### Docker

If you don't want all this strange stuff on your system you can make a docker
environment with all the paths and dependencies set and installed, just do:

```bash
docker build -t safety-itps . && docker run -v "$(pwd):/src" -it safety
```

Now you can edit the source and view it in the environment in the `/src`
directory.


### Manually

To build this project you need a few dependencies, below is there debian
package names:

```bash
apt-get install -y \
    m4 \
    zlib1g-dev \
    python \
    openjdk-7-jdk \
    opam \
    oasis \
    ocaml
```

You need Java to compile the java programs, this project is in OCaml and
opam is nice to get OCaml packages, while oasis is the build tool.
You should also get some OCaml packages:

```bash
opam init -y
eval $(opam config env)
opam install -y \
    core \
    sawja
```

Finally you need to build z3 from source with the OCaml bindings.

```bash
eval $(opam config env)
git clone 'https://github.com/Z3Prover/z3.git'
cd z3
git apply --whitespace=nowarn /z3-patches/*.patch
python scripts/mk_make.py --ml
cd build
make
sudo make install
```

You should add the opam envs to your `.bashrc` or similar:

```bash
echo 'eval $(opam config env)' >> ~/.bashrc
```

## Building

Once your in the build environment you can make the project with

```bash
make setup
make
```

Finally, you run the simple or hard benchmarks with the Makefile,
and you can test a specific test with `make example`.

```bash
make simple
make hard
make example program=benchmark/simple/fib.pass
```

### Making an Example

This is useful for testing out a single program by hand, now
`make example program=benchmark/<program>` outputs the generated Z3 and
it produces an HTML view of the JBir representation of the program.
