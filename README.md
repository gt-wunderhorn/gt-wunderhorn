# Wunderhorn

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

To build this project you need a few dependencies, below is their debian
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

You need Java to compile the java programs, OCaml to compile the project,
opam to act as the pacakge manager, and oasis to build it.
You should also get some OCaml packages
([sawja](http://sawja.inria.fr/), [core](https://janestreet.github.io/)):

```bash
opam init -y
eval $(opam config env)
opam install -y \
    core \
    sawja
```

You should add the opam envs to your `.bashrc` or similar:

```bash
echo 'eval $(opam config env)' >> ~/.bashrc
```

Finally you need to build Z3 from source with the OCaml bindings.

```bash
eval $(opam config env)
git clone 'https://github.com/Z3Prover/z3.git'
cd z3
git checkout tags/z3-4.5.0
git apply --whitespace=nowarn /z3-patches/*.patch
python scripts/mk_make.py --ml
cd build
make
sudo make install
```

#### Z3 Source Formatting

If you want your Z3 source to be formatted nicely when you run
`make example program=<arg>` you will need `scheme-format` in your path.

To install you must build it from the
[repo](https://github.com/russellw/scheme-format/).
This project is written in
[Chicken Scheme](https://www.call-cc.org/)
so you should install it:

```bash
apt-get install -y \
    chicken-bin \
    libchicken-dev
```

Finally build it and add it to your path:
```bash
git clone 'https://github.com/russellw/scheme-format.git'
cd scheme-format
csc main.scm -o scheme-format
sudo mv scheme-format /usr/bin/
```

## Building

Once your in the build environment you can build the project with

```bash
make setup
make
```

Finally, you run the simple or hard benchmarks with the Makefile,
and you can test a specific test with `make example`.

```bash
make simple # or
make hard   # or
make example program=benchmark/simple/fib.pass
```

### Making an Example

This is useful for testing out a single program by hand, now
`make example program=benchmark/<program>` outputs the generated Z3 and
it produces an HTML view of the JBir representation of the program.

## Running

The `main` program takes two arguments:

```bash
./main.byte <classpath> [print | print-ir | run]
```

 - `print` saves the Z3 output of the program as `example.z3`
 - `print-ir` prints the IR used in this project to standard out
 - `run` does the entire verification process (including trying to satisfy the clauses)
