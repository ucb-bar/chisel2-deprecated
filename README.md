About Chisel
============

Chisel is a new open-source hardware construction language developed
at UC Berkeley that supports advanced hardware design using highly
parameterized generators and layered domain-specific hardware languages.

Chisel is embedded in the [Scala](http://www.scala-lang.org/) programming
language, which raises the level of hardware design abstraction by providing
concepts including object orientation, functional programming, parameterized
types, and type inference.

Chisel can generate a high-speed C++-based cycle-accurate software simulator,
or low-level Verilog designed to pass on to standard ASIC or FPGA tools
for synthesis and place and route.

Visit the [community website](http://chisel.eecs.berkeley.edu/) for more
information.

Getting started
===============

Chisel Users
------------

To start working on a circuit with Chisel, create simple build.sbt
and scala source file containing your Chisel code as follow.

    $ cat build.sbt
    scalaVersion := "2.11.7"

    libraryDependencies += "edu.berkeley.cs" %% "chisel" % "latest.release"

(You want your build.sbt file to contain a reference to Scala version greater
or equal to 2.10 and a dependency on the Chisel library.)

Edit the source files for your circuit

    $ cat Hello.scala
    import Chisel._

    class HelloModule extends Module {
      val io = new Bundle {}
      printf("Hello World!\n")
    }

    class HelloModuleTests(c: HelloModule) extends Tester(c) {
      step(1)
    }

    object hello {
      def main(args: Array[String]): Unit = {
        chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
           () => Module(new HelloModule())){c => new HelloModuleTests(c)}
      }
    }

At this point you will need to [download and install sbt](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html#installing-sbt)
for your favorite distribution. You will need sbt version 0.13.0 or higher
because [recent versions of sbt](http://www.scala-sbt.org/0.13.0/docs/Community/Changes.html)
generate jars without the scala third-point version number
(i.e. chisel_2.10-2.0.2.jar instead of chisel_2.10*.2*-2.0.2.jar).

Execute sbt run to generate the C++ simulation source for your circuit, and (assuming you have a g++ compiler installed), compile it, and execute it under the tester.

    $ sbt run


Going further, you should read on the [sbt directory structure](http://www.scala-sbt.org/release/docs/Getting-Started/Directories.html)
to organize your files for bigger projects. SBT is the &quot;official&quot;
build system for Scala but you can use any other Java build system you
like (Maven, etc).

Chisel is implemented 100% in Scala!


Chisel developers
-----------------

You should have git, make, scala, and sbt installed on your
development system. First, clone the Chisel repository and change to
the project directory:

    $ git clone https://github.com/ucb-bar/chisel.git
    $ cd chisel

Compile and install your local copy of Chisel:

    $ make clean test publish-local

In order to use your local copy of Chisel in your own projects, you
will need to update your build.sbt files so the Chisel library
dependency is satisfied by your local copy. Replace

    libraryDependencies += "edu.berkeley.cs" %% "chisel" % "latest.release"

with:

    libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.3-SNAPSHOT"

Before you generate a pull request, run the following command
to insure all unit tests pass.

    $ make test

You can follow Chisel metrics on style compliance and code coverage
on the [website](https://chisel.eecs.berkeley.edu/unit_test_trends.html).

If you are debugging an issue in a third-party project which depends
on the Chisel jar, first check that the chisel version in your chisel
code base and in the third-party project library dependency match.
After editing the chisel code base, delete the local jar cache directory
to make sure you are not picking up incorrect jar files, then publish
the Chisel jar locally and remake your third-party project. Example:

    $ cat *srcTop*/chisel/project/build.scala
    ...
    version := "2.3-SNAPSHOT"
    ...

    $ cat *srcTop*/riscv-sodor/project/build.scala
    ...
    libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.3-SNAPSHOT"
    ...

    $ cd *srcTop*/chisel && make publish-local
    $ cd *srcTop*/riscv-sodor && make run-emulator


Documentation
-------------

In order to generate the Chisel documentation (html and pdf formats),
you'll need the LaTeX tools, tex4ht, texlive, python bs4
BeautifulSoup, imagemagick, and source-highlight.

To generate all the documentation:

    $ cd doc
    $ make

### Dependencies
The following
apt-get installs should work for ubuntu 14.04 LTS

    $ sudo apt-get install python-bs4 python-jinja2 imagemagick source-highlight
    $ sudo apt-get install tex4ht texlive-latex-base
    $ sudo apt-get install texlive-latex-recommended texlive-latex-extra
    $ sudo apt-get install texlive-fonts-recommended texlive-fonts-extra
    
On Mac OsX first install [MacTeX](https://tug.org/mactex/mactex-download.html) then use brew 

	$ brew install miktex
	$ brew install imagemagick source-highlight
	$ brew install gawk

and then downaload Beautiful Soup from [site](http://www.crummy.com/software/BeautifulSoup/#Download) unpack and run inside the folder

	$  python setup.py install


