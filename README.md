'backannotate' branch
=====================

Donggyu Kim
(dgkim@eecs.berkeley.edu)


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
    scalaVersion := "2.10.2"

    addSbtPlugin("com.github.scct" % "sbt-scct" % "0.2.1")

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

    class HelloModuleTests(c: HelloModule) extends Tester(c, Array(c.io)) {
        defTests {
            true
        }
    }

    object hello {
        def main(args: Array[String]): Unit = {
            chiselMainTest(Array[String]("--backend", "c", "--genHarness"),
                () => Module(new HelloModule())){c => new HelloModuleTests(c)}
        }
    }

At this point you will need to [download and install sbt](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html#installing-sbt)
for your favorite distribution. You will need sbt version 0.12.4 or higher
because [recent versions of sbt](http://www.scala-sbt.org/0.12.4/docs/Community/Changes.html)
generate jars without the scala third-point version number
(i.e. chisel_2.10-2.0.2.jar instead of chisel_2.10*.2*-2.0.2.jar).

Execute sbt run to generate the C++ simulation source for your circuit

    $ sbt run

Compile the resulting C++ output to generate a simulation executable

    $ g++ -std=c++11 -o HelloModule HelloModule.cpp HelloModule-emulator.cpp

Run the simulation executable for one clock cycle to generate a simulation trace

    $ ./HelloModule 1
    Hello World!

Going further, you should read on the [sbt directory structure](http://www.scala-sbt.org/release/docs/Getting-Started/Directories.html)
to organize your files for bigger projects. SBT is the &quot;official&quot;
build system for Scala but you can use any other Java build system you
like (Maven, etc).

Chisel is implemented 100% in Scala!


Chisel developers
-----------------

Before you generate a pull request, run the following commands
to insure all unit tests (with code coverage) pass
and to check for coding style compliance respectively.

    $ sbt scct:test
    $ sbt scalastyle

You can follow Chisel metrics on style compliance and code coverage
on the [website](https://chisel.eecs.berkeley.edu/unit_test_trends.html).

If you are debugging an issue in a third-party project which depends
on the Chisel jar, first check that the chisel version in your chisel
code base and in the third-party project library dependency match.
After editing the chisel code base, delete the local jar cache directory
to make sure you are not picking up incorrect jar files, then publish
the Chisel jar locally and remake your third-party project. Example:

    $ cat *srcTop*/chisel/build.sbt
    ...
    version := "2.1-SNAPSHOT"
    ...

    $ cat *srcTop*/riscv-sodor/project/build.scala
    ...
    libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.1-SNAPSHOT"
    ...

    $ rm -rf ~/.sbt ~/.ivy2
    $ cd *srcTop*/chisel && sbt publish-local
    $ cd *srcTop*/riscv-sodor && make run-emulator

Publishing to public Maven repo:

    $ diff -u build.sbt
    -version := "2.1-SNAPSHOT"
    +version := "2.1"

    $ sbt publish-signed

Making the Chisel jar file with Maven (>=3.0.4)

    $ mvn install
