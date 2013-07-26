About Chisel
============

Chisel is a new open-source hardware construction language developed
at UC Berkeley that supports advanced hardware design using highly
parameterized generators and layered domain-specific hardware languages.

Chisel is embedded in the Scala programming language, which raises the level
of hardware design abstraction by providing concepts including object
orientation, functional programming, parameterized types, and type inference.

Chisel can generate a high-speed C++-based cycle-accurate software simulator,
or low-level Verilog designed to pass on to standard ASIC or FPGA tools
for synthesis and place and route.

Visit the [community website](http://chisel.eecs.berkeley.edu/) for more
information.

Getting started
===============

Chisel Users
------------

To start working on a circuit with Chisel, first create a project
directory with a standard Scala/SBT layout.

    $ mkdir -p chisel-hello/src/main/scala
    $ cd chisel-hello

Insure that your build.sbt contains a reference to Scala version greater
or equal to 2.10 and add a dependency on the Chisel library.

    $ diff -u prev build.sbt
    +scalaVersion := "2.10.2"
    +libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.0-SNAPSHOT"

Edit the source files for your circuit

    $ cat src/main/scala/Hello.scala
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

Execute sbt run to generate the C++ simulation source for your circuit

    $ sbt run

Compile the resulting C++ output to generate a simulation executable

    $ g++ -std=c++11 -o HelloModule HelloModule.cpp HelloModule-emulator.cpp

Run the simulation executable for one clock cycle to generate a simulation trace

    $ ./HelloModule 1
    Hello World!


Chisel developpers
------------------

Checking coding style compliance

    $ sbt scalastyle

Running unit tests with code coverage

    $ sbt scct:test

Publishing jar to local system

    $ sbt publish-local
