About Chisel
============

Chisel is a new open-source hardware construction language developed
at UC Berkeley that supports advanced hardware design using highly
parameterized generators and layered domain-specific hardware languages.

Chisel is embedded in the [Scala](http://www.scala-lang.org/) programming
language, which raises the level of hardware design abstraction by providing
concepts including object orientation, functional programming, parameterized
types, and type inference.

Visit the [community website](http://chisel.eecs.berkeley.edu/) for more
information.

**NOTE**: This README.md describes (and associated repository contains)
version 2.x of Chisel, and while we continue to support this version of
Chisel, we encourage people to migrate to the new version:
[Chisel3](https://github.com/ucb-bar/chisel3)

We've removed the Getting Started section of this document.
If you're just getting started, you should be using Chisel3.

Documentation
-------------

Documentation has been moved to a separate [repo](https://github.com/ucb-bar/chisel-doc).


Chisel3
=======

We're releasing snapshot versions of Chisel3. To facilitate the
transition from Chisel2, you should ensure that your designs build and
test in Chisel3 compatibility mode by passing the following arguments
to Chisel:

    --minimumCompatibility 3.0.0

If you invoke chiselMain() or chiselMainTest() directly, you should
add these arguments to your current argument list:

    object hello {
      def main(args: Array[String]): Unit = {
        chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--minimumCompatibility", "3.0.0"),
           () => Module(new HelloModule())){c => new HelloModuleTests(c)}
      }
    }

This will report errors for the following Chisel3 issues:

 * Vec(Reg) should be replaced with Reg(Vec),
 * type-only vals (no associated data) must be wrapped in a Wire() if they will be the destination of a wiring operation (":=" or " < >"),
 * masked bit patterns ('b??') should be created using BitPat(), not UInt() or Bits(),
 * the "clone" method required for parameterized Bundles has been renamed "cloneType",
 * the con and alt inputs to a Mux must be type-compatible - both signed or both unsigned,
 * bulk-connection to a node that has been procedurally assigned-to is illegal,
 * != is deprecated, use =/= instead,
 * use SeqMem(...) instead of Mem(..., seqRead),
 * use SeqMem(n:Int, out: => T) instead of SeqMem(out: => T, n:Int),
 * use Mem(n:Int, t:T) instead of Mem(out:T, n:Int),
 * use Vec(n:Int, gen: => T) instead of Vec(gen: => T, n:Int),
 * Mem(..., orderedWrites) is no longer supported,
 * masked writes are only supported for Mem[Vec[_]],
 * connections between UInt and SInt are illegal.
 * module io's must be wrapped in IO().

In addition, the following incompatibilities require code changes:

 * the Node class and object no longer exist (the class should have been private in Chisel2)
 * printf() is defined in the Chisel object and produces simulation printf()'s. To use the Scala Predef.printf(), you need to qualify it with Predef.
 * in Chisel2, bulk-connects <> with unconnected source components do not update connections from the unconnected components. In Chisel3, bulk-connects strictly adhere to last connection semantics and unconnected OUTPUTs will be connected to INPUTs resulting in the assignment of random values to those inputs.
