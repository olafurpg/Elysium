package nz.daved.elysium.misc

import scala.annotation.compileTimeOnly
import scala.meta._

@compileTimeOnly("@nz.daved.inline.macros.Main not expanded")
class Main extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Defn) = meta {
    defn match {
      case q"object $name { ..$stats }" =>
        val main = q"""def main(args: Array[String]): Unit = { ..$stats }"""
        q"""object $name {
              $main
            }"""
      case _ => defn
    }
  }
}

@compileTimeOnly("@nz.daved.inline.macros.PrintTime not expanded")
class PrintTime extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Defn) = meta {
    defn match {
      case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" =>
        val body = q"""
                  {
                    val start = System.nanoTime()
                    val result = $expr
                    val elapsed =
                      _root_.java.util.concurrent.TimeUnit.MILLISECONDS.convert(
                      System.nanoTime() - start,
                      _root_.java.util.concurrent.TimeUnit.NANOSECONDS
                    )
                    println("Method " + ${name.syntax} + " ran in " + elapsed + "ms")
                    result
                  }
                  """
        q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $body"
      case _ => defn
    }
  }
}

@compileTimeOnly("@nz.daved.inline.macros.Benchmark not expanded")
class Benchmark extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Defn) = meta {
    defn match {
      case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" =>
        println(name)
        val benchmarkName = Term.Name(name.syntax + "_Benchmark")
        println(benchmarkName)
        val args = paramss.head.map(x => Term.Name(x.name.syntax))
        q"""
        ..$mods def $benchmarkName[..$tparams](...$paramss): Long = {
          val N = 100
          val times = (1 to N).map { i =>
            println(i)
            val start = System.nanoTime()
            ${Term.Apply(name, args)}
            System.nanoTime() - start
          }
          val avg = times.sum / N
          avg
        }
        ..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr
        """
      case _ => defn
    }
  }
}
