package org.lolczak.alacarte.approach1.io

import org.lolczak.alacarte.approach1.calculator.Term
import org.lolczak.alacarte.approach1.control._
import Teletype._
import FileSystem._
import scalaz.std.AllInstances._


object IoApp extends App {

  type T[A] = Coproduct[Teletype, FileSystem, A]

  def cat(path: FilePath): Term[T, Int] = for {
    content <- readFile[T](path)
    size = content.length
    _ <- Term.monad[T].traverse[Char, List, Unit](content.toList)(putChar[T](_))
  } yield size

  val size = Exec.exec[T,Int](cat("build.sbt")).unsafePerformIO()

  println(s"size: $size")

}
