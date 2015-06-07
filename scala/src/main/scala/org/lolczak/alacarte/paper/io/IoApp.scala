package org.lolczak.alacarte.paper.io

import org.lolczak.alacarte.paper.calculator.Term
import org.lolczak.alacarte.paper.control._
import Teletype._
import FileSystem._
import org.lolczak.alacarte.paper.io.Exec.exec
import scalaz.Scalaz._

object IoApp extends App {

  type T[A] = Coproduct[Teletype, FileSystem, A]

  def cat(path: FilePath): Term[T, Int] = for {
    content <- readFile[T](path)
    size = content.length
    _ <- Term.monad[T].traverse[Char, List, Unit](content.toList)(putChar[T](_))
  } yield size

  val size = exec(cat("build.sbt")).unsafePerformIO()

  println(s"size: $size")

}
