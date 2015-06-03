package org.lolczak.alacarte.approach1.io

import org.lolczak.alacarte.approach1.calculator.{TermInstances, Pure, Term}
import org.lolczak.alacarte.approach1.control._
import ExecInstances._
import TeletypeInstances._
import FileSystemInstances._
import org.lolczak.alacarte.approach1.control.InjectInstances._
import org.lolczak.alacarte.approach1.control.CoproductInstances._
import scalaz.std.AllInstances._


object IoApp extends App {

  type T[A] = Coproduct[Teletype, FileSystem, A]

  def readFile[F[_]](path: FilePath)(implicit I0: FileSystem :<: F): Term[F, String] = Term.inject[FileSystem,F, String](ReadFile(path, Pure(_)))

  def putChar[F[_]](char: Char)(implicit I0: Teletype :<: F): Term[F, Unit] = Term.inject[Teletype, F, Unit](PutChar(char, Pure()))


  def cat(path: FilePath): Term[T, Int] = for {
    content <- readFile[T](path)
    size = content.length
    _ <- TermInstances.monad[T].traverse[Char, List, Unit](content.toList)(putChar[T](_))
  } yield size

  val size = Exec.exec[T,Int](cat("/Users/lolczak/.profile")).unsafePerformIO()

  println(s"size: $size")

}
