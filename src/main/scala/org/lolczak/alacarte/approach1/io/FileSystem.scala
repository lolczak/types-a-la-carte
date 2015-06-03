package org.lolczak.alacarte.approach1.io

import scala.io.Source
import scalaz.Functor
import scalaz.effect.IO

sealed trait FileSystem[A]

case class ReadFile[A](path: FilePath, f: String => A) extends FileSystem[A]

case class WriteFile[A](path: FilePath, content: String, a: A) extends FileSystem[A]

object FileSystemInstances {

  implicit val fsFunctor = new Functor[FileSystem] {
    override def map[A, B](fa: FileSystem[A])(f: (A) => B): FileSystem[B] = fa match {
      case ReadFile(path, g) => ReadFile(path, f compose g)
      case WriteFile(path, content, result) => WriteFile(path, content, f(result))
    }
  }

  implicit val fsExec = new Exec[FileSystem] {
    override def functor: Functor[FileSystem] = fsFunctor

    override def execAlgebra[A](expr: FileSystem[IO[A]]): IO[A] = expr match {
      case ReadFile(path, g) => IO { Source.fromFile(path).mkString } flatMap g
      case WriteFile(path, c, r) => ???
    }
  }

}