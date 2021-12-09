package aoc

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

def readResourceLines(s: String): Seq[String] =
  val file = getClass.getClassLoader.getResource(s).toURI
  Files.readAllLines(Path.of(file)).asScala.toSeq.map(_.strip())

extension [T](map: Seq[Seq[T]])
  def point(x: Int, y: Int): T            = map(y)(x)
  def getPoint(x: Int, y: Int): Option[T] = map.lift(y).flatMap(_.lift(x))

extension [T] (x:T)
  inline def pipe[X](inline f: T => X) = f(x)
