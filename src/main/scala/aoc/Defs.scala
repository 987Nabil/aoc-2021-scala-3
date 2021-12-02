package aoc

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

def readResourceLines(s: String): Seq[String] =
  val file = getClass.getClassLoader.getResource(s).toURI
  Files.readAllLines(Path.of(file)).asScala.toSeq.map(_.strip())
