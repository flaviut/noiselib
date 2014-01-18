package factorygame.util

import scala.reflect.ClassTag
import scala.collection.Traversable
import spire.syntax.cfor._

class Array2D[@specialized(Specializable.Primitives) T: ClassTag](size: Int) extends Traversable[Pos[T]] {
	private val arr = new Array[T](size * size)

	def apply(x: Int, y: Int): T = arr(index(x, y))

	def apply(pos: (Int, Int)): T = this(pos._1, pos._2)

	def update(x: Int, y: Int, v: T): Unit = arr.update(index(x, y), v)

	def update(pos: (Int, Int), v: T): Unit = update(pos._1, pos._2, v)

	def foreach[U](f: (Pos[T]) => U): Unit =
		cforRange2(0 until size, 0 until size)((x: Int, y: Int) =>
			f(Pos(x, y, this(x, y)))
		)

	private def index(x: Int, y: Int) = x * size + y
}

final case class Pos[@specialized(Specializable.Primitives) T](x: Int, y: Int, v: T)
