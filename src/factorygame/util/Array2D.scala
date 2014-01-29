package factorygame.util

import scala.reflect.ClassTag
import scala.collection.Traversable
import spire.syntax.cfor._

class Array2D[@specialized(Specializable.Primitives) T: ClassTag](val width: Int, val height: Int) extends Traversable[Pos[T]] {
	private val arr = new Array[T](width * height)

	override def size = width * height

	def apply(x: Int, y: Int): T = arr(index(x, y))

	def apply(pos: (Int, Int)): T = this(pos._1, pos._2)

	def update(x: Int, y: Int, v: T): Unit = arr.update(index(x, y), v)

	def update(pos: (Int, Int), v: T): Unit = update(pos._1, pos._2, v)

	def foreach[U](f: (Pos[T]) => U): Unit =
		cforRange2(0 until width, 0 until height)((x: Int, y: Int) =>
			f(Pos(x, y, this(x, y)))
		)

	def transform(f: (Pos[T] => T)): Array2D[T] = {
		cforRange2(0 until width, 0 until height)((x: Int, y: Int) =>
			this(x, y) = f(Pos(x, y, this(x, y)))
		)
		this
	}

	private def index(x: Int, y: Int) = {
		if (!(x >= 0 && x < width && y >= 0 && y < height))
			throw new IndexOutOfBoundsException(s"Indexes $x, $y are out of bounds of $width, $height")
		x + y * width
	}
}

final case class Pos[@specialized(Specializable.Primitives) T](x: Int, y: Int, v: T)
