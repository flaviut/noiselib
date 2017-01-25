package noiselib.util

import scala.reflect.ClassTag
import scala.collection.Traversable
import spire.syntax.cfor._
import scala.collection.parallel.{IterableSplitter, ParIterable}
import scala.collection.generic.GenericCompanion

protected trait BaseBuffer2D[@specialized(Specializable.Primitives) T] {
	def width: Int

	def height: Int

	def size: Int = width * height

	/**
	 * Must throw an IndexOutOfBoundsException if the index is out of bounds
	 */
	def apply(x: Int, y: Int): T

	def apply(pos: (Int, Int)): T = this(pos._1, pos._2)

	/**
	 * Must throw an IndexOutOfBoundsException if the index is out of bounds
	 */
	def update(x: Int, y: Int, v: T): Unit

	def update(pos: (Int, Int), v: T): Unit = update(pos._1, pos._2, v)
}

trait Buffer2D[@specialized(Specializable.Primitives) T] extends Traversable[Indexer2D[T]] with BaseBuffer2D[T] {
	override def size: Int = width * height

	override def companion: GenericCompanion[Traversable] = super.companion

	def transform(f: (Indexer2D[T] => T)): Unit =
		cforRange2(0 until height, 0 until width)((y: Int, x: Int) =>
			this(x, y) = f(Indexer2D(x, y, this(x, y)))
		)

	def tabulate(f: (Indexer2D[Null] => T)): Unit =
		cforRange2(0 until height, 0 until width)((y: Int, x: Int) =>
			this(x, y) = f(Indexer2D(x, y, null))
		)

	override def foldLeft[R](initAccm: R)(f: (R, Indexer2D[T]) => R): R = {
		var accm = initAccm
		foreach(v => {accm = f(accm, v)})
		accm
	}

	def foreach[U](f: (Indexer2D[T]) => U): Unit =
		cforRange2(0 until height, 0 until width)((y: Int, x: Int) =>
			f(Indexer2D(x, y, this(x, y)))
		)

	override def par = new ParBuffer2D[T](this)
}

class ParBuffer2D[@specialized(Specializable.Primitives) T](buff: Buffer2D[T])
		extends ParIterable[Indexer2D[T]] with BaseBuffer2D[T] {
	private val bufView = buff.view.par

	override def update(x: Int, y: Int, v: T): Unit = buff.update(x, y, v)

	override def apply(x: Int, y: Int): T = buff.apply(x, y)

	override def height: Int = buff.height

	override def width: Int = buff.width

	def transform(f: Indexer2D[T] => T): Unit =
		for (y <- (0 until width).par) {
			cfor(0)(_ < height, _ + 1)(x => {
				this(x, y) = f(Indexer2D(x, y, this(x, y)))
			})
		}

	def tabulate(f: Indexer2D[Null] => T): Unit =
		for (y <- (0 until width).par) {
			cfor(0)(_ < height, _ + 1)(x => {
				this(x, y) = f(Indexer2D(x, y, null))
			})
		}

	override def splitter: IterableSplitter[Indexer2D[T]] = bufView.getClass.getDeclaredMethod("splitter")
	                        .invoke(bufView)
	                        .asInstanceOf[IterableSplitter[Indexer2D[T]]]

	override def seq: Iterable[Indexer2D[T]] = buff.toIterable
}

class Array2D[@specialized(Specializable.Primitives) T: ClassTag](val width: Int, val height: Int) extends Buffer2D[T] {
	private val arr = new Array[T](width * height)

	def apply(x: Int, y: Int): T = arr(index(x, y))

	def update(x: Int, y: Int, v: T): Unit = arr.update(index(x, y), v)

	private def index(x: Int, y: Int) = {
		if (!(x >= 0 && x < width && y >= 0 && y < height))
			throw new IndexOutOfBoundsException(s"Indexes $x, $y are out of bounds of $width, $height")
		x + y * width
	}
}
