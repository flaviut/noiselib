package factorygame.util

import scala.collection.mutable
import java.util.NoSuchElementException
import Specializable.BestOfBreed

abstract class EventDispatch[@specialized(BestOfBreed) S, @specialized E] {
	private val eventMap = mutable.Map.empty[S, mutable.Set[E]]
	private val eventSet = eventMap.keySet

	def +(specifier: S, callback: E) = add(specifier, callback)

	def add(specifier: S, callback: E) = {
		if (!(eventMap contains specifier)) {
			eventMap(specifier) = mutable.Set.empty[E]
		}
		eventMap(specifier) += callback
	}

	def -(specifier: S, callback: E) = remove(specifier, callback)

	def remove(specifier: S, callback: E) = {
		try {
			eventMap(specifier) remove callback
			if (eventMap(specifier).size == 0)
				eventMap remove specifier
		} catch {
			case e: NoSuchElementException => Logger.logWarning(
				"Either nothing is registered for this event or the given callback is not in use",
				Some(e))
		}
	}

	protected def contains(specifier: S) = eventMap contains specifier

	/** Iterates through events */
	protected def foreach[U](specifier: S, f: E => U) = {
		if (contains(specifier)) {
			eventMap(specifier) foreach f
		}
	}

	/** Iterates through keys */
	protected def foreach[U](f: S => U) = eventSet foreach f


	def poll()
}
