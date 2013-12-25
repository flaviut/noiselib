package factorygame.util


object MouseInput extends EventDispatch[Button, (MouseEvent) => Unit] {

	def poll(): Unit = {
		import org.lwjgl.input.Mouse._
		while (next) {
			val button = getEventButton

		}
	}
}

case class MouseEvent(button: Int, isDown: Boolean, x: Int, y: Int, dx: Int, dy: Int, dWheel: Int)

case class Button(value: Int, wheel: Boolean)