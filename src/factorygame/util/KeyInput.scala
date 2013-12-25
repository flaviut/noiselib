package factorygame.util


/**
 * Each game loop that the key is pressed, the callback is executed
 */
object KeyInput extends EventDispatch[Key, () => Unit] {

	def poll(): Unit = {
		import org.lwjgl.input.Keyboard.{KeyEvent => _, _}
		foreach(key => {
			if (isKeyDown(key.value)) {
				foreach(key, func => func())
			}
		})
	}


}

/* Taken from lwjgl.input.Keyboard */
object Key {
	final val Escape = Key(0x01)
	final val _1 = Key(0x02)
	final val _2 = Key(0x03)
	final val _3 = Key(0x04)
	final val _4 = Key(0x05)
	final val _5 = Key(0x06)
	final val _6 = Key(0x07)
	final val _7 = Key(0x08)
	final val _8 = Key(0x09)
	final val _9 = Key(0x0A)
	final val _0 = Key(0x0B)
	final val Minus = Key(0x0C)
	final val Equals = Key(0x0D)
	final val Backspace = Key(0x0E)
	final val Tab = Key(0x0F)
	final val Q = Key(0x10)
	final val W = Key(0x11)
	final val E = Key(0x12)
	final val R = Key(0x13)
	final val T = Key(0x14)
	final val Y = Key(0x15)
	final val U = Key(0x16)
	final val I = Key(0x17)
	final val O = Key(0x18)
	final val P = Key(0x19)
	final val LBracket = Key(0x1A)
	final val RBracket = Key(0x1B)
	final val Return = Key(0x1C)
	final val LControl = Key(0x1D)
	final val A = Key(0x1E)
	final val S = Key(0x1F)
	final val D = Key(0x20)
	final val F = Key(0x21)
	final val G = Key(0x22)
	final val H = Key(0x23)
	final val J = Key(0x24)
	final val K = Key(0x25)
	final val L = Key(0x26)
	final val Semicolon = Key(0x27)
	final val Apostrophe = Key(0x28)
	final val Grave = Key(0x29)
	final val LShift = Key(0x2A)
	final val Backslash = Key(0x2B)
	final val Z = Key(0x2C)
	final val X = Key(0x2D)
	final val C = Key(0x2E)
	final val V = Key(0x2F)
	final val B = Key(0x30)
	final val N = Key(0x31)
	final val M = Key(0x32)
	final val Comma = Key(0x33)
	final val Period = Key(0x34)
	final val Slash = Key(0x35)
	final val RShift = Key(0x36)
	final val Multiply = Key(0x37)
	final val LAlt = Key(0x38)
	final val Space = Key(0x39)
	final val Capital = Key(0x3A)
	final val F1 = Key(0x3B)
	final val F2 = Key(0x3C)
	final val F3 = Key(0x3D)
	final val F4 = Key(0x3E)
	final val F5 = Key(0x3F)
	final val F6 = Key(0x40)
	final val F7 = Key(0x41)
	final val F8 = Key(0x42)
	final val F9 = Key(0x43)
	final val F10 = Key(0x44)
	final val Numlock = Key(0x45)
	final val ScrollLock = Key(0x46)
	final val Keypad7 = Key(0x47)
	final val Keypad8 = Key(0x48)
	final val Keypad9 = Key(0x49)
	final val KeypadSubtract = Key(0x4A)
	final val Keypad4 = Key(0x4B)
	final val Keypad5 = Key(0x4C)
	final val Keypad6 = Key(0x4D)
	final val KeypadAdd = Key(0x4E)
	final val Keypad1 = Key(0x4F)
	final val Keypad2 = Key(0x50)
	final val Keypad3 = Key(0x51)
	final val Keypad0 = Key(0x52)
	final val KeypadPeriod = Key(0x53)
	final val F11 = Key(0x57)
	final val F12 = Key(0x58)
	final val KeypadEQUALS = Key(0x8D)
	final val KeypadENTER = Key(0x9C)
	final val RCtrl = Key(0x9D)
	final val KeypadComma = Key(0xB3)
	final val KeypadDivide = Key(0xB5)
	final val SysRQ = Key(0xB7)
	final val RAlt = Key(0xB8)
	final val Pause = Key(0xC5)
	final val KeypadHome = Key(0xC7)
	final val KeypadUp = Key(0xC8)
	final val KeypadPgup = Key(0xC9)
	final val KeypadLeft = Key(0xCB)
	final val KeypadRight = Key(0xCD)
	final val KeypadEnd = Key(0xCF)
	final val KeypadDown = Key(0xD0)
	final val KeypadPgdn = Key(0xD1)
	final val KeypadInsert = Key(0xD2)
	final val KeypadDelete = Key(0xD3)
	final val LMeta = Key(0xDB)
	@deprecated("Use LMeta instead", "Always")
	final val LWin = LMeta
	final val RMeta = Key(0xDC)
	@deprecated("Use RMeta instead", "Always")
	final val RWin = RMeta
}

case class Key(value: Int) extends AnyVal

/*while (next) {
val key = Key(getEventKey)
foreach(key,
(f) => f(KeyEvent(getEventKeyState, isRepeatEvent)))
}*/