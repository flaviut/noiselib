package object factorygame {
	var PreventOpt: Any = 0.asInstanceOf[Any]

	def compareFuncs[T](f1: () => T, f2: () => T) = {
		val t1 = timeFunc(f1)
		val t2 = timeFunc(f2)
		(t1, t2)
	}

	def timeFunc[T](f: () => T) = {
		val ret = 0.to(10).map(_ => {
			val start = System.nanoTime()
			PreventOpt = PreventOpt == f()
			val stop = System.nanoTime()
			(stop - start).toInt
		})
		ret.min
	}
}
