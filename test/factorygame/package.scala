package object factorygame {
	var PreventOpt = 0

	def compareFuncs(f1: () => Int, f2: () => Int) = {
		val t1 = timeFunc(f1)
		val t2 = timeFunc(f2)
		(t1, t2)
	}

	def timeFunc(f: () => Int) = {
		val ret = 0.to(10).map(_ => {
			val start = System.nanoTime()
			PreventOpt += f()
			val stop = System.nanoTime()
			(stop - start).toInt
		})
		ret.min
	}
}
