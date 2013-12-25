package factorygame.util

import org.scalacheck.Gen
import scala.Array

object Gens {
	lazy val bytes2 = for {b0 <- Gen.choose(Byte.MinValue, Byte.MaxValue)
	                       b1 <- Gen.choose(Byte.MinValue, Byte.MaxValue)} yield Array(b0, b1)
	lazy val bytes4 = for {v0 <- bytes2; v1 <- bytes2} yield v0 ++ v1
	lazy val bytes8 = for {v0 <- bytes4; v1 <- bytes4} yield v0 ++ v1

	lazy val nibbles2 = for {n0 <- Gen.choose(0.toByte, 15.toByte)
	                         n1 <- Gen.choose(0.toByte, 15.toByte)} yield Array(n0, n1)
	lazy val nibbles4 = for {v0 <- nibbles2; v1 <- nibbles2} yield v0 ++ v1
	lazy val nibbles8 = for {v0 <- nibbles4; v1 <- nibbles4} yield v0 ++ v1
	lazy val nibbles16 = for {v0 <- nibbles8; v1 <- nibbles8} yield v0 ++ v1


}
