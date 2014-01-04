package factorygame

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait TestBase extends FlatSpec with GeneratorDrivenPropertyChecks with MustMatchers
