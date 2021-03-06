package noiselib

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait TestBase extends FlatSpec with GeneratorDrivenPropertyChecks with MustMatchers

trait ParallelTestBase extends TestBase with BeforeAndAfterAll with ParallelTestExecution
