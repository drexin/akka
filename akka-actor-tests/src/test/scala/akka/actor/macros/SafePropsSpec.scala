package akka.actor.macros

import akka.testkit.AkkaSpec
import akka.testkit.ImplicitSender
import scala.tools.reflect.{ ToolBox, ToolBoxError }
import akka.actor.Actor

class TestActor(xs: List[String]) extends Actor {
  def receive: Actor.Receive = {
    case "ping" ⇒ sender ! "pong"
  }
}

class Foo
class Bar extends Foo

class FooActor(foo: Foo) extends Actor {
  def receive: Actor.Receive = PartialFunction.empty
}

class BarActor(foo: Bar) extends Actor {
  def receive: Actor.Receive = PartialFunction.empty
}

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SafePropsSpec extends AkkaSpec() with ImplicitSender {
  "A SafeProps" must {

    val compileOptions = "-cp akka-actor/target/classes:akka-actors/target/classes"
    val m = scala.reflect.runtime.currentMirror
    val tb = m.mkToolBox(options = compileOptions)

    "compile" in {
      tb.eval(
        tb.parse("""
                   |import akka.actor.macros.SafeProps
                   |import akka.actor.macros.TestActor
                   |
                   |SafeProps[TestActor]("foo" :: Nil)
                 """.stripMargin))
    }

    "not compile" in {
      intercept[ToolBoxError] {
        tb.eval(
          tb.parse("""
                     |import akka.actor.macros.SafeProps
                     |import akka.actor.macros.TestActor
                     |
                     |SafeProps[TestActor](1 :: Nil)
                   """.stripMargin))
      }.message should include("No matching constructor found on class TestActor for types (List[Int])")
    }

    "accept subtype instances as parameters" in {
      tb.eval(
        tb.parse("""
                   |import akka.actor.macros._
                   |
                   |SafeProps[FooActor](new Bar)
                 """.stripMargin))
    }

    "not accept supertype instances as parameters" in {
      intercept[ToolBoxError] {
        tb.eval(
          tb.parse("""
                     |import akka.actor.macros._
                     |
                     |SafeProps[BarActor](new Foo)
                   """.stripMargin))
      }.message should include("No matching constructor found on class BarActor for types (akka.actor.macros.Foo)")
    }

    "create a TestActor" in {
      val actor = system.actorOf(SafeProps[TestActor]("foo" :: Nil))

      actor ! "ping"

      expectMsg("pong")
    }
  }
}
