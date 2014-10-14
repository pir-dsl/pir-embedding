package edu.uwm.cs.pir.domain

sealed trait InputState {
  def nextState: InputState
}

sealed abstract class Action[+A] extends Function1[InputState, (InputState, A)] {
  def map[B](f: A => B): Action[B] = flatMap { x => Action(f(x)) }
  def flatMap[B](f: A => Action[B]): Action[B] = new ChainedAction(this, f)

  private class ChainedAction[+A, B](action1: Action[B], f: B => Action[A]) extends Action[A] {
    def apply(state1: InputState) = {
      val (state2, intermediateResult) = action1(state1)
      val action2 = f(intermediateResult)
      action2(state2)
    }
  }
}

object Action {
  def apply[A](expression: => A): Action[A] = new SimpleAction(expression)
  private class SimpleAction[+A](expression: => A) extends Action[A] {
    def apply(state: InputState) = (state.nextState, expression)
  }
}

private class UnitAction[+A](value: A) extends Action[A] {
  def apply(state: InputState) = (state, value)
}

object IOApplication {
  private class InputStateImpl(id: BigInt) extends InputState {
    def nextState = new InputStateImpl(id + 1)
  }

  final def main(args: Array[String]): Unit = {
    val ioAction = iomain(args)
    ioAction(new InputStateImpl(0))
  }

  def iomain(args: Array[String]): Action[_] = {
    for {
      _ <- Action(print("Test"))
    } yield ()
  }
}
