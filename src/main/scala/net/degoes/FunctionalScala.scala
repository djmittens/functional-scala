package net.degoes

import scalaz.zio._
import scalaz.zio.console._

object FunctionalScala extends App {
  /*
  Tick tack toe, start with the data model.
   */

  case class Position private(x: Int, y: Int)
  object Position {
    def apply(x: Int, y: Int): Option[Position] = {
      if((0 to 3).contains(x) && (0 to 3).contains(y))
        Some(new Position(x, y))
      else
        None
    }
  }

  trait Piece {}

  final object Piece {
    final case object X extends Piece
    final case object O extends Piece
  }

  case class Action(position: Position, piece: Piece)

  trait Query[+A] { self =>
    type S

    def zero: A

    def step(s: S, action: Action): Either[S, A]

    final def zip[B] (that: Query[B]): Query[(A, B)] = {
      ???
    }
  }

  object Query {
  }


  case class Board(actions: List[Action]) {
    def isValid(a: Action): Boolean = {
      !actions.exists(_.position == a.position)
    }

    def applyMove(a: Action): Option[Board] = {
      if(isValid(a))
        Some(copy(actions = a :: actions))
      else
        None
    }

    def hasWon(p: Piece): Boolean = {
      ???
    }

  }



  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Hello World!")
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
}
