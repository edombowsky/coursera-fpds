package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("neighborsWithHistory should correctly return all valid neighbouring blocks") {
    new Level1 {
      val block = Block(Pos(1, 1), Pos(1, 1))
      val history = List(Left, Up)
      val neighbors = Stream(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))
      assert(neighborsWithHistory(block, history) == neighbors)
    }
  }

  test("newNeighborsOnly should return neighbours that are not explored before") {
    new Level1 {
      val neighbors = Stream(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))
      val explored = Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      val newNeighbors = Stream((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))
      assert(newNeighborsOnly(neighbors, explored) == newNeighbors)
    }
  }

  test("find neighbours") {
    new Level1 {
      val actual = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet
      val expected = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(actual === expected)
    }
  }
  
  test("Avoid loops") {
    new Level1 {
      val actual = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )

      val expected = Set(
                       (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
                     ).toStream
    }
  }
  
  trait SmallLevel extends SolutionChecker {
    val level = """oo---
                  |ooSoo
                  |oo-oo
                  |---oo""".stripMargin
  }

  test("from start small") {
    new SmallLevel {
      val initial: Stream[(Block, List[Move])] = Stream((startBlock, List()))
      val explored: Set[Block] = Set(startBlock)
      val observedResult = from(initial, explored).toList
      val expectedResult: List[(Block, List[Move])] = List(
          (Block(Pos(1, 2), Pos(1, 2)), List()),
          (Block(Pos(1, 0), Pos(1, 1)), List(Left)),
          (Block(Pos(1, 3), Pos(1, 4)), List(Right)),
          (Block(Pos(0, 0), Pos(0, 1)), List(Up,Left)),
          (Block(Pos(2, 0), Pos(2, 1)), List(Down,Left)),
          (Block(Pos(2, 3), Pos(2, 4)), List(Down,Right)),
          (Block(Pos(3, 3), Pos(3, 4)), List(Down,Down,Right))
      )
      assert(observedResult == expectedResult)
    }
  }
}
