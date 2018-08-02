import hw.sudoku._
import Solution._


class TrivialTestSuite extends org.scalatest.FunSuite {
	test("The solution object must be defined") {
	val obj : hw.sudoku.SudokuLike = Solution
		}
val  goodBoard = "85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58"
val almostFilledBoard="534678912672195348198342567859761423426853791713924856961537284287...635345286179"
val boardfun="2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
val board1234="..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.."
def emptyboard(): Board ={
val keys= 0.to(80).toList.map(x=>(x/9,x%9))
val boardvals=0.to(80).toList.map(x=>List(1,2,3,4,5,6,7,8,9))
	new Board (keys.zip(boardvals).toMap)
}

val pty = emptyboard()
		test("peers"){	
			assert(peers(1,1) == List((0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2),(3,1),
				(4,1), (5,1), (6,1), (7,1), (8,1), (1,3), (1,4),(1,5),(1,6),(1,7),(1,8)))
		}

		test("new board"){
			assert(pty.place(0,0,2).place(0,1,1).place(0,2,3).place(0,3,4).place(0,4,5).place(0,5,6).place(0,6,7).place(0,7,8)
			 == new Board(Map((8,8) -> List(1))))
		}

			test("parse"){
				assert(parse(goodBoard)
				 == new Board(Map((8,8) -> List(1))))
			}

			// test("nextStates 1"){
			// 	assert((parse(goodBoard).nextStates)== 2)
			// }
			//why is this returning an empty list
			// test("nextStates 2"){
			// 	assert((parse(board1234).nextStates) == 3)
			// }
			// test("solve"){
			// 	assert(parse(goodBoard).solve ==  Some(almostFilledBoard))
			// }
			// test("solve 2"){
			// 	assert(parse(goodBoard).solve ==  Some(almostFilledBoard))
			// }
			// test("solve hard"){
			// 	assert(parse(boardHardtRY).solve == Some(board1234))
			// }
}