import hw.sudoku._

object Solution extends SudokuLike {
type T = Board

val possibleList= List(0,1,2,3,4,5,6,7,8)
val block0= List(0,1,2)
val block1= List(3,4,5)
val block2= List(6,7,8)
val digits=List(1,2,3,4,5,6,7,8,9)
//Remove the element in the row before you start passing
// You can use a Set instead of a List (or, any Iterable)

def emptyboard(): Board ={
	val keys= 0.to(80).toList.map(x=>(x/9,x%9))
	val boardvals=0.to(80).toList.map(x=>List(1,2,3,4,5,6,7,8,9))
	new Board (keys.zip(boardvals).toMap)
}

//1.to(9).map(row=>1to(9).map(col=>(row,col))).flatten

def parse(str: String): Board = {
	if (str.length > 81|| str.length < 0){
		throw new IllegalArgumentException("wrong board")
	}
	else{
		val newEmptyBoard= emptyboard
		parseHelper(str,0,newEmptyBoard)

	}
}

def parseHelper(str: String, pos: Int, tempBoard: Board): Board = { //pos should start with 0 end at 81
if(pos< 81){
	val keyrow=pos/9
	val keycol=pos%9
	if(str(pos) =='.'){
		parseHelper(str,pos+1,tempBoard)
	}//closing if
	else
	 parseHelper(str,pos+1,tempBoard.place(keyrow,keycol,str(pos).asDigit))

}//closing outer if of pos
else
tempBoard
}//closing parseHelper


def peers(row: Int, col: Int): List[(Int, Int)] = {
	//List((5,6),(6,7))
	val rowlist= makeList(row,0)//makes a list of rows 8 times
	val columnList=makeList(col,0)
	val rowCoordinateList= rowlist.zip(0.to(8).toList).toSet//.filter(_!=((row,col)))
	val colCoordinateList= (0.to(8).toList).zip(columnList).toSet//.filter(_!=((row,col)))
	val coordSet=Set((row,col))
	// (rowCoordinateList::colCoordinateList)
	// filter out coordinates that make each box
	val rowcol= rowCoordinateList.union(colCoordinateList)
	val rowfilter= (row%9)/3
	val colfilter= (col%9)/3

	if((rowfilter==0) && (colfilter==0)){
		val BoxList = rowBoxHelper(block0).toSet.toList.intersect(colBoxHelper(block0).toSet.toList).toSet
		((rowcol.union(BoxList)).diff(coordSet)).toList
	}
	
	else if(rowfilter==0&&colfilter==1){
		val BoxList = (rowBoxHelper(block0).toSet.toList.intersect(colBoxHelper(block1).toSet.toList)).toSet
		((rowcol.union(BoxList)).diff(coordSet)).toList
	}

	else if(rowfilter==0&&colfilter==2){
		val BoxList = (rowBoxHelper(block0).toSet.toList.intersect(colBoxHelper(block2).toSet.toList)).toSet
		((rowcol.union(BoxList)).diff(coordSet)).toList
	}
	
	else if(rowfilter==1&&colfilter==0){
		val BoxList = (rowBoxHelper(block1).toSet.toList.intersect(colBoxHelper(block0).toSet.toList)).toSet
		((rowcol.union(BoxList)).diff(coordSet)).toList
	}
	
	else if(rowfilter==1&&colfilter==1){
		val BoxList = (rowBoxHelper(block1).toSet.toList.intersect(colBoxHelper(block1).toSet.toList)).toSet
		((rowcol.union(BoxList)).diff(coordSet)).toList
	}
	
	else if(rowfilter==1&&colfilter==2){
		val BoxList = (rowBoxHelper(block1).toSet.toList.intersect(colBoxHelper(block2).toSet.toList)).toSet
		((rowcol.union(BoxList)).diff(coordSet)).toList
	}
	
	else if(rowfilter==2&&colfilter==0){
		val BoxList = (rowBoxHelper(block2).toSet.toList.intersect(colBoxHelper(block0).toSet.toList)).toSet
		((rowcol.union(BoxList)).diff(coordSet)).toList
	}
	
	else if(rowfilter==2&&colfilter==1){
		val BoxList = (rowBoxHelper(block2).toSet.toList.intersect(colBoxHelper(block1).toSet.toList)).toSet
		((rowcol.union(BoxList)).diff(coordSet)).toList
	}
	
	else{
		val BoxList = (rowBoxHelper(block2).toSet.toList.intersect(colBoxHelper(block2).toSet.toList)).toSet
	 ((rowcol.union(BoxList)).diff(coordSet)).toList
	}
	
}//closing peers

def makeList(n:Int,x:Int): List[Int]={
	if(x < 9) n::makeList(n,x+1)
	else Nil
}

def rowBoxHelper(list: List[Int]):List[(Int,Int)]={
	list match{
		case Nil=> Nil
		case h::t=> makeList(h,0).zip(possibleList)++rowBoxHelper(t)
	}
}

def colBoxHelper(list: List[Int]):List[(Int,Int)]={
	list match{
		case Nil=> Nil
		case h::t=> possibleList.zip((makeList(h,0)))++colBoxHelper(t)
	}
}

}//closing solution






class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] 
{
 def availableValuesAt(row: Int, col: Int): List[Int] ={
// // Assumes that a missing value means all values are available. Feel // free to change this.
 available.getOrElse((row, col), 1.to(9).toList)
 }

def valueAt(row: Int, col: Int): Option[Int] = {

	if(row < 0|| row > 8|| col < 0|| col >8)
	throw new IllegalArgumentException("Wrong coordinates, not available on the board")

	else if(available((row,col)).length == 1) Some(available((row,col))(0)) 
	
	else None
} 


//each element of map has one element in the list
def isSolved (): Boolean = {
	available.values.forall(_.length ==1)
}

def isUnsolvable (): Boolean = {
		available.values.exists(_.length ==0)
}






def place(row: Int, col: Int, value: Int): Board = {
// (a) Remove value from set of available values of each peer (i.e., peers(row, col)).
// (b) While doing (1), if a peer is constrained to exactly 1 value, remove that value from its peers.
 		require(availableValuesAt(row, col).contains(value)) 
 		val oldBoard = new Board(available)
		val newtempMap= available + ((row,col) -> List(value))
		val tempcoordinates=Solution.peers(row,col)
		val newBoaredAfterRemovingFromPeers=placeHelper(tempcoordinates,(available + ((row,col) -> List(value))),value)
		(boardComparer(tempcoordinates,oldBoard,newBoaredAfterRemovingFromPeers,value))
		 //transverse through newMap to eliminate other possiblities
}//closing place
//removes the value from the list
def placeHelper(peerscor: List[(Int,Int)], tempMani: Map[(Int, Int), List[Int]], value: Int):  Board={
	peerscor match{
		case Nil => new Board(tempMani)
		case h::t =>{
					if(tempMani(h).contains(value)){
						val x= (tempMani((h)).toSet).diff(Set(value)).toList
						//length se compare karke place call karna padega
					placeHelper(t,tempMani+ (h-> x),value)
					}
		 			else {
		 				placeHelper(t,tempMani,value)
		 			}

		} 

	}
}

def boardComparer(possList: List[(Int,Int)], board : Board, newBoard: Board, value: Int): Board = {
	possList match {
					case (Nil) => newBoard
					case head :: tail => {if (newBoard.availableValuesAt(head._1, head._2).length == 1)
											   {
											   	if (board.availableValuesAt(head._1, head._2).length > 1)
											   	newBoard.place(head._1, head._2, newBoard.valueAt(head._1, head._2).get)
												else
												boardComparer(tail, board, newBoard, value)
												}
										else
										boardComparer(tail, board, newBoard, value)
									}
				}//closing pattern match
}



// You can return any Iterable (e.g., Stream)
def nextStates(): List[Board] = { 
	if (isUnsolvable()||isSolved) {
		List () 
	}
	else {
		//get a list of all the possible values at each coordinate
		//coordinates
		val keys= 0.to(80).toList.map(x=>(x/9,x%9))
		//HAVENT DONE THE SORTINGT THING
		nextStatesHelper(keys,available)
		// ListMap(nextStatesUnsorted.toSeq.sortWith(_._2.size < _._2.size):_*)
		//sum ((k,v,))
	} 
}
def nextStatesHelper(keys: List[(Int,Int)],available: Map[(Int, Int), List[Int]]): List[Board]={
	keys match{
			case Nil=> Nil
			case h::t=>{
					if(available(h).size > 1){
							listHelper(available(h),h) ::: nextStatesHelper(t,available)
						}
						else
						nextStatesHelper(t,available)
					}
						

				}
}
def listHelper(alist: List[Int],coordinates: (Int,Int)): List[Board]={
	alist match {
		case Nil => Nil
		case _ => {
					place(coordinates._1,coordinates._2,alist(0)) :: listHelper(alist.tail,coordinates)
					}

	}
}

// def sort(BoardList: List[Board]): List[Board]={
// 	BoardList match{
// 		case Nil=> Nil
// 		case h::h2:t=> 
// 	}
// }

//returns the list of new Boards with exactly one value placed
// sort the returned List to ensure that he boards with fewer available values occur earlier in the lists

def solve(): Option[Board] = 
 {
	if(isUnsolvable)
	None
	else if(isSolved())
	Some(this)

	else{
		solveHelper(this.nextStates)
	}


}//closing solve

//nextStates is a list of Boards
//extract each board and call solve on it
//if it returns Some(this)then return else Return None




def solveHelper(boardtosolve: List[Board]): Option[Board]={
	boardtosolve match{
		case Nil=> None
		case h::t=> { 
						val temp = h.solve
						temp match{
							case None => solveHelper(t)
							case Some(x)=> Some(x)
						}
		}
	}
}
}//








