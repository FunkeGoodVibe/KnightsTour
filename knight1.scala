// Part 1 about finding and counting Knight's tours
//==================================================

object CW7a {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1a) Complete the function that tests whether the position 
//     is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {		
	
	if ((x._1 > -1) && (x._2 > -1) && (x._1 < dim) && (x._2 < dim) && !(path.contains(x))){	 
		true
	} else {
		false
	}
	
}

//(1b) Complete the function that calculates for a position 
//     all legal onward moves that are not already in the path. 
//     The moves should be ordered in a "clockwise" manner.
 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {

		//All possible moves positions in clockwise order 
			val m1 = (x._1 + 1, x._2 + 2)
			val m2 = (x._1 + 2, x._2 + 1)
			val m3 = (x._1 + 2, x._2 - 1)
			val m4 = (x._1 + 1, x._2 - 2)
			val m5 = (x._1 - 1, x._2 - 2)
			val m6 = (x._1 - 2, x._2 - 1)
			val m7 = (x._1 - 2, x._2 + 1)
			val m8 = (x._1 - 1, x._2 + 2)

		for (n <- List (m1,m2,m3,m4,m5,m6,m7,m8); if is_legal(dim,path)(n)) yield n
		
}


//some test cases
/*
assert(legal_moves(8, Nil, (2,2)) == List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))
*/

//(1c) Complete the two recursive functions below. 
//     They exhaustively search for knight's tours starting from the 
//     given path. The first function counts all possible tours, 
//     and the second collects all tours in a list of paths.


def count_tours(dim: Int, path: Path) : Int = {

	enum_tours(dim, path).size

}

def enum_tours(dim: Int, path: Path) : List[Path] = {

	if (path == Nil){

		Nil
	}
	else if (path.size == dim * dim) {

		List(path)

	} else {

			val currentHeadListOfLegalMoves = legal_moves(dim, path, path.head)  //TYPE == LIST( i.e. of Pos) ~8 legal moves (HEAD)

			(for (n <- currentHeadListOfLegalMoves) yield enum_tours(dim, n::path)).flatten	//TYPE == LIST (ofList ofPos's)  //update the path head, with legal moves
		

	}

}
	
}




