// Part 2 about finding a single tour for a board
//================================================

// copy any function you need from file knight1.scala

object CW7b {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


//(2a) Implement a first-function that finds the first 
//     element, say x, in the list xs where f is not None. 
//     In that case Return f(x), otherwise None. If possible,
//     calculate f(x) only once.

/*def f(pos: Pos) : Option[Path] = {

	val myList = List((2,2), (9,9), (2,4))
	myList.find(myList._1 > 8, myList._2 > 8)

}*/
def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {		
	
	if ((x._1 < 0) || (x._2 < 0) || (x._1 > dim) || (x._2 > dim) || (path.contains(x))){	 
		false
	} else {
		true

	}
	
}
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

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {

	if (xs == Nil) None

	else {

		if (f(xs.head) != None){

			f(xs.head)

		} else {

			first(xs.tail, f)

		}

	}

}

//(2b) Implement a function that uses the first-function for
//     trying out onward moves, and searches recursively for a
//     knight tour on a dim * dim-board.


def first_tour(dim: Int, path: Path) : Option[Path] = {

	//if (path == Nil) None
	//if (path.filter(_ == None).size > 0) None
	if (path.size == dim * dim) Some(path)

	else first( legal_moves(dim, path, path.head), 
				(x:Pos) => first_tour(dim , x::path)
													);

}



}

