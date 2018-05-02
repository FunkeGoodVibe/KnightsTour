// Part 3 about finding a single tour using the Warnsdorf Rule
//=============================================================

// copy any function you need from files knight1.scala and
// knight2.scala

object CW7c {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(3a) Complete the function that calculates a list of onward
//     moves like in (1b) but orders them according to Warnsdorf’s 
//     rule. That means moves with the fewest legal onward moves 
//     should come first.

def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {        
        
    if ((x._1 < 0) || (x._2 < 0) || (x._1 >= dim) || (x._2 >= dim) || (path.contains(x))) {     
        false
    } else {
        true
    }
}
def moves_from_here(dim: Int, path: Path, x: Pos) : List[Pos] = {

        val m1 = (x._1 + 1, x._2 + 2)
        val m2 = (x._1 + 2, x._2 + 1)
        val m3 = (x._1 + 2, x._2 - 1)
        val m4 = (x._1 + 1, x._2 - 2)
        val m5 = (x._1 - 1, x._2 - 2)
        val m6 = (x._1 - 2, x._2 - 1)
        val m7 = (x._1 - 2, x._2 + 1)
        val m8 = (x._1 - 1, x._2 + 2)
        
        List (m1,m2,m3,m4,m5,m6,m7,m8)
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
        
        for (n <- moves_from_here(dim,path,x); if is_legal(dim,path)(n)) yield n         
} 

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
    
    if (xs == Nil) 
        None
    else {
        //println(xs)
        val v = f(xs.head)
        
        if (v != None) {
             v
        } else {
            first(xs.tail, f)
        }
    }
}

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {

    //def ordered_moves(boardsize, visitedPath, pos):
    val my_legal_moves = legal_moves(dim, path, x)

    // Attach the number of onward moves to each move 
    type myLegalMoveSizePair = (Pos, Int) //store the position, number of legal moves PAIR

    val myorder = for (n <- my_legal_moves) yield (n, legal_moves(dim,path,n).size)//the current position, and size of legal moves
    val sort = myorder.sortWith(_._2 < _._2)
    val new_sorted = for (x <- sort) yield x._1

    new_sorted
}
    
def first_tour(dim: Int, path: Path) : Option[Path] = {
     //println("first_tour", path)
    if (path.size == dim * dim) {
         Some(path)       
    }
    else 
        first(ordered_moves(dim, path, path.head), (x:Pos) => first_tour(dim , x::path))
}

def closed_first_tour(dim: Int, path: Path) : Option[Path] = {
        
    if (path.size == dim * dim) {
        val mfh = moves_from_here(dim, path, path.head)

             if (mfh.contains(path.last)) {
                Some(path)
            }else
                 first(ordered_moves(dim, path, path.head), (x:Pos) => closed_first_tour(dim , x::path))      
    
    }else 
        first(ordered_moves(dim, path, path.head), (x:Pos) => closed_first_tour(dim , x::path))
}

def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
        closed_first_tour(dim, path)
}

def first_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
     first_tour(dim, path)
}

}
