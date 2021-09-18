package dijkstra

import scala.collection.mutable
import scala.annotation.tailrec

object ShortestPath {

    def run(g: EdgeWeightedDigraph, sourceV: Int): Either[String, ShortestPathCalc] = {
        // Either[Left, Right] => Left is Error, Right is Success
        
        val size = g.adj.size

        if (sourceV >= size) Left(s"Source vertex must in range [0, $size)")
        // sourceV is index of Vertex

        else {
            val edgeTo = mutable.ArrayBuffer.fill[Option[DirectedEdge]](size)(None)
            val distTo = mutable.ArrayBuffer.fill(size)(Double.PositiveInfinity)

            // init source distance and add to the queue
            distTo(sourceV) = 0.0
            val sourceDist = (sourceV, distTo(sourceV))
            val sortByWeight: Ordering[(Int, Double)] = (a, b) => a._2.compareTo(b._2)
            // a compareTo b -> case a > b => return 1, case a < b => return -1
            val queue = mutable.PriorityQueue[(Int, Double)](sourceDist)(sortByWeight)
            // int: index of vertex, double: weight, 
            while (queue.nonEmpty) {
                val (minDestV, _) = queue.dequeue()
                val edges = g.adj.getOrElse(minDestV, List.empty)

                edges.foreach { e =>
                    if (distTo(e.to) > distTo(e.from) + e.weight) {
                        distTo(e.to) = distTo(e.from) + e.weight
                        edgeTo(e.to) = Some(e)
                        if (!queue.exists(_._1 == e.to)) queue.enqueue((e.to, distTo(e.to)))
                    }
                }
            }
            // println("********************")
            // println("edgeTo: " + edgeTo)
            // println("*******************")
            // println("distTo:" + distTo(3))
            // println("********************")
            Right(new ShortestPathCalc(edgeTo, distTo))
        }
    }
}

class ShortestPathCalc(edgeTo: mutable.ArrayBuffer[Option[DirectedEdge]], distTo: mutable.ArrayBuffer[Double]) {
    def pathTo(v: Int): Either[String, Seq[DirectedEdge]] = {
        @tailrec // 末尾再帰
        def go(list: List[DirectedEdge], vv: Int): List[DirectedEdge] = 
            edgeTo(vv) match {
                case Some(e) => go(e +: list, e.from)
                case None => list
            }
        hasPath(v).map(b => if(!b) Seq() else go(List(), v)) 
    }

    def hasPath(v: Int): Either[String, Boolean] = 
        distTo.lift(v).map(_ < Double.PositiveInfinity).toRight(s"Vertex $v does not exist")

    def distToV(v: Int): Either[String, Double] = 
        distTo.lift(v).toRight(s"Vertex $v does not exist.")
}