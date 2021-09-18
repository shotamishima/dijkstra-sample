package dijkstra

final case class DirectedEdge(from: Int, to: Int, weight: Double)
final case class EdgeWeightedDigraph(adj: Map[Int, List[DirectedEdge]] = Map.empty)

object EdgeWeightedDigraphOps {

    implicit class EdgeWeightedDigraphOps(g: EdgeWeightedDigraph) {

        def addEdge(e: DirectedEdge): EdgeWeightedDigraph = {
            val list = g.adj.getOrElse(e.from, List.empty)
            val adj = g.adj + (e.from -> (list :+ e))
            EdgeWeightedDigraph(adj)
        }
    }
}



