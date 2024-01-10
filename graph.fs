namespace graph
open System.Collections.Generic

type GraphL<'T when 'T: comparison>  = {
   Vertices:  Set<'T>
   Edges: Map<'T * 'T, float> 
}

type GraphExp<'T> = 
   | MakeGraph 
   | AddVertex of 'T 
   | AddEdge of 'T * 'T * float
   | FindShortestPath of 'T * 'T


module GraphL =
   open System
   open System.Collections
   let makeGraph = {
      Vertices = Set.empty;
      Edges = Map.empty
   }

   // Function to add edge in the graph
   let addEdge (source: 'T) (dest: 'T) (weight: float) (graph: GraphL<'T>) = 
      if graph.Vertices.Contains(source) && graph.Vertices.Contains(dest) && not (graph.Edges.ContainsKey(source,dest)) then
         {graph with Edges = Map.add(source, dest) weight graph.Edges;}
      else
         raise (System.Exception("Graph does not contain those vertex/ edge already exists!"));
         graph;

   // Function to add vertex in the graph
   let addVertex (v: 'T) (graph: GraphL<'T>) =
      if not (graph.Vertices.Contains(v)) then
         {graph with Vertices = Set.add(v) graph.Vertices;}
      else
         raise (System.Exception("Vertex already exists in graph!"));
         graph;
   
   // Function to get neighbors of a vertex in the graph
   let getNeighbors (vertex: 'T) (graph: GraphL<'T>) =
      graph.Edges 
      |> Map.filter (fun (src, dest) _ -> src = vertex || dest = vertex)
      |> Map.fold (fun acc (src, dest) weight -> 
         if src = vertex then Set.add dest acc
         else Set.add src acc
      ) Set.empty<'T>
   
    // Function to get weight of the edge in the graph
   let getWeight (source: 'T) (dest: 'T) (graph: GraphL<'T>) =
      let key = source, dest
      match Map.tryFind key graph.Edges with
      | Some weight -> Some weight
      | None -> None

   // A priority queue type
   type PriorityQueue<'T when 'T : comparison>() =
      let heap = new SortedDictionary<'T, int>()
      member this.Enqueue(item, priority) = 
         if not (heap.ContainsKey(item)) then
            heap.Add(item, priority)
         else
            heap.[item] <- priority
      member this.TryDequeue() =
         if heap.Count > 0 then
               let first = heap.Keys |> Seq.head
               let b = heap.Remove(first)
               if b then
                  Some first
               else None
         else None
      member this.isEmpty = 
         heap.Count = 0


    // Function to find the shortest path using Dijkstra's algorithm
   let findShortestPath (startV:'T) (endV:'T) (graph:GraphL<'T>) =

      if not (graph.Vertices.Contains(endV)) ||  not (graph.Vertices.Contains(startV)) then
         raise (System.Exception("Vertex does not exist!"));
      else
         let mutable distances = graph.Vertices |> Seq.map(fun e -> e,  Double.PositiveInfinity) |> Map.ofSeq;
         let vertices = Set.toList (graph.Vertices);
         
         let unvisitedQ = PriorityQueue<'T>();
         
         let mutable predecessor = Map.empty<'T, 'T Option>
         let mutable visitedSet = Set.empty<'T>;
         let visited = ResizeArray<'T>();


         for v in vertices do
            unvisitedQ.Enqueue(v,int Double.PositiveInfinity);
            predecessor <- Map.add v None predecessor
         
         distances <- distances.Change(startV, 
            fun x -> ( match x with 
                     | Some s -> Some 0.0
                     | _ -> None)
         )
         unvisitedQ.Enqueue(startV,0);
         let mutable loopCondition = true 

         // run algorithm until no more vertices to visit:
         // while not unvisitedQ.isEmpty do
         while loopCondition do
            // Pop the vertex from the queue with the *minimum* distance:
            match unvisitedQ.TryDequeue() with
            | Some curV -> if Map.find curV distances = Double.PositiveInfinity then
                              // there's no point in continuing alg if is curV not reachable from src
                              loopCondition <- false
                           elif Set.contains curV visitedSet then // already visited
                              let neighbors = getNeighbors curV graph   // visit adjacent nodes:
                              for kvp in neighbors do
                                 match getWeight curV kvp graph with
                                 | Some weight -> 
                                    let alternatePathDis = distances.[curV] + weight
                                    // found a less-expensive path:
                                    if alternatePathDis < distances.[kvp] then
                                       // update distances, and update priority queue:   
                                       distances <- distances.Change(kvp, 
                                          fun x -> ( match x with 
                                                   | Some s -> Some alternatePathDis
                                                   | _ -> None)
                                       )
                                       unvisitedQ.Enqueue(kvp, int alternatePathDis)
                                       predecessor <- Map.add kvp (Some curV) predecessor
                                 | None -> loopCondition <- true
                           else // visited
                              visitedSet <- Set.add curV visitedSet
                              visited.Add(curV)
                              let neighbors = getNeighbors curV graph
                              for kvp in neighbors do
                                 match getWeight curV kvp graph with
                                 | Some weight -> 
                                    let alternatePathDis = distances.[curV] + weight
                                    if alternatePathDis < distances.[kvp] then
                                       distances <- distances.Change(kvp, 
                                          fun x -> ( match x with 
                                                   | Some s -> Some alternatePathDis
                                                   | _ -> None)
                                       )
                                       unvisitedQ.Enqueue(kvp, int alternatePathDis)
                                       predecessor <- Map.add kvp (Some curV) predecessor
                                 | None -> loopCondition <- true
            | None -> loopCondition <- false
         
         // Print the shortest path from startV to endV
         let rec reconstructPath (endVertex: 'T) (path: 'T list) =
            match Map.tryFind endVertex predecessor with
            | Some (Some prevVertex) -> reconstructPath prevVertex (prevVertex :: path)
            | Some None -> path
            | None -> path
         let shortestPath = reconstructPath endV [endV]
         shortestPath |> List.iter (printfn "%A")

      graph;

