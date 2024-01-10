// Snigdha Ghosh Dastidar

open System
open System.Collections.Generic
open System.Collections

let rec eval (g: graph.GraphL<'T>) (e: graph.GraphExp<'T>) =
   match e with
   | graph.MakeGraph -> graph.GraphL.makeGraph
   | graph.AddVertex v -> graph.GraphL.addVertex v g
   | graph.AddEdge (s, d, w) -> graph.GraphL.addEdge s d w g
   | graph.FindShortestPath( startV, endV) -> graph.GraphL.findShortestPath startV endV g


[<EntryPoint>]
let main argv =

   // example 1
   //        B
   //  5 /  5|  \ 10
   // A --5- D -1- E
   // 10 \   | 2
   //        C

   let mutable g = graph.GraphL.makeGraph;
   g <- eval g (graph.AddVertex "A")
   g <- eval g (graph.AddVertex "B")
   g <- eval g (graph.AddVertex "C")
   g <- eval g (graph.AddVertex "D")
   g <- eval g (graph.AddVertex "E")
   g <- eval g (graph.AddEdge ("A", "B", 5.0))
   g <- eval g (graph.AddEdge ("A", "D", 15.0))
   g <- eval g (graph.AddEdge ("A", "C", 10.0))
   g <- eval g (graph.AddEdge ("B", "E", 10.0))
   g <- eval g (graph.AddEdge ("D", "E", 1.0))
   g <- eval g (graph.AddEdge ("D", "B", 5.0))
   g <- eval g (graph.AddEdge ("C", "D", 2.0))

   printfn "Vertices: %A" g.Vertices
   printfn "Edges: %A" g.Edges
   printfn "Path: "
   let s = eval g (graph.FindShortestPath("A", "E"))


   // example 2
   //      B 
   //  10/ | \20
   // A   30  C
   //      | /70
   //      D

   // let mutable g2 = graph.GraphL.makeGraph;
   // g2 <- eval g2 (graph.AddVertex "A")
   // g2 <- eval g2 (graph.AddVertex "B")
   // g2 <- eval g2 (graph.AddVertex "C")
   // g2 <- eval g2 (graph.AddVertex "D")
   // g2 <- eval g2 (graph.AddEdge ("A", "B", 10.0))
   // g2 <- eval g2 (graph.AddEdge ("B", "C", 20.0))
   // g2 <- eval g2 (graph.AddEdge ("B", "D", 30.0))
   // g2 <- eval g2 (graph.AddEdge ("C", "D", 70.0))

   // printfn "Vertices: %A" g2.Vertices
   // printfn "Edges: %A" g2.Edges
   // printfn "Path: "
   // let s = eval g2 (graph.FindShortestPath("A", "D"))

   0 // return an integer exit code
