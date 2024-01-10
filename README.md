# Embedded-DSL-for-Discrete-Mathematics

Domain Specific Language for discrete mathematics - graph theory - Dijkstra's algorithm in F#.

The main gist of this project is to find the shortest path between two vertices based on the weight of the
edges of an undirected graph. The function prints out the final shortest path.

File graph.fs is to define the graph, priority queue and expressions. I created a module graph and defined
the functions such as initializing the graph, adding vertices, edges, and finding the shortest path.
Implemented Dijkstra’s algorithm, priority queue using sorted dictionary and helping functions for the
algorithm. I used predecessor to keep track of the shortest path.
File program.fs has the evaluate function that calls the respective graph functions to do what the user
wants. The user has to make the graph by calling the eval function to add nodes and edges with weight.
Then calling FindShortestPath, gives the path.
