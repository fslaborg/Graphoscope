namespace Graphoscope.RandomModels

open Graphoscope
open Graphoscope.Graphs

type RegularRingLattice() =

    /// <summary> 
    /// Returns an Undirected graph in a ring lattice in whichall node have the same degree and there are no self loops.
    /// </summary>
    /// <param name="n">Specifies the number of nodes</param>
    /// <param name="k">Specifies the degree of the nodes</param>
    /// <returns>An Undirected.UndirectedGraph</returns>

    static member initUndirectedGraph (n: int) (k: int) =
        // Following standard practice,
        // odd values of k will be rounded down to the previous even integer.
        let g:  Undirected.UndirectedGraph<int, _, float> =
            Undirected.UndirectedGraph.empty
            |> Undirected.UndirectedGraph.addNodes (Array.init n (fun i -> i, i))

        // Connect each node to its half-k succeeding neighbors
        for i in 0 .. n-1 do
            for j in i+1 .. i+(k/2) do
                Undirected.UndirectedGraph.addEdge (i, j%n, 1.) g|>ignore
        g