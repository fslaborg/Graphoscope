namespace Graphoscope.RandomModels

open Graphoscope

type RegularRingLattice() =

    /// <summary> 
    /// Returns an Undirected graph in a ring lattice in whichall node have the same degree and there are no self loops.
    /// </summary>
    /// <param name="n">Specifies the number of nodes</param>
    /// <param name="k">Specifies the degree of the nodes</param>
    /// <returns>An UndirectedGraph</returns>

    static member initUndirectedGraph (n: int) (k: int) =
        // Following standard practice,
        // odd values of k will be rounded down to the previous even integer.
        let g:  UndirectedGraph<int, float> =
            UndirectedGraph.empty
            |> UndirectedGraph.addNodes [|0 .. n-1|]

        // Connect each node to its half-k succeeding neighbors
        for i in 0 .. n-1 do
            for j in i+1 .. i+(k/2) do
                UndirectedGraph.addEdge (i, j%n, 1.) g|>ignore
        g