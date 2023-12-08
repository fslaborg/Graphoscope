namespace Graphoscope.Measures

open Graphoscope
open Graphoscope.Graphs
open System.Collections.Generic

/// <summary> 
/// Computes the graph density
/// </summary>
type InformationEntropy() =
    
    /// <summary> 
    /// Computes the information entropy of the given FContextMap <paramref name="graph"/>.
    /// </summary>
    /// <param name="labelF">The function to get the desired information for the entropy of the nodedata.</param>
    /// <param name="graph">The graph for which to compute the information entropy.</param>
    /// <param name="n">The node for which to compute the information entropy.</param>
    /// <remarks> This calculation does not consider self loops </remarks>
    /// <returns>
    /// The information entropy of the given node .
    /// </returns> 
    static member ofFContextMap (labelF:'NodeData -> 'Information) (graph: Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) (n:'NodeKey) =
        let r = 
            graph.Item n
            |> Directed.FContext.neighbours 
            |> Seq.choose(fun (nk,ed) -> 
                if nk=n then None
                else
                    Some (
                        nk,
                        graph.Item nk
                        |>fun (s,d,p) -> d
                        |>labelF 
                    )
            )
            |> Seq.distinctBy fst
        let countAll = Seq.length r|>float
        
        r
        |> Seq.groupBy snd
        |> Seq.sumBy(fun (bin,v) ->
            let counts      = Seq.length v|>float
            let p           = counts / countAll
            let surprise    = System.Math.Log10((1./p))
            p * surprise
        )  

    /// <summary> 
    /// Computes the information entropy of the given UndirectedFContext <paramref name="graph"/>.
    /// </summary>
    /// <param name="labelF">The function to get the desired information for the entropy of the nodedata.</param>
    /// <param name="graph">The graph for which to compute the information entropy.</param>
    /// <param name="n">The node for which to compute the information entropy.</param>
    /// <remarks> This calculation does not consider self loops </remarks>
    /// <returns>
    /// The information entropy of the given node .
    /// </returns> 
    static member OfUndirectedFContextMap (labelF:'NodeData -> 'Information) (graph:Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) (n:'NodeKey) =
        let r = 
            Undirected.FContext.neighbours graph.[n] 
            |> Seq.choose(fun (nk,ed) -> 
                if nk=n then None
                else
                    Some (
                        nk,
                        graph.Item nk
                        |>fun (p,d,s) -> d
                        |>labelF 
                    )
            )
            |> Seq.distinctBy fst
        let countAll = Seq.length r|>float
        
        r
        |> Seq.groupBy snd
        |> Seq.sumBy(fun (bin,v) ->
            let counts      = Seq.length v|>float
            let p           = counts / countAll
            let surprise    = System.Math.Log10((1./p))
            p * surprise
        )  
              

    //TODO
    /// <summary> 
    /// Computes the information entropy of the given FContextMap <paramref name="graph"/>.
    /// </summary>
    /// <param name="labelF">The function to get the desired information for the entropy of the nodedata.</param>
    /// <param name="graph">The graph for which to compute the information entropy.</param>
    /// <param name="n">The node for which to compute the information entropy.</param>
    /// <remarks> This calculation does not consider self loops </remarks>
    /// <returns>
    /// The information entropy of the given node .
    /// </returns> 
    static member ofLilMatrix (labelF:'NodeData -> 'Information) (graph: Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) (n:'NodeKey) =   
        System.NotImplementedException() |> raise

    /// <summary> 
    /// Computes the information entropy of the given FContextMap <paramref name="graph"/>.
    /// </summary>
    /// <param name="labelF">The function to get the desired information for the entropy of the nodedata.</param>
    /// <param name="graph">The graph for which to compute the information entropy.</param>
    /// <param name="n">The node for which to compute the information entropy.</param>
    /// <remarks> This calculation does not consider self loops </remarks>
    /// <returns>
    /// The information entropy of the given node .
    /// </returns> 
    static member compute ((labelF:'NodeData -> 'Information),(graph: Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>),(n:'NodeKey)) =
        InformationEntropy.ofLilMatrix labelF graph n

    
    /// <summary> 
    /// Computes the information entropy of the given FContextMap <paramref name="graph"/>.
    /// </summary>
    /// <param name="labelF">The function to get the desired information for the entropy of the nodedata.</param>
    /// <param name="graph">The graph for which to compute the information entropy.</param>
    /// <param name="n">The node for which to compute the information entropy.</param>
    /// <remarks> This calculation does not consider self loops </remarks>
    /// <returns>
    /// The information entropy of the given node .
    /// </returns> 
    static member compute ((labelF:'NodeData -> 'Information),(graph: Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>),(n:'NodeKey)) =
        InformationEntropy.ofFContextMap labelF graph n

    /// <summary> 
    /// Computes the information entropy of the given FContextMap <paramref name="graph"/>.
    /// </summary>
    /// <param name="labelF">The function to get the desired information for the entropy of the nodedata.</param>
    /// <param name="graph">The graph for which to compute the information entropy.</param>
    /// <param name="n">The node for which to compute the information entropy.</param>
    /// <remarks> This calculation does not consider self loops </remarks>
    /// <returns>
    /// The information entropy of the given node .
    /// </returns> 
    static member computeUndirected ((labelF:'NodeData -> 'Information),(graph:Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>),(n:'NodeKey)) =
        InformationEntropy.OfUndirectedFContextMap labelF graph n
