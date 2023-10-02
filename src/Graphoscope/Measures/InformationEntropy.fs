namespace Graphoscope.Measures

open Graphoscope
open System.Collections.Generic

/// <summary> 
/// Computes the graph density
/// </summary>
type InformationEntropy() =
    
    /// <summary> 
    /// Computes the information entropy of the given FGraph <paramref name="graph"/>.
    /// </summary>
    /// <param name="labelF">The function to get the desired information for the entropy of the nodedata.</param>
    /// <param name="graph">The graph for which to compute the information entropy.</param>
    /// <param name="n">The node for which to compute the information entropy.</param>
    /// <remarks> This calculation does not consider self loops </remarks>
    /// <returns>
    /// The information entropy of the given node .
    /// </returns> 
    static member ofFGraph (labelF:'NodeData -> 'Information) (graph:FGraph<'NodeKey,'NodeData,'EdgeData>) (n:'NodeKey) =
        let r = 
            graph.Item n
            |> FContext.neighbours 
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

    //TODO
    /// <summary> 
    /// Computes the information entropy of the given FGraph <paramref name="graph"/>.
    /// </summary>
    /// <param name="labelF">The function to get the desired information for the entropy of the nodedata.</param>
    /// <param name="graph">The graph for which to compute the information entropy.</param>
    /// <param name="n">The node for which to compute the information entropy.</param>
    /// <remarks> This calculation does not consider self loops </remarks>
    /// <returns>
    /// The information entropy of the given node .
    /// </returns> 
    static member ofDiGraph (labelF:'NodeData -> 'Information) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) (n:'NodeKey) =   
        System.NotImplementedException() |> raise

    /// <summary> 
    /// Computes the information entropy of the given FGraph <paramref name="graph"/>.
    /// </summary>
    /// <param name="labelF">The function to get the desired information for the entropy of the nodedata.</param>
    /// <param name="graph">The graph for which to compute the information entropy.</param>
    /// <param name="n">The node for which to compute the information entropy.</param>
    /// <remarks> This calculation does not consider self loops </remarks>
    /// <returns>
    /// The information entropy of the given node .
    /// </returns> 
    static member compute ((labelF:'NodeData -> 'Information),(graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>),(n:'NodeKey)) =
        InformationEntropy.ofDiGraph labelF graph n

    
    /// <summary> 
    /// Computes the information entropy of the given FGraph <paramref name="graph"/>.
    /// </summary>
    /// <param name="labelF">The function to get the desired information for the entropy of the nodedata.</param>
    /// <param name="graph">The graph for which to compute the information entropy.</param>
    /// <param name="n">The node for which to compute the information entropy.</param>
    /// <remarks> This calculation does not consider self loops </remarks>
    /// <returns>
    /// The information entropy of the given node .
    /// </returns> 
    static member compute ((labelF:'NodeData -> 'Information),(graph:FGraph<'NodeKey,'NodeData,'EdgeData>),(n:'NodeKey)) =
        InformationEntropy.ofFGraph labelF graph n

    