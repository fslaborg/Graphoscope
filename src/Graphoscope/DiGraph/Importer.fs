namespace Graphoscope.DiGraph

module Import =

    open FSharp.Data
    open Graphoscope.DiGraph

    let private nodesFromEdges(edges: array<'Node * 'Node * float>) : array<'Node> = 
        edges
        |> Array.map(fun (orig, _, _) -> orig) 
        |> Array.append (edges |> Array.map(fun (_, dest, _)  -> dest))
        |> Array.distinct
        |> Array.sort

    let private createGraph (edges: array<'Node * 'Node * float>) = 
        Constructors.createFromEdges edges

    let private createFromUnWeightedEdgeList (edges: array<'Node * 'Node>) : DiGraph<'Node, float> = 
        edges
        |> Array.map(fun (f,t) -> (f,t,1.0) )
        |> createGraph

    let private createFromWeightedEdgeList (edges: array<'Node * 'Node * float>) : DiGraph<'Node, float> = 
        edges
        |> createGraph

    /// creates a graph from a edge list file. 
    let createFromEdgeList (fullpath: string) (delimiter: string) (headerRows: int) (weightsIncluded: bool) =
        let rows  = CsvFile.Load(fullpath, delimiter, skipRows = headerRows, hasHeaders = false).Rows

        if  weightsIncluded then
            rows
            |> Seq.map (fun row -> row[0], row[1], float row[2] )
            |> Seq.toArray
            |> createFromWeightedEdgeList
        else 
            rows
            |> Seq.map (fun row -> row[0], row[1])
            |> Seq.toArray
            |> createFromUnWeightedEdgeList