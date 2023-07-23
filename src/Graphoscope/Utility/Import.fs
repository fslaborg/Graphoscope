namespace Graphoscope.Utility

module Import =

    open FSharp.Data
    open Graphoscope
    open System

    let private importRows (fullpath: string) (delimiter: string) (headerRows: int) (weightsIncluded: bool)  = 
        let rows  = CsvFile.Load(fullpath, delimiter, skipRows = headerRows, hasHeaders = false).Rows
        rows
        |> Seq.map (fun row -> int row[0], int row[1], if weightsIncluded then float row[2] else 1.0)
        |> Seq.toArray
    
    /// <summary> 
    /// Imports and builds a Directed graph from an edge list file
    /// </summary>
    /// <param name="fullpath">The path and file name to the file to be imported</param> 
    /// <param name="delimiter">The delimter between the nodes in the file. Often a space or a tab which can be indicated with a \t </param> 
    /// <param name="headerRows">The number of meta data rows at the start of the file. These often begin with a % and there is usually 2.</param> 
    /// <param name="weightsIncluded">Specifies if  there is a third column with the edge weights. These will be treated as floats.</param> 
    /// <returns>A Directed graph containing the nodes and edges specified in the file</returns>
    let importDirectedGraph (fullpath: string) (delimiter: string) (headerRows: int) (weightsIncluded: bool) = 
        importRows fullpath delimiter headerRows weightsIncluded
        |> DiGraph.Builders.createFromEdges

    /// <summary> 
    /// Imports and builds a Undirected graph from an edge list file
    /// </summary>
    /// <param name="fullpath">The path and file name to the file to be imported</param> 
    /// <param name="delimiter">The delimter between the nodes in the file. Often a space or a tab which can be indicated with a \t </param> 
    /// <param name="headerRows">The number of meta data rows at the start of the file. These often begin with a % and there is usually 2.</param> 
    /// <param name="weightsIncluded">Specifies if  there is a third column with the edge weights. These will be treated as floats.</param> 
    /// <returns>A Undirected graph containing the nodes and edges specified in the file</returns>
    let importUnDirectedGraph (fullpath: string) (delimiter: string) (headerRows: int) (weightsIncluded: bool) = 
        importRows fullpath delimiter headerRows weightsIncluded
        |> Graph.Builders.createFromEdges
    
 