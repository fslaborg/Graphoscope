namespace Graphoscope.Undirected

module UndirectedHelpers =

    open FSharp.Data
    open UndirectedGraph

    let createFromEdgeList (fullpath: string) (delimiter: string) (headerRows: int) (weightsIncluded: bool) =
        let rows  = CsvFile.Load(fullpath, delimiter, skipRows = headerRows, hasHeaders = false).Rows

        if  weightsIncluded then
            rows
            |> Seq.map (fun row -> row[0], row[1], float row[2] )
            |> Seq.toArray
            |> UndirectedGraph.UndirectedGraph
        else 
            rows
            |> Seq.map (fun row -> row[0], row[1])
            |> Seq.toArray
            |> UndirectedGraph.UndirectedGraph