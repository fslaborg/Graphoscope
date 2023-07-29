namespace Graphoscope.IO
open System.Text

module GDF =
    //Typedefinition, later used to changes the type of the associated value to the correct type.
    type GDFValue =
        | VARCHAR   of string
        | BOOLEAN   of bool
        | DOUBLE    of float
        | INTEGER   of int

    type GDFNode = {
        // Node: Node
        NodeInfo: seq<string*GDFValue>
    }

    type GDFEdge = {
        // SourceNode: Node
        // TargetNode: Node
        EdgeInfo: seq<string*GDFValue>
    }

    type GDFItem =
        | Node of GDFNode 
        | Edge of GDFEdge
        | Unknown of string

    //Transforms string into string [], deleting quotes in the progress and splitting at commas
    let splitElementInfos (line:string) =
        let chars = line.ToCharArray()

        let rec stringDeconstruction insideQuote i vertexInfos (sb:StringBuilder) =
            match chars.[i] with
            // Handle quote marks
            | '\'' when i = chars.Length-1  -> sb.ToString() :: vertexInfos
            | '\'' when insideQuote         -> stringDeconstruction false  (i+1) vertexInfos (sb)
            | '\''                          -> stringDeconstruction true  (i+1) vertexInfos (sb)
            // Handle commas
            | ',' when insideQuote          -> stringDeconstruction insideQuote  (i+1) vertexInfos (sb.Append ',')
            | ',' when i = chars.Length-1   -> sb.ToString() :: "" :: vertexInfos
            | ','                           -> stringDeconstruction insideQuote  (i+1) (sb.ToString() :: vertexInfos) (sb.Clear())
            // Handle every other symbol
            | c when i = chars.Length-1     -> sb.Append(c).ToString() :: vertexInfos
            | c                             -> stringDeconstruction insideQuote  (i+1) vertexInfos (sb.Append c)

        stringDeconstruction false 0 [] (StringBuilder())
        |> List.rev
        |> List.toArray

    //Returns the parameter name and a function to turn a string into the paramter type as tupel 
    let private getParsing (headerValue:string) =
        let headerName,headerType  = headerValue.Trim().Split ' ' |> fun x -> x.[0],Array.tail x|>String.concat","

        if      headerType.Trim().Contains "VARCHAR"                                       then headerName,(fun x -> match x with |" "| "" ->  (VARCHAR "")                 |_ -> (VARCHAR x))
        elif    headerType.Trim().Contains "INT"                                           then headerName,(fun x -> match x with |" "| "" ->  (INTEGER 0)                  |_ -> (INTEGER (int x)))
        elif    headerType.Trim().Contains "DOUBLE"                                        then headerName,(fun x -> match x with |" "| "" ->  (DOUBLE 0.0)                 |_ -> (DOUBLE (float x)))
        elif    headerType.Trim().Contains "BOOLEAN"                                       then headerName,(fun x -> match x with |" "| "" ->  (BOOLEAN false)              |"true" -> (BOOLEAN true)|"false" -> (BOOLEAN false)|_ -> failwith"unknown value in visible")
        elif    headerType.Trim().Contains "node1" ||headerValue.Trim().Contains "node2"   then headerName,(fun x -> match x with |" "| "" ->  (VARCHAR "")                 |_ -> (VARCHAR x))
        else failwith "unknown typeAnnotation in header"

    //Returns the correct parser given on the given pattern. Used for nodeheader and edgeheader recognition
    let private getParser (s:string) (pattern:string) =
        let regexPattern = $"(?<={pattern}).*"
        let r = System.Text.RegularExpressions.Regex.Match(s,regexPattern) 
        if r.Success then 
            let parsing = 
                r.Value
                |>splitElementInfos
                |> Array.map(fun x -> 
                    getParsing x) 
            Some(parsing)
        else None

    //Returns the SOME line parsed by the pasingArray. In case of failure return None
    let private tryGetElements (parsingArray:array<string * (string -> GDFValue)> option) (GDFItemF: array<string * GDFValue> -> GDFItem) (line:string) = 
        try
            line
            |>splitElementInfos
            Array.map2(fun (n,p) l -> n,p l) (Option.get parsingArray) 
            |> fun x -> Some (GDFItemF x)
        with _ -> None
    
    //Returns the nodeheader if true
    let private (|NodeHeader|_|) (line:string) =
        match line.StartsWith("nodedef>") with
        | true -> Some <| getParser line "nodedef>"
        | false -> None

    //Returns the edgeHeader if true
    let private (|EdgeHeader|_|) (line:string) =
        match line.StartsWith("edgedef>") with
        | true -> Some <| getParser line "edgedef>"
        | false -> None

    //Returns a GDFItem.Node if matched and not an Edge
    let private (|Node|_|) (parsingArray:array<string * (string -> GDFValue)> option) (isEdge:bool) (line:string) =
        if isEdge then
            None
        else
            tryGetElements parsingArray (fun x -> GDFItem.Node {NodeInfo=x}) line
    
    //Returns a GDFItem.Edge if matched and an Edge
    let private (|Edge|_|) (parsingArray:array<string * (string -> GDFValue)> option) (isEdge:bool) (line:string) =
        if isEdge then   
            tryGetElements parsingArray (fun x -> (GDFItem.Edge {EdgeInfo=x})) line 
        else
            None

    //Parses an seq of lines 
    let parser (lines : seq<string>) =
        let en = lines.GetEnumerator()
        let rec loop (parserBuilt: array<string * (string -> GDFValue)> option) (isEdge:bool)   =        
            seq {   
                match en.MoveNext() with
                | true -> 
                    
                        match en.Current, isEdge with
                        | NodeHeader nh, _ ->                         
                            yield! loop (nh) false
                        | EdgeHeader eh, _ ->                        
                            yield! loop (eh) true
                        | Node parserBuilt isEdge n, false -> 
                            yield n
                            yield! loop parserBuilt isEdge
                        | Edge parserBuilt isEdge e, true -> 
                            yield e
                            yield! loop parserBuilt isEdge               
                        | _,_  -> yield GDFItem.Unknown en.Current
                    
                    
                | false -> ()

            }
        loop None false

