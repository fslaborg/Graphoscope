namespace Graphoscope.Random

open Graphoscope.ArrayAdjacencyGraph
open FSharpx.Collections
open System
open System.Collections.Generic

// Adaptation of the gilbert random plane networks
// Gilbert, E.N., 1961. Random plane networks. Journal of the society for industrial and applied mathematics, 9(4), pp.533-543.
/// Returns an ArrayAdjacencyGraph that is generated randomly with the given parameters.
///
/// numberOfVertices indicates the number of vertices the final graph will have. 
///
/// probability represents the probability of an edge between 2 vertices.   
///
/// fVertexKey is a function that is used to transform an integer (the index of the vertex) into the 'Vertex type.  
///
/// fLabel is a function that transforms the 'Vertex type into a label of the 'Label type.   
///
/// fWeight is a funtion that takes two 'Vertices and returns a weight between them in form of an 'Edge type.    
let initArrayAdjacencyGraph (numberOfVertices: int) (probability: float) (isDirected: bool) (fVertexKey: int -> 'Vertex) (fLabel: 'Vertex -> 'Label) (fWeight: 'Vertex*'Vertex -> 'Edge) =
    if probability > 1. || probability < 0. then failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

    let rnd         = new System.Random()
    let vertexEdges = System.Collections.Generic.Dictionary<'Vertex,LEdge<'Vertex,'Edge>ResizeArray>()
    let labelDict   = System.Collections.Generic.Dictionary<'Vertex,'Label>()

    let vertices = 
            [|
            for i=0 to (numberOfVertices-1) do
                fVertexKey i
            |]

    for i=0 to (numberOfVertices-1) do          
        let vertex = vertices.[i]
        let label   = fLabel vertex
        labelDict.Add(vertex,label)
        vertexEdges.Add(vertex,ResizeArray())

    if isDirected then
        for s in vertices do
    
            for t in vertices do

                if rnd.NextDouble() < probability then
            
                    if s=t then                                            
                        let w       = fWeight (s,t)
                        let valueS  = vertexEdges.Item s 
                        (vertexEdges.Item s).Add(s,s,w)
                        (vertexEdges.Item s).Add(s,s,w)
                    else
                        let w       = fWeight (s,t)
                        let valueS  = vertexEdges.Item s 
                        let valueT  = vertexEdges.Item t
                        (vertexEdges.Item s).Add(s,t,w)
                        (vertexEdges.Item t).Add(s,t,w)                  
    else
        for i=0 to vertices.Length-1 do 
            let s = vertices.[i]

            for j=i to vertices.Length-1 do
                if rnd.NextDouble() < probability then
                    let t = vertices.[j]

                    if s=t then 
                        let w       = fWeight (s,t)
                        let valueS  = vertexEdges.Item s 
                        (vertexEdges.Item s).Add(s,s,w)
                    else
                        let w       = fWeight (s,t)
                        let valueS  = vertexEdges.Item s 
                        let valueT  = vertexEdges.Item t
                        (vertexEdges.Item s).Add(s,t,w)
                        (vertexEdges.Item t).Add(s,t,w)        
    
    ArrayAdjacencyGraph(vertexEdges,labelDict)
