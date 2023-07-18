module Undirected 
    type UndirectedGraph<'Node when 'Node: equality> (nodes : ResizeArray<'Node>, edges : ResizeArray<ResizeArray<(int * float)>>) = 
        
        member this.Nodes = nodes
        member this.Edges = edges
 
        
