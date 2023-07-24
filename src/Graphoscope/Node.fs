namespace Graphoscope

open System


/// <summary> 
/// Node representation with generic key and data seperated by default
/// </summary>
type Node<'NodeKey, 'NodeData> = 'NodeKey * 'NodeData

/// <summary> 
/// Edge representation with generic node key and edge data seperated by default
/// </summary>
type Edge<'NodeKey, 'EdgeData> = 'NodeKey * 'NodeKey * 'EdgeData 



[<CustomComparison; CustomEquality>]
type DiNode<'NodeData> =
    { Id : int; Data : 'NodeData }
    interface IEquatable<DiNode<'NodeData>> with
        member this.Equals other = other.Id.Equals this.Id

    override this.Equals other =
        match other with
        | :? DiNode<'NodeData> as p -> (this :> IEquatable<_>).Equals p
        | _ -> false
    
    override this.GetHashCode () = this.Id.GetHashCode()

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? DiNode<'NodeData> as n -> (this :> IComparable<_>).CompareTo n
            | _ -> -1

    interface IComparable<DiNode<'NodeData>> with
        member this.CompareTo other = other.Id.CompareTo this.Id