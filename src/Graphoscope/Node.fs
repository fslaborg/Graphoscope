namespace Graphoscope

open System


type Node<'NodeKey, 'Node> = 'NodeKey * 'Node


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