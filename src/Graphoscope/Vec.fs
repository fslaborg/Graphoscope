namespace Graphoscope

open System

type Vec<'T when 'T: equality>  (items: 'T[], count: int) =
    let mutable _items = items
    let mutable _size = count

    [<Literal>]
    static let DEFAULT_CAPACITY = 32

    static let throwNonNegativeIndexException () =
        raise (IndexOutOfRangeException "Cannot have an index less than 0")

    static let throwIndexOutOfRangeException () =
        raise (IndexOutOfRangeException "Index great than count")

    static let throwInvalidArgException () =
        invalidArg "Length" "Length extends past Count"

    static let nextPowerOf2 (n: int) =
        let rec loop (x: int) =
            if x < n then
                loop (x <<< 1)
            else
                x

        loop 1

    new () =
        let items : 'T[] = Array.zeroCreate DEFAULT_CAPACITY
        Vec (items, 0)

    new (source: 'T[]) =
        let newSize = nextPowerOf2 source.Length
        let newItems = Array.zeroCreate newSize
        Array.Copy (source, newItems, source.Length)
        Vec (newItems, source.Length)

    member _.Items = _items

    member private _.SetCount n = _size <- n

    member _.Item
        with get i =
            if i < 0 then
                throwNonNegativeIndexException()
            elif i >= _size then
                throwIndexOutOfRangeException()
            else
                _items[i]
        and set i v =
            if i < 0 then
                throwNonNegativeIndexException()
            elif i >= _size then
                throwIndexOutOfRangeException()
            else
                _items[i] <- v

    member _.Length = _size

    member _.Add (p: 'T) =
        if _size < _items.Length then
            _items[_size] <- p
        else
            let oldItems = _items
            _items <- Array.zeroCreate (oldItems.Length * 2)
            Array.Copy (oldItems, _items, oldItems.Length)
            _items[_size] <- p

        _size <- _size + 1

    member _.Add (other: 'T[]) =
        if _size + other.Length > _items.Length then
            let newSize = nextPowerOf2 (_size + other.Length)
            let newItems = Array.zeroCreate newSize
            Array.Copy (_items, newItems, _size)
            _items <- newItems

        Array.Copy (other, 0, _items, _size, other.Length)
        _size <- _size + other.Length

    member _.Remove (i: int) : 'T =
        if i < 0 then
            throwNonNegativeIndexException()
        elif i >= _size then
            throwIndexOutOfRangeException()
        else
            _size <- _size - 1
            let item = _items[i]
            _items[i] <- _items[_size]
            item

    member _.Pop () : 'T =
        if _size <= 0 then
            throwNonNegativeIndexException()
        else
            _size <- _size - 1
            _items[_size]

   
    member _.SplitAt (i: int) : Vec<'T> =
        if i < 0 then
            throwNonNegativeIndexException()
        elif i >= _size then
            throwIndexOutOfRangeException()
        else
            let newItems = Array.zeroCreate _items.Length
            let newCount = _size - i
            Array.Copy (_items, i, newItems, 0, newCount)
            _size <- _size - newCount
            Vec (newItems, newCount)

    member _.SplitAll () : Vec<'T> =
        let newItems = Array.zeroCreate _size
        Array.Copy (_items, newItems, _size)
        let newCount = _size
        _size <- 0
        Vec (newItems, newCount)

    member _.RemoveRange (index: int, count: int) : Vec<'T> =
        if index < 0 then
            throwIndexOutOfRangeException()
        if count < 0 then
            throwIndexOutOfRangeException()
        if _size - index < count then
            throwInvalidArgException()

        let newItems = Array.zeroCreate _items.Length
        Array.Copy (_items, index, newItems, 0, count)

        _size <- _size - count
        Array.Copy (_items, index + count, _items, index, _size - index)

        Vec (newItems, count)

    override v.ToString() =
        v.Items.ToString()

    override v.GetHashCode() =
        v.Items.GetHashCode()

    override v.Equals v2 =
        match v2 with
        | :? Vec<'T> as vv -> v.Items = vv.Items
        | _ -> false

module Vec =
    let map (f: 'T -> 'R) (vec: Vec<'T>) =
        let len = vec.Length
        let res = new Vec<'R>()

        for i = 0 to len - 1 do
            res.Add(f vec.[i])

        res

    let iteri (f: int -> 'T -> unit) (vec: Vec<'T>): unit =
        for i = 0 to vec.Length - 1 do 
            f i vec[i]

    let inline sum (vec: Vec<'T>): 'T =
        let mutable acc = LanguagePrimitives.GenericZero<'T>
        for i = 0 to vec.Length - 1 do
            acc <- acc + vec[i]
        acc

    let inline sumBy (f: 'T -> 'R) (vec: Vec<'T>): 'R =
        let mutable acc = LanguagePrimitives.GenericZero<'R>
        for i = 0 to vec.Length - 1 do
            acc <- acc + (f vec[i])
        acc

    let tryHead (vec : Vec<'T>) =
        if vec.Length = 0 then None
        else Some vec[0]


