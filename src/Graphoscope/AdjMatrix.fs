namespace Graphoscope

open FSharpAux
open System.Collections.Generic



/// A FsSparseMatrix
type AdjMatrix<'T>(sparseValues : 'T array, sparseRowOffsets : int array, ncols:int, columnValues: int array) =     
    member m.NumCols = ncols
    member m.NumRows = sparseRowOffsets.Length - 1
    member m.SparseColumnValues = columnValues
    member m.SparseRowOffsets =  sparseRowOffsets (* nrows + 1 elements *)
    member m.SparseValues =  sparseValues

    member m.MinIndexForRow i = m.SparseRowOffsets.[i]
    member m.MaxIndexForRow i = m.SparseRowOffsets.[i+1]
            

    member m.Item 
        with get (i,j) = 
            let imax = m.NumRows
            let jmax = m.NumCols
            if j < 0 || j >= jmax || i < 0 || i >= imax then raise (new System.ArgumentOutOfRangeException()) else
            let kmin = m.MinIndexForRow i
            let kmax = m.MaxIndexForRow i
            let rec loopRow k =
                (* note: could do a binary chop here *)
                if k >= kmax then defaultEmptyValue else
                let j2 = columnValues.[k]
                if j < j2 then defaultEmptyValue else
                if j = j2 then sparseValues.[k] else 
                loopRow (k+1)
            loopRow kmin
