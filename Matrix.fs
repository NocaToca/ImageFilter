namespace ImageMath

type Matrix = {Width : int; Height : int; Content : float array2d}

module MatrixModule = 

    open System

    //For folding elements within our matrix
    let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
        seq {
            for x in 0 .. Array2D.length1 array - 1 do
                for y in 0 .. Array2D.length2 array - 1 do
                    yield (x, y, array.[x, y])
        }
        |> Seq.fold (fun acc (x, y, e) -> folder x y acc e) state

    //Creates a new matrix
    let CreateNewMatrix width height = 
        let content = 
            Array2D.init width height (fun _ _ -> 0.0)
        {Width = width ; Height = height; Content = content}

    //Creates a basic sub matrix
    let CreateSubMatrix matrix start_col end_col start_row end_row = 
        let height = 
            end_row - start_row
        let width = 
            end_col - start_col
        let content = 
            Array2D.init height width (fun y x -> 
                matrix.Content.[x + start_col, y + start_row])
        
        {Width = width ; Height = height ; Content = content}

    //Prints the contents of a matrix in a readable format
    let PrintMatrix (matrix : Matrix) = 
        matrix.Content
        |> Array2D.map (fun s -> s.ToString())
        |> Array2D.mapi (fun _ y s -> if y = matrix.Width - 1 then s + "\n" else s + ", ")
        |> foldi (fun _ _ acc s -> acc + s) ""
        |> printfn "%s"

    //Sums all elements within the matrix
    let SumMatrix (matrix : Matrix) = 
        matrix.Content
        |> foldi (fun _ _ acc x -> acc + x) 0.0

    let row i (arr: 'T[,]) = arr.[i..i, *] |> Seq.cast<'T> |> Seq.toArray
    let column i (arr: 'T[,]) = arr.[*, i..i] |> Seq.cast<'T> |> Seq.toArray

    //A lot of basic matrix functions
    let GetRowVector (matrix : Matrix) (row_number : int) = 
        let vector = column row_number matrix.Content
        VectorModule.CreateVector vector

    let GetColumnVector (matrix : Matrix) (column_number : int) = 
        let vector = row column_number matrix.Content 
        VectorModule.CreateVector vector

    let SetValue (matrix : Matrix) (row : int) (column : int) (value : float) =
        matrix.Content.[row, column] <- value
    
    let ValueAt (matrix : Matrix) (row : int) (column : int) =
        // if row >= matrix.Height || column >= matrix.Width then
        //     printfn "Height: %A Width: %A" matrix.Height matrix.Width
        //     printfn "Attempt to grab: %A %A" row column
        matrix.Content.[row, column]
    
    let SetColumn (matrix : Matrix) (row : int) (values : float array) =
        for x in 0 .. matrix.Width - 1 do
            SetValue matrix row x values.[x]
    
    let SetRow (matrix : Matrix) (column : int) (values : float array) = 
        for x in 0 .. matrix.Height - 1 do
            SetValue matrix x column values.[x]

    let FindMax (matrix : Matrix) = 
        matrix.Content
        |> foldi (fun _ _ x y -> if x > y then x else y) (ValueAt matrix 0 0)

    let Normalize (matrix : Matrix) = 
        let f = FindMax matrix
        let content = 
            matrix.Content
            |> Array2D.map (fun x -> x/f)
        {Width = matrix.Width; Height = matrix.Height; Content = content}

    let map func (matrix : Matrix) = 
        let content = 
            matrix.Content
            |> Array2D.map func
        {Width = matrix.Width; Height = matrix.Height; Content = content}
    
    let mapi func (matrix : Matrix) =
        let content = 
            matrix.Content
            |> Array2D.mapi func
        {Width = matrix.Width; Height = matrix.Height; Content = content}

    //Vector one and vector two NEED to have the same length
    let OuterProduct (vector_one : Vector) (vector_two : Vector) =
        let content = 
            Array2D.init vector_one.Length vector_two.Length (fun x y -> (VectorModule.ValueAt vector_one x) * (VectorModule.ValueAt vector_two y))
        {Width = vector_one.Length; Height = vector_two.Length; Content = content}