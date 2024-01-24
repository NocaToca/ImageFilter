namespace ImageMath

type Vector = {Length : int; Content: float array}

module VectorModule = 

    open System

    let CreateEmptyVector length = 
        let values =
            Array.zeroCreate length
        {Length = length; Content = values}
    
    let CreateVector (values : float array) =
        let count = 
            values.Length
        {Length = count; Content = values}

    let ValueAt (vector : Vector) (index : int) = 
        vector.Content.[index]
    
    let SetValue (vector : Vector) (index : int) (value : float) =
        vector.Content.[index] <- value

    let map func vector =
        let new_content = 
            vector.Content
            |> Array.map func
        {Length = vector.Length; Content = new_content}

    let CreateLinSpace (start : int) (_end : int) (size : int) = 
        let stepsize = float (_end - start)/ float (size - 1)
        let values = 
            [|for i in 0 .. size - 1 -> float start + float i * stepsize|]
        CreateVector values
    
    let PrintVector (vector : Vector) = 
        vector.Content
        |> Array.mapi (fun x s -> if x = vector.Length - 1 then s.ToString() else s.ToString() + ", ")
        |> Array.fold (fun acc s -> acc + s) ""
        |> printfn "%s"

    