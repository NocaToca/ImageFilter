namespace ImageMath

module MathModule =

    let pi = 3.1415926535897932384626433832795028841971693993751
    let e = 2.7182818284590452353602874713527

    let Gaussian (sigma : float) (x : float) (mu : float) = 
        let a = 1.0 / sqrt (2.0 * pi)
        let b = -1.0 / 2.0 * (((x - mu) / sigma) ** 2.0)
        a * (e ** b)

    let AddMatrices (m1 : Matrix) (m2 : Matrix) =
        let width = max m1.Width m2.Width
        let height = max m1.Height m2.Height
        m1.Content
        |> Array2D.mapi (fun x y value -> value + MatrixModule.ValueAt m2 x y)
    
    let SubMatrices (m1 : Matrix) (m2 : Matrix) = 
        let width = max m1.Width m2.Width
        let height = max m1.Height m2.Height
        m1.Content
        |> Array2D.mapi (fun x y value -> value - MatrixModule.ValueAt m2 x y)

    let Dot (v1 : Vector) (v2 : Vector) = 
        v1.Content
        |> Array.mapi (fun i x -> x * VectorModule.ValueAt v2 i)
        |> Array.fold (fun acc x -> acc + x) 0.0

    let MultiplyElementWise (m1 : Matrix) (m2 : Matrix) = 
        let result = MatrixModule.CreateNewMatrix m1.Height m2.Width

        for y in 0 .. m1.Height - 1 do
            for x in 0 .. m2.Width - 1 do
                MatrixModule.SetValue result x y (MatrixModule.ValueAt m1 x y * MatrixModule.ValueAt m2 x y)
        
        result

    let Multiply (m1 : Matrix) (m2 : Matrix) = 
        if m1.Width <> m2.Height then
            failwith "Matrix m1 and m2 cannot be multiplied together!"

        let result = MatrixModule.CreateNewMatrix m1.Height m2.Width

        for y in 0 .. m1.Height - 1 do
            let v1 = MatrixModule.GetColumnVector m1 y
            for x in 0 .. m2.Width - 1 do
                let v2 = MatrixModule.GetRowVector m2 x
                MatrixModule.SetValue result x y (Dot v1 v2)
        
        result
        
    
    
    
