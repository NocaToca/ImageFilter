// For more information see https://aka.ms/fsharp-console-apps
namespace Processing

module Images =

    open ImageMath

    type Pixel = {R : float; G : float; B : float}
    type Image = {Data : Pixel array2d ; Width : int ; Height : int}

    let SobelX = 
        let matrix = MatrixModule.CreateNewMatrix 3 3
        let row1 = VectorModule.CreateVector [|-1.0; -2.0; -1.0|]
        let row2 = VectorModule.CreateVector [|0.0; 0.0; 0.0|]
        let row3 = VectorModule.CreateVector [|1.0; 2.0; 1.0|]
        MatrixModule.SetRow matrix 0 row1.Content
        MatrixModule.SetRow matrix 1 row2.Content
        MatrixModule.SetRow matrix 2 row3.Content
        matrix

    let SobelY = 
        let matrix = MatrixModule.CreateNewMatrix 3 3
        let col1 = VectorModule.CreateVector [|1.0; 2.0; 1.0|]
        let col2 = VectorModule.CreateVector [|0.0; 0.0; 0.0|]
        let col3 = VectorModule.CreateVector [|-1.0; -2.0; -1.0|]
        MatrixModule.SetColumn matrix 0 col1.Content
        MatrixModule.SetColumn matrix 1 col2.Content
        MatrixModule.SetColumn matrix 2 col3.Content
        matrix

    

    let ConvertGreyscale (image : Image) =
        let greyscale_data =
            image.Data
            |> Array2D.map (fun pixel -> 
                let grey = (pixel.R +  pixel.G + pixel.B) / 3.0
                {R = grey; G = grey ; B = grey}) 
        {Data = greyscale_data ; Width = image.Width ; Height = image.Height  }
    
    //We will just take the red channel
    let ConvertMatrix (image : Image) =
        // printfn "%A %A" image.Width image.Height
        let matrix = MatrixModule.CreateNewMatrix image.Width image.Height
        let content = 
            matrix.Content
            |> Array2D.mapi (fun x y _ -> image.Data.[x,y].R)
        {Width = image.Width; Height = image.Height; Content = content}

    let GaussianKernal (sigma : float) (size : int) = 
        let a = size / 2
        let vector_kernal = 
            VectorModule.CreateLinSpace (-1 * a) a size
            |> VectorModule.map (fun x -> MathModule.Gaussian sigma x 0.0)

        let matrix_kernal = MatrixModule.OuterProduct vector_kernal vector_kernal
        let max_value = MatrixModule.FindMax matrix_kernal
        let mul = 1.0/max_value

        let content = 
            matrix_kernal.Content
            |> Array2D.mapi (fun x y value -> value * mul)

        {Width = matrix_kernal.Width; Height = matrix_kernal.Height; Content = content}

    let Convolution (image : Matrix) (kernal : Matrix) = 
        let image_x = image.Width
        let image_y = image.Height

        let kernal_x = kernal.Width
        let kernal_y = kernal.Height

        let padding_x = (kernal_x - 1)/2
        let padding_y = (kernal_y - 1)/2

        let pad_image_x = image_x + (2 * padding_x)
        let pad_image_y = image_y + (2 * padding_y)

        let padded_image = 
            MatrixModule.CreateNewMatrix pad_image_x pad_image_y
            |> MatrixModule.mapi (fun x y _ ->
                let original_x = x - padding_x
                let original_y = y - padding_y
                if original_x >= 0 && original_x < image_x && original_y >= 0 && original_y < image_y then
                    MatrixModule.ValueAt image original_x original_y
                else 
                    0.0
                )
        
        // let result_image = 
        //     MatrixModule.mapi (fun x y value -> 
        //     if x < padding_x && y > padding_y && y < image_y then
        //         MatrixModule.ValueAt image x padding_y 
        //     elif x < padding_x && y < padding_y then
        //         MatrixModule.ValueAt image 0 0
        //     elif x > padding_x && y < padding_y && x < image_x then
        //         MatrixModule.ValueAt image x padding_y 
        //     elif x > padding_x && y >= image_y && x < image_x then
        //         MatrixModule.ValueAt image x (image_y - 1) 
        //     elif x >= image_x && y < padding_y then
        //         MatrixModule.ValueAt image (image_x - 1) 0 
        //     elif x >= image_x && y >= image_y then
        //         MatrixModule.ValueAt image (image_x - 1) (image_y - 1) 
        //     elif x < padding_y && y >= image_y then
        //         MatrixModule.ValueAt image 0 (image_y - 1) 
        //     elif x >= image_x && y > padding_y && y < image_y then
        //         MatrixModule.ValueAt image (image_x - 1) y 
        //     else
        //         value
        //     ) padded_image

        MatrixModule.CreateNewMatrix image_x image_y
        |> MatrixModule.mapi (fun x y value ->
            let sub = MatrixModule.CreateSubMatrix padded_image x (x + kernal_x) y (y + kernal_y)
            let m = (MathModule.MultiplyElementWise sub kernal)
            MatrixModule.SumMatrix m) 
        
        

    let GaussianFilterMatrix (matrix : Matrix) (sigma : float) (size : int)=
        let result = 
            Convolution matrix (GaussianKernal sigma size)
            |> MatrixModule.Normalize
        result

    let SobelEdgeDetection (image : Matrix) (sigma : float) (threshold : int) =
        let smoothed = GaussianFilterMatrix (MatrixModule.Normalize image) sigma 5

        let sobel_x = Convolution smoothed SobelX
        let sobel_y = Convolution smoothed SobelY

        let GradientMagnitude = 
            MatrixModule.CreateNewMatrix sobel_x.Width sobel_y.Height
            |> MatrixModule.mapi (fun x y _ -> sqrt (((MatrixModule.ValueAt sobel_x x y) ** 2.0) + ((MatrixModule.ValueAt sobel_y x y) ** 2.0)))

        let threshold_value = float threshold / (MatrixModule.FindMax GradientMagnitude)

        let magnitude_output = 
            MatrixModule.map (fun value -> value * threshold_value) GradientMagnitude
            |> MatrixModule.map (fun value -> if value <= 0.8 then 0 else value)

        let angles_output = 
            MatrixModule.CreateNewMatrix magnitude_output.Width magnitude_output.Height
            |> MatrixModule.mapi (fun x y _ -> (atan2 (MatrixModule.ValueAt sobel_y x y) (MatrixModule.ValueAt sobel_x x y)) * 180.0/MathModule.pi) 
        
        (magnitude_output, angles_output)

    let NonMaxAlg (image : Matrix) (angles : Matrix) =
        let image_x = image.Width
        let image_y = image.Height

        // printfn "Image x and y %A %A" image_x image_y

        let output = MatrixModule.CreateNewMatrix image_x image_y

        for y in 1 .. image_y - 2 do
            for x in 1 .. image_x - 2 do
                let angle = MatrixModule.ValueAt angles x y

                let is_max =
                    if (angle >= -22.5 && angle <= 22.5) || (angle < -157.5 && angle >= -180.0) then
                        MatrixModule.ValueAt image x y >= MatrixModule.ValueAt image (x+1) y && MatrixModule.ValueAt image x y >= MatrixModule.ValueAt image (x - 1) y 
                    elif (angle >= 22.5 && angle <= 67.5) || (angle < -122.5 && angle >= -157.5) then
                        MatrixModule.ValueAt image x y >= MatrixModule.ValueAt image (x + 1) (y + 1) && MatrixModule.ValueAt image x y >= MatrixModule.ValueAt image (x - 1) (y - 1)
                    elif (angle >= 67.5 && angle <= 112.5) || (angle < -67.5 && angle >= -112.5) then
                        MatrixModule.ValueAt image x y >= MatrixModule.ValueAt image x (y + 1) && MatrixModule.ValueAt image x y >= MatrixModule.ValueAt image x (y - 1)
                    elif (angle >= 112.5 && angle <= 157.5) || (angle < -22.5 && angle >= -67.5) then
                        MatrixModule.ValueAt image x y >= MatrixModule.ValueAt image  (x - 1) (y + 1) && MatrixModule.ValueAt image x y >= MatrixModule.ValueAt image  (x + 1) (y - 1)
                       
                    else
                        false

                if is_max then
                    MatrixModule.SetValue output x y (MatrixModule.ValueAt image x y)

        output

    let HessianDeterminates (image : Matrix) =

        let threshold = 0.225

        let sobel_x = SobelX
        let sobel_y = SobelY
        
        let x_derv = (Convolution image sobel_x)
        let xx_derv = Convolution x_derv sobel_x
        let xy_derv = Convolution (Convolution image sobel_x) sobel_y
        let yy_derv = Convolution (Convolution image sobel_y) sobel_y

        let image_x = image.Width
        let image_y = image.Height

        let output = 
            MatrixModule.CreateNewMatrix image_x image_y
            |> MatrixModule.mapi (fun x y _ -> 
                (MatrixModule.ValueAt xx_derv x y) * (MatrixModule.ValueAt yy_derv x y) - ((MatrixModule.ValueAt xy_derv x y) ** 2)
                - (threshold * ((MatrixModule.ValueAt xx_derv x y + MatrixModule.ValueAt yy_derv x y) ** 2))) 
            |> MatrixModule.map (fun x -> if x < 0 then 0 else x)
            |> MatrixModule.Normalize

        output

        

    let NonMaxSupressionAxB (matrix : Matrix) (angles : Matrix) (a : int) (b : int) = 
        let matrix_x = matrix.Width
        let matrix_y = matrix.Height

        let output = MatrixModule.CreateNewMatrix matrix_x matrix_y

        for x in b .. b .. matrix_x -  1 do
            for y in a .. a .. matrix_y - 1 do
                let a_inc = (a - 1) / 2
                let b_inc = (b - 1) / 2

                let neighborhood_matrix = MatrixModule.CreateSubMatrix matrix (x - b_inc) (x + b_inc + 1) (y - a_inc) (y + a_inc + 1)
                let neighborhood_angle = MatrixModule.CreateSubMatrix angles (x - b_inc) (x + b_inc + 1) (y - a_inc) (y + a_inc + 1)

                let nieghborhood_suppressed = NonMaxAlg neighborhood_matrix neighborhood_angle

                let max = MatrixModule.FindMax nieghborhood_suppressed

                for x2 in -a_inc .. a_inc do
                    for y2 in -b_inc .. b_inc do
                        let value = MatrixModule.ValueAt nieghborhood_suppressed (x2 + a_inc) (y2 + b_inc)
                        let is_max = value = max
                        if is_max then MatrixModule.SetValue output (x + x2) (y + y2) value
                            else MatrixModule.SetValue output (x + x2) (y + y2) 0.0
        
        output

    let Hessian (matrix : Matrix) (angles : Matrix) =
        let hes = HessianDeterminates matrix
        NonMaxSupressionAxB hes angles 25 25

    


    let MatrixToImage (image : Matrix) =
        // printfn "%A %A" image.Width image.Height
        let values = 
            image.Content
            |> Array2D.map (fun value -> {R = value; G = value; B = value})
        {Data = values; Width = image.Width; Height = image.Height} 


    let TestGuassFilter (matrix : Image) = 
        let matrix =
            ConvertGreyscale matrix
            |> ConvertMatrix
            |> MatrixToImage
        matrix

    let ProcessImage (image : Image) = 
        let matrix = 
            ConvertGreyscale image
            |> ConvertMatrix
        match (SobelEdgeDetection matrix 1.0 100) with
            | (magnitude, angles) -> 
                NonMaxAlg (MatrixModule.Normalize magnitude) angles
                |> MatrixModule.Normalize
                |> MatrixToImage
        
    let DetectKeyPoint (image : Image) =
        let matrix = 
            ConvertGreyscale image
            |> ConvertMatrix
        match (SobelEdgeDetection matrix 1.0 255) with
            | (magnitude, angles) -> 
                let smoothed = GaussianFilterMatrix matrix 1 5
                Hessian smoothed angles
                |> MatrixModule.Normalize
                |> MatrixToImage

    let test () = 
        MathModule.Multiply SobelX SobelY
        |> MatrixModule.PrintMatrix

    let generateGradientImage =
        let width = 20
        let height = 10
        let data =
            array2D [
                for y in 0 .. width do
                    [|for x in 0 .. height do
                        let random = new System.Random()
                        let intensity = random.NextDouble() * 255.0
                        yield { R = intensity; G = intensity; B = intensity }|]
            ]
        { Data = data; Width = width; Height = height }

    [<EntryPoint>]
    let main argv = 
        DetectKeyPoint generateGradientImage
        // SobelY
        // |> MatrixModule.PrintMatrix
        0

