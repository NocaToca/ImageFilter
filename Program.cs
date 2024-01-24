
using static Processing.Images;
using System;
using System.Drawing;
using Microsoft.FSharp.Core;
using SixLabors.ImageSharp;
using Newtonsoft.Json;

public static class Program{

    
    public struct Keypoint{

        [JsonProperty("x")]
        public float x;

        [JsonProperty("y")]
        public float y;

        [JsonConstructor]
        public Keypoint(float x, float y){
            this.x = x;
            this.y = y;
        }

    }

    public static void Main(string [] args){
        // test();

        Bitmap luna = new Bitmap(@"fox.jpg");

        var pixels = new Pixel[luna.Width, luna.Height];
        for(int x = 0; x <luna.Width; x++){
            for(int y = 0; y < luna.Height; y++){
                var pixel = luna.GetPixel(x,y);
                pixels[x,y] = new Pixel(pixel.R, pixel.G, pixel.B);
            }
        }

        var image = DetectKeyPoint(new Processing.Images.Image(pixels, luna.Width, luna.Height));

        Bitmap filtered = new Bitmap(luna.Width, luna.Height);

        Console.WriteLine($"Width: {image.Width} Height: {image.Height}");
        Console.WriteLine($"Original Width: {luna.Width} Orignial Height: {luna.Height}");

        List<Keypoint> keypoints = new List<Keypoint>();

        for(int x = 0; x <luna.Width; x++){
            for(int y = 0; y < luna.Height; y++){
                var pixel = image.Data[x,y];
                System.Drawing.Color col;
                try{
                    col = System.Drawing.Color.FromArgb((int)Math.Abs(pixel.R * 255), (int)Math.Abs(pixel.G * 255), (int)Math.Abs(pixel.B * 255));
                } catch {
                    col = System.Drawing.Color.Black;
                }

                if(pixel.R > .15){
                    keypoints.Add(new Keypoint(x/(float)luna.Width, y/(float)luna.Height));
                }

                
                filtered.SetPixel(x, y, col);
            }
        }

        string json = JsonConvert.SerializeObject(keypoints);
        File.WriteAllText("output.json", json);


        filtered.Save("fox_filtered.jpg");

    }

}