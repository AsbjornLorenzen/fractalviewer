type pixelpoint = int * int 
type color = ImgUtil.color
type canvas = ImgUtil.canvas
type complexnumber = float * float

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

//counter calculates iterations for a point in the plane
let rec counter (z:complexnumber) (c:complexnumber) (n:int) (max:int) : int =
    //max is max amount of iterations
    let (a,b) = z
    let (a2,b2) = (a*a,b*b)
    let result = (a2 - b2 + fst c,(a+a)*b + snd c)
    if a2 + b2 > 4.0 then n
    elif (n+1) >= max then max
    else counter result c (n+1) max

// ---------------------- The code for histogram colors -------------------

let count_iter (pos:complexnumber) (dif:float) (res:pixelpoint) (max:int) : (int [,] * int []) = //types array2d and array
    let mutable pixelarray = Array2D.create (fst res) (snd res) 0
    let mutable iter_count = Array.create (max+1) 0
    let mutable (xpos,ypos) = pos //mutables are used for adding dif every round to reduce amount of multiplications needed during computation
    for i=0 to (fst res)-1 do
        for j=0 to (snd res)-1 do
            let res = counter (0.0,0.0) (xpos,ypos) 0 max
            pixelarray.[i,j] <- res
            iter_count.[res] <- iter_count.[res] + 1
            ypos <- ypos - dif 
        ypos <- snd pos
        xpos <- xpos + dif
    (pixelarray,iter_count) 

let determineindex (max:int) =
    // a range is low 10%, b is 10% to 25%, c is above 25% (percent iterations of max)
    let (a0,a1) = (0,max/10)
    let (b0,b1) = (a1+1,max/4)
    let (c0,c1) = (b1+1,max)
    ((a0,a1),(b0,b1),(c0,c1))

let mapittocolor (itlist:int[]) (range:int*int) (colors:(int*int) []) : ((int*color) []) = 
    // itlist is the whole histogram
    // range is inclusive of both min and max <- might have to be changed ater, look at hist init func
    // colors is a range eg [(10,255),(120,120)(120,120)] meaning a low value has red=10, max has red=255
    // remember, lowest is 0, max is max iterations
    let rdif = float <| (snd colors.[0]) - (fst colors.[0])
    let gdif = float <| (snd colors.[1]) - (fst colors.[1])
    let bdif = float <| (snd colors.[2]) - (fst colors.[2])
    let (low,high) = range //amount of iterations
    let span = float <| itlist.[low] - itlist.[high]
    let localhist = Array.init (high-low+1) (fun i -> (itlist.[low+i]-itlist.[low]) |> float |> ( / ) span) //hist shows percent of pixels that have this value or lower
    let colorcalc (fraction:float) : color =
        ImgUtil.fromRgb((fst colors.[0]) + int (fraction*rdif),(fst colors.[1]) + int (fraction*gdif),(fst colors.[2]) + int (fraction*bdif))
    let res = Array.init (localhist.Length) (fun j -> (j+low,colorcalc localhist.[j]))
    res

let buildhist (iterations:int[]) : int[] =
    let mutable counter = 0
    let histogram = Array.init (iterations.Length) (fun i -> 
        counter <- counter + iterations.[i]
        counter)
    histogram

let buildcolormap (iterations:int[]) (max:int) : (Map<int,color>) =
    let hist = buildhist iterations
    let (a,b,c) = determineindex max //index is amount of interations
    // Color settings are customizable by editing the ranges written below
    let arraya = mapittocolor hist a [|(30,150);(30,0);(30,150)|]
    let arrayb = mapittocolor hist b [|(120,190);(30,255);(120,190)|]
    let arrayc = mapittocolor hist c [|(120,190);(120,190);(30,255)|]
    let fullarray = Array.append arraya <| Array.append arrayb arrayc
    let map = Map.ofArray fullarray |> Map.add max (ImgUtil.fromRgb (0,0,0)) //sets max to black
    map

// --------------------------- Fixed colors: ---------------------------

let fixedcolormap (max:int) : (Map<int,color>) =
    // Returns a map from iterations to a color. The map has three bands - one for the first 10% of iterations, one for the next 15%, and one for the remaining 75%.
    let ((a0,a1),(b0,b1),(c0,c1)) = determineindex max //ranges are decided/changed in determineindex
    let colorfromrange (range:int*int) (num:int) (colors:(int*int) []) : color =
        let factor = (float (num - fst range)) / (float ((snd range - fst range)))
        let r = (snd colors.[0]) - (fst colors.[0]) |> float |> ( * ) factor |> int |> ( + ) (fst colors.[0])
        let g = (snd colors.[1]) - (fst colors.[1]) |> float |> ( * ) factor |> int |> ( + ) (fst colors.[1])
        let b = (snd colors.[2]) - (fst colors.[2]) |> float |> ( * ) factor |> int |> ( + ) (fst colors.[2])
        ImgUtil.fromRgb (r,g,b)
    let colorarray = Array.init (max+1) (fun n ->
        match n with
        | n when n = max -> (n,ImgUtil.fromRgb (0,0,0))
        | n when c0 <= n && n <= c1 -> (n,colorfromrange (c0,c1) n [|(120,190);(120,190);(30,255)|])
        | n when b0 <= n && n <= b1 -> (n,colorfromrange (b0,b1) n [|(120,190);(30,255);(120,190)|])
        | n when a0 <= n && n <= a1 -> (n,colorfromrange (a0,a1) n [|(30,150);(30,0);(30,150)|])
        | _ -> (n,ImgUtil.fromRgb (255,255,255)) //white pixels shouldn't appear, they are a sign that something went wrong
        )
    let colormap = Map.ofArray colorarray 
    colormap

// ------------------------------ Make single frames: ---------------------------

let makeframe_fixed (pos:complexnumber) (dif:float) (res:pixelpoint) (max:int) (filename:string) (colormap:(Map<int,color>)) : unit =
    // Make a single frame. Takes colormap as argument, and simply maps the amount of iterations with the colormap.
    let (pixelarray,_) = count_iter pos dif res max
    let canvas = ImgUtil.init (fst res) (snd res) (fun (i,j) -> colormap.[pixelarray.[i,j]])
    ImgUtil.toPngFile filename canvas

let makeframe_hist (pos:complexnumber) (dif:float) (res:pixelpoint) (max:int) (filename:string) : unit =
    // Make a single frame, using a histogram to spread the colors according to the fraction of pixels with the current color. 
    // This ensures that the color doesn't depend on max iterations, which creates better images for large max iteration values
    let (pixelarray,iter_count) = count_iter pos dif res max
    let colormap = buildcolormap iter_count max
    let canvas = ImgUtil.init (fst res) (snd res) (fun (i,j) -> colormap.[(pixelarray.[i,j])])
    ImgUtil.toPngFile filename canvas

// ----------------------------- General use: --------------------------

let zoomin (oldpos:complexnumber) (dif:float) (res:pixelpoint) : complexnumber =
    // Assumes that we are interested in halving dif
    (fst oldpos + 0.25 * dif * float (fst res),snd oldpos - 0.25 * dif * float (fst res))

let zoomout (oldpos:complexnumber) (dif:float) (res:pixelpoint) : complexnumber =
    // Assumes that we are doubling dif
    (fst oldpos - 0.5 * dif * float (fst res),snd oldpos + 0.5 * dif * float (fst res))

let topleft (res:pixelpoint) (center:complexnumber) (dif:float) : complexnumber =
    // calculate coordinate of top left pixel
    let a = fst res/2 |> float |> ( * ) dif |> ( - ) (fst center)
    let b = snd res/2 |> float |> ( * ) dif |> ( + ) (snd center)
    (a,b)


let rec userinput (center:complexnumber) (dif:float) (res:pixelpoint) : unit =
    let moveunit = float (fst res) / 4.0 //when moving, we are shifting by a quarter of the displayed image
    (* printfn "Begin makeframe: %f" stopWatch.Elapsed.TotalMilliseconds *)
    do makeframe_hist (topleft res center dif) dif res 100 "zoom/Manual_image.png"
    (* printfn "End makeframe: %f" stopWatch.Elapsed.TotalMilliseconds *)
    printfn "Fractal rendered!"
    let rec inputloop () =
        let input = System.Console.ReadKey(true)
        printfn "Received %A..." input.Key
        match input with
            | key when key.Key = System.ConsoleKey.UpArrow -> userinput (fst center,snd center + (moveunit*dif)) dif res
            | key when key.Key = System.ConsoleKey.DownArrow -> userinput (fst center,snd center - (moveunit*dif)) dif res
            | key when key.Key = System.ConsoleKey.RightArrow -> userinput (fst center + (moveunit*dif),snd center) dif res
            | key when key.Key = System.ConsoleKey.LeftArrow -> userinput (fst center - (moveunit*dif),snd center) dif res
            | key when key.Key = System.ConsoleKey.A -> userinput (zoomin center dif res) (0.5*dif) res
            | key when key.Key = System.ConsoleKey.D -> userinput (zoomout center dif res) (2.0*dif) res
            | key when key.Key = System.ConsoleKey.I -> printfn "Current location is: %A" center; inputloop ()
            | key when key.Key = System.ConsoleKey.Q -> exit 0
            | _ -> 
                do printfn "Use arrow-keys to navigate, A to zoom in, and D to zoom out, I to print coordinates - Q to quit"
                inputloop ()
    inputloop()
    
let makezoom (mode:string) (center:complexnumber) (dif:float) (res:pixelpoint) (max:int) (frames:int) : unit = 
    // generates series of images for using in a zoom gif
    // center is her the actual center of the image, not the top left!
    // mode is either "hist" or "fixed"
    let name = "zoom"
    let zoomdegree = 0.05 //this means dif is 5 % smaller for every round.
    let filename (name:string) (n:int) = "zoom/" + name + (string n) + ".png"
    let rec zoomer (dif:float) (n:int) : unit =
        if n % 10 = 0 then printfn "%A frames rendered, t=%Ams" n stopWatch.Elapsed.TotalMilliseconds
        if n < frames then
            let pos = topleft res center dif
            if mode = "fixed" then let colormap = fixedcolormap max in makeframe_fixed pos dif res max (filename name n) colormap
            elif mode = "hist" then makeframe_hist pos dif res max (filename name n)
            zoomer (dif*(1.0-zoomdegree)) (n+1)
        else 
            printfn "Zoom rendered in %Ams" stopWatch.Elapsed.TotalMilliseconds
    zoomer dif 0
        
[<EntryPoint>]
let main (args:string array) : int =
    //args contains: mode, x coord, y coord, max iterations, frames
    System.IO.Directory.CreateDirectory "zoom/" |> ignore //creates a directory for the images if one doesn't already exist
    let res = (800,800) //res can be changed here
    let manres = (1600,1600) //higher resolution for manually created images
    let dif = 0.001
    try
        if args.[0] = "manual" then userinput (0.0,0.0) dif manres |> ignore
        else
            let (mode,x,y,max,frames) = (args.[0],float args.[1],float args.[2],int args.[3],int args.[4])
            if args.Length <> 5 then raise (System.ArgumentException "Error: \"To create a zoom, you must enter exaclty 5 arguments: mode (manual,histzoom or zoom), x-coord (float), y-coord (float), max iterations (int), frames (int)\"")
            if mode = "zoom" then do makezoom "fixed" (x,y) dif res max frames |> ignore
            if mode = "histzoom" then do makezoom "hist" (x,y) dif res max frames |> ignore
        exit 0
    with
        | :? System.ArgumentException as ex -> printfn "%A" ex.Message; 1

//Example of generating 200 frames with 100 max iterations at (-0.207107867093967,1.122757063632597i):
//fsharpc -r img_util.dll fractalimages.fsx && mono fractalimages.exe zoom -0.207107867093967 1.122757063632597 100 200