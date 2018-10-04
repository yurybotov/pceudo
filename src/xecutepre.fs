open config
open cmdline
open elem
open inputstrings
open splitlines
open parse

// выбор таргет генератора кода
let generate ( e : Elm list ) =
    if target = "stm8s" then stm8.printcode (stm8.generate e)
    elif target = "stm8l" then stm8.printcode (stm8.generate e)
    elif target = "avr" then avr.printcode (avr.generate e)
    elif target = "msp430" then msp430.printcode (msp430.generate e)
    else ""

// запуск внешнего приложения (для запуска нативных инструментов)
let exec exefile parameters =
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- exefile
    p.StartInfo.Arguments <- parameters
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.UseShellExecute <- false
    p.Start() |> ignore
    printfn "%s" (p.StandardOutput.ReadToEnd())


[<EntryPoint>]
let main argv =
    printfn "Naive pseudo assembler. Copyright 2018 Yury Botov."
    // разбор командной строки
    let name,ext = cmdlineparser (Array.toList argv)

    // разбивка файлов на строки и наложение препроцессора
    printfn "Preprocessing..."
    let lines = readLines (name+"."+ext)  
 
    // разбивка строк на примитивы
    let ops = splitLines lines

    // парсинг псевдо-ассемблера
    printfn "Parsing..."
    let parsed = parse ops

    // генерирование таргет ассемблера
    printfn "Generation..."
    let out = generate parsed
    
    // сохраняем asm файл
    System.IO.File.WriteAllText(name+".asm", out)

    printfn "Native assembler..."
    if target = "stm8s" || target = "stm8l" then
        exec @"C:\Program Files (x86)\STMicroelectronics\st_toolset\asm\asm.exe" ("-sym -li="+name + ".lsr "+name + ".asm ")
        exec @"C:\Program Files (x86)\STMicroelectronics\st_toolset\asm\lyn.exe" (""+name + ".obj,"+name + ",;")
        exec @"C:\Program Files (x86)\STMicroelectronics\st_toolset\asm\obsend.exe" (""+name + ",f,"+name + ".s19,s")
        exec @"C:\Program Files (x86)\STMicroelectronics\st_toolset\asm\abslist.exe" (""+ name + ".lsr -o "+ name + ".lst -fmt srec -exe "+ name + ".s19 -map "+ name + ".map")
    elif target = "avr" then 
        exec @"C:\Program Files (x86)\WinAVR\avr\bin\as.exe" ( "-al="+name+".lst  -mmcu=avr1 -o " + name + ".o " + name + ".asm")
        exec @"C:\Program Files (x86)\WinAVR\avr\bin\ld.exe" ( "-o a.out "+name + ".o")
    elif target = "msp430" then
        ()

    0