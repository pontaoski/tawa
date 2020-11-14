// Learn more about F# at http://fsharp.org

open System
open SimpleLang.Parser
open SimpleLang.Codegen

let yate = "type Yeet struct {
    yeet: struct {
        yote: int
        yate: string
    }
    funky: func(int, int) int
}"

[<EntryPoint>]
let main argv =
    printf "testcase 1:\n\t%A\n" (must expression "5")
    printf "testcase 2:\n\t%A\n" (must expression "yoot(5)")
    printf "testcase 3:\n\t%A\n" (must expression "yoot")
    printf "testcase 4:\n\t%A\n" (must expression "if 0 then 1 else 0")
    printf "testcase 5:\n\t%A\n\n\n" (must program "func main() int64 { main(5); }")
    printf "testcase 6:\n\t%A\n\n\n" (must program "type Yeet int64")
    printf "testcase 7:\n\t%A\n\n\n" (must program "type Yeet func(int, int) int")
    printf "testcase 8:\n\t%A\n\n\n" (must program yate)

    0 // return an integer exit code
