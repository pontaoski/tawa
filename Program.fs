// Learn more about F# at http://fsharp.org

open System
open SimpleLang.Parser
open SimpleLang.Codegen

[<EntryPoint>]
let main argv =
    printf "testcase 1:\n\t%A\n" (must expression "5")
    printf "testcase 2:\n\t%A\n" (must expression "yoot(5)")
    printf "testcase 3:\n\t%A\n" (must expression "yoot")
    printf "testcase 4:\n\t%A\n" (must expression "if 0 then 1 else 0")
    printf "testcase 5:\n\t%A\n\n\n" (must program "func main() int64 { main(5); }")

    codegen (must program "func main() int64 { main(5); }")
    printf "\n\n\n"
    codegen (must program "func not(input: bool) bool { if true then false else true; }")
    printf "\n\n\n"
    codegen (must program "func not(input: bool) bool => if true then false else true")
    printf "\n\n\n"
    codegen (must program "func main() {}")

    0 // return an integer exit code
