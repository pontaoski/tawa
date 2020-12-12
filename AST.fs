namespace SimpleLang

module AST =
    open FParsec

    type Kind = int64

    type Identifier = string

    type Type =
        | Ident of Identifier
        | FunctionPointer of arguments: Type list * returns: Type option
        | Struct of fields: (Identifier * Type) list

    type Literal =
        | Integer of int64

    and Expression =
        | Lit of Literal
        | Var of Identifier
        | Call of fn: Identifier * args: (Expression list)
        | Block of (Position * Expression) list
        | If of cond: Expression * thenExpr: Expression * elseExpr: Expression

    type TopLevel =
        | Func of name: Identifier * arguments: (Identifier * Type) list * returns: Type option * expr: Expression
        | TypeDeclaration of name: Identifier * kind: Type

    type ASTNode =
        | T of TopLevel
        | E of Expression

module Codegen =
    open AST
    open LLVMSharp
    open System.Collections.Generic

    let llvmFalse = LLVMBool 0
    let llvmTrue = LLVMBool 1

    type IdentKind =
        | T of LLVMTypeRef
        | V of LLVMValueRef

        member this.AssertType =
            match this with
                | T(t) -> t
                | V(_) -> failwith "Expected a type"

        member this.AssertVal =
            match this with
                | T(_) -> failwith "Expected a value"
                | V(v) -> v

    type ContextDict = Dictionary<string, IdentKind>

    let boolType = LLVM.IntType(uint32 1)

    let defaultTypes =
        let d = new ContextDict()
        d.Add("int8", T(LLVM.Int8Type()))
        d.Add("int16", T(LLVM.Int16Type()))
        d.Add("int32", T(LLVM.Int32Type()))
        d.Add("int64", T(LLVM.Int64Type()))
        d.Add("int128", T(LLVM.Int128Type()))
        d.Add("float16", T(LLVM.HalfType()))
        d.Add("float32", T(LLVM.FloatType()))
        d.Add("float64", T(LLVM.DoubleType()))
        d.Add("float128", T(LLVM.FP128Type()))
        d.Add("bool", T(boolType))
        d.Add("niets", T(LLVM.VoidType()))
        d.Add("true", V(LLVM.ConstInt(boolType, uint64 1, llvmFalse)))
        d.Add("false", V(LLVM.ConstInt(boolType, uint64 0, llvmFalse)))
        d

    let rec codegenExpression (e: Expression) (b: LLVMBuilderRef) (h: ContextDict) =
        match e with
            | Lit(l) ->
                match l with
                    | Integer(i) -> LLVM.ConstInt(LLVM.Int64Type(), uint64 i, llvmFalse)
            | Var(v) ->
                h.[v].AssertVal
            | Call(c, a) ->
                let genArg a' =
                    codegenExpression a' b h

                LLVM.BuildCall(b, h.[c].AssertVal, List.map genArg a |> List.toArray, "calltmp")
            | Block(statements) ->
                let compileStatement s =
                    codegenExpression s b h

                let getExpr s: Expression =
                    let (_, expr) = s
                    expr

                let retVal = ((List.map (getExpr >> compileStatement) statements)) |> List.last

                retVal
            | If(cond, thenExpr, elseExpr) ->
                let condVal = codegenExpression cond b h
                let zero = LLVM.ConstInt(LLVM.Int32Type(), uint64 0, llvmFalse)
                let condCmp = LLVM.BuildICmp(b, LLVMIntPredicate.LLVMIntNE, condVal, zero, "ifcond")

                // the code where the function is
                let startBloc = LLVM.GetInsertBlock(b)
                let fn = LLVM.GetBasicBlockParent(startBloc)

                // then
                let thenBloc = LLVM.AppendBasicBlock(fn, "then")
                LLVM.PositionBuilderAtEnd(b, thenBloc)
                let thenVal = codegenExpression thenExpr b h
                let newThenBloc = LLVM.GetInsertBlock(b)

                // else
                let elseBloc = LLVM.AppendBasicBlock(fn, "else")
                LLVM.PositionBuilderAtEnd(b, elseBloc)
                let elseVal = codegenExpression elseExpr b h
                let newElseBloc = LLVM.GetInsertBlock(b)

                // merge
                let mergeBloc = LLVM.AppendBasicBlock(fn, "ifcont")
                LLVM.PositionBuilderAtEnd(b, mergeBloc)
                let phi = LLVM.BuildPhi(b, LLVM.Int32Type(), "iftmp")
                LLVM.AddIncoming(phi, [|thenVal; elseVal|], [| newThenBloc; newElseBloc |], uint32 2)

                // let's go back to the start bloc to add the conditional
                LLVM.PositionBuilderAtEnd(b, startBloc)
                LLVM.BuildCondBr(b, condCmp, thenBloc, elseBloc) |> ignore

                // now let's chain the branches to the merge bloc
                LLVM.PositionBuilderAtEnd(b, newThenBloc)
                LLVM.BuildBr(b, mergeBloc) |> ignore

                LLVM.PositionBuilderAtEnd(b, newElseBloc)
                LLVM.BuildBr(b, mergeBloc) |> ignore

                LLVM.PositionBuilderAtEnd(b, mergeBloc)
                phi

    let rec codegenType (modu: LLVMModuleRef) (builder: LLVMBuilderRef) (h: ContextDict) (t: Type) =
        match t with
            | Ident n -> h.[n].AssertType
            | FunctionPointer(a, r) ->
                let toType (args: Type list, returns: Type option) =
                    let retType =
                        match returns with
                            | Some(v) ->
                                codegenType modu builder h v
                            | None -> LLVM.VoidType()

                    let inTypes = (args |> List.map (codegenType modu builder h)) |> List.toArray

                    LLVM.FunctionType(retType, inTypes, false)

                toType(a, r)
            | Struct fields ->
                let getType a =
                    let (_, a') = a
                    a'

                LLVM.StructType(List.map (getType >> (codegenType modu builder h)) fields |> List.toArray, false)

    let rec codegenToplevel (modu: LLVMModuleRef) (builder: LLVMBuilderRef) (t: TopLevel) (h: ContextDict) =
        let toType (args: (Identifier * Type) list, returns: Type option) =
            let retType =
                match returns with
                    | Some(v) ->
                        codegenType modu builder h v
                    | None -> LLVM.VoidType()

            let argType (x: Identifier * Type) =
                let (_, kind) = x
                codegenType modu builder h kind

            let inTypes = (args |> (List.map argType)) |> List.toArray

            LLVM.FunctionType(retType, inTypes, false)

        match t with
            | Func(name, args, returns, statements) ->
                let fn = LLVM.AddFunction(modu, name, toType(args, returns))
                let bloc = LLVM.AppendBasicBlock(fn, "entry")

                h.Add(name, V(fn))

                LLVM.PositionBuilderAtEnd(builder, bloc)

                if args.Length > 0 then
                    for i in uint32 0..(LLVM.CountParams(fn)-uint32 1) do
                        let (ident, _) = args.[int i]
                        h.Add(ident, V(LLVM.GetParam(fn, i)))

                match returns with
                    | Some _ -> LLVM.BuildRet(builder, codegenExpression statements builder h) |> ignore
                    | None -> LLVM.BuildRetVoid(builder) |> ignore

                if args.Length > 0 then
                    for i in uint32 0..(LLVM.CountParams(fn)-uint32 1) do
                        let (ident, _) = args.[int i]
                        h.Remove(ident) |> ignore

                if LLVM.VerifyFunction(fn, LLVMVerifierFailureAction.LLVMPrintMessageAction).Value <> 0 then
                    LLVM.DeleteFunction(fn)

            | TypeDeclaration(name, kind) ->
                h.Add(name, T(codegenType modu builder h kind))

    let codegen (tls: AST.TopLevel list) =
        let builder = LLVM.CreateBuilder()
        let modu = LLVM.ModuleCreateWithName("main")
        let ctx = new ContextDict(defaultTypes)

        let compile t =
            codegenToplevel modu builder t ctx

        (List.map compile tls) |> ignore

        compile (("_start", [], None, (Call("main", []))) |> Func)

        ("main", modu)

    let compile (mods: (string * LLVMModuleRef) list) =
        let comp (amod: (string * LLVMModuleRef)) =
            let name, modu = amod

            let fname = sprintf "/tmp/tawa-%s.ll" name
            let str = ref ""
            let res = LLVM.PrintModuleToFile(modu, fname, str)

            if res.Value = 0 then
                printfn "Transpiled module %s successfully" name
            else
                printfn "Failed to transpile module %s: %s" name str.contents
                exit 1

            fname

        let files = (List.map comp) mods

        let args = ["-Wno-override-module"; "-nostdlib"] @ files @ ["-o"; "prog"]

        let proc = new System.Diagnostics.Process()
        proc.StartInfo.FileName <- "clang"
        for arg in args do
            proc.StartInfo.ArgumentList.Add(arg)

        proc.Start() |> ignore
        proc.WaitForExit()

        if proc.ExitCode = 0 then
            printfn "Compiled program"
        else
            printfn "Failed to compile program"
            exit 1

        ()
