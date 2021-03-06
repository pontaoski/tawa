namespace SimpleLang

module Parser =
    open FParsec
    open AST

    let position: Parser<_,_> = fun stream -> FParsec.Reply stream.Position

    // helper funcs
    let str s = pstring s
    let wrappedBy x y = between x x y
    let skipWs x = between spaces spaces x

    // primitives
    let lParen = str "("
    let rParen = str ")"
    let lBracket = str "{"
    let rBracket = str "}"
    let comma = str ","
    let eos = (str ";") <|> newlineReturn ";" <|> (eof >>% ";")

    // identifiers
    let identifierLiteral: Parser<Identifier,unit> =
        let firstChar c = isLetter c || c = '_' || c = '\''
        let generalChar c = firstChar c || isDigit c

        many1Satisfy2L firstChar generalChar "identifier"

    // expressions
    let (typeLiteral: Parser<Type,unit>), typeLiteralImpl = createParserForwardedToRef()

    let parseIdentType = identifierLiteral |>> Ident
    let parseFuncType: Parser<Type,unit> =
        let arguments =
            lParen >>. (sepBy (skipWs typeLiteral) comma) .>> rParen

        let returns =
            (opt typeLiteral)

        (str "func") >>.
             skipWs arguments
        .>>. returns
        |>> FunctionPointer
    let parseStructType: Parser<Type,unit> =
        let eoStructField = (str ",") <|> newlineReturn ","

        let field: Parser<(Identifier*Type),unit> =
            (identifierLiteral .>> (skipWs (str ":")) .>>. typeLiteral)

        (str "struct") >>. (skipWs lBracket) >>. (many (spaces >>. field .>> eoStructField .>> spaces)) .>> spaces .>> rBracket |>> Struct


    do typeLiteralImpl :=
        parseFuncType <|> parseStructType <|> parseIdentType

    // literals
    let literal: Parser<Literal,unit> =
        let integerLiteral =
            pint64 |>> Integer
        
        integerLiteral

    // expressions
    let (expression: Parser<Expression,unit>), anyExpressionImpl = createParserForwardedToRef()

    // statements
    let statement: Parser<(Position * Expression),unit> =
        position .>>. expression .>> eos

    let fnCall =
        let arguments =
            sepBy expression (skipWs comma)

        identifierLiteral .>> (skipWs lParen) .>>. (skipWs arguments) .>> spaces .>> rParen

    let block =
        lBracket >>. spaces >>. (many (skipWs statement)) .>> rBracket

    let ifElse =
        (str "if") >>. spaces >>. expression .>> spaces .>> (str "then") .>> spaces .>>. expression .>> spaces .>> (str "else") .>> spaces .>>. expression
        |>> fun((a, b), c) -> (a, b, c)

    do anyExpressionImpl :=
        attempt block |>> Block
        <|> attempt (literal |>> Lit)
        <|> attempt (fnCall |>> Call)
        <|> attempt (ifElse |>> If)
        <|> (identifierLiteral |>> Var)

    // toplevels
    let topLevel =
        let functionToplevel: Parser<TopLevel,unit> =
            let argument: Parser<(Identifier*Type),unit> =
                (identifierLiteral .>> (skipWs (str ":")) .>>. typeLiteral)

            let arguments =
                lParen >>. (sepBy (skipWs argument) comma) .>> rParen

            let returns =
                (opt typeLiteral)

            let parseExpr =
                let bloc = (block |>> Block)
                let other = (skipWs (str "=>")) >>. expression
                attempt bloc <|> other

            (str "func") >>.
                 (skipWs identifierLiteral)
            .>>. (skipWs arguments)
            .>>. (skipWs returns)
            .>>. (parseExpr)
            |>> fun (((a, b), c), d) -> Func(a, b, c, d)

        let typeToplevel: Parser<TopLevel,unit> =
            (str "type") >>. (skipWs identifierLiteral) .>>. typeLiteral
            |>> TypeDeclaration

        typeToplevel <|> functionToplevel

    let program =
        let toplevel = topLevel .>> eos

        (spaces >>. toplevel) |> many1

    let parseProgram programText filename = runParserOnString program () filename programText

    let test p str =
        match run p str with
        | Success (result, _, _) -> printfn "Success: %A" result
        | Failure (errorMessage, _, _) -> printfn "Failure: %s" errorMessage

    let must p str =
        match run p str with
        | Success (result, _, _) -> result
        | Failure (errorMessage, e, _) -> failwith (sprintf "%s %A" errorMessage e)
