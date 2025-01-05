module [start]

import cli.Stdin
import cli.Stdout
import Lexer
import Parser
import Evaluator
import Object

start =
    Stdout.line! "Welcome to the REPL of Marmoset (Monkey) programming language, written in Roc! 🐵🤘"

    Task.loop! 0 \_ ->
        Stdout.write! ">> "

        line = Stdin.line!

        line
        |> Lexer.new
        |> Parser.new
        |> Parser.parseProgram
        |> \(_, program) -> Evaluator.eval program
        |> Object.toStr
        |> Stdout.line!

        Task.ok (Step 0)
