module [start]

import cli.Stdin
import cli.Stdout
import Lexer
import Parser
import Evaluator
import Object
import Environment

start =
    Stdout.line! "Welcome to the REPL of Marmoset (Monkey) programming language, written in Roc! 🐵🤘"
    environment = Environment.new

    Task.loop! environment \env ->
        Stdout.write! ">> "

        line = Stdin.line!

        (evaled_program, new_env) =
            line
            |> Lexer.new
            |> Parser.new
            |> Parser.parseProgram
            |> \(_, program) -> Evaluator.eval program env

        evaled_program |> Object.toStr |> Stdout.line!

        Task.ok (Step new_env)
