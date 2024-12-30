module [start]

import cli.Stdin
import cli.Stdout
import Lexer

start =
    Stdout.line! "Welcome to the REPL of Marmoset (Monkey programming language) written in Roc! 🐵"

    Task.loop! 0 \_ ->
        Stdout.write! ">> "

        line = Stdin.line!
        tokens = Lexer.lex line

        Task.forEach! tokens \token ->
            token |> Inspect.toStr |> Stdout.write!

        Task.ok (Step 0)
