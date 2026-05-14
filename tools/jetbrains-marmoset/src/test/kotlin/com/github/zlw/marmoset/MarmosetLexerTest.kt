package com.github.zlw.marmoset

import com.intellij.psi.tree.IElementType
import kotlin.test.Test
import kotlin.test.assertEquals

class MarmosetLexerTest {
    @Test
    fun `native lexer recognizes core token classes`() {
        val tokens = lex("""fn read(path: Str) -> Result[Int, Error] = try parse(path) or io_shim.read(path) # fallback""")

        assertEquals(MarmosetTokenTypes.KEYWORD, tokens["fn"])
        assertEquals(MarmosetTokenTypes.FUNCTION_DECLARATION, tokens["read"])
        assertEquals(MarmosetTokenTypes.TYPE, tokens["Str"])
        assertEquals(MarmosetTokenTypes.TYPE, tokens["Result"])
        assertEquals(MarmosetTokenTypes.OPERATOR, tokens["->"])
        assertEquals(MarmosetTokenTypes.OPERATOR, lex("""fn apply(f: (Int) ~> Int) ~> Int = f(1)""")["~>"])
        assertEquals(MarmosetTokenTypes.KEYWORD, tokens["try"])
        assertEquals(MarmosetTokenTypes.FUNCTION_CALL, tokens["parse"])
        assertEquals(MarmosetTokenTypes.KEYWORD, tokens["or"])
        assertEquals(MarmosetTokenTypes.METHOD_CALL, tokens["io_shim.read"])
        assertEquals(MarmosetTokenTypes.COMMENT, tokens["# fallback"])
    }

    @Test
    fun `string interpolation exposes embedded expression tokens`() {
        val tokens = lex("""let result = Write.write(writer, "#{value}\n")""")

        assertEquals(MarmosetTokenTypes.INTERPOLATION_START, tokens["#{"])
        assertEquals(MarmosetTokenTypes.IDENTIFIER, tokens["value"])
        assertEquals(MarmosetTokenTypes.INTERPOLATION_END, tokens["}"])
    }

    private fun lex(source: String): Map<String, IElementType> {
        val lexer = MarmosetLexer()
        lexer.start(source)
        val tokens = linkedMapOf<String, IElementType>()
        while (lexer.tokenType != null) {
            val tokenText = tokenText(source, lexer.tokenStart, lexer.tokenEnd)
            if (tokenText.isNotBlank()) {
                tokens[tokenText] = lexer.tokenType!!
            }
            lexer.advance()
        }
        return tokens
    }

    private fun tokenText(source: String, tokenStart: Int, tokenEnd: Int): String {
        if (tokenStart >= 2 && source[tokenStart - 1] == '.' && isIdentifierPart(source[tokenStart - 2])) {
            var receiverStart = tokenStart - 2
            while (receiverStart > 0 && isIdentifierPart(source[receiverStart - 1])) {
                receiverStart--
            }
            return source.substring(receiverStart, tokenEnd)
        }
        return source.substring(tokenStart, tokenEnd)
    }

    private fun isIdentifierPart(ch: Char): Boolean =
        ch == '_' || ch == '?' || ch.isLetterOrDigit()
}
