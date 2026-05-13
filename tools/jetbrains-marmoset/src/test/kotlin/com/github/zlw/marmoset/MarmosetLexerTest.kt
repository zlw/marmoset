package com.github.zlw.marmoset

import com.intellij.psi.tree.IElementType
import kotlin.test.Test
import kotlin.test.assertEquals

class MarmosetLexerTest {
    @Test
    fun `native lexer recognizes core token classes`() {
        val tokens = lex("""fn read(path: Str) -> Result[Int, Error] = try parse(path) or 0 # fallback""")

        assertEquals(MarmosetTokenTypes.KEYWORD, tokens["fn"])
        assertEquals(MarmosetTokenTypes.TYPE, tokens["Str"])
        assertEquals(MarmosetTokenTypes.TYPE, tokens["Result"])
        assertEquals(MarmosetTokenTypes.OPERATOR, tokens["->"])
        assertEquals(MarmosetTokenTypes.KEYWORD, tokens["try"])
        assertEquals(MarmosetTokenTypes.KEYWORD, tokens["or"])
        assertEquals(MarmosetTokenTypes.NUMBER, tokens["0"])
        assertEquals(MarmosetTokenTypes.COMMENT, tokens["# fallback"])
    }

    private fun lex(source: String): Map<String, IElementType> {
        val lexer = MarmosetLexer()
        lexer.start(source)
        val tokens = linkedMapOf<String, IElementType>()
        while (lexer.tokenType != null) {
            val tokenText = source.substring(lexer.tokenStart, lexer.tokenEnd)
            if (tokenText.isNotBlank()) {
                tokens[tokenText] = lexer.tokenType!!
            }
            lexer.advance()
        }
        return tokens
    }
}
