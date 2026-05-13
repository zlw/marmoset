package com.github.zlw.marmoset

import com.intellij.lexer.LexerBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType

class MarmosetLexer : LexerBase() {
    private var buffer: CharSequence = ""
    private var startOffset: Int = 0
    private var endOffset: Int = 0
    private var tokenStart: Int = 0
    private var tokenEnd: Int = 0
    private var tokenType: IElementType? = null

    override fun start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int) {
        this.buffer = buffer
        this.startOffset = startOffset
        this.endOffset = endOffset
        this.tokenStart = startOffset
        locateToken()
    }

    override fun getState(): Int = 0
    override fun getTokenType(): IElementType? = tokenType
    override fun getTokenStart(): Int = tokenStart
    override fun getTokenEnd(): Int = tokenEnd
    override fun getBufferSequence(): CharSequence = buffer
    override fun getBufferEnd(): Int = endOffset

    override fun advance() {
        tokenStart = tokenEnd
        locateToken()
    }

    private fun locateToken() {
        if (tokenStart >= endOffset) {
            tokenEnd = tokenStart
            tokenType = null
            return
        }

        val ch = buffer[tokenStart]
        when {
            ch.isWhitespace() -> scanWhile(TokenType.WHITE_SPACE) { it.isWhitespace() }
            ch == '#' -> scanComment()
            ch == '"' -> scanString()
            ch.isDigit() -> scanNumber()
            isIdentifierStart(ch) -> scanIdentifier()
            isOperator(ch) -> scanWhile(MarmosetTokenTypes.OPERATOR, ::isOperator)
            isPunctuation(ch) -> scanFixed(MarmosetTokenTypes.PUNCTUATION, tokenStart + 1)
            else -> scanFixed(TokenType.BAD_CHARACTER, tokenStart + 1)
        }
    }

    private fun scanComment() {
        var cursor = tokenStart
        while (cursor < endOffset && buffer[cursor] != '\n') {
            cursor++
        }
        scanFixed(MarmosetTokenTypes.COMMENT, cursor)
    }

    private fun scanString() {
        var cursor = tokenStart + 1
        var escaped = false
        while (cursor < endOffset) {
            val ch = buffer[cursor]
            cursor++
            if (escaped) {
                escaped = false
            } else if (ch == '\\') {
                escaped = true
            } else if (ch == '"') {
                break
            }
        }
        scanFixed(MarmosetTokenTypes.STRING, cursor)
    }

    private fun scanNumber() {
        var cursor = tokenStart
        while (cursor < endOffset && buffer[cursor].isDigit()) {
            cursor++
        }
        if (cursor + 1 < endOffset && buffer[cursor] == '.' && buffer[cursor + 1].isDigit()) {
            cursor++
            while (cursor < endOffset && buffer[cursor].isDigit()) {
                cursor++
            }
        }
        scanFixed(MarmosetTokenTypes.NUMBER, cursor)
    }

    private fun scanIdentifier() {
        var cursor = tokenStart + 1
        while (cursor < endOffset && isIdentifierPart(buffer[cursor])) {
            cursor++
        }
        val text = buffer.subSequence(tokenStart, cursor).toString()
        val type = when {
            text in KEYWORDS -> MarmosetTokenTypes.KEYWORD
            text in BUILTINS -> MarmosetTokenTypes.BUILTIN
            text.first().isUpperCase() -> MarmosetTokenTypes.TYPE
            isAfterKeyword("fn") -> MarmosetTokenTypes.FUNCTION_DECLARATION
            isAfterDot() && isFollowedByCall(cursor) -> MarmosetTokenTypes.METHOD_CALL
            isFollowedByCall(cursor) -> MarmosetTokenTypes.FUNCTION_CALL
            else -> MarmosetTokenTypes.IDENTIFIER
        }
        scanFixed(type, cursor)
    }

    private fun scanWhile(type: IElementType, predicate: (Char) -> Boolean) {
        var cursor = tokenStart + 1
        while (cursor < endOffset && predicate(buffer[cursor])) {
            cursor++
        }
        scanFixed(type, cursor)
    }

    private fun scanFixed(type: IElementType, end: Int) {
        tokenType = type
        tokenEnd = end
    }

    private fun isAfterKeyword(keyword: String): Boolean {
        var cursor = tokenStart - 1
        while (cursor >= startOffset && buffer[cursor].isWhitespace()) {
            cursor--
        }
        val end = cursor + 1
        while (cursor >= startOffset && isIdentifierPart(buffer[cursor])) {
            cursor--
        }
        if (end == cursor + 1) {
            return false
        }
        val start = cursor + 1
        return buffer.subSequence(start, end).toString() == keyword
    }

    private fun isAfterDot(): Boolean {
        var cursor = tokenStart - 1
        while (cursor >= startOffset && buffer[cursor].isWhitespace()) {
            cursor--
        }
        return cursor >= startOffset && buffer[cursor] == '.'
    }

    private fun isFollowedByCall(identifierEnd: Int): Boolean {
        var cursor = identifierEnd
        while (cursor < endOffset && buffer[cursor].isWhitespace()) {
            cursor++
        }
        return cursor < endOffset && buffer[cursor] == '('
    }

    companion object {
        private val KEYWORDS = setOf(
            "as",
            "case",
            "derive",
            "else",
            "enum",
            "export",
            "extern",
            "fn",
            "from",
            "if",
            "impl",
            "import",
            "is",
            "let",
            "match",
            "or",
            "override",
            "return",
            "shape",
            "trait",
            "try",
            "type",
            "wrap",
        )

        private val BUILTINS = setOf(
            "false",
            "len",
            "none",
            "puts",
            "true",
        )

        private fun isIdentifierStart(ch: Char): Boolean =
            ch == '_' || ch.isLetter()

        private fun isIdentifierPart(ch: Char): Boolean =
            ch == '_' || ch == '?' || ch.isLetterOrDigit()

        private fun isOperator(ch: Char): Boolean =
            ch in "=!+-*/%<>|&"

        private fun isPunctuation(ch: Char): Boolean =
            ch in "(){}[],:;."
    }
}
