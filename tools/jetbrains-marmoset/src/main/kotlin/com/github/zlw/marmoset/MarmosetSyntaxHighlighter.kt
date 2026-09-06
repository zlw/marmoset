package com.github.zlw.marmoset

import com.intellij.lexer.Lexer
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors
import com.intellij.openapi.editor.HighlighterColors
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType

class MarmosetSyntaxHighlighter : SyntaxHighlighterBase() {
    override fun getHighlightingLexer(): Lexer = MarmosetLexer()

    override fun getTokenHighlights(tokenType: IElementType): Array<TextAttributesKey> =
        when (tokenType) {
            MarmosetTokenTypes.COMMENT -> LINE_COMMENT_KEYS
            MarmosetTokenTypes.STRING -> STRING_KEYS
            MarmosetTokenTypes.NUMBER -> NUMBER_KEYS
            MarmosetTokenTypes.KEYWORD -> KEYWORD_KEYS
            MarmosetTokenTypes.BUILTIN -> BUILTIN_KEYS
            MarmosetTokenTypes.TYPE -> TYPE_KEYS
            MarmosetTokenTypes.FUNCTION_DECLARATION -> FUNCTION_DECLARATION_KEYS
            MarmosetTokenTypes.FUNCTION_CALL -> FUNCTION_CALL_KEYS
            MarmosetTokenTypes.METHOD_CALL -> METHOD_CALL_KEYS
            MarmosetTokenTypes.INTERPOLATION_START -> OPERATOR_KEYS
            MarmosetTokenTypes.INTERPOLATION_END -> OPERATOR_KEYS
            MarmosetTokenTypes.OPERATOR -> OPERATOR_KEYS
            MarmosetTokenTypes.PUNCTUATION -> PUNCTUATION_KEYS
            TokenType.BAD_CHARACTER -> BAD_CHARACTER_KEYS
            else -> EMPTY_KEYS
        }

    companion object {
        private val EMPTY_KEYS = emptyArray<TextAttributesKey>()
        private val LINE_COMMENT_KEYS = pack(DefaultLanguageHighlighterColors.LINE_COMMENT)
        private val STRING_KEYS = pack(DefaultLanguageHighlighterColors.STRING)
        private val NUMBER_KEYS = pack(DefaultLanguageHighlighterColors.NUMBER)
        private val KEYWORD_KEYS = pack(DefaultLanguageHighlighterColors.KEYWORD)
        private val BUILTIN_KEYS = pack(DefaultLanguageHighlighterColors.PREDEFINED_SYMBOL)
        private val TYPE_KEYS = pack(DefaultLanguageHighlighterColors.CLASS_REFERENCE)
        private val FUNCTION_DECLARATION_KEYS = pack(DefaultLanguageHighlighterColors.FUNCTION_DECLARATION)
        private val FUNCTION_CALL_KEYS = pack(DefaultLanguageHighlighterColors.FUNCTION_CALL)
        private val METHOD_CALL_KEYS = pack(DefaultLanguageHighlighterColors.INSTANCE_METHOD)
        private val OPERATOR_KEYS = pack(DefaultLanguageHighlighterColors.OPERATION_SIGN)
        private val PUNCTUATION_KEYS = pack(DefaultLanguageHighlighterColors.BRACES)
        private val BAD_CHARACTER_KEYS = pack(HighlighterColors.BAD_CHARACTER)
    }
}
