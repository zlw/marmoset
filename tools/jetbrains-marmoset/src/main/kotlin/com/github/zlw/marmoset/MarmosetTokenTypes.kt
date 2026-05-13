package com.github.zlw.marmoset

import com.intellij.psi.tree.IElementType

object MarmosetTokenTypes {
    val COMMENT = IElementType("MARMOSSET_COMMENT", MarmosetLanguage)
    val STRING = IElementType("MARMOSSET_STRING", MarmosetLanguage)
    val NUMBER = IElementType("MARMOSSET_NUMBER", MarmosetLanguage)
    val KEYWORD = IElementType("MARMOSSET_KEYWORD", MarmosetLanguage)
    val BUILTIN = IElementType("MARMOSSET_BUILTIN", MarmosetLanguage)
    val TYPE = IElementType("MARMOSSET_TYPE", MarmosetLanguage)
    val IDENTIFIER = IElementType("MARMOSSET_IDENTIFIER", MarmosetLanguage)
    val OPERATOR = IElementType("MARMOSSET_OPERATOR", MarmosetLanguage)
    val PUNCTUATION = IElementType("MARMOSSET_PUNCTUATION", MarmosetLanguage)
}
