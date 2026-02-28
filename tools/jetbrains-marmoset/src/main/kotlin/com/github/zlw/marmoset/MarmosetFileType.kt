package com.github.zlw.marmoset

import com.intellij.openapi.fileTypes.LanguageFileType
import javax.swing.Icon

object MarmosetFileType : LanguageFileType(MarmosetLanguage) {
    override fun getName(): String = "Marmoset"
    override fun getDescription(): String = "Marmoset source file"
    override fun getDefaultExtension(): String = "mr"
    override fun getIcon(): Icon = MarmosetIcons.FILE
}
