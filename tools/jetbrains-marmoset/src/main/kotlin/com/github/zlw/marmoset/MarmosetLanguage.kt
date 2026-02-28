package com.github.zlw.marmoset

import com.intellij.lang.Language

object MarmosetLanguage : Language("Marmoset") {
    private fun readResolve(): Any = MarmosetLanguage
}
