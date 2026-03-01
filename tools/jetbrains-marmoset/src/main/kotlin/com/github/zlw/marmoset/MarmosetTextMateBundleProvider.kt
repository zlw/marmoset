package com.github.zlw.marmoset

import org.jetbrains.plugins.textmate.api.TextMateBundleProvider
import java.nio.file.Path

class MarmosetTextMateBundleProvider : TextMateBundleProvider {
    override fun getBundles(): List<TextMateBundleProvider.PluginBundle> {
        val url = MarmosetTextMateBundleProvider::class.java.getResource("/textmate")
            ?: return emptyList()
        val path = Path.of(url.toURI())
        return listOf(TextMateBundleProvider.PluginBundle("Marmoset", path))
    }
}
