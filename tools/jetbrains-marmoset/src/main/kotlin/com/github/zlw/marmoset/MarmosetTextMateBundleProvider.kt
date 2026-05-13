package com.github.zlw.marmoset

import org.jetbrains.plugins.textmate.api.TextMateBundleProvider
import java.net.URI
import java.net.URL
import java.nio.file.FileSystem
import java.nio.file.FileSystemAlreadyExistsException
import java.nio.file.FileSystemNotFoundException
import java.nio.file.FileSystems
import java.nio.file.Path

class MarmosetTextMateBundleProvider : TextMateBundleProvider {
    override fun getBundles(): List<TextMateBundleProvider.PluginBundle> {
        val url = MarmosetTextMateBundleProvider::class.java.getResource("/textmate")
            ?: return emptyList()
        val path = bundlePathFromResource(url)
        return listOf(TextMateBundleProvider.PluginBundle("Marmoset", path))
    }

    internal companion object {
        private val openedJarFileSystems = mutableListOf<FileSystem>()

        internal fun bundlePathFromResource(url: URL): Path {
            val uri = url.toURI()
            return try {
                Path.of(uri)
            } catch (error: FileSystemNotFoundException) {
                if (uri.scheme != "jar") {
                    throw error
                }
                ensureJarFileSystem(uri)
                Path.of(uri)
            }
        }

        @Synchronized
        private fun ensureJarFileSystem(resourceUri: URI) {
            val rootUri = jarRootUri(resourceUri)
            try {
                openedJarFileSystems += FileSystems.newFileSystem(rootUri, emptyMap<String, Any>())
            } catch (_: FileSystemAlreadyExistsException) {
                // Another provider call already opened this plugin jar for jar-backed resource paths.
            }
        }

        private fun jarRootUri(resourceUri: URI): URI {
            val rawUri = resourceUri.toString()
            val separator = rawUri.indexOf("!")
            require(separator >= 0) { "jar resource URI is missing '!': $resourceUri" }
            return URI.create(rawUri.substring(0, separator))
        }
    }
}
