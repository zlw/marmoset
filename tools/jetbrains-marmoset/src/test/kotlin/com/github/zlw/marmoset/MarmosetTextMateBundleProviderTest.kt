package com.github.zlw.marmoset

import java.net.URI
import java.nio.file.Files
import java.util.jar.JarEntry
import java.util.jar.JarOutputStream
import kotlin.test.Test
import kotlin.test.assertTrue

class MarmosetTextMateBundleProviderTest {
    @Test
    fun `bundle path supports resources packaged inside plugin jar`() {
        val tempDir = Files.createTempDirectory("marmoset-textmate-provider-test")
        val jarPath = tempDir.resolve("marmoset-plugin.jar")
        JarOutputStream(Files.newOutputStream(jarPath)).use { jar ->
            jar.putNextEntry(JarEntry("textmate/"))
            jar.closeEntry()
            jar.putNextEntry(JarEntry("textmate/package.json"))
            jar.write("""{"name":"marmoset"}""".toByteArray())
            jar.closeEntry()
        }

        val resourceUrl = URI.create("jar:${jarPath.toUri()}!/textmate").toURL()
        val bundlePath = MarmosetTextMateBundleProvider.bundlePathFromResource(resourceUrl)
        val bundlePathAgain = MarmosetTextMateBundleProvider.bundlePathFromResource(resourceUrl)

        assertTrue(Files.isDirectory(bundlePath))
        assertTrue(Files.exists(bundlePath.resolve("package.json")))
        assertTrue(Files.isDirectory(bundlePathAgain))
    }
}
