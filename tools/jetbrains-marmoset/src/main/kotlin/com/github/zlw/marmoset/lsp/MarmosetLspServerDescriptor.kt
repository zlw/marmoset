package com.github.zlw.marmoset.lsp

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.platform.lsp.api.ProjectWideLspServerDescriptor
import java.nio.file.Path

internal class MarmosetLspServerDescriptor(project: Project) :
    ProjectWideLspServerDescriptor(project, "Marmoset") {

    override fun isSupportedFile(file: VirtualFile): Boolean = file.extension == "mr"

    override fun createCommandLine(): GeneralCommandLine {
        val marmosetRoot = System.getenv("MARMOSET_ROOT")
            ?.takeIf { it.isNotBlank() }
            ?: error("MARMOSET_ROOT is not set; set it to the Marmoset repo root")
        val commandPath = Path.of(marmosetRoot, "marmoset")

        return GeneralCommandLine(commandPath.toString(), "lsp")
            .withEnvironment("MARMOSET_ROOT", marmosetRoot)
    }
}
