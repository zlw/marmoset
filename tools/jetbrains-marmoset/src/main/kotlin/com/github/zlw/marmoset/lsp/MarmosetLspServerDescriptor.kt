package com.github.zlw.marmoset.lsp

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.platform.lsp.api.ProjectWideLspServerDescriptor

internal class MarmosetLspServerDescriptor(project: Project) :
    ProjectWideLspServerDescriptor(project, "Marmoset") {

    override fun isSupportedFile(file: VirtualFile): Boolean = file.extension == "mr"

    override fun createCommandLine(): GeneralCommandLine =
        GeneralCommandLine("marmoset", "lsp")
}
