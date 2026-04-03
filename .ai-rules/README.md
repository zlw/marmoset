# .ai-rules

Shared source of truth for AI workflows in this repository.

## Layout

- `references/`: shared workflow guidance loaded on demand
- `skills/`: canonical skill definitions shared across Codex and Claude

## Tool Mapping

- Codex discovers the repo-local skills through `.agents/skills/`
- Claude discovers the same shared skills through `.claude/skills/`
- Both directories should contain only symlinks back into `.ai-rules/skills/`

## Current Conventions

- Keep the actual workflow logic here, not in tool-specific discovery directories
- Prefer one canonical skill per workflow
- Use symlinks from tool-specific paths back into this directory when a tool requires a fixed discovery path
- Use hyphenated skill names so the same skill works in both Claude and Codex
- Put local-only review artifacts in `docs/review/`
