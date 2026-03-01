#!/bin/sh
hash=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
ts=$(date -u '+%Y-%m-%dT%H:%M:%SZ')
echo "let git_hash = \"$hash\""
echo "let build_time = \"$ts\""
