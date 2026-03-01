#!/usr/bin/env python3
"""Convert shell test files to .mr fixture files."""
import re
import sys
import os

def convert_shell_to_fixtures(shell_path, output_dir, prefix):
    with open(shell_path) as f:
        content = f.read()

    tests = []
    lines = content.split('\n')
    i = 0
    while i < len(lines):
        line = lines[i]
        stripped = line.strip()

        # Skip Go-inspection tests (already in OCaml)
        if stripped.startswith('run_emit_go_not_contains_from_stdin') or \
           stripped.startswith('run_codegen_deterministic_from_stdin') or \
           stripped.startswith('run_build_ok_not_contains_from_stdin'):
            while i < len(lines) and lines[i].strip() != 'EOF':
                i += 1
            i += 1
            continue

        # test_case: collect full command handling both \ continuation AND multi-line single quotes
        if stripped.startswith('test_case '):
            raw_lines = [lines[i]]
            while True:
                full_so_far = '\n'.join(raw_lines)
                in_single_quote = full_so_far.count("'") % 2 != 0
                last_rstrip = raw_lines[-1].rstrip()
                if in_single_quote or last_rstrip.endswith('\\'):
                    i += 1
                    if i >= len(lines):
                        break
                    raw_lines.append(lines[i])
                    continue
                break

            full = '\n'.join(raw_lines)

            # Extract name (first double-quoted string)
            name_m = re.match(r'\s*test_case\s+"([^"]+)"', full)
            if name_m:
                name = name_m.group(1)

                # Extract single-quoted source
                sq_start = full.find("'")
                sq_end = full.rfind("'")
                if sq_start != -1 and sq_end > sq_start:
                    source = full[sq_start+1:sq_end]
                else:
                    source = ''

                # Determine true/false from text after last single quote
                after_source = full[sq_end+1:] if sq_end != -1 else full
                if '"true"' in after_source:
                    should_succeed = 'true'
                    error_fragment = None
                elif '"false"' in after_source:
                    should_succeed = 'false'
                    # Check for error fragment
                    err_m = re.search(r'"false"\s+"([^"]*)"', after_source)
                    error_fragment = err_m.group(1) if err_m else None
                else:
                    should_succeed = 'true'
                    error_fragment = None

                if should_succeed == 'true':
                    tests.append(('build', name, '', source))
                else:
                    if error_fragment and error_fragment != '__ANY_ERROR__':
                        tests.append(('reject', name, f'# error: {error_fragment}', source))
                    else:
                        tests.append(('reject', name, '# error: *', source))

            i += 1
            continue

        # expect_runtime_output "name" "expected" << 'EOF' ... EOF (single-line expected)
        m = re.match(r'''expect_runtime_output\s+"([^"]+)"\s+"((?:[^"\\]|\\.)*)"\s*<<\s*'EOF'\s*$''', stripped)
        if m:
            name = m.group(1)
            expected = m.group(2)
            i += 1
            source_lines = []
            while i < len(lines) and lines[i].strip() != 'EOF':
                source_lines.append(lines[i])
                i += 1
            i += 1
            source = '\n'.join(source_lines)
            annotations = []
            for out_line in expected.split('\\n'):
                annotations.append(f'# output: {out_line}')
            tests.append(('run', name, '\n'.join(annotations), source))
            continue

        # expect_build "name" "expected" << 'EOF' ... EOF
        m = re.match(r'''expect_build\s+"([^"]+)"\s+"((?:[^"\\]|\\.)*)"\s*<<\s*'EOF'\s*$''', stripped)
        if m:
            name = m.group(1)
            expected = m.group(2)
            i += 1
            source_lines = []
            while i < len(lines) and lines[i].strip() != 'EOF':
                source_lines.append(lines[i])
                i += 1
            i += 1
            source = '\n'.join(source_lines)
            if expected == '':
                tests.append(('build', name, '', source))
            elif expected == '__ANY_ERROR__':
                tests.append(('reject', name, '# error: *', source))
            else:
                tests.append(('reject', name, f'# error: {expected}', source))
            continue

        # expect_build "name" << 'EOF' ... EOF (no expected = build-only)
        m = re.match(r'''expect_build\s+"([^"]+)"\s*<<\s*'EOF'\s*$''', stripped)
        if m:
            name = m.group(1)
            i += 1
            source_lines = []
            while i < len(lines) and lines[i].strip() != 'EOF':
                source_lines.append(lines[i])
                i += 1
            i += 1
            source = '\n'.join(source_lines)
            tests.append(('build', name, '', source))
            continue

        # expect_runtime_output with multi-line expected (newline literal in the string)
        m = re.match(r'''expect_runtime_output\s+"([^"]+)"\s+"((?:[^"\\]|\\.)*)\s*$''', stripped)
        if m:
            # Collect lines until we see the closing quote + << 'EOF'
            full = stripped
            while i + 1 < len(lines):
                peek = lines[i + 1].rstrip()
                full += '\n' + peek
                i += 1
                if re.search(r'''"\s*<<\s*'EOF'\s*$''', peek):
                    break
            m2 = re.match(r'''expect_runtime_output\s+"([^"]+)"\s+"((?:[^"\\]|\\.)*)"\s*<<\s*'EOF'$''', full, re.DOTALL)
            if m2:
                name = m2.group(1)
                expected = m2.group(2)
                i += 1
                source_lines = []
                while i < len(lines) and lines[i].strip() != 'EOF':
                    source_lines.append(lines[i])
                    i += 1
                i += 1
                source = '\n'.join(source_lines)
                annotations = []
                for out_line in expected.split('\n'):
                    annotations.append(f'# output: {out_line}')
                tests.append(('run', name, '\n'.join(annotations), source))
                continue

        i += 1

    # Write fixture files
    os.makedirs(output_dir, exist_ok=True)
    for idx, (mode, name, annotation, source) in enumerate(tests, 1):
        safe_name = re.sub(r'[^a-zA-Z0-9_]', '_', name.lower())
        safe_name = re.sub(r'_+', '_', safe_name).strip('_')[:60]
        filename = f'{prefix}{idx:02d}_{safe_name}.mr'

        header = f'# {prefix.upper()}{idx:02d}: {name}'
        parts = [header]
        if annotation:
            parts.append(annotation)
        parts.append(source.strip())
        parts.append('')

        filepath = os.path.join(output_dir, filename)
        with open(filepath, 'w') as f:
            f.write('\n'.join(parts))

    return len(tests)

if __name__ == '__main__':
    shell_path = sys.argv[1]
    output_dir = sys.argv[2]
    prefix = sys.argv[3]
    count = convert_shell_to_fixtures(shell_path, output_dir, prefix)
    print(f"Converted {count} tests from {os.path.basename(shell_path)} -> {output_dir}")
