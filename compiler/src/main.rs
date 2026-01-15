use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use picolang_compiler::diagnostics::Diagnostic;
use picolang_compiler::ir::lower_program_to_ir;
use picolang_compiler::llvm_codegen::compile_ir_to_object;
use picolang_compiler::module_loader::{load_program_from_root_file, ModuleDiagnostics};

fn main() -> ExitCode {
    let mut args = env::args().skip(1);
    let Some(path) = args.next() else {
        eprintln!("usage: picolang-compiler <path-to-entry-file.pico | path-to-project-dir>");
        return ExitCode::FAILURE;
    };

    if args.next().is_some() {
        eprintln!("error: expected exactly one argument");
        eprintln!("usage: picolang-compiler <path-to-entry-file.pico | path-to-project-dir>");
        return ExitCode::FAILURE;
    }

    let ok = handle_entry_path(Path::new(&path));
    if ok {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn handle_entry_path(path: &Path) -> bool {
    let meta = match fs::metadata(path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!(
                "error: failed to read metadata for {}: {}",
                path.display(),
                e
            );
            return false;
        }
    };

    let entry_file = if meta.is_dir() {
        let main_path = path.join("main.pico");
        if !main_path.is_file() {
            eprintln!(
                "error: directory {} does not contain a main.pico file (expected entry point)",
                path.display()
            );
            return false;
        }
        main_path
    } else {
        path.to_path_buf()
    };

    println!(
        "=== Building program rooted at file {} ===",
        entry_file.display()
    );

    let (program, module_diags) = match load_program_from_root_file(&entry_file) {
        Ok(x) => x,
        Err(e) => {
            eprintln!(
                "error: failed to load program from {}: {}",
                entry_file.display(),
                e
            );
            return false;
        }
    };

    if !module_diags.is_empty() {
        for md in &module_diags {
            print_module_diags(md);
        }
        return false;
    }

    let ir_module = lower_program_to_ir(&program);

    let module_name = entry_file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("program");

    let obj_path = entry_file.with_extension("o");

    match compile_ir_to_object(&ir_module, module_name, &obj_path) {
        Ok(()) => {
            println!();
            println!("LLVM codegen: wrote object file to {}", obj_path.display());
        }
        Err(e) => {
            eprintln!("LLVM codegen failed: {}", e);
            return false;
        }
    }

    let exe_path = exe_path_for_object(&obj_path);
    match link_object_to_executable(&obj_path, &exe_path) {
        Ok(()) => {
            println!("Linker: wrote executable to {}", exe_path.display());
            true
        }
        Err(e) => {
            eprintln!("Linker failed: {}", e);
            false
        }
    }
}

fn print_module_diags(md: &ModuleDiagnostics) {
    println!("==============================");
    println!("Semantic / parse errors in {}", md.path.display());
    println!("------------------------------");
    let filename = md.path.to_string_lossy().to_string();
    print_diagnostics(&filename, &md.source, &md.diagnostics);
}

fn print_diagnostics(filename: &str, source: &str, diags: &[Diagnostic]) {
    let mut diags = diags.to_vec();
    diags.sort_by_key(|d| d.span.start);

    let mut seen: HashSet<(usize, String)> = HashSet::new();
    let mut printed = Vec::new();

    for d in diags {
        let (line, col) = byte_to_line_col(source, d.span.start);
        let key = (line, d.message.clone());
        if seen.insert(key) {
            printed.push((d, line, col));
        }
    }

    let count = printed.len();
    if count == 0 {
        println!("(no diagnostics)");
        return;
    }

    let plural = if count == 1 { "" } else { "s" };
    println!("{} error{}", count, plural);
    println!();

    for (d, line, col) in printed {
        print_one_diagnostic(filename, source, &d, line, col);
        println!();
    }
}

fn print_one_diagnostic(filename: &str, source: &str, d: &Diagnostic, line: usize, col: usize) {
    let start = d.span.start;
    let end = d.span.end.min(source.len());

    let line_start = source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line_end = source[end..]
        .find('\n')
        .map(|off| end + off)
        .unwrap_or_else(|| source.len());
    let line_text = &source[line_start..line_end];

    let rel_start = start.saturating_sub(line_start);
    let rel_end = {
        let r = end.saturating_sub(line_start);
        r.max(rel_start + 1)
    };

    let mut underline = String::new();
    for (byte_idx, ch) in line_text.char_indices() {
        if byte_idx < rel_start {
            underline.push(if ch == '\t' { '\t' } else { ' ' });
        } else if byte_idx < rel_end {
            underline.push('^');
        }
    }

    println!("\x1b[31merror\x1b[0m: {}", d.message);
    println!(" --> {}:{}:{}", filename, line, col);
    println!("  |");
    println!("{:>3} | {}", line, line_text);
    println!("    | {}", underline);
}

fn byte_to_line_col(source: &str, byte_idx: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;

    for (idx, ch) in source.char_indices() {
        if idx >= byte_idx {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    (line, col)
}

fn link_object_to_executable(obj_path: &Path, exe_path: &Path) -> Result<(), String> {
    use std::process::Command;

    let mut cmd = Command::new("cc");
    cmd.arg(obj_path).arg("-o").arg(exe_path);

    if cfg!(target_os = "linux") {
        cmd.arg("-no-pie");
    }

    if let Some(runtime_dir) = env::var_os("PICOLANG_RUNTIME_LIB_DIR") {
        cmd.arg("-L").arg(runtime_dir);
        cmd.arg("-lpicolang_runtime");
    }

    let status = cmd
        .status()
        .map_err(|e| format!("failed to spawn linker 'cc': {e}"))?;

    if !status.success() {
        return Err(format!("linker 'cc' exited with status {status}"));
    }

    Ok(())
}

fn exe_path_for_object(obj_path: &Path) -> PathBuf {
    use std::ffi::OsStr;

    let stem = obj_path.file_stem().unwrap_or_else(|| OsStr::new("a.out"));
    let mut exe = obj_path.with_file_name(stem);
    if cfg!(target_os = "windows") {
        exe.set_extension("exe");
    }
    exe
}
