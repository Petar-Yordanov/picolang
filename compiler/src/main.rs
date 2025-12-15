use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use mylang_compiler::diagnostics::Diagnostic;
use mylang_compiler::ir::lower_modules_to_ir;
use mylang_compiler::llvm_codegen::compile_ir_to_object;
use mylang_compiler::module_loader::{
    load_program_from_dir, load_program_from_root_file, ModuleDiagnostics,
};
use mylang_compiler::parser::parse_source;
use mylang_compiler::symbols::{build_symbol_table, check_module_exprs};

fn main() {
    let arg = env::args().nth(1);

    match arg {
        Some(path) => {
            let path = PathBuf::from(path);
            handle_entry_path(&path);
        }

        None => {
            println!("=== Building program from examples/modules (expected to succeed) ===");

            match load_program_from_dir(Path::new("examples/modules")) {
                Ok((program, module_diags)) => {
                    if !module_diags.is_empty() {
                        for md in &module_diags {
                            print_module_diags(md);
                        }
                    } else {
                        let ir_module = lower_modules_to_ir(program.modules.iter().map(|m| &m.ast));

                        println!("==============================");
                        println!("Lowered IR for program in directory examples/modules");
                        println!("------------------------------");
                        println!("{:#?}", ir_module);

                        // Emit object file via LLVM backend + link to executable
                        let obj_path = Path::new("examples_modules.o");
                        match compile_ir_to_object(&ir_module, "examples_modules", obj_path) {
                            Ok(()) => {
                                println!();
                                println!(
                                    "LLVM codegen: wrote object file to {}",
                                    obj_path.display()
                                );

                                let exe_path = exe_path_for_object(obj_path);
                                match link_object_to_executable(obj_path, &exe_path) {
                                    Ok(()) => {
                                        println!(
                                            "Linker: wrote executable to {}",
                                            exe_path.display()
                                        );
                                    }
                                    Err(e) => {
                                        eprintln!("Linker failed: {}", e);
                                    }
                                }
                            }
                            Err(e) => {
                                eprintln!("LLVM codegen failed: {}", e);
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!("error: failed to load program from examples/modules: {}", e);
                }
            }

            println!();
            println!("=== Parsing examples/error_modules (expected to show errors) ===");
            handle_error_dir("examples/error_modules");
        }
    }
}

fn handle_entry_path(path: &Path) {
    let meta = match fs::metadata(path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!(
                "error: failed to read metadata for {}: {}",
                path.display(),
                e
            );
            return;
        }
    };

    if meta.is_dir() {
        println!("=== Building program from directory {} ===", path.display());

        let main_path = path.join("main.my");
        if !main_path.is_file() {
            eprintln!(
                "error: directory {} does not contain a main.my file (expected entry point)",
                path.display()
            );
            return;
        }

        println!("Using {} as program entry file", main_path.display());

        match load_program_from_root_file(&main_path) {
            Ok((program, module_diags)) => {
                if !module_diags.is_empty() {
                    for md in &module_diags {
                        print_module_diags(md);
                    }
                } else {
                    let ir_module = lower_modules_to_ir(program.modules.iter().map(|m| &m.ast));

                    println!("==============================");
                    println!("Lowered IR for program rooted at {}", main_path.display());
                    println!("------------------------------");
                    println!("{:#?}", ir_module);

                    let module_name = path
                        .file_name()
                        .and_then(|s| s.to_str())
                        .unwrap_or("program");
                    let obj_path = path.join("program.o");

                    match compile_ir_to_object(&ir_module, module_name, &obj_path) {
                        Ok(()) => {
                            println!();
                            println!("LLVM codegen: wrote object file to {}", obj_path.display());

                            let exe_path = exe_path_for_object(&obj_path);
                            match link_object_to_executable(&obj_path, &exe_path) {
                                Ok(()) => {
                                    println!("Linker: wrote executable to {}", exe_path.display());
                                }
                                Err(e) => {
                                    eprintln!("Linker failed: {}", e);
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("LLVM codegen failed: {}", e);
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!(
                    "error: failed to load program from {}: {}",
                    main_path.display(),
                    e
                );
            }
        }
    } else {
        println!("=== Building program rooted at file {} ===", path.display());
        match load_program_from_root_file(path) {
            Ok((program, module_diags)) => {
                if !module_diags.is_empty() {
                    for md in &module_diags {
                        print_module_diags(md);
                    }
                } else {
                    let ir_module = lower_modules_to_ir(program.modules.iter().map(|m| &m.ast));

                    println!("==============================");
                    println!("Lowered IR for program rooted at {}", path.display());
                    println!("------------------------------");
                    println!("{:#?}", ir_module);

                    let module_name = path
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("program");
                    let obj_path = path.with_extension("o");

                    match compile_ir_to_object(&ir_module, module_name, &obj_path) {
                        Ok(()) => {
                            println!();
                            println!("LLVM codegen: wrote object file to {}", obj_path.display());

                            let exe_path = exe_path_for_object(&obj_path);
                            match link_object_to_executable(&obj_path, &exe_path) {
                                Ok(()) => {
                                    println!("Linker: wrote executable to {}", exe_path.display());
                                }
                                Err(e) => {
                                    eprintln!("Linker failed: {}", e);
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("LLVM codegen failed: {}", e);
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!(
                    "error: failed to load program from {}: {}",
                    path.display(),
                    e
                );
            }
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

fn handle_error_dir(dir: &str) {
    let mut entries: Vec<_> = match fs::read_dir(dir) {
        Ok(read_dir) => read_dir
            .filter_map(|entry| {
                let entry = entry.ok()?;
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("my") {
                    Some(path)
                } else {
                    None
                }
            })
            .collect(),
        Err(e) => {
            eprintln!("error: failed to read directory {}: {}", dir, e);
            return;
        }
    };

    entries.sort();

    for path in entries {
        handle_error_file(&path);
    }
}

fn handle_error_file(path: &Path) {
    let filename = path.to_string_lossy().to_string();
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("failed to read {}: {}", filename, e);
            return;
        }
    };

    match parse_source(&source) {
        Ok(module) => {
            let (symtab, mut sym_diags) = build_symbol_table(&module);
            let mut expr_diags = check_module_exprs(&module, &symtab);
            sym_diags.append(&mut expr_diags);

            if !sym_diags.is_empty() {
                println!("==============================");
                println!("Semantic errors (symbol table / exprs) in {}", filename);
                println!("------------------------------");
                print_diagnostics(&filename, &source, &sym_diags);
            } else {
                println!("==============================");
                println!("Parsed & resolved symbols for {}", filename);
                println!("------------------------------");
                println!("(no diagnostics)");
            }
        }
        Err(diags) => {
            println!("==============================");
            println!("Parsing file: {}", filename);
            println!("------------------------------");
            print_diagnostics(&filename, &source, &diags);
        }
    }
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
            if ch == '\t' {
                underline.push('\t');
            } else {
                underline.push(' ');
            }
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

    if let Some(runtime_dir) = env::var_os("MYLANG_RUNTIME_LIB_DIR") {
        cmd.arg("-L").arg(runtime_dir);
        cmd.arg("-lmylang_runtime");
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
