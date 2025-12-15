use crate::ast::Module;
use crate::diagnostics::{Diagnostic, Span};
use crate::parser::parse_source;
use crate::symbols::{build_symbol_table, check_module_exprs};

use std::collections::HashSet;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct LoadedModule {
    pub path: PathBuf,
    pub source: String,
    pub ast: Module,
}

#[derive(Debug)]
pub struct ModuleDiagnostics {
    pub path: PathBuf,
    pub source: String,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
pub struct Program {
    pub modules: Vec<LoadedModule>,
}

impl Program {
    pub fn is_empty(&self) -> bool {
        self.modules.is_empty()
    }
}

pub fn load_program_from_root_file(
    root_file: &Path,
) -> io::Result<(Program, Vec<ModuleDiagnostics>)> {
    let root_dir = root_file
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();

    let mut modules = Vec::new();
    let mut all_diags = Vec::new();

    let mut worklist: Vec<PathBuf> = vec![root_file.to_path_buf()];
    let mut seen_files: HashSet<PathBuf> = HashSet::new();

    while let Some(path) = worklist.pop() {
        let path = PathBuf::from(&path);
        if !seen_files.insert(path.clone()) {
            continue; // already processed
        }

        let source = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => {
                let diag = Diagnostic::new_error(
                    format!("failed to read file: {}", e),
                    Span { start: 0, end: 0 },
                );
                all_diags.push(ModuleDiagnostics {
                    path: path.clone(),
                    source: String::new(),
                    diagnostics: vec![diag],
                });
                continue;
            }
        };

        match parse_source(&source) {
            Ok(module) => {
                let (symtab, mut sym_diags) = build_symbol_table(&module);
                let mut expr_diags = check_module_exprs(&module, &symtab);
                sym_diags.append(&mut expr_diags);

                let mut seen: HashSet<(usize, String)> = HashSet::new();
                let mut deduped = Vec::new();
                for d in sym_diags {
                    let key = (d.span.start, d.message.clone());
                    if seen.insert(key) {
                        deduped.push(d);
                    }
                }

                if !deduped.is_empty() {
                    all_diags.push(ModuleDiagnostics {
                        path: path.clone(),
                        source: source.clone(),
                        diagnostics: deduped,
                    });
                }

                modules.push(LoadedModule {
                    path: path.clone(),
                    source: source.clone(),
                    ast: module,
                });

                let imports = find_imports_in_source(&source);
                for import_name in imports {
                    let rel = import_name.replace('.', "/") + ".my";
                    let imported_path = PathBuf::from(&root_dir.join(rel));
                    if !seen_files.contains(&imported_path) {
                        worklist.push(imported_path);
                    }
                }
            }
            Err(parse_diags) => {
                all_diags.push(ModuleDiagnostics {
                    path: path.clone(),
                    source,
                    diagnostics: parse_diags,
                });
            }
        }
    }

    Ok((Program { modules }, all_diags))
}

pub fn load_program_from_dir(dir: &Path) -> io::Result<(Program, Vec<ModuleDiagnostics>)> {
    let main_path = dir.join("main.my");
    if main_path.is_file() {
        return load_program_from_root_file(&main_path);
    }

    let mut modules = Vec::new();
    let mut all_diags = Vec::new();

    let mut entries: Vec<PathBuf> = fs::read_dir(dir)?
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("my") {
                Some(path)
            } else {
                None
            }
        })
        .collect();

    entries.sort();

    for path in entries {
        let path = PathBuf::from(&path);
        let source = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => {
                let diag = Diagnostic::new_error(
                    format!("failed to read file: {}", e),
                    Span { start: 0, end: 0 },
                );
                all_diags.push(ModuleDiagnostics {
                    path: path.clone(),
                    source: String::new(),
                    diagnostics: vec![diag],
                });
                continue;
            }
        };

        match parse_source(&source) {
            Ok(module) => {
                let (symtab, mut sym_diags) = build_symbol_table(&module);
                let mut expr_diags = check_module_exprs(&module, &symtab);
                sym_diags.append(&mut expr_diags);

                if !sym_diags.is_empty() {
                    all_diags.push(ModuleDiagnostics {
                        path: path.clone(),
                        source: source.clone(),
                        diagnostics: sym_diags,
                    });
                }

                modules.push(LoadedModule {
                    path: path.clone(),
                    source,
                    ast: module,
                });
            }
            Err(parse_diags) => {
                all_diags.push(ModuleDiagnostics {
                    path: path.clone(),
                    source,
                    diagnostics: parse_diags,
                });
            }
        }
    }

    Ok((Program { modules }, all_diags))
}

fn find_imports_in_source(src: &str) -> Vec<String> {
    let mut imports = Vec::new();

    for line in src.lines() {
        let line_no_comment = line.splitn(2, "//").next().unwrap_or("").trim();
        if !line_no_comment.starts_with("import ") {
            continue;
        }

        let rest = &line_no_comment["import ".len()..];
        let semi = rest.find(';').unwrap_or(rest.len());
        let name = rest[..semi].trim();

        if !name.is_empty() {
            imports.push(name.to_string());
        }
    }

    imports
}
