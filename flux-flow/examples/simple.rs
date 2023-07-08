use std::{
    fs::{read_to_string, File},
    io::Write,
    path::Path,
    time::{Duration, Instant},
};

use flux_flow::compiler::parser::{parse, syntax_tree::Item};
use notify::Watcher;

const IN: &str = "in.rs";
const OUT: &str = "out.rs";

fn compile(prev: &mut String) {
    let code = read_to_string(IN).unwrap();
    if code.is_empty() || &code == prev {
        return;
    }
    println!("{} -> {}", prev.len(), code.len());
    *prev = code.clone();

    let out = &mut File::create(OUT).unwrap();

    let start = Instant::now();
    let mut count = 0;
    let count = loop {
        count += 1;
        parse(&code);
        if start.elapsed() > Duration::from_millis(100) {
            break count;
        }
    };

    println!("Took {:#?} ({count} iterations)", start.elapsed() / count);
    writeln!(out, "{code}").unwrap();
    writeln!(out).unwrap();

    let result = parse(&code);

    let map = result.syntax_tree.map_nodes::<Item, _>(|x| x);

    for diagnostic in result.diagnostics {
        let line_start = code[..diagnostic.range.start]
            .char_indices()
            .rev()
            .find_map(|(index, char)| (char == '\n').then_some(index + 1))
            .unwrap_or(0);
        let line_end = code
            .char_indices()
            .skip(diagnostic.range.end)
            .find_map(|(index, char)| (char == '\n').then_some(index))
            .unwrap_or(code.len());
        writeln!(out, "{}", &code[line_start..line_end]).unwrap();

        let pad = code[line_start..diagnostic.range.start].chars().count();

        for _ in 0..pad {
            write!(out, " ").unwrap();
        }

        if diagnostic.range.is_empty() {
            write!(out, "\\").unwrap();
        } else {
            for _ in 0..diagnostic.range.len() {
                write!(out, "^").unwrap();
            }
        }

        writeln!(out, " [{:?}] {:?}", diagnostic.severity, diagnostic.error).unwrap();
    }
    writeln!(out).unwrap();

    result.syntax_tree.visualize(&code);
}

fn main() -> anyhow::Result<()> {
    let mut prev = String::new();
    compile(&mut prev);

    let mut watcher = notify::recommended_watcher(move |res| match res {
        Ok(_event) => compile(&mut prev),
        Err(e) => println!("watch error: {:?}", e),
    })?;

    watcher.watch(Path::new(IN), notify::RecursiveMode::NonRecursive)?;
    std::thread::park();

    Ok(())
}
