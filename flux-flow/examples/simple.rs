use std::{
    fs::{read_to_string, File},
    io::Write,
    thread::sleep,
    time::{Duration, Instant},
};

use flux_flow::compiler::parser::parse;

fn main() {
    let mut prev = String::new();
    loop {
        sleep(Duration::from_millis(200));
        let code = read_to_string("in.rs").unwrap();
        if code == prev {
            continue;
        }
        prev = code.clone();

        let out = &mut File::create("out.rs").unwrap();

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

        writeln!(out, "{:#?}", result.syntax_tree).unwrap();
    }
}
