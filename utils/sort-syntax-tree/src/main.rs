use std::{
    fs::{read_to_string, File},
    io::{BufWriter, Write},
    iter::once,
};

const SYNTAX_TREE_RS_PATH: &str = "flux-flow/src/compiler/parser/syntax_tree.rs";

const SYNTAX_TREE_NODES_START: &str = "syntax_tree_nodes! {";
const SYNTAX_TREE_NODES_END: &str = "}";

fn main() {
    let file = read_to_string(SYNTAX_TREE_RS_PATH).unwrap();
    let mut lines = file.lines();

    assert!(lines.next_back() == Some(SYNTAX_TREE_NODES_END));
    assert!(lines.next_back() == Some(""));

    let syntax_tree_node_lines = lines
        .skip_while(|&line| line != SYNTAX_TREE_NODES_START)
        .skip(1)
        .collect::<Vec<_>>();

    let mut syntax_tree_nodes = syntax_tree_node_lines
        .split(|&line| line.is_empty())
        .collect::<Vec<_>>();

    syntax_tree_nodes.sort_unstable_by_key(|group| {
        group
            .iter()
            .find(|line| !line.starts_with("///"))
            .map(|line| line.trim_start_matches(char::is_alphabetic).trim_start())
    });

    let output = file
        .lines()
        .take_while(|&line| line != SYNTAX_TREE_NODES_START)
        .chain(once(SYNTAX_TREE_NODES_START))
        .chain(
            syntax_tree_nodes
                .into_iter()
                .flat_map(|group| group.iter().chain(once(&"")))
                .copied(),
        )
        .chain(once(SYNTAX_TREE_NODES_END));

    let mut writer = BufWriter::new(File::create(SYNTAX_TREE_RS_PATH).unwrap());
    for line in output {
        writeln!(writer, "{}", line).unwrap();
    }
}
