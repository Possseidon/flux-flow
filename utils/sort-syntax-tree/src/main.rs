fn main() {
    let lines = std::io::stdin()
        .lines()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    let mut groups = lines.split(String::is_empty).collect::<Vec<_>>();

    groups.sort_unstable_by_key(|group| {
        group.first().map(|line| {
            line.trim_start_matches(|c: char| {
                c.is_alphanumeric() || ['_', '!', '{', '(', ' '].contains(&c)
            })
        })
    });

    for group in groups {
        for line in group {
            println!("{line}");
        }
        println!();
    }
}
