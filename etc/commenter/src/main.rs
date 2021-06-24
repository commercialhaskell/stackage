use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

use regex::Regex;

type H = HashMap<Header, Vec<(String, String, String)>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Header {
    Versioned { package: String, version: String },
    Missing { package: String },
}

fn main() {
    let mut lib_exes: H = Default::default();
    let mut tests: H = Default::default();
    let mut benches: H = Default::default();
    let mut last_header: Option<Header> = None;
    let empty = Regex::new(r#"^\s*$"#).unwrap();
    let header_versioned =
        Regex::new(r#"^(?P<package>[a-zA-z]([a-zA-z0-9.-]*?))-(?P<version>(\d+(\.\d+)*)).+?is out of bounds for:$"#).unwrap();
    let header_missing =
        Regex::new(r#"^(?P<package>[a-zA-z]([a-zA-z0-9.-]*)).+?depended on by:$"#).unwrap();
    let package =
        Regex::new(r#"^- \[ \] (?P<package>[a-zA-z]([a-zA-z0-9.-]*?))-(?P<version>(\d+(\.\d+)*)).+?Used by: (?P<component>.+)$"#)
        .unwrap();

    if let Ok(lines) = read_lines("./comments.txt") {
        for line in lines {
            if let Ok(line) = line {
                if empty.captures(&line).is_some() {
                } else if let Some(cap) = package.captures(&line) {
                    let root = last_header.clone().unwrap();
                    let package = cap.name("package").unwrap().as_str();
                    let version = cap.name("version").unwrap().as_str();
                    let component = cap.name("component").unwrap().as_str();
                    match component {
                        "library" | "executable" => {
                            insert(&mut lib_exes, root, package, version, component)
                        }
                        "benchmark" => insert(&mut benches, root, package, version, "benchmarks"),
                        "test-suite" => insert(&mut tests, root, package, version, component),
                        _ => panic!("Bad component: {}", component),
                    }
                } else if let Some(cap) = header_versioned.captures(&line) {
                    let package = cap.name("package").unwrap().as_str().to_owned();
                    let version = cap.name("version").unwrap().as_str().to_owned();
                    last_header = Some(Header::Versioned { package, version });
                } else if let Some(cap) = header_missing.captures(&line) {
                    let package = cap.name("package").unwrap().as_str().to_owned();
                    last_header = Some(Header::Missing { package });
                } else {
                    panic!("Unhandled: {:?}", line);
                }
            }
        }
    }

    if !lib_exes.is_empty() {
        println!("\nLIBS + EXES\n");
    }
    for (header, packages) in lib_exes {
        for (package, version, component) in packages {
            printer("        ", &package, true, &version, &component, &header);
        }
    }

    if !tests.is_empty() {
        println!("\nTESTS\n");
    }
    for (header, packages) in tests {
        for (package, version, component) in packages {
            printer("    ", &package, false, &version, &component, &header);
        }
    }

    if !benches.is_empty() {
        println!("\nBENCHMARKS\n");
    }
    for (header, packages) in benches {
        for (package, version, component) in packages {
            printer("    ", &package, false, &version, &component, &header);
        }
    }
}

fn printer(
    indent: &str,
    package: &str,
    lt0: bool,
    version: &str,
    component: &str,
    header: &Header,
) -> () {
    let lt0 = if lt0 { " < 0" } else { "" };
    println!(
        "{indent}- {package}{lt0} # tried {package}-{version}, but its *{component}* {cause}",
        indent = indent,
        package = package,
        lt0 = lt0,
        version = version,
        component = component,
        cause = match header {
            Header::Versioned { package, version } => format!(
                "does not support: {package}-{version}",
                package = package,
                version = version
            ),
            Header::Missing { package } => format!(
                "requires the disabled package: {package}",
                package = package
            ),
        },
    );
}

fn insert(h: &mut H, header: Header, package: &str, version: &str, component: &str) {
    (*h.entry(header).or_insert_with(|| vec![])).push((
        package.to_owned(),
        version.to_owned(),
        component.to_owned(),
    ));
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
