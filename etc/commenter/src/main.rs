use std::collections::HashMap;
use std::io::{self, BufRead};

use lazy_regex::regex;
use regex::Regex;
use structopt::StructOpt;

type H = HashMap<Header, Vec<(String, String, String)>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Header {
    Versioned { package: String, version: String },
    Missing { package: String },
}

#[derive(Debug, StructOpt)]
#[structopt(
    name = "commenter",
    about = "Automates generation of bounds in  build-constraints.yaml"
)]
enum Opt {
    Clear,
    Add,
    Outdated,
}

fn main() {
    let opt = Opt::from_args();
    match opt {
        Opt::Clear => clear(),
        Opt::Add => add(),
        Opt::Outdated => outdated(),
    }
}

fn clear() {
    commenter::clear();
}

fn outdated() {
    commenter::outdated();
}

fn add() {
    let mut lib_exes: H = Default::default();
    let mut tests: H = Default::default();
    let mut benches: H = Default::default();
    let mut last_header: Option<Header> = None;

    let header_versioned = regex!(
        r#"^(?P<package>[a-zA-z]([a-zA-z0-9.-]*?))-(?P<version>(\d+(\.\d+)*)).+?is out of bounds for:$"#
    );
    let header_missing = regex!(r#"^(?P<package>[a-zA-z]([a-zA-z0-9.-]*)).+?depended on by:$"#);
    let package = regex!(
        r#"^- \[ \] (?P<package>[a-zA-z]([a-zA-z0-9.-]*?))-(?P<version>(\d+(\.\d+)*)).+?Used by: (?P<component>.+)$"#
    );

    // Ignore everything until the bounds issues show up.
    let mut process_line = false;

    for line in io::stdin().lock().lines().flatten() {
        if is_reg_match(&line, regex!(r#"^\s*$"#)) {
            // noop
        } else if line == "curator: Snapshot dependency graph contains errors:" {
            process_line = true;
        } else if !process_line {
            println!("[INFO] {}", line);
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

    let mut auto_lib_exes = vec![];
    let mut auto_tests = vec![];
    let mut auto_benches = vec![];

    if !lib_exes.is_empty() {
        println!("\nLIBS + EXES\n");
    }
    for (header, packages) in lib_exes {
        for (package, version, component) in packages {
            let s = printer("        ", &package, true, &version, &component, &header);
            println!("{}", s);
            auto_lib_exes.push(s);
        }
    }

    if !tests.is_empty() {
        println!("\nTESTS\n");
    }
    for (header, packages) in tests {
        for (package, version, component) in packages {
            let s = printer("    ", &package, false, &version, &component, &header);
            println!("{}", s);
            auto_tests.push(s);
        }
    }

    if !benches.is_empty() {
        println!("\nBENCHMARKS\n");
    }
    for (header, packages) in benches {
        for (package, version, component) in packages {
            let s = printer("    ", &package, false, &version, &component, &header);
            println!("{}", s);
            auto_benches.push(s);
        }
    }

    println!();
    println!(
        "Adding {lib_exes} libs, {tests} tests, {benches} benches to build-constraints.yaml",
        lib_exes = auto_lib_exes.len(),
        tests = auto_tests.len(),
        benches = auto_benches.len()
    );
    commenter::add(auto_lib_exes, auto_tests, auto_benches);
}

fn printer(
    indent: &str,
    package: &str,
    lt0: bool,
    version: &str,
    component: &str,
    header: &Header,
) -> String {
    let lt0 = if lt0 { " < 0" } else { "" };
    format!(
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
    )
}

fn insert(h: &mut H, header: Header, package: &str, version: &str, component: &str) {
    (*h.entry(header).or_insert_with(Vec::new)).push((
        package.to_owned(),
        version.to_owned(),
        component.to_owned(),
    ));
}

fn is_reg_match(s: &str, r: &Regex) -> bool {
    r.captures(s).is_some()
}
