use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::{BufRead, BufReader, LineWriter, Lines, Write};
use std::path::Path;
use std::process::Command;

use lazy_regex::regex;

pub fn clear() {
    handle(true, |loc, _lines| match loc {
        // Add empty array to keep yaml valid
        Location::Lib => vec!["        []".to_owned()],
        Location::Test | Location::Bench => vec![],
    });
}

pub fn add(lib: Vec<String>, test: Vec<String>, bench: Vec<String>) {
    handle(true, |loc, mut lines| {
        lines.extend(match loc {
            Location::Lib => lib.clone(),
            Location::Test => test.clone(),
            Location::Bench => bench.clone(),
        });
        lines.sort();
        lines
    });
}

enum VersionTag {
    Manual(String),
    Auto(String),
}

impl VersionTag {
    fn tag(&self) -> &'static str {
        match self {
            VersionTag::Manual(_) => "manual",
            VersionTag::Auto(_) => "auto",
        }
    }

    fn version(&self) -> &str {
        match self {
            VersionTag::Manual(s) => &s,
            VersionTag::Auto(s) => &s,
        }
    }
}

pub fn outdated() {
    let mut all: Vec<String> = vec![];
    let disabled = handle(false, |_loc, lines| {
        all.extend(lines);
        vec![]
    });
    let mut map: BTreeMap<String, VersionTag> = BTreeMap::new();
    for DisabledPackage { package, version } in disabled {
        map.insert(package, VersionTag::Manual(version));
    }
    let mut support: BTreeMap<(String, String), BTreeSet<(String, String)>> = BTreeMap::new();
    for v in all.into_iter() {
        let caps = regex!("tried ([^ ]+)-([^,-]+),").captures(&v).unwrap();
        let package = caps.get(1).unwrap().as_str().to_owned();
        let version = caps.get(2).unwrap().as_str().to_owned();
        map.insert(package.clone(), VersionTag::Auto(version.clone()));

        if let Some(caps) = regex!("does not support: ([^ ]+)-([^-]+)").captures(&v) {
            let dep_package = caps.get(1).unwrap().as_str().to_owned();
            let dep_version = caps.get(2).unwrap().as_str().to_owned();
            let entry = support.entry((dep_package, dep_version)).or_default();
            entry.insert((package, version));
        }
    }

    let entries = map.len() + support.len();
    let mut i = 0;

    for (package, version) in map {
        if is_boot(&package) {
            continue;
        }
        if i % 100 == 0 {
            println!("{:02}%", ((i as f64 / entries as f64) * 100.0).floor());
        }
        i += 1;
        let latest = latest_version(&package);
        if version.version() != latest {
            println!(
                "{} mismatch, {}: {}, hackage: {}",
                package,
                version.tag(),
                version.version(),
                latest
            );
        }
    }

    for ((package, version), dependents) in support {
        if is_boot(&package) {
            continue;
        }

        if i % 100 == 0 {
            println!("{:02}%", ((i as f64 / entries as f64) * 100.0).floor());
        }
        i += 1;
        let latest = latest_version(&package);
        if version != latest {
            let max = 3;
            let dependents_stripped = dependents.len().checked_sub(max).unwrap_or(0);
            let dependents = dependents
                .into_iter()
                .take(max)
                .map(|(p, v)| format!("{}-{}", p, v))
                .collect::<Vec<String>>()
                .join(", ");
            let dependents = if dependents_stripped > 0 {
                format!("{} and {} more", dependents, dependents_stripped)
            } else {
                dependents
            };

            println!(
                "{} mismatch, snapshot: {}, hackage: {}, dependents: {}",
                package, version, latest, dependents,
            );
        }
    }
}

fn is_boot(package: &str) -> bool {
    [
        "Cabal",
        "base",
        "bytestring",
        "containers",
        "containers",
        "directory",
        "filepath",
        "deepseq",
        "ghc",
        "ghc-boot",
        "ghc-boot-th",
        "ghc-prim",
        "integer-gmp",
        "process",
        "stm",
        "template-haskell",
        "text",
        "time",
    ]
    .contains(&package)
}

fn latest_version(pkg: &str) -> String {
    String::from_utf8(
        Command::new("latest-version")
            .args([pkg])
            .output()
            .unwrap()
            .stdout,
    )
    .unwrap()
    .trim()
    .to_owned()
}

enum State {
    LookingForLibBounds,
    ProcessingLibBounds,
    LookingForTestBounds,
    ProcessingTestBounds,
    LookingForBenchBounds,
    ProcessingBenchBounds,
    Done,
}

struct DisabledPackage {
    package: String,
    version: String,
}

fn parse_disabled_package(s: &str) -> Option<DisabledPackage> {
    if let Some(caps) = regex!(r#"- *([^ ]+) < *0 *# *([\d.]+)"#).captures(s) {
        let package = caps.get(1).unwrap().as_str().to_owned();
        let version = caps.get(2).unwrap().as_str().to_owned();
        Some(DisabledPackage { package, version })
    } else {
        None
    }
}

fn handle<F>(write: bool, mut f: F) -> Vec<DisabledPackage>
where
    F: FnMut(Location, Vec<String>) -> Vec<String>,
{
    let path = "build-constraints.yaml";
    let mut new_lines: Vec<String> = vec![];
    let mut disabled_packages: Vec<DisabledPackage> = vec![];

    let mut state = State::LookingForLibBounds;
    let mut buf = vec![];
    for line in read_lines(path).map(|s| s.unwrap()) {
        if let Some(disabled_package) = parse_disabled_package(&line) {
            disabled_packages.push(disabled_package);
        }

        match state {
            State::LookingForLibBounds => {
                if line == r#"    "Library and exe bounds failures":"# {
                    state = State::ProcessingLibBounds;
                }
                new_lines.push(line);
            }
            State::ProcessingLibBounds => {
                if line == r#"    # End of Library and exe bounds failures"# {
                    new_lines.extend(f(Location::Lib, buf).into_iter());
                    buf = vec![];
                    new_lines.push(line);
                    state = State::LookingForTestBounds;
                } else {
                    // Remove empty section
                    if line != "        []" {
                        buf.push(line);
                    }
                }
            }
            State::LookingForTestBounds => {
                if line == r#"    # Test bounds issues"# {
                    state = State::ProcessingTestBounds;
                }
                new_lines.push(line);
            }
            State::ProcessingTestBounds => {
                if line == r#"    # End of Test bounds issues"# {
                    new_lines.extend(f(Location::Test, buf).into_iter());
                    buf = vec![];
                    new_lines.push(line);
                    state = State::LookingForBenchBounds;
                } else {
                    buf.push(line);
                }
            }
            State::LookingForBenchBounds => {
                if line == r#"    # Benchmark bounds issues"# {
                    state = State::ProcessingBenchBounds;
                }
                new_lines.push(line);
            }
            State::ProcessingBenchBounds => {
                if line == r#"    # End of Benchmark bounds issues"# {
                    new_lines.extend(f(Location::Bench, buf).into_iter());
                    buf = vec![];
                    new_lines.push(line);
                    state = State::Done;
                } else {
                    buf.push(line);
                }
            }
            State::Done => {
                new_lines.push(line);
            }
        }
    }

    if write {
        let file = File::create(path).unwrap();
        let mut file = LineWriter::new(file);

        for line in new_lines {
            file.write_all((line + "\n").as_bytes()).unwrap();
        }
        file.flush().unwrap();
    }

    disabled_packages
}

enum Location {
    Lib,
    Test,
    Bench,
}

fn read_lines<P>(filename: P) -> Lines<BufReader<File>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename).unwrap();
    BufReader::new(file).lines()
}
