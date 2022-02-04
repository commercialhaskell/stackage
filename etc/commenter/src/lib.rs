use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::{BufRead, BufReader, LineWriter, Lines, Write};
use std::path::Path;
use std::process::Command;

use lazy_regex::regex;
use serde::{Deserialize, Deserializer};

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
    Manual(Version),
    Auto(Version),
}

impl VersionTag {
    fn tag(&self) -> &'static str {
        match self {
            VersionTag::Manual(_) => "manual",
            VersionTag::Auto(_) => "auto",
        }
    }

    fn version(&self) -> &Version {
        match self {
            VersionTag::Manual(s) => s,
            VersionTag::Auto(s) => s,
        }
    }
}

pub fn outdated() {
    let mut all: Vec<String> = vec![];
    let (versioned, disabled) = handle(false, |_loc, lines| {
        all.extend(lines);
        vec![]
    });

    for DisabledPackage { package } in disabled {
        println!("WARN: {package} is disabled without a noted version");
    }

    let mut map: BTreeMap<Package, VersionTag> = BTreeMap::new();
    for VersionedPackage { package, version } in versioned {
        map.insert(package, VersionTag::Manual(version));
    }
    let mut support: BTreeMap<(Package, Version), BTreeSet<(Package, Version)>> = BTreeMap::new();
    for v in all.into_iter() {
        let caps = regex!("tried ([^ ]+)-([^,-]+),").captures(&v).unwrap();
        let package = Package(caps.get(1).unwrap().as_str().to_owned());
        let version = Version(caps.get(2).unwrap().as_str().to_owned());
        map.insert(package.clone(), VersionTag::Auto(version.clone()));

        if let Some(caps) = regex!("does not support: ([^ ]+)-([^-]+)").captures(&v) {
            let dep_package = Package(caps.get(1).unwrap().as_str().to_owned());
            let dep_version = Version(caps.get(2).unwrap().as_str().to_owned());
            let entry = support.entry((dep_package, dep_version)).or_default();
            entry.insert((package, version));
        }
    }

    let latest_versions = {
        let mut packages: Vec<Package> = map.iter().map(|(package, _)| package.clone()).collect();
        packages.append(
            &mut support
                .iter()
                .map(|((package, _), _)| package.clone())
                .collect(),
        );
        latest_version(packages.into_iter())
    };

    for (package, version) in map {
        if is_boot(&package) {
            continue;
        }
        let latest = latest_versions.get(&package).unwrap();
        if version.version() != latest {
            println!(
                "{package} mismatch, {tag}: {version}, hackage: {latest}",
                tag = version.tag(),
                version = version.version(),
            );
        }
    }

    for ((package, version), dependents) in support {
        if is_boot(&package) {
            continue;
        }

        let latest = latest_versions.get(&package).unwrap();
        if &version != latest {
            let max = 3;
            let dependents_stripped = dependents.len().saturating_sub(max);
            let dependents = dependents
                .into_iter()
                .take(max)
                .map(|(p, v)| format!("{p}-{v}"))
                .collect::<Vec<String>>()
                .join(", ");
            let dependents = if dependents_stripped > 0 {
                format!("{dependents} and {dependents_stripped} more")
            } else {
                dependents
            };

            println!(
                "{package} mismatch, snapshot: {version}, hackage: {latest}, dependents: {dependents}"
            );
        }
    }
}

fn is_boot(package: &Package) -> bool {
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
        "ghc-bignum",
        "ghc-boot",
        "ghc-boot-th",
        "ghc-prim",
        "ghc-lib-parser", // not a boot lib, but tied to the GHC version.
        "integer-gmp",
        "process",
        "stm",
        "template-haskell",
        "text",
        "time",
    ]
    .contains(&&*package.0)
}

fn latest_version(packages: impl Iterator<Item = Package>) -> BTreeMap<Package, Version> {
    String::from_utf8(
        Command::new("latest-version")
            .args(packages.map(|p| p.0))
            .output()
            .expect("Could not find latest-version in PATH")
            .stdout,
    )
    .unwrap()
    .trim()
    .to_owned()
    .lines()
    .map(|s| {
        let VersionedPackage { package, version } = parse_versioned_package_canonical(s).unwrap();
        (package, version)
    })
    .collect()
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

#[derive(PartialEq, Eq, Debug)]
struct VersionedPackage {
    package: Package,
    version: Version,
}

fn parse_versioned_package_canonical(s: &str) -> Option<VersionedPackage> {
    if let Some(caps) = regex!(r#"^(.+)-([\d.]+)$"#).captures(s) {
        let package = Package(caps.get(1).unwrap().as_str().to_owned());
        let version = Version(caps.get(2).unwrap().as_str().to_owned());
        Some(VersionedPackage { package, version })
    } else {
        None
    }
}

fn parse_versioned_package_yaml(s: &str) -> Option<VersionedPackage> {
    if let Some(caps) = regex!(r#"- *([^ ]+) < *0 *# *([\d.]+)"#).captures(s) {
        let package = Package(caps.get(1).unwrap().as_str().to_owned());
        let version = Version(caps.get(2).unwrap().as_str().to_owned());
        Some(VersionedPackage { package, version })
    } else if let Some(caps) = regex!(r#"- *([^ ]+) *# *([\d.]+)"#).captures(s) {
        let package = Package(caps.get(1).unwrap().as_str().to_owned());
        let version = Version(caps.get(2).unwrap().as_str().to_owned());
        Some(VersionedPackage { package, version })
    } else {
        None
    }
}

struct DisabledPackage {
    package: String,
}

fn parse_disabled_package(s: &str) -> Option<DisabledPackage> {
    if !regex!(r#"- *([^ ]+) < *0 *# tried"#).is_match(s) {
        if let Some(caps) = regex!(r#"- *([^ ]+) < *0 *# *\d*[^\d ]"#).captures(s) {
            let package = caps.get(1).unwrap().as_str().to_owned();
            Some(DisabledPackage { package })
        } else {
            None
        }
    } else {
        None
    }
}

fn handle<F>(write: bool, mut f: F) -> (Vec<VersionedPackage>, Vec<DisabledPackage>)
where
    F: FnMut(Location, Vec<String>) -> Vec<String>,
{
    let path = "build-constraints.yaml";
    let mut new_lines: Vec<String> = vec![];
    let mut versioned_packages: Vec<VersionedPackage> = vec![];
    let mut disabled_packages: Vec<DisabledPackage> = vec![];

    let mut state = State::LookingForLibBounds;
    let mut buf = vec![];
    for line in read_lines(path).map(|s| s.unwrap()) {
        if let Some(versioned_package) = parse_versioned_package_yaml(&line) {
            versioned_packages.push(versioned_package);
        } else if let Some(disabled_package) = parse_disabled_package(&line) {
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

    (versioned_packages, disabled_packages)
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

#[derive(Deserialize)]
struct SnapshotYaml {
    // flags: BTreeMap<Package, BTreeMap<PackageFlag, bool>>,
    // publish_time
    packages: Vec<SnapshotPackage>,
    // hidden
    // resolver
}

#[derive(Deserialize)]
struct SnapshotPackage {
    hackage: PackageWithVersionAndSha,
    // pantry-tree
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
struct Package(String);

impl fmt::Display for Package {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
struct Version(String);

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

// zstd-0.1.3.0@sha256:4c0a372251068eb6086b8c3a0a9f347488f08b570a7705844ffeb2c720c97223,3723
struct PackageWithVersionAndSha {
    name: Package,
    version: Version,
}

impl<'de> serde::Deserialize<'de> for PackageWithVersionAndSha {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: String = String::deserialize(deserializer)?;
        let r = regex!(r#"^(.+?)-([.\d]+)@sha256:[\da-z]+,\d+$"#);
        if let Some(caps) = r.captures(&s) {
            let name = Package(caps.get(1).unwrap().as_str().to_owned());
            let version = Version(caps.get(2).unwrap().as_str().to_owned());
            Ok(Self { name, version })
        } else {
            Err(serde::de::Error::invalid_value(
                serde::de::Unexpected::Other(&s),
                &"Invalid PackageVersionWithSha",
            ))
        }
    }
}

fn yaml_from_file<A, P: AsRef<Path>>(path: P) -> Result<A, Box<dyn Error>>
where
    A: for<'de> Deserialize<'de>,
{
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let u = serde_yaml::from_reader(reader)?;
    Ok(u)
}

struct Snapshot {
    packages: BTreeMap<Package, Diff<Version>>,
}

#[derive(Clone, Copy)]
enum Diff<A> {
    Left(A),
    Right(A),
    Both(A, A),
}

fn to_diff(a: SnapshotYaml, b: SnapshotYaml) -> Snapshot {
    let mut packages = BTreeMap::new();
    for s in a.packages {
        let package = s.hackage;
        packages.insert(package.name, Diff::Left(package.version));
    }
    for s in b.packages {
        let package = s.hackage;
        let name = package.name;
        let version = package.version;
        if let Some(a) = packages.remove(&name) {
            match a {
                Diff::Left(a) => {
                    if a == version {
                        packages.remove(&name);
                    } else {
                        packages.insert(name, Diff::Both(a, version));
                    }
                }
                _ => unreachable!(),
            }
        } else {
            packages.insert(name, Diff::Right(version));
        }
    }

    Snapshot { packages }
}

pub fn diff_snapshot(a: String, b: String) {
    let diff = to_diff(yaml_from_file(a).unwrap(), yaml_from_file(b).unwrap());
    for (name, diff) in diff.packages {
        let s = match diff {
            Diff::Left(a) => format!("- {name}-{a}"),
            Diff::Right(b) => format!("+ {name}-{b}"),
            Diff::Both(a, b) => format!("~ {name}-{a} -> {b}"),
        };
        println!("{s}");
    }
}

#[derive(PartialEq, Eq, Debug)]
struct DisabledTransitively {
    child: VersionedPackage,
    parent: Package,
}

fn parse_disabled_transitviely(s: &str) -> Option<DisabledTransitively> {
    let r = regex!(
        r#"- *([^ ]+) < *0 *# tried [^ ]+-([\d.]+), but its \*[^*]+\* requires the disabled package: ([^ ]+)"#
    );
    if let Some(caps) = r.captures(s) {
        let package = Package(caps.get(1).unwrap().as_str().to_owned());
        let version = Version(caps.get(2).unwrap().as_str().to_owned());
        let parent = Package(caps.get(3).unwrap().as_str().to_owned());
        Some(DisabledTransitively {
            child: VersionedPackage { package, version },
            parent,
        })
    } else {
        None
    }
}

#[test]
fn test_parse_disabled_transitviely() {
    let s = "- Network-NineP < 0 # tried Network-NineP-0.4.7.1, but its *library* requires the disabled package: mstate";
    assert_eq!(
        parse_disabled_transitviely(s),
        Some(DisabledTransitively {
            child: VersionedPackage {
                package: Package("Network-NineP".to_owned()),
                version: Version("0.4.7.1".to_owned())
            },
            parent: Package("mstate".to_owned()),
        })
    )
}

type M = BTreeMap<Package, (Vec<VersionedPackage>, Option<usize>)>;

pub fn disabled() {
    let mut disabled_transitively: Vec<DisabledTransitively> = vec![];
    handle(false, |loc, lines| {
        match loc {
            Location::Lib => disabled_transitively.extend(
                lines
                    .into_iter()
                    .map(|line| parse_disabled_transitviely(&line))
                    .flatten()
                    .collect::<Vec<_>>(),
            ),
            Location::Test | Location::Bench => (),
        }
        vec![]
    });

    let mut packages: BTreeSet<Package> = BTreeSet::new();
    let mut disabled: M = BTreeMap::new();

    for DisabledTransitively { child, parent } in disabled_transitively {
        packages.insert(child.package.clone());
        packages.insert(parent.clone());
        disabled
            .entry(child.package.clone())
            .or_insert_with(|| (vec![], None));
        let t = disabled.entry(parent).or_insert_with(|| (vec![], None));
        t.0.push(child);
    }

    let mut packages_len = packages.len();
    while packages_len > 0 {
        let mut new_packages: BTreeSet<Package> = BTreeSet::new();
        for package in packages {
            let (_, count) = disabled.get(&package).unwrap();
            if count.is_none() && !process(&package, &mut disabled) {
                new_packages.insert(package.clone());
            }
        }
        packages = new_packages;
        packages_len = packages.len();
    }

    let mut v: Vec<_> = disabled
        .into_iter()
        .map(|(package, (_, count))| (count, package))
        .collect();
    v.sort();
    for (count, package) in v {
        let count = count.unwrap();
        if count != 0 {
            println!("{package} is disabled with {count} dependents");
        }
    }
}

fn process(package: &Package, m: &mut M) -> bool {
    let (children, count) = m.get(package).unwrap();
    assert!(count.is_none(), "{:?}", package);
    let mut count = 0;
    for child in children {
        let (_, child_count) = m
            .get(&child.package)
            .unwrap_or_else(|| panic!("{}", child.package));
        match child_count {
            None => return false,
            Some(i) => count += 1 + i,
        }
    }
    m.entry(package.clone())
        .and_modify(|tup| tup.1 = Some(count))
        .or_insert_with(|| panic!("{}", package));
    true
}
