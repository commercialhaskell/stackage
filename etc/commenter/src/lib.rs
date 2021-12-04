use std::fs::File;
use std::io::{BufRead, BufReader, LineWriter, Lines, Write};
use std::path::Path;

pub fn clear() {
    handle(|loc, _lines| match loc {
        // Add empty array to keep yaml valid
        Location::Lib => vec!["        []".to_owned()],
        Location::Test | Location::Bench => vec![],
    });
}

pub fn add(lib: Vec<String>, test: Vec<String>, bench: Vec<String>) {
    handle(|loc, mut lines| {
        lines.extend(match loc {
            Location::Lib => lib.clone(),
            Location::Test => test.clone(),
            Location::Bench => bench.clone(),
        });
        lines.sort();
        lines
    })
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

fn handle<F>(f: F)
where
    F: Fn(Location, Vec<String>) -> Vec<String>,
{
    let path = "build-constraints.yaml";
    let mut new_lines: Vec<String> = vec![];

    let mut state = State::LookingForLibBounds;
    let mut buf = vec![];
    for line in read_lines(path).map(|s| s.unwrap()) {
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

    let file = File::create(path).unwrap();
    let mut file = LineWriter::new(file);

    for line in new_lines {
        file.write_all((line + "\n").as_bytes()).unwrap();
    }
    file.flush().unwrap();
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
