use std::process::Command;

/// Run git, returning its trimmed output, or None if it couldn't tell us.
fn git(args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .args(args)
        .output()
        .ok()?;

    if !output
        .status
        .success()
    {
        return None;
    }

    let text = String::from_utf8(output.stdout).ok()?;

    Some(
        text.trim()
            .to_owned(),
    )
}

fn main() {
    // rerun on source changes so the dirty marker stays honest, and when git
    // moves, the reflog (on commit and checkout) and the index (on staging)
    println!("cargo::rerun-if-changed=src");
    println!("cargo::rerun-if-changed=Cargo.toml");
    println!("cargo::rerun-if-changed=.git/logs/HEAD");
    println!("cargo::rerun-if-changed=.git/index");

    const VERSION: &str = concat!("v", env!("CARGO_PKG_VERSION"));

    // a release is the tag naming this version with nothing modified on top
    let released = git(&["describe", "--tags", "--exact-match", "HEAD"])
        .is_some_and(|tag| tag == VERSION)
        && git(&["diff", "--quiet", "HEAD"]).is_some();

    let suffix = match released {
        true => None,
        false => git(&[
            "describe",
            "--always",
            "--abbrev=7",
            "--dirty=.dev",
            "--exclude=*",
        ]),
    };

    match suffix {
        Some(commit) => println!("cargo::rustc-env=TECHNIQUE_VERSION={}+{}", VERSION, commit),
        None => println!("cargo::rustc-env=TECHNIQUE_VERSION={}", VERSION),
    }
}
