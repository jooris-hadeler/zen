fn main() {
    // Set the `TARGET` environment variable to the target triple of the build.
    println!(
        "cargo:rustc-env=VERSION={} build for {}",
        env!("CARGO_PKG_VERSION"),
        std::env::var("TARGET").unwrap()
    );
}
