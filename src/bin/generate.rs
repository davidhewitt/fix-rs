use std::env;

fn main() {
    let args: Vec<_> = env::args().into_iter().collect();
    let src = args.get(1).expect("expected source as first argument");
    let dest = args.get(2).expect("expected dest as second argument");
    fix_rs::generate_dictionary(src, dest)
        .map_err(|e| e.to_string())
        .expect("failed to generate dictionary");
}
