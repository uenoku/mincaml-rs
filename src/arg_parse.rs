use getopts;
use std::error::Error;

#[derive(Debug)]
pub struct Opts {
    pub filename: String,
    pub globalname: Option<String>,
    pub pararell: bool,
}

pub fn parse(args: Vec<String>) -> Result<Opts, String> {
    // define options
    let mut opts = getopts::Options::new();
    opts.optopt("f", "file", "file to compile.", "FILE");
    opts.optopt("g", "global", "global file.", "GLOBAL");
    opts.optflag("p", "pararell", "PARAREL");

    // parse options
    let args = opts.parse(args).or_else(geterr).unwrap();

    // get options
    let file = args.opt_str("file").unwrap();
    let gfile = args.opt_str("global");
    let p = args.opt_present("pararell");

    Ok(Opts {
        filename: file,
        globalname: gfile,
        pararell: p,
    })
}
fn geterr<T: Error, S>(err: T) -> Result<S, String> {
    Err(err.description().to_owned())
}
