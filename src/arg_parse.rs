use getopts;
use std::error::Error;

#[derive(Debug)]
pub struct Opts {
    pub filename: String,
    pub globalname: Option<String>,
    pub pararell: Option<usize>,
    pub shared: bool,
}

pub fn parse(args: Vec<String>) -> Result<Opts, String> {
    // define options
    let mut opts = getopts::Options::new();
    opts.optopt("f", "file", "file to compile.", "FILE");
    opts.optopt("g", "global", "global file.", "GLOBAL");
    opts.optopt("p", "pararell", "core number", "PARAREL");
    opts.optflag("s", "share-memory", "SHARE");

    // parse options
    let args = opts.parse(args).or_else(geterr).unwrap();

    // get options
    let file = args.opt_str("file").unwrap();
    let gfile = args.opt_str("global");
    let p = args.opt_present("pararell");
    let s = args.opt_present("share-memory");
    let num = args
        .opt_str("pararell")
        .unwrap_or(String::from("0"))
        .parse::<usize>()
        .unwrap();

    Ok(Opts {
        filename: file,
        globalname: gfile,
        pararell: if p { Some(num) } else { None },
        shared: s,
    })
}
fn geterr<T: Error, S>(err: T) -> Result<S, String> {
    Err(err.description().to_owned())
}
