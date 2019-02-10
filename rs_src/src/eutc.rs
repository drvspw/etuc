extern crate chrono;

#[macro_use]
extern crate rustler;

#[macro_use]
extern crate lazy_static;

use chrono::{DateTime, Utc, SecondsFormat};

use rustler::{Env, Term, NifResult, Encoder};

mod atoms {
    rustler_atoms! {
        atom ok;
        //atom error;
        //atom __true__ = "true";
        //atom __false__ = "false";
    }
}

rustler_export_nifs! {
    "eutc", // module name
    [
        ("timestamp", 0, timestamp),
        ("iso8601", 0, iso8601),
    ], // nif functions
    Some(on_load) // on_load callback
}

// callback: called on loading nif library
fn on_load(_env: Env, _info: Term) -> bool {
    true
}

fn timestamp<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let now: DateTime<Utc> = Utc::now();

    let ts: i64 = now.timestamp();
    let nanosec: u32 = now.timestamp_subsec_nanos();

    Ok((ts*1000000000 + nanosec as i64).encode(env))
}

pub fn iso8601<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let now: DateTime<Utc> = Utc::now();
    let now_str: String = now.to_rfc3339_opts(SecondsFormat::Secs, true);
        
    Ok(now_str.encode(env))
}
