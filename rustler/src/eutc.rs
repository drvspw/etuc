extern crate chrono;

#[macro_use]
extern crate rustler;

use chrono::{DateTime, Utc, SecondsFormat};

use rustler::{Env, Term, NifResult, Encoder};

mod atoms {
    rustler_atoms! {
        atom ok;
        atom seconds;
        atom nanos;
        atom iso8601;
    }
}

rustler_export_nifs! {
    "eutc", // module name
    [
        ("timestamp", 0, timestamp),
        ("now", 0, now)
    ], // nif functions
    Some(on_load) // on_load callback
}

// callback: called on loading nif library
fn on_load(_env: Env, _info: Term) -> bool {
    true
}

fn timestamp<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    // current timestamp
    let now: DateTime<Utc> = Utc::now();
    let ts: i64 = now.timestamp_nanos();

    // encode and return
    Ok((ts).encode(env))
}

fn now<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    // current utc time
    let now: DateTime<Utc> = Utc::now();

    // format into iso8601 string
    let now_str: String = now.to_rfc3339_opts(SecondsFormat::AutoSi, true);

    // timestamp
    let ts: i64 = now.timestamp_nanos();
    let seconds : i64 = ts / 1000000000;
    let nanos: i64 = ts % 1000000000;

    let keys: Vec<Term> = vec![atoms::seconds().encode(env), atoms::nanos().encode(env), atoms::iso8601().encode(env)];
    let values: Vec<Term> = vec![seconds.encode(env), nanos.encode(env), now_str.encode(env)];

    Term::map_from_arrays(env, &keys, &values)
}
