extern crate chrono;

#[macro_use]
extern crate rustler;

use chrono::{DateTime, Utc, SecondsFormat, Datelike, Timelike};
use rustler::{Env, Term, NifResult, Encoder, Error};

mod atoms {
    rustler_atoms! {
        atom ok;
        atom seconds;
        atom nanos;
        atom iso8601;
        atom datetime;
    }
}

rustler_export_nifs! {
    "eutc", // module name
    [
        ("timestamp", 0, timestamp),
        ("now", 0, now),
        ("system_time", 0, system_time),
        ("datetime", 0, datetime),
        ("parse_iso8601", 1, parse_iso8601)
    ], // nif functions
    Some(on_load) // on_load callback
}

// callback: called on loading nif library
fn on_load(_env: Env, _info: Term) -> bool {
    true
}

fn parse_iso8601<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    // get as binary
    let binary = args[0].decode_as_binary()?;

    // convert binary to std::str
    let datetime_str = match ::std::str::from_utf8(binary.as_slice()) {
        Ok(string) => string,
        Err(_) => return Err(Error::BadArg),
    };

    // convert to DateTime<FixedOffset>
    let datetime = match DateTime::parse_from_rfc3339(&datetime_str) {
        Ok(datetime) => datetime,
        Err(_) => match DateTime::parse_from_rfc2822(&datetime_str) {
            Ok(datetime) => datetime,
            Err(_) => return Err(Error::BadArg),
        }
    };

    // convert to DateTime<Utc>
    let utc: DateTime<Utc> = datetime.with_timezone(&Utc);
    from_datetime(&env, &utc)
}

fn timestamp<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    // current timestamp
    let now: DateTime<Utc> = Utc::now();
    let ts: i64 = now.timestamp_nanos() / 1000;

    let micro = ts % 1000000;
    let m1 = (ts - micro) / 1000000;
    let sec = m1 % 1000000;
    let mega = (m1 - sec ) / 1000000;

    // encode and return
    Ok((mega, sec, micro).encode(env))
}

fn datetime<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    // current timestamp
    let now: DateTime<Utc> = Utc::now();

    // format datetime tuple
    let datetime = ((now.year(), now.month(), now.day()), (now.hour(), now.minute(), now.second()));

    // encode and return
    Ok(datetime.encode(env))
}

fn system_time<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    // current timestamp
    let now: DateTime<Utc> = Utc::now();
    let ts: i64 = now.timestamp_nanos();

    // encode and return
    Ok((ts).encode(env))
}

fn now<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    // current utc time
    let now: DateTime<Utc> = Utc::now();
    from_datetime( &env, &now )
}

fn from_datetime<'a>(env: &Env<'a>, now: &DateTime<Utc>) -> NifResult<Term<'a>> {
    // format into iso8601 string
    let now_str: String = now.to_rfc3339_opts(SecondsFormat::AutoSi, true);

    // timestamp
    let ts: i64 = now.timestamp_nanos();
    let seconds : i64 = ts / 1000000000;
    let nanos: i64 = ts % 1000000000;
    let datetime = ((now.year(), now.month(), now.day()), (now.hour(), now.minute(), now.second()));

    let keys: Vec<Term> = vec![atoms::seconds().encode(*env), atoms::nanos().encode(*env),
                               atoms::datetime().encode(*env), atoms::iso8601().encode(*env)];
    let values: Vec<Term> = vec![seconds.encode(*env), nanos.encode(*env), datetime.encode(*env), now_str.encode(*env)];

    Term::map_from_arrays(*env, &keys, &values)
}
