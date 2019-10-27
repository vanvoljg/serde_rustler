use crate::{atoms, Error};
use rustler::{types::tuple, Binary, Decoder, Encoder, Env, Term};
use std::cmp::{min, max};

/// Converts an `&str` to either an existing atom or an Elixir bitstring.
pub fn str_to_term<'a>(env: &Env<'a>, string: &str) -> Result<Term<'a>, Error> {
    atoms::str_to_term(env, string).or_else(|_| Ok(string.encode(*env)))
}

/// Attempts to convert a stringable term into a `String`.
pub fn term_to_str(term: &Term) -> Result<String, Error> {
    atoms::term_to_string(term)
        .or_else(|_| term.decode())
        .or(Err(Error::ExpectedStringable))
}

pub fn is_nil(term: &Term) -> bool {
    atoms::nil().eq(term)
}

/// Parses a boolean from a Term.
pub fn parse_bool(term: &Term) -> Result<bool, Error> {
    if atoms::true_().eq(term) {
        Ok(true)
    } else if atoms::false_().eq(term) {
        Ok(false)
    } else {
        Err(Error::ExpectedBoolean)
    }
}

pub fn parse_binary(term: Term) -> Result<&[u8], Error> {
    validate_binary(&term)?;
    let binary: Binary = term.decode().or(Err(Error::ExpectedBinary))?;
    Ok(binary.as_slice())
}

pub fn parse_number<'a, T: Decoder<'a>>(term: &Term<'a>) -> Result<T, Error> {
    if !term.is_number() {
        return Err(Error::InvalidNumber);
    }

    term.decode().or(Err(Error::ExpectedNumber))
}

pub fn parse_str(term: Term) -> Result<&str, Error> {
    let bytes = parse_binary(term)?;
    std::str::from_utf8(bytes).or(Err(Error::ExpectedStringable))
}

/// Asserts that the term is an Elixir binary
pub fn validate_binary(term: &Term) -> Result<(), Error> {
    if !term.is_binary() {
        Err(Error::ExpectedBinary)
    } else {
        Ok(())
    }
}

/// Assert that the term is an Elixir tuple, and if so, return the underlying `Vec<Term>`.
pub fn validate_tuple(term: Term, len: Option<usize>) -> Result<Vec<Term>, Error> {
    if !term.is_tuple() {
        return Err(Error::ExpectedTuple);
    }

    let tuple = tuple::get_tuple(term).or(Err(Error::ExpectedTuple))?;
    match len {
        None => Ok(tuple),
        Some(len) => {
            if tuple.len() == len {
                Ok(tuple)
            } else {
                Err(Error::InvalidTuple)
            }
        }
    }
}

pub fn parse_decimal(term: Term) -> Result<String, Error> {
  match validate_struct(&term, Some("Elixir.Decimal")) {
    Err(_) => Err(Error::InvalidDecimal),
    Ok(_) => {
      let coef: i64 = term.map_get(atoms::coef().to_term(term.get_env())).or(Err(Error::InvalidDecimal))?.decode().or(Ok(0 as i64))?;
      let exp: i32 = term.map_get(atoms::exp().to_term(term.get_env())).or(Err(Error::InvalidDecimal))?.decode().or(Ok(0 as i32))?;
      let sign: i64 = term.map_get(atoms::sign().to_term(term.get_env())).or(Err(Error::InvalidDecimal))?.decode().or(Ok(1 as i64))?;
      return Ok(format!("{:.*}", min(max(-exp, 2), 6) as usize, sign as f64 * (coef as f64 * 10f64.powi(exp))));
    }
  }
}

pub fn validate_struct<'a>(term: &Term<'a>, name: Option<&str>) -> Result<Term<'a>, Error> {
    if !term.is_map() {
        return Err(Error::ExpectedMap);
    }

    let __struct__ = atoms::__struct__().to_term(term.get_env());
    let struct_name_term = term.map_get(__struct__).or(Err(Error::ExpectedStruct))?;

    match name {
        Some(name) => {
            let name_term =
                atoms::str_to_term(&term.get_env(), name).or(Err(Error::InvalidStructName))?;

            if struct_name_term.eq(&name_term) {
                Ok(struct_name_term)
            } else {
                Err(Error::ExpectedStruct)
            }
        }
        _ => Ok(struct_name_term),
    }
}
