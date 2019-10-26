use crate::{atoms, Error};
use rustler::{types::tuple, Binary, Decoder, Encoder, Env, Term};
use bigint::{ToBigUint};

const POWER_OF_2_TO_52: i64 = 4_503_599_627_370_496;

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

fn ratio(coef: u64, exp: i64) -> (u64, u64) {
  if exp >= 0 {
    (coef * u64::pow(10, exp as u32), 1)
  } else {
    (coef, u64::pow(10, -exp as u32))
  }
}

fn scale_up(num: u64, den: u64, exp: i64) -> (u64, i64) {
  if num >= den { return (num, exp); }
  scale_up(num << 1, den, exp - 1)
}

fn scale_down(num: u64, den: u64, mut exp: i64) -> (u64, i64) {
  println!("(scale_down) num: {}, den: {}, exp: {}", num, den, exp);
  let mut new_den = den.to_biguint().unwrap() << 1;
  let bignum = num.to_biguint().unwrap();
  let mut i = 0;
  while bignum >= new_den {
    i = i + 1;
    new_den = new_den << 1;
    exp = exp + 1;
  }
  return (den >> (52 - i), exp);
}

fn decimal_to_float(sign: i8, num: u64, den: u64, mut exp: i64) -> f64 {
  let quo = num / den;
  let rem = num - quo * den;
  let tmp: u64 = match den >> 1 {
    den if rem > den => quo + 1,
    den if rem < den => quo,
    _ if (quo & 1) == 1 => quo + 1,
    _ => quo
  };

  let m_sign: u64 = if sign == -1 { 1 } else { 0 };
  let man: i64 = tmp as i64 - POWER_OF_2_TO_52;
  if man >= POWER_OF_2_TO_52 { exp = exp + 1; }
  // evil floating point bit level hacking
  let m_man: u64 = unsafe { std::mem::transmute(man) };
  let m_exp: u64 = unsafe { std::mem::transmute::<i64, u64>(exp) + 1023 };
  let mem: u64 = ((m_sign & 1) << 63) | ((m_exp & 0x7ff) << 52) | (m_man & 0xfffffffffffff);
  println!("{:#b}", m_sign & 1);
  println!("{:#b}", m_exp);
  println!("{:#b}", m_man);
  println!("{:#b}", mem);
  let result: f64 = unsafe { std::mem::transmute(mem) };
  result
}

pub fn parse_decimal(term: Term) -> Result<f64, Error> {
  match validate_struct(&term, Some("Elixir.Decimal")) {
    Err(_) => Err(Error::InvalidDecimal),
    Ok(_) => {
      let coef: u64 = term.map_get(atoms::coef().to_term(term.get_env())).or(Err(Error::InvalidDecimal))?.decode().or(Ok(0.0 as u64))?;
      let exp: i64 = term.map_get(atoms::exp().to_term(term.get_env())).or(Err(Error::InvalidDecimal))?.decode().or(Ok(0.0 as i64))?;
      let sign: i8 = term.map_get(atoms::sign().to_term(term.get_env())).or(Err(Error::InvalidDecimal))?.decode().or(Ok(1))?;
      println!("sign: {}, coef: {}, exp: {}", sign, coef, exp);
      // return Ok(coef as f64 * f64::powf(10 as f64, exp as f64) * sign as f64);
      let (num, den) = ratio(coef, exp);
      println!("num: {}, den: {}", num, den);
      if num == 0 { return Ok(0.0); }
      let boundary = den << 52;
      if num >= boundary {
        let (den, exp) = scale_down(num, boundary, 52);
        println!("den: {}, exp: {}", den, exp);
        return Ok(decimal_to_float(sign, num, den, exp));
      }
      let (num, exp) = scale_up(num, boundary, 52);
      println!("num: {}, exp: {}", num, exp);
      Ok(decimal_to_float(sign, num, den, exp))
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
