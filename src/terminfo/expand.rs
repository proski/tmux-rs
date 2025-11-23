// Copyright 2019 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Parameterized string expansion

use std::array::from_fn;
use std::iter::repeat_n;

#[derive(Clone, Copy, PartialEq)]
enum States {
    Nothing,
    Delay,
    Percent,
    SetVar,
    GetVar,
    PushParam,
    CharConstant,
    CharClose,
    IntConstant(i32),
    FormatPattern(Flags, FormatState),
    SeekIfElse(usize),
    SeekIfElsePercent(usize),
    SeekIfEnd(usize),
    SeekIfEndPercent(usize),
}

#[derive(Copy, PartialEq, Clone)]
enum FormatState {
    Flags,
    Width,
    Precision,
}

/// Types of parameters a capability can use
#[derive(Clone)]
pub enum Parameter {
    Number(i32),
    String(Vec<u8>),
}

impl From<i32> for Parameter {
    fn from(value: i32) -> Parameter {
        Parameter::Number(value)
    }
}

impl From<&[u8]> for Parameter {
    fn from(value: &[u8]) -> Parameter {
        Parameter::String(value.to_vec())
    }
}

impl<const N: usize> From<&[u8; N]> for Parameter {
    fn from(value: &[u8; N]) -> Parameter {
        Parameter::String(value.to_vec())
    }
}

/// Error reported when expanding a string
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum Error {
    #[error("Not enough stack elements")]
    StackUnderflow,
    #[error("Parameter type not expected by operator")]
    TypeMismatch,
    #[error("Unrecognized format option: {0}")]
    UnrecognizedFormatOption(char),
    #[error("Invalid variable name: {0}")]
    InvalidVariableName(char),
    #[error("Invalid parameter index: {0}")]
    InvalidParameterIndex(char),
    #[error("Malformed character constant")]
    MalformedCharacterConstant,
    #[error("Integer constant too large")]
    IntegerConstantOverflow,
    #[error("Integer constant malformed")]
    MalformedIntegerConstant,
    #[error("Overflow in format width")]
    FormatWidthOverflow,
    #[error("Overflow in format precision")]
    FormatPrecisionOverflow,
}

/// Context for variable expansion
///
/// To be compatible with ncurses, the `ExpandContext` instance should be the same
/// for the same terminal.
pub struct ExpandContext {
    /// Static variables A-Z
    static_variables: [Parameter; 26],
    /// Dynamic variables a-z
    dynamic_variables: [Parameter; 26],
}

impl ExpandContext {
    /// Return a newly initialized ExpandContext
    pub fn new() -> Self {
        Self {
            static_variables: from_fn(|_| Parameter::from(0)),
            dynamic_variables: from_fn(|_| Parameter::from(0)),
        }
    }

    /// Expand a parameterized capability
    ///
    /// # Arguments
    /// * `cap`    - string to expand
    /// * `params` - vector of params for %p1 etc
    pub fn expand(&mut self, cap: &[u8], params: &[Parameter]) -> Result<Vec<u8>, Error> {
        let mut state = States::Nothing;

        // expanded cap will only rarely be larger than the cap itself
        let mut output = Vec::with_capacity(cap.len());

        let mut stack: Vec<Parameter> = Vec::new();

        // Copy parameters into a local vector for mutability
        let mut mparams = [
            Parameter::from(0),
            Parameter::from(0),
            Parameter::from(0),
            Parameter::from(0),
            Parameter::from(0),
            Parameter::from(0),
            Parameter::from(0),
            Parameter::from(0),
            Parameter::from(0),
        ];
        for (dst, src) in mparams.iter_mut().zip(params.iter()) {
            *dst = (*src).clone();
        }

        for &c in cap.iter() {
            let cur = c as char;
            let mut old_state = state;
            match state {
                States::Nothing => {
                    if cur == '%' {
                        state = States::Percent;
                    } else if cur == '$' {
                        state = States::Delay;
                    } else {
                        output.push(c);
                    }
                }
                States::Delay => {
                    old_state = States::Nothing;
                    if cur == '>' {
                        state = States::Nothing;
                    }
                }
                States::Percent => {
                    match cur {
                        '%' => {
                            output.push(c);
                            state = States::Nothing;
                        }
                        'c' => {
                            match stack.pop() {
                                // if c is 0, use 0200 (128) for ncurses compatibility
                                Some(Parameter::Number(0)) => output.push(128u8),
                                // Don't check bounds. ncurses just casts and truncates.
                                Some(Parameter::Number(c)) => output.push(c as u8),
                                Some(_) => return Err(Error::TypeMismatch),
                                None => return Err(Error::StackUnderflow),
                            }
                        }
                        'p' => state = States::PushParam,
                        'P' => state = States::SetVar,
                        'g' => state = States::GetVar,
                        '\'' => state = States::CharConstant,
                        '{' => state = States::IntConstant(0),
                        'l' => match stack.pop() {
                            Some(Parameter::String(s)) => {
                                stack.push(Parameter::from(s.len() as i32));
                            }
                            Some(_) => return Err(Error::TypeMismatch),
                            None => return Err(Error::StackUnderflow),
                        },
                        '+' | '-' | '*' | '/' | '|' | '&' | '^' | 'm' => {
                            match (stack.pop(), stack.pop()) {
                                (Some(Parameter::Number(y)), Some(Parameter::Number(x))) => {
                                    stack.push(Parameter::from(match cur {
                                        '+' => x + y,
                                        '-' => x - y,
                                        '*' => x * y,
                                        '/' => x / y,
                                        '|' => x | y,
                                        '&' => x & y,
                                        '^' => x ^ y,
                                        'm' => x % y,
                                        _ => unreachable!("logic error"),
                                    }));
                                }
                                (Some(_), Some(_)) => return Err(Error::TypeMismatch),
                                _ => return Err(Error::StackUnderflow),
                            }
                        }
                        '=' | '>' | '<' | 'A' | 'O' => match (stack.pop(), stack.pop()) {
                            (Some(Parameter::Number(y)), Some(Parameter::Number(x))) => {
                                stack.push(Parameter::from(
                                    if match cur {
                                        '=' => x == y,
                                        '<' => x < y,
                                        '>' => x > y,
                                        'A' => x > 0 && y > 0,
                                        'O' => x > 0 || y > 0,
                                        _ => unreachable!("logic error"),
                                    } {
                                        1
                                    } else {
                                        0
                                    },
                                ));
                            }
                            (Some(_), Some(_)) => return Err(Error::TypeMismatch),
                            _ => return Err(Error::StackUnderflow),
                        },
                        '!' | '~' => match stack.pop() {
                            Some(Parameter::Number(x)) => {
                                stack.push(Parameter::Number(match cur {
                                    '!' if x > 0 => 0,
                                    '!' => 1,
                                    '~' => !x,
                                    _ => unreachable!("logic error"),
                                }));
                            }
                            Some(_) => return Err(Error::TypeMismatch),
                            None => return Err(Error::StackUnderflow),
                        },
                        'i' => match (&mparams[0], &mparams[1]) {
                            (&Parameter::Number(x), &Parameter::Number(y)) => {
                                mparams[0] = Parameter::from(x + 1);
                                mparams[1] = Parameter::from(y + 1);
                            }
                            (_, _) => return Err(Error::TypeMismatch),
                        },

                        // printf-style support for %doxXs
                        'd' | 'o' | 'x' | 'X' | 's' => {
                            if let Some(arg) = stack.pop() {
                                let flags = Flags::default();
                                let res = format(arg, FormatOp::from_char(cur), flags)?;
                                output.extend(res);
                            } else {
                                return Err(Error::StackUnderflow);
                            }
                        }
                        ':' | '#' | ' ' | '.' | '0'..='9' => {
                            let mut flags = Flags::default();
                            let mut fstate = FormatState::Flags;
                            match cur {
                                ':' => (),
                                '#' => flags.alternate = true,
                                ' ' => flags.space = true,
                                '.' => fstate = FormatState::Precision,
                                '0'..='9' => {
                                    flags.width = cur as usize - '0' as usize;
                                    fstate = FormatState::Width;
                                }
                                _ => unreachable!("logic error"),
                            }
                            state = States::FormatPattern(flags, fstate);
                        }

                        // conditionals
                        '?' | ';' => (),
                        't' => match stack.pop() {
                            Some(Parameter::Number(0)) => state = States::SeekIfElse(0),
                            Some(Parameter::Number(_)) => (),
                            Some(_) => return Err(Error::TypeMismatch),
                            None => return Err(Error::StackUnderflow),
                        },
                        'e' => state = States::SeekIfEnd(0),
                        c => return Err(Error::UnrecognizedFormatOption(c)),
                    }
                }
                States::PushParam => {
                    // params are 1-indexed
                    stack.push(
                        mparams[match cur.to_digit(10) {
                            Some(d) => d as usize - 1,
                            None => return Err(Error::InvalidParameterIndex(cur)),
                        }]
                        .clone(),
                    );
                }
                States::SetVar => match cur {
                    'A'..='Z' => {
                        if let Some(arg) = stack.pop() {
                            let idx = (cur as u8) - b'A';
                            self.static_variables[idx as usize] = arg;
                        } else {
                            return Err(Error::StackUnderflow);
                        }
                    }
                    'a'..='z' => {
                        if let Some(arg) = stack.pop() {
                            let idx = (cur as u8) - b'a';
                            self.dynamic_variables[idx as usize] = arg;
                        } else {
                            return Err(Error::StackUnderflow);
                        }
                    }
                    _ => {
                        return Err(Error::InvalidVariableName(cur));
                    }
                },
                States::GetVar => match cur {
                    'A'..='Z' => {
                        let idx = (cur as u8) - b'A';
                        stack.push(self.static_variables[idx as usize].clone());
                    }
                    'a'..='z' => {
                        let idx = (cur as u8) - b'a';
                        stack.push(self.dynamic_variables[idx as usize].clone());
                    }
                    _ => {
                        return Err(Error::InvalidVariableName(cur));
                    }
                },
                States::CharConstant => {
                    stack.push(Parameter::from(i32::from(c)));
                    state = States::CharClose;
                }
                States::CharClose => {
                    if cur != '\'' {
                        return Err(Error::MalformedCharacterConstant);
                    }
                }
                States::IntConstant(i) => {
                    if cur == '}' {
                        stack.push(Parameter::from(i));
                        state = States::Nothing;
                    } else if let Some(digit) = cur.to_digit(10) {
                        match i
                            .checked_mul(10)
                            .and_then(|i_ten| i_ten.checked_add(digit as i32))
                        {
                            Some(i) => {
                                state = States::IntConstant(i);
                                old_state = States::Nothing;
                            }
                            None => return Err(Error::IntegerConstantOverflow),
                        }
                    } else {
                        return Err(Error::MalformedIntegerConstant);
                    }
                }
                States::FormatPattern(ref mut flags, ref mut fstate) => {
                    old_state = States::Nothing;
                    match (*fstate, cur) {
                        (_, 'd') | (_, 'o') | (_, 'x') | (_, 'X') | (_, 's') => {
                            if let Some(arg) = stack.pop() {
                                let res = format(arg, FormatOp::from_char(cur), *flags)?;
                                output.extend(res);
                                // will cause state to go to States::Nothing
                                old_state = States::FormatPattern(*flags, *fstate);
                            } else {
                                return Err(Error::StackUnderflow);
                            }
                        }
                        (FormatState::Flags, '#') => {
                            flags.alternate = true;
                        }
                        (FormatState::Flags, '-') => {
                            flags.left = true;
                        }
                        (FormatState::Flags, '+') => {
                            flags.sign = true;
                        }
                        (FormatState::Flags, ' ') => {
                            flags.space = true;
                        }
                        (FormatState::Flags, '0'..='9') => {
                            flags.width = cur as usize - '0' as usize;
                            *fstate = FormatState::Width;
                        }
                        (FormatState::Width, '0'..='9') => {
                            flags.width = match flags
                                .width
                                .checked_mul(10)
                                .and_then(|w| w.checked_add(cur as usize - '0' as usize))
                            {
                                Some(width) => width,
                                None => return Err(Error::FormatWidthOverflow),
                            }
                        }
                        (FormatState::Width, '.') | (FormatState::Flags, '.') => {
                            *fstate = FormatState::Precision;
                        }
                        (FormatState::Precision, '0'..='9') => {
                            flags.precision = match flags
                                .precision
                                .checked_mul(10)
                                .and_then(|w| w.checked_add(cur as usize - '0' as usize))
                            {
                                Some(precision) => precision,
                                None => return Err(Error::FormatPrecisionOverflow),
                            }
                        }
                        _ => return Err(Error::UnrecognizedFormatOption(cur)),
                    }
                }
                States::SeekIfElse(level) => {
                    if cur == '%' {
                        state = States::SeekIfElsePercent(level);
                    }
                    old_state = States::Nothing;
                }
                States::SeekIfElsePercent(level) => {
                    if cur == ';' {
                        if level == 0 {
                            state = States::Nothing;
                        } else {
                            state = States::SeekIfElse(level - 1);
                        }
                    } else if cur == 'e' && level == 0 {
                        state = States::Nothing;
                    } else if cur == '?' {
                        state = States::SeekIfElse(level + 1);
                    } else {
                        state = States::SeekIfElse(level);
                    }
                }
                States::SeekIfEnd(level) => {
                    if cur == '%' {
                        state = States::SeekIfEndPercent(level);
                    }
                    old_state = States::Nothing;
                }
                States::SeekIfEndPercent(level) => {
                    if cur == ';' {
                        if level == 0 {
                            state = States::Nothing;
                        } else {
                            state = States::SeekIfEnd(level - 1);
                        }
                    } else if cur == '?' {
                        state = States::SeekIfEnd(level + 1);
                    } else {
                        state = States::SeekIfEnd(level);
                    }
                }
            }
            if state == old_state {
                state = States::Nothing;
            }
        }
        Ok(output)
    }
}

#[derive(Copy, PartialEq, Clone, Default)]
struct Flags {
    width: usize,
    precision: usize,
    alternate: bool,
    left: bool,
    sign: bool,
    space: bool,
}

#[derive(Copy, Clone)]
enum FormatOp {
    Digit,
    Octal,
    HexLower,
    HexUpper,
    String,
}

impl FormatOp {
    fn from_char(c: char) -> FormatOp {
        match c {
            'd' => FormatOp::Digit,
            'o' => FormatOp::Octal,
            'x' => FormatOp::HexLower,
            'X' => FormatOp::HexUpper,
            's' => FormatOp::String,
            _ => panic!("bad FormatOp char"),
        }
    }
}

fn format(val: Parameter, op: FormatOp, flags: Flags) -> Result<Vec<u8>, Error> {
    let mut s = match val {
        Parameter::Number(d) => {
            match op {
                FormatOp::Digit => {
                    if flags.sign {
                        format!("{:+01$}", d, flags.precision)
                    } else if d < 0 {
                        // C doesn't take sign into account in precision calculation.
                        format!("{:01$}", d, flags.precision + 1)
                    } else if flags.space {
                        format!(" {:01$}", d, flags.precision)
                    } else {
                        format!("{:01$}", d, flags.precision)
                    }
                }
                FormatOp::Octal => {
                    if flags.alternate {
                        // Leading octal zero counts against precision.
                        format!("0{:01$o}", d, flags.precision.saturating_sub(1))
                    } else {
                        format!("{:01$o}", d, flags.precision)
                    }
                }
                FormatOp::HexLower => {
                    if flags.alternate && d != 0 {
                        format!("0x{:01$x}", d, flags.precision)
                    } else {
                        format!("{:01$x}", d, flags.precision)
                    }
                }
                FormatOp::HexUpper => {
                    if flags.alternate && d != 0 {
                        format!("0X{:01$X}", d, flags.precision)
                    } else {
                        format!("{:01$X}", d, flags.precision)
                    }
                }
                FormatOp::String => return Err(Error::TypeMismatch),
            }
            .into_bytes()
        }
        Parameter::String(mut s) => match op {
            FormatOp::String => {
                if flags.precision > 0 && flags.precision < s.len() {
                    s.truncate(flags.precision);
                }
                s
            }
            _ => return Err(Error::TypeMismatch),
        },
    };
    if flags.width > s.len() {
        let n = flags.width - s.len();
        if flags.left {
            s.extend(repeat_n(b' ', n));
        } else {
            let mut s_ = Vec::with_capacity(flags.width);
            s_.extend(repeat_n(b' ', n));
            s_.extend(s);
            s = s_;
        }
    }
    Ok(s)
}

#[cfg(test)]
mod test {
    use super::{Error, ExpandContext, Parameter};

    #[test]
    fn test_basic_setabf() {
        let mut expand_context = ExpandContext::new();
        assert_eq!(
            expand_context.expand(b"\\E[48;5;%p1%dm", &[Parameter::from(1)]),
            Ok(b"\\E[48;5;1m".to_vec())
        );
    }

    #[test]
    fn test_multiple_int_constants() {
        let mut expand_context = ExpandContext::new();
        assert_eq!(
            expand_context.expand(b"%{1}%{2}%d%d", &[]),
            Ok(b"21".to_vec())
        );
    }

    #[test]
    fn test_op_i() {
        let mut expand_context = ExpandContext::new();
        assert_eq!(
            expand_context.expand(
                b"%p1%d%p2%d%p3%d%i%p1%d%p2%d%p3%d",
                &[Parameter::from(1), Parameter::from(2), Parameter::from(3)],
            ),
            Ok(b"123233".to_vec())
        );
        assert_eq!(
            expand_context.expand(b"%p1%d%p2%d%i%p1%d%p2%d", &[]),
            Ok(b"0011".to_vec())
        );
    }

    #[test]
    fn test_param_stack_failure_conditions() {
        let mut expand_context = ExpandContext::new();
        fn get_res(
            fmt: &[u8],
            cap: &str,
            params: &[Parameter],
            expand_context: &mut ExpandContext,
        ) -> Result<Vec<u8>, Error> {
            let mut fmt = fmt.to_vec();
            fmt.extend(cap.as_bytes());
            expand_context.expand(&fmt, params)
        }

        let caps = ["%d", "%c", "%s", "%Pa", "%l", "%!", "%~"];
        for &cap in &caps {
            let res = get_res(b"", cap, &[], &mut expand_context);
            assert_eq!(
                res,
                Err(Error::StackUnderflow),
                "Op {cap} - stack underflow not detected with 0 stack entries",
            );
            let p = if cap == "%s" || cap == "%l" {
                Parameter::from(b"foo")
            } else {
                Parameter::from(97)
            };
            let res = get_res(b"%p1", cap, &[p], &mut expand_context);
            assert!(res.is_ok(), "Op {cap} failed with 1 stack entry: {res:?}");
        }
        let caps = ["%+", "%-", "%*", "%/", "%m", "%&", "%|", "%A", "%O"];
        for &cap in &caps {
            let res = expand_context.expand(cap.as_bytes(), &[]);
            assert_eq!(
                res,
                Err(Error::StackUnderflow),
                "Binop {cap} - stack underflow not detected with 0 stack entries",
            );
            let res = get_res(b"%{1}", cap, &[], &mut expand_context);
            assert_eq!(
                res,
                Err(Error::StackUnderflow),
                "Binop {cap} - stack underflow not detected with 1 stack entry",
            );
            let res = get_res(b"%{1}%{2}", cap, &[], &mut expand_context);
            assert!(
                res.is_ok(),
                "Binop {cap} failed with 2 stack entries: {res:?}"
            );
        }
    }

    #[test]
    fn test_push_bad_param() {
        let mut expand_context = ExpandContext::new();
        assert_eq!(
            expand_context.expand(b"%pa", &[]),
            Err(Error::InvalidParameterIndex('a'))
        );
    }

    #[test]
    fn test_comparison_ops() {
        let v = [
            ('<', [1u8, 0u8, 0u8]),
            ('=', [0u8, 1u8, 0u8]),
            ('>', [0u8, 0u8, 1u8]),
        ];
        let mut expand_context = ExpandContext::new();
        for &(op, bs) in &v {
            let s = format!("%{{1}}%{{2}}%{op}%d");
            let res = expand_context.expand(s.as_bytes(), &[]);
            assert_eq!(res, Ok(vec![b'0' + bs[0]]));

            let s = format!("%{{1}}%{{1}}%{op}%d");
            let res = expand_context.expand(s.as_bytes(), &[]);
            assert_eq!(res, Ok(vec![b'0' + bs[1]]));

            let s = format!("%{{2}}%{{1}}%{op}%d");
            let res = expand_context.expand(s.as_bytes(), &[]);
            assert_eq!(res, Ok(vec![b'0' + bs[2]]));
        }
    }

    #[test]
    fn test_conditionals() {
        let mut expand_context = ExpandContext::new();
        let s = b"\\E[%?%p1%{8}%<%t3%p1%d%e%p1%{16}%<%t9%p1%{8}%-%d%e38;5;%p1%d%;m";
        let res = expand_context.expand(s, &[Parameter::from(1)]);
        assert_eq!(res, Ok(b"\\E[31m".to_vec()));
        let res = expand_context.expand(s, &[Parameter::from(8)]);
        assert_eq!(res, Ok(b"\\E[90m".to_vec()));
        let res = expand_context.expand(s, &[Parameter::from(42)]);
        assert_eq!(res, Ok(b"\\E[38;5;42m".to_vec()));
    }

    #[test]
    fn test_format() {
        let mut expand_context = ExpandContext::new();
        assert_eq!(
            expand_context.expand(
                b"%p1%s%p2%2s%p3%2s%p4%.2s",
                &[
                    Parameter::from(b"foo"),
                    Parameter::from(b"foo"),
                    Parameter::from(b"f"),
                    Parameter::from(b"foo")
                ],
            ),
            Ok(b"foofoo ffo".to_vec())
        );
        assert_eq!(
            expand_context.expand(b"%p1%:-4.2s", &[Parameter::from(b"foo")]),
            Ok(b"fo  ".to_vec())
        );

        assert_eq!(
            expand_context.expand(b"%p1%d%p1%.3d%p1%5d%p1%:+d", &[Parameter::from(1)]),
            Ok(b"1001    1+1".to_vec())
        );
        assert_eq!(
            expand_context.expand(
                b"%p1%o%p1%#o%p2%6.4x%p2%#6.4X",
                &[Parameter::from(15), Parameter::from(27)],
            ),
            Ok(b"17017  001b0X001B".to_vec())
        );
    }
}
