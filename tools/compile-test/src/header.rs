use std::iter::Peekable;
use std::str::Chars;

fn skip_whitespace(input: &mut Peekable<Chars>) {
    while let Some(ch) = input.peek() {
        if ch.is_whitespace() {
            input.next();
        } else {
            break;
        }
    }
}

fn parse_compiler_key(input: &mut Peekable<Chars>) -> Option<String> {
    let mut key = String::new();
    while let Some(ch) = input.peek() {
        if ch.eq(&'=') && !key.is_empty() {
            return Some(key);
        } else if ch.is_ascii_alphanumeric() {
            key.push(*ch);
            input.next();
        } else {
            break;
        }
    }
    if key.is_empty() { None } else { Some(key) }
}

fn parse_string_compiler_option_array(input: &mut Peekable<Chars>) -> Vec<String> {
    debug_assert!(input.peek().is_some_and(|ch| ch == &'['));
    input.next();
    let mut value = vec![];
    let mut current = String::new();
    while let Some(ch) = input.peek() {
        if ch.eq(&']') {
            input.next();
            break;
        } else if ch.is_whitespace() {
            input.next();
        } else if ch.eq(&',') {
            if !current.is_empty() {
                value.push(std::mem::take(&mut current));
            }
            input.next();
            skip_whitespace(input);
        } else if ch.is_ascii() {
            current.push(*ch);
            input.next();
        } else {
            unreachable!("character `{ch}` is not expected in a compiler option array");
        }
    }
    if !current.is_empty() {
        value.push(current);
    }
    value
}

fn parse_compiler_option_list(input: &mut Peekable<Chars>) -> Option<serde_json::Value> {
    let mut value = vec![];
    let mut current = String::new();
    let consume_current = |v: String| {
        if v.is_empty() {
            None
        } else if v == "true" {
            Some(serde_json::Value::Bool(true))
        } else if v == "false" {
            Some(serde_json::Value::Bool(false))
        } else {
            Some(serde_json::Value::String(v))
        }
    };
    while let Some(ch) = input.peek() {
        if ch.is_whitespace() {
            if let Some(option) = consume_current(std::mem::take(&mut current)) {
                value.push(option);
            }
            break;
        } else if ch.eq(&'[') {
            let array = parse_string_compiler_option_array(input);
            value.push(serde_json::Value::Array(
                array.into_iter().map(serde_json::Value::String).collect(),
            ));
        } else if ch.eq(&',') {
            if let Some(option) = consume_current(std::mem::take(&mut current)) {
                value.push(option);
            }
            input.next();
            skip_whitespace(input);
        } else if ch.is_ascii_alphanumeric() {
            current.push(*ch);
            input.next();
        } else {
            return None;
        }
    }
    if let Some(current) = consume_current(std::mem::take(&mut current)) {
        value.push(current);
    }
    if value.is_empty() {
        None
    } else if value.len() == 1 {
        Some(std::mem::take(&mut value[0]))
    } else {
        unreachable!()
    }
}

pub(super) fn parse_compiler_options(input: &str) -> Vec<(String, serde_json::Value)> {
    let mut result = Vec::new();
    let mut input = input.chars().peekable();
    loop {
        skip_whitespace(&mut input);
        let Some(key) = parse_compiler_key(&mut input) else {
            return result;
        };
        skip_whitespace(&mut input);
        match input.peek() {
            Some(&'=') => input.next(),
            Some(_) => {
                return result;
            }
            None => {
                result.push((key, serde_json::Value::Bool(true)));
                return result;
            }
        };
        skip_whitespace(&mut input);
        if let Some(options) = parse_compiler_option_list(&mut input) {
            result.push((key, options));
        }
    }
}
