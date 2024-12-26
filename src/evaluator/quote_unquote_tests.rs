#[cfg(test)]
use super::{tests::test_eval, *};

#[test]
fn test_quote() {
    let tests = [
        ("quote(5)", "5"),
        ("quote(5 + 8)", "(5 + 8)"),
        ("quote(foobar)", "foobar"),
        ("quote(foobar + barfoo)", "(foobar + barfoo)"),
    ];

    for (test_input, test_value) in tests {
        let eval = test_eval(test_input);
        match eval.as_ref() {
            Object::Quote(expr) => {
                assert_eq!(expr.to_string(), test_value);
            }
            _ => panic!("object is not Quote, got {eval:?}"),
        }
    }
}

#[test]
fn test_quote_unquote() {
    let tests = [
        ("quote(unquote(4))", "4"),
        ("quote(unquote(4 + 4))", "8"),
        ("quote(8 + unquote(4 + 4))", "(8 + 8)"),
        ("quote(unquote(4 + 4) + 8)", "(8 + 8)"),
    ];

    for (test_input, test_value) in tests {
        let eval = test_eval(test_input);
        match eval.as_ref() {
            Object::Quote(expr) => {
                assert_eq!(expr.to_string(), test_value);
            }
            _ => panic!("object is not Quote, got {eval:?}"),
        }
    }
}
