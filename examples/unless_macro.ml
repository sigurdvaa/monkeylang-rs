let unless = macro(condition, consequence, alternative) {
    quote(if (!(unquote(condition))) {
        unquote(consequence);
    } else {
        unquote(alternative);
    });
};
let a = 10;
let b = 5;
unless(a > b, puts(b, " is not greater than ", a), puts(a, " is greater than ", b));
