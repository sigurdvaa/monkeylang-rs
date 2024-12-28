let factorial = fn(n) {
    let loop = fn(c, acc) {
        if (c > 1) {
            return loop(c - 1, c * acc);
        }
        return acc;
    }
    return loop(n, 1);
}
let n = 20;
puts("Factorial of ", n, ": ", factorial(n));
