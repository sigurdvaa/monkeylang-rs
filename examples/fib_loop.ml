let fib = fn(n) {
    if (n < 1) {
        return 0;
    } 
    if (n < 3) {
        return 1;
    } 
    let loop = fn(c, a, b) {
        if (c < n) {
            return loop(c + 1, b, a + b);
        } else {
            return b;
        }
    }
    return loop(2, 1, 1);
}

let n = 35;
puts("Fibonacci of " + string(n) + ": " + string(fib(n)), "\n");

let n = 92;
puts("Fibonacci of " + string(n) + ": " + string(fib(n)), "\n");
