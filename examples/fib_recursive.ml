let fib = fn(n) {
    if (n < 1) {
        return 0;
    }
    if (n < 3) {
        return 1;
    }
    return fib(n-1) + fib(n-2);
};
let n = 25;
puts("Fibonacci of " + string(n) + ": " + string(fib(n)));
