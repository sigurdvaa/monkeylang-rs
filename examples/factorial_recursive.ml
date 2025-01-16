let factorial = fn(n) {
    if (n < 2) {
        return 1;
    }
    return n * factorial(n - 1);
};
let n = 20;
puts("Factorial of ", n, ": ", factorial(n), "\n");
