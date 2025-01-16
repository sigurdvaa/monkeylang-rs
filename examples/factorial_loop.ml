let factorial = fn(n) {
    let acc = 1;
    loop {
        let acc = acc * n;
        let n = n - 1;
        if (n < 2) {
            return acc;
        }
    }
};
let n = 20;
puts("Factorial of ", n, ": ", factorial(n), "\n");
