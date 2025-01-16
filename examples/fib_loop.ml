let fib = fn(n) {
    if (n < 1) {
        return 0;
    }
    if (n < 3) {
        return 1;
    }
    let c = 3;
    let a = 1;
    let b = 1;
    loop {
        if (c > n) {
            return b;
        }
        let c = c + 1;
        let tmp = a + b;
        let a = b;
        let b = tmp;
    }
}

let n = 35;
puts("Fibonacci of " + string(n) + ": " + string(fib(n)), "\n");

let n = 92;
puts("Fibonacci of " + string(n) + ": " + string(fib(n)), "\n");
