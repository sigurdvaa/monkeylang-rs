let fib = fn(n) {
    let init_cache = fn(c, cache) {
        if (c < n) {
            let cache = push(cache, null);
            return init_cache(c + 1, cache);
        }
        return cache;
    };
    let cache = init_cache(0, []);

    let fib_cache = fn(n, cache) {
        if (n < 1) {
            return [0, cache];
        }
        if (n < 3) {
            return [1, cache];
        }
        if (cache[n] == null) {
            let a = fib_cache(n - 1, cache);
            let b = fib_cache(n - 2, a[1]);
            let cache = push(b[1], a[0] + b[0], n);
        }
        return [cache[n], cache];
    };

    let result = fib_cache(n, cache);
    return result[0];
};
let n = 92;
puts("Fibonacci of " + string(n) + ": " + string(fib(n)));
