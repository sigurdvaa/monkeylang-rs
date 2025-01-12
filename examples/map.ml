let map = fn(list, func) {
    let run = fn(list, func, acc) {
        if (len(list) == 0) { return acc; }
        return run(rest(list), func, push(acc, func(first(list))));
    };
    return run(list, func, []);
};
puts(map([1, 2, 3], string),"\n");
