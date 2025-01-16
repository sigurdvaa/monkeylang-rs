let counter = fn(x) {
    if (x < 100) {
        puts(x, " ");
        counter(x + 1);
    }
};
counter(0);
puts("\n");
