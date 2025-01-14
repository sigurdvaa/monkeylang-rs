let foo = fn(x) {
    puts("Printing value before exit: ", x, "\n")
    exit x;
    puts("exit failed\n")
}
foo(99)
foo(1)
