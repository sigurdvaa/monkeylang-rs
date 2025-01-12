let double = fn(x) { x * 2 };
let arr = [1, 2, 3];
let mapped = map(arr, double);
let stringed= map(mapped, string);
puts("original: ", arr, "\n")
puts("doubled:  ", mapped, "\n");
puts("stringed: ", stringed, "\n");
