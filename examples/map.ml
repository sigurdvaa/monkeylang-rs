let arr = [1, 2, 3];
puts("original: ", arr, "\n")

let double = fn(x) { x * 2 };
let doubled= map(arr, double);
puts("doubled:  ", doubled, "\n");

let stringed= map(doubled, string);
puts("stringed: ", stringed, "\n");
