let contains_substring ~needle haystack =
  try ignore (Str.search_forward (Str.regexp_string needle) haystack 0); true
  with Not_found -> false

let%test "empty needle" = contains_substring "hello" ~needle:""
let%test "match at start" = contains_substring "hello world" ~needle:"hello"
let%test "match in middle" = contains_substring "hello world" ~needle:"lo wo"
let%test "match at end" = contains_substring "hello world" ~needle:"world"
let%test "no match" = not (contains_substring "hello" ~needle:"xyz")
let%test "needle longer than haystack" = not (contains_substring "hi" ~needle:"hello")
let%test "exact match" = contains_substring "hello" ~needle:"hello"
