let contains_substring ~needle haystack =
  let needle_len = String.length needle in
  if needle_len = 0 then
    true
  else
    let haystack_len = String.length haystack in
    if needle_len > haystack_len then
      false
    else
      let last_start = haystack_len - needle_len in
      let rec matches_at i j =
        if j = needle_len then
          true
        else if String.get haystack (i + j) <> String.get needle j then
          false
        else
          matches_at i (j + 1)
      in
      let rec search i =
        if i > last_start then
          false
        else if matches_at i 0 then
          true
        else
          search (i + 1)
      in
      search 0

let%test "empty needle" = contains_substring "hello" ~needle:""
let%test "match at start" = contains_substring "hello world" ~needle:"hello"
let%test "match in middle" = contains_substring "hello world" ~needle:"lo wo"
let%test "match at end" = contains_substring "hello world" ~needle:"world"
let%test "no match" = not (contains_substring "hello" ~needle:"xyz")
let%test "needle longer than haystack" = not (contains_substring "hi" ~needle:"hello")
let%test "exact match" = contains_substring "hello" ~needle:"hello"
