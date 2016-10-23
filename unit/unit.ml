let test label result =
  if result
  then
    Printf.printf "%s: success\n" label
  else
    Printf.printf "%s: failure\n" label

let _ =
  test "merge strictly connected" Graph_test.test_merge_strictly_connected
