let with_suppressed_stderr f arg =
  let oldstderr = Unix.dup Unix.stderr in
  let newstderr = open_out "/dev/null" in
  Unix.dup2 (Unix.descr_of_out_channel newstderr) Unix.stderr;

  let res = f arg in

  flush stderr;
  Unix.dup2 oldstderr Unix.stderr;

  res
