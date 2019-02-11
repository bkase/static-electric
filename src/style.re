open Core_kernel;

include Free_group.Make(String);

let render = t =>
  String.Map.fold(t, ~init=[], ~f=(~key, ~data, acc) =>
    if (data > 0) {
      [key, ...acc];
    } else {
      acc;
    }
  )
  |> List.rev;
