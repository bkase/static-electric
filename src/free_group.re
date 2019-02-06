open Core_kernel;

module Make = (E: Comparable.S) => {
  type t = E.Map.t(int);

  let empty: t = E.Map.empty;

  let return = x => E.Map.singleton(x, 1);

  let combine = (op, x, y) =>
    E.Map.merge(x, y, ~f=(~key as _, w) =>
      switch (w) {
      | `Both(a, b) =>
        let res = op(a, b);
        if (res != 0) {
          Some(res);
        } else {
          None;
        };
      | `Left(a) => Some(a)
      | `Right(b) => Some(op(0, 1) * b)
      }
    );

  let (+) = combine((+));

  let (-) = combine((-));

  let inv = x => E.Map.map(x, ~f=a => (-1) * a);
};

/*
   TODO: Figure out how to ppx_inline_test with reasonml
 let%test_module "group laws" = {
   module G = Make(String);

   let%test_unit "xx" = Quickcheck.test;
   String.gen;
 };
 */
