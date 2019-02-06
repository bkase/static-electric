open Tyxml.Html;
let title_ = title;
open Core_kernel;

module Hlist = {
  type t('a) =
    | []: t(unit)
    | ::(elt('a), t('b)): t('a => 'b);
};
include Hlist;

let a = (~class_=?, ~style=?, ~href=?, ~children, ()) =>
  a(
    ~a=List.filter_opt([href |> Option.map(~f=a_href), class_, style]),
    children,
  );

let div = (~class_=?, ~style=?, ~children, ()) =>
  div(
    ~a=
      List.filter_opt([
        class_,
        style |> Option.map(~f=Fn.compose(a_style, Style.render)),
      ]),
    children,
  );

let body = (~children, ()) => body(children);

let head = (~title, ~children, ()) => head(title_(txt(title)), children);
let meta = (~charset=?, ~children as _: t(unit), ()) =>
  meta(~a=List.filter_opt([charset |> Option.map(~f=a_charset)]), ());

let html =
    (~children: t(([< Html_types.head], [< Html_types.body]) => unit), ()) =>
  switch (children) {
  | [head, body] => html(head, body)
  };

let text = s => txt(s);
