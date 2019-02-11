open Tyxml.Html;
let title_ = title;
open Core_kernel;

module Hlist = {
  type t('a) =
    | []: t(unit)
    | ::(elt('a), t('b)): t('a => 'b);
};
include Hlist;

let style_of_class = class_ =>
  Option.map(class_, ~f=Fn.compose(a_class, Style.render));

let a = (~class_=?, ~href=?, ~children, ()) =>
  a(
    ~a=
      List.filter_opt([
        href |> Option.map(~f=a_href),
        style_of_class(class_),
      ]),
    children,
  );

let div = (~class_=?, ~children, ()) =>
  div(~a=List.filter_opt([style_of_class(class_)]), children);

let body = (~class_=?, ~children, ()) =>
  body(~a=List.filter_opt([style_of_class(class_)]), children);

let head = (~title, ~children, ()) => head(title_(txt(title)), children);
let meta = (~charset=?, ~children as _: t(unit), ()) =>
  meta(~a=List.filter_opt([charset |> Option.map(~f=a_charset)]), ());

let html =
    (~children: t(([< Html_types.head], [< Html_types.body]) => unit), ()) =>
  switch (children) {
  | [head, body] => html(head, body)
  };

let text = s => txt(s);

/* headings
 * TODO: It would be nice to share code here, but the types aren't trivial */

let h1 = (~class_=?, ~children, ()) =>
  h1(~a=List.filter_opt([style_of_class(class_)]), children);
let h2 = (~class_=?, ~children, ()) =>
  h2(~a=List.filter_opt([style_of_class(class_)]), children);
let h3 = (~class_=?, ~children, ()) =>
  h3(~a=List.filter_opt([style_of_class(class_)]), children);
let h4 = (~class_=?, ~children, ()) =>
  h4(~a=List.filter_opt([style_of_class(class_)]), children);
let h5 = (~class_=?, ~children, ()) =>
  h5(~a=List.filter_opt([style_of_class(class_)]), children);
let h6 = (~class_=?, ~children, ()) =>
  h6(~a=List.filter_opt([style_of_class(class_)]), children);
