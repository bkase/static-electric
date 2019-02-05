open Tyxml.Html;
open Core_kernel;

module Hlist = {
  type t('a) =
    | []: t(unit)
    | ::(elt('a), t('b)): t('a => 'b);
};
open Hlist;

let a = (~class_=?, ~style=?, ~href=?, ~children, ()) =>
  a(~a=List.filter_opt([href, class_, style]), children);

let div = (~class_=?, ~style=?, ~children, ()) =>
  div(~a=List.filter_opt([class_, style]), children);

let body = (~children, ()) => body(children);

let head = (~title, ~children, ()) => head(title, children);
let meta = (~charset=?, ~children as _: t(unit), ()) =>
  meta(~a=List.filter_opt([charset]), ());

let html =
    (~children: t(([< Html_types.head], [< Html_types.body]) => unit), ()) =>
  switch (children) {
  | [head, body] => html(head, body)
  };

let title = title(txt("Example Title"));

let test =
  <html>
    <head title> <meta charset={a_charset("utf-8")} /> </head>
    <body>
      <div> <a href={a_href("#test")}> {txt("Hello world")} </a> </div>
    </body>
  </html>;

let s = Format.asprintf("%a", Tyxml.Html.pp(), test);
print_endline(s);
