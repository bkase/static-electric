open Core_kernel;
open Static_electric;
open Html;

let test =
  <html>
    <head title="Example Title"> <meta charset="utf-8" /> </head>
    <body>
      <div style={Style.return("bold")}>
        <a href="#test"> {text("Hello world")} </a>
      </div>
    </body>
  </html>;

let s = Format.asprintf("%a", Tyxml.Html.pp(), test);
print_endline(s);
