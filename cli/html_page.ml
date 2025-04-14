let template =
  Mustache.of_string
    {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>{{{title}}}</title>
  <meta charset="utf-8" />
  <link rel="stylesheet" href="{{{odoc_assets_path}}}/odoc.css" />
  <link rel="stylesheet" href="{{{odoc_assets_path}}}/odoc-notebook.css" />
  <meta name="generator" content="odoc-notebook %%VERSION%%" />
  <meta name="viewport" content=
  "width=device-width,initial-scale=1.0" />
  <link rel="stylesheet" href="{{{odoc_assets_path}}}/katex.min.css" />
  <script src="{{{odoc_assets_path}}}/katex.min.js"></script>
  <script src="{{{odoc_assets_path}}}/{{frontend}}" type="module"></script>
  <script>
  //<![CDATA[

          document.addEventListener("DOMContentLoaded", function () {
            var elements = Array.from(document.getElementsByClassName("odoc-katex-math"));
            for (var i = 0; i < elements.length; i++) {
              var el = elements[i];
              var content = el.textContent;
              var new_el = document.createElement("span");
              new_el.setAttribute("class", "odoc-katex-math-rendered");
              var display = el.classList.contains("display");
              katex.render(content, new_el, { throwOnError: false, displayMode: display });
              el.replaceWith(new_el);
            }
          });
        
//]]>
</script>
</head>
<body class="odoc">
  <nav class="odoc-nav">{{{breadcrumbs}}}</nav>
  <div class="odoc-search">
    <div class="search-inner">
      <input class="search-bar" placeholder=
      "🔎 Type '/' to search..." />
      <div class="search-snake"></div>
      <div class="search-result"></div>
    </div>
  </div>
  <header class="odoc-preamble">{{{header}}}{{{preamble}}}</header>
  <div class="odoc-tocs">
    {{#localtoc}}
      <nav class="odoc-toc odoc-local-toc">{{{localtoc}}}</nav>
    {{/localtoc}}
    <nav class="odoc-toc odoc-global-toc">{{{globaltoc}}}</nav>
  </div>
  <div class="odoc-content">{{{content}}}</div>
  {{{post_content}}}
</body>
</html>|}

type spec = {
  title : string;
  header : string;
  odoc_assets_path : string;
  breadcrumbs : string;
  preamble : string;
  frontend : string;
  localtoc : string option;
  globaltoc : string;
  content : string;
  post_content : string;
}

let create
    {
      title;
      header;
      odoc_assets_path;
      breadcrumbs;
      preamble;
      frontend;
      localtoc;
      globaltoc;
      content;
      post_content;
    } =
  let json =
    `O
      ([
        ("title", `String title);
        ("header", `String header);
        ("odoc_assets_path", `String odoc_assets_path);
        ("breadcrumbs", `String breadcrumbs);
        ("preamble", `String preamble);
        ("frontend", `String frontend);
        ("localtoc", match localtoc with Some l -> `String l | None -> `Null);
        ("globaltoc", `String globaltoc);
        ("content", `String content);
        ("post_content", `String post_content);
      ] @
      (match localtoc with
      | Some l -> [ ("localtoc", `String l) ]
      | None -> []))
  in
  try Ok (Mustache.render template json)
  with Mustache.Render_error err ->
    Format.eprintf "%a@." Mustache.pp_render_error err;
    Error (Fmt.to_to_string Mustache.pp_render_error err)
