let template =
  Mustache.of_string
    {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>{{{title}}}</title>
  <meta charset="utf-8" />
  <link rel="stylesheet" href="{{{odoc_assets_path}}}/odoc.css" />
  <meta name="generator" content="odoc-notebook %%VERSION%%" />
  <meta name="viewport" content=
  "width=device-width,initial-scale=1.0" />
  <script src="{{{odoc_assets_path}}}/highlight.pack.js"></script>
  <script type="text/javascript">
  hljs.initHighlightingOnLoad();
  </script>
  <script src="{{{odoc_assets_path}}}/main.bc.js" type="module"></script>
</head>
<body class="odoc">
  <nav class="odoc-nav">{{{breadcrumbs}}}</nav>
  <header class="odoc-preamble">{{{preamble}}}</header>
  <nav class="odoc-toc">{{{toc}}}</nav>
  <div class="odoc-content">{{{content}}}</div>
  {{{post_content}}}
</body>
</html>|}

type spec = {
  title : string;
  odoc_assets_path : string;
  breadcrumbs : string;
  preamble : string;
  toc : string;
  content : string;
  post_content : string;
}

let create
    {
      title;
      odoc_assets_path;
      breadcrumbs;
      preamble;
      toc;
      content;
      post_content;
    } =
  let json =
    `O
      [
        ("title", `String title);
        ("odoc_assets_path", `String odoc_assets_path);
        ("breadcrumbs", `String breadcrumbs);
        ("preamble", `String preamble);
        ("toc", `String toc);
        ("content", `String content);
        ("post_content", `String post_content);
      ]
  in
  try Ok (Mustache.render template json)
  with Mustache.Render_error err ->
    Format.eprintf "%a@." Mustache.pp_render_error err;
    Error (Fmt.to_to_string Mustache.pp_render_error err)
