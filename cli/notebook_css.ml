(* Notebook CSS *)

let notebook_css =
  {|
button.panel {
  background-color: #2d2f3f;
  border: solid 1px #f8f8f2;
  color: #f8f8f2;
  margin-left: 5px;
}

div.editor {
  background-color: #2d2f3f;
  border: 1px solid #2d2f3f;
  padding: 5px;
  border-radius: 5px;
}

div.odoc-src-output {
  margin-top: 10px;
}

pre.stdout {
  border: none;
  border-top: 1px solid #f8f8f2;
  border-radius: 0;
  color: #f8f8f2;
}

.hidden {
  display: none;
}

.at-tags > .libs {
  display: none;
}

.at-tags > .merlinonly {
  display: none;
}

.at-tags > .switch {
  display: none;
}

.at-tags > .published > .at-tag {
  display : none;
}

.at-tags > .published > p {
  ::before {
    content: "Published: ";
  }
}

.at-tags > .published > p::before {
    content: "Published: ";
}

@media only screen and (max-width: 210ex) {
 div.editor {
    max-width: 50vw;
  }
}

@media only screen and (max-width: 110ex) {
  div.editor {
      max-width: 80vw;
  }
}

|}
