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
  max-width: 700px;
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
  
.gtoctree {
  --spacing: 1.5rem;
  --radius: 10px;
}

.gtoctree li {
  display: block;
  position: relative;
  padding-left: calc(2 * var(--spacing) - var(--radius) - 2px);
}

.gtoctree ul {
  margin-left: calc(var(--radius) - var(--spacing));
  padding-left: 0;
}

.gtoctree ul li {
  border-left: 2px solid #ddd;
}

.gtoctree ul li:last-child {
  border-color: transparent;
}

.gtoctree ul li::before {
  content: '';
  display: block;
  position: absolute;
  top: calc(var(--spacing) / -2);
  left: -2px;
  width: calc(var(--spacing) + 2px);
  height: calc(var(--spacing) + 1px);
  border: solid #ddd;
  border-width: 0 0 2px 2px;
}

.gtoctree summary {
  display: block;
  cursor: pointer;
}

.gtoctree summary::marker,
.gtoctree summary::-webkit-details-marker {
  display: none;
}

.gtoctree summary:focus {
  outline: none;
}

.gtoctree summary:focus-visible {
  outline: 1px dotted #000;
}

.gtoctree li::after,
.gtoctree summary::before {
  content: '';
  display: block;
  position: absolute;
  top: calc(var(--spacing) / 2 - var(--radius));
  left: calc(var(--spacing) - var(--radius) - 1px);
  width: calc(2 * var(--radius));
  height: calc(2 * var(--radius));
  border-radius: 50%;
  background: #ddd;
}
.gtoctree summary::before {
  z-index: 1;
  background: #696 url('expand-collapse.svg') 0 0;
}

.gtoctree details[open] > summary::before {
  background-position: calc(-2 * var(--radius)) 0;
}
|}
