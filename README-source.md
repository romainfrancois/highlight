
<style type="text/css">.rcode{
  padding: 5px ;
}

.number{
    color: rgb(21,20,181) ;
}

.functioncall{
    color: red ;
}

.string{
    color: rgb(153,153,255) ;
}

.keyword{
    color: black;
}

.special{
    color: black;
}

.symbol_argument{
    color: rgb( 177,63,5) ;
    font-weight: bold ;
}

.argument{
    color: rgb( 177,63,5) ;
}

.comment{
    color: rgb( 190, 190, 190) ;
}

.roxygencomment{
    color: rgb(0,151,255);
}

.symbol_formalargs{
    color: rgb(18,182,18);
    font-style: italic;
}

.eqformalargs{
    color: rgb(18,182,18);
}

.assignement{
    color: rgb(55,55,98);
}

.package{
    color: rgb(150,182,37);
}

.slot{
    font-style:italic;
}

.symbol{
    color: black ;
}

.prompt{
    color: black ;
}

.line{
    color: gray ;   
}
</style>
<style type="text/css">
pre {
  border: 1px solid black ;
}
</style>
highlight
=========

highlight is a syntax highlighter for R code.

Installation
------------

You can install highlight from github with:

<pre><span class='comment'># install.packages("devtools")</span>
<span class='package'>devtools</span><span class='keyword'>::</span><span class='functioncall'>install_github</span><span class='keyword'>(</span><span class='string'>"romainfrancois/highlight"</span><span class='keyword'>)</span></pre>
Typical highlighting (lestrade)
-------------------------------

Typical syntax highlighting is only interested in whether a token is a function call, a keyword, a string, ... this is what the `lestrade` detective does:

<pre><span class='functioncall'>highlight</span><span class='keyword'>(</span> <span class='symbol_argument'>file</span> <span class='argument'>=</span> <span class='string'>"css_file.R"</span><span class='keyword'>,</span> <span class='symbol_argument'>detective</span> <span class='argument'>=</span> <span class='symbol'>lestrade</span> <span class='keyword'>)</span></pre>
This will look like this:

<pre><span class='symbol'>css_file</span> <span class='assignment'>&lt;-</span> <span class='keyword'>function</span><span class='keyword'>(</span> <span class='symbol_formalargs'>filename</span> <span class='eqformalargs'>=</span> <span class='string'>"default.css"</span> <span class='keyword'>)</span><span class='keyword'>{</span>
  <span class='keyword'>if</span><span class='keyword'>(</span> <span class='functioncall'>file.exists</span><span class='keyword'>(</span><span class='symbol'>filename</span><span class='keyword'>)</span> <span class='keyword'>)</span><span class='keyword'>{</span>
    <span class='keyword'>return</span><span class='keyword'>(</span><span class='functioncall'>normalizePath</span><span class='keyword'>(</span><span class='symbol'>filename</span><span class='keyword'>)</span><span class='keyword'>)</span>
  <span class='keyword'>}</span>

  <span class='symbol'>f</span> <span class='assignment'>&lt;-</span> <span class='functioncall'>file.path</span><span class='keyword'>(</span> <span class='functioncall'>Sys.getenv</span><span class='keyword'>(</span><span class='string'>"HOME"</span><span class='keyword'>)</span><span class='keyword'>,</span> <span class='string'>".R"</span><span class='keyword'>,</span> <span class='string'>"highlight"</span><span class='keyword'>,</span> <span class='symbol'>filename</span> <span class='keyword'>)</span>
  <span class='keyword'>if</span><span class='keyword'>(</span> <span class='functioncall'>file.exists</span><span class='keyword'>(</span><span class='symbol'>f</span><span class='keyword'>)</span> <span class='keyword'>)</span><span class='keyword'>{</span>
    <span class='keyword'>return</span><span class='keyword'>(</span><span class='symbol'>f</span><span class='keyword'>)</span>
  <span class='keyword'>}</span>

  <span class='symbol'>f</span> <span class='assignment'>&lt;-</span> <span class='functioncall'>system.file</span><span class='keyword'>(</span> <span class='string'>"stylesheet"</span><span class='keyword'>,</span> <span class='symbol'>filename</span> <span class='keyword'>,</span> <span class='symbol_argument'>package</span> <span class='argument'>=</span> <span class='string'>"highlight"</span> <span class='keyword'>)</span>
  <span class='keyword'>if</span><span class='keyword'>(</span> <span class='functioncall'>file.exists</span><span class='keyword'>(</span> <span class='symbol'>f</span> <span class='keyword'>)</span><span class='keyword'>)</span><span class='keyword'>{</span>
    <span class='keyword'>return</span><span class='keyword'>(</span> <span class='symbol'>f</span><span class='keyword'>)</span>
  <span class='keyword'>}</span>

  <span class='functioncall'>stop</span><span class='keyword'>(</span> <span class='functioncall'>glue</span><span class='keyword'>(</span><span class='string'>"file not found: '{filename}'"</span><span class='keyword'>)</span> <span class='keyword'>)</span>
<span class='keyword'>}</span></pre>
This differentiates <span class="functioncall">function calls</span>, <span class="symbol_formalargs">formal arguments</span>, <span class="symbol_argument">used arguments</span> ... because `lestrade` identified these tokens as such.

semantic highlighting (sherlock)
--------------------------------

Traditional highlighting only reveals the obvious, so the package also benefits from the investigation of `sherlock` for semantic highlighting. In semantic highlighting, every symbol gets a different color.

<pre><span class='functioncall'>highlight</span><span class='keyword'>(</span> <span class='symbol_argument'>file</span> <span class='argument'>=</span> <span class='string'>"css_file.R"</span><span class='keyword'>,</span> <span class='symbol_argument'>detective</span> <span class='argument'>=</span> <span class='symbol'>sherlock</span> <span class='keyword'>)</span></pre>
<pre><span class='symbol' style='color: #CC66AA ; '>css_file</span> <span class='assignment'>&lt;-</span> <span class='keyword'>function</span><span class='keyword'>(</span> <span class='symbol_formalargs' style='color: #CC6684 ; '>filename</span> <span class='eqformalargs'>=</span> <span class='string'>"default.css"</span> <span class='keyword'>)</span><span class='keyword'>{</span>
  <span class='keyword'>if</span><span class='keyword'>(</span> <span class='functioncall' style='color: #66CC97 ; '>file.exists</span><span class='keyword'>(</span><span class='symbol' style='color: #CC6684 ; '>filename</span><span class='keyword'>)</span> <span class='keyword'>)</span><span class='keyword'>{</span>
    <span class='keyword'>return</span><span class='keyword'>(</span><span class='functioncall' style='color: #81CC66 ; '>normalizePath</span><span class='keyword'>(</span><span class='symbol' style='color: #CC6684 ; '>filename</span><span class='keyword'>)</span><span class='keyword'>)</span>
  <span class='keyword'>}</span>

  <span class='symbol' style='color: #8166CC ; '>f</span> <span class='assignment'>&lt;-</span> <span class='functioncall' style='color: #85CC66 ; '>file.path</span><span class='keyword'>(</span> <span class='functioncall' style='color: #8B66CC ; '>Sys.getenv</span><span class='keyword'>(</span><span class='string'>"HOME"</span><span class='keyword'>)</span><span class='keyword'>,</span> <span class='string'>".R"</span><span class='keyword'>,</span> <span class='string'>"highlight"</span><span class='keyword'>,</span> <span class='symbol' style='color: #CC6684 ; '>filename</span> <span class='keyword'>)</span>
  <span class='keyword'>if</span><span class='keyword'>(</span> <span class='functioncall' style='color: #66CC97 ; '>file.exists</span><span class='keyword'>(</span><span class='symbol' style='color: #8166CC ; '>f</span><span class='keyword'>)</span> <span class='keyword'>)</span><span class='keyword'>{</span>
    <span class='keyword'>return</span><span class='keyword'>(</span><span class='symbol' style='color: #8166CC ; '>f</span><span class='keyword'>)</span>
  <span class='keyword'>}</span>

  <span class='symbol' style='color: #8166CC ; '>f</span> <span class='assignment'>&lt;-</span> <span class='functioncall' style='color: #6695CC ; '>system.file</span><span class='keyword'>(</span> <span class='string'>"stylesheet"</span><span class='keyword'>,</span> <span class='symbol' style='color: #CC6684 ; '>filename</span> <span class='keyword'>,</span> <span class='symbol_argument' style='color: #6684CC ; '>package</span> <span class='argument'>=</span> <span class='string'>"highlight"</span> <span class='keyword'>)</span>
  <span class='keyword'>if</span><span class='keyword'>(</span> <span class='functioncall' style='color: #66CC97 ; '>file.exists</span><span class='keyword'>(</span> <span class='symbol' style='color: #8166CC ; '>f</span> <span class='keyword'>)</span><span class='keyword'>)</span><span class='keyword'>{</span>
    <span class='keyword'>return</span><span class='keyword'>(</span> <span class='symbol' style='color: #8166CC ; '>f</span><span class='keyword'>)</span>
  <span class='keyword'>}</span>

  <span class='functioncall' style='color: #66CC6C ; '>stop</span><span class='keyword'>(</span> <span class='functioncall' style='color: #CC668D ; '>glue</span><span class='keyword'>(</span><span class='string'>"file not found: '{filename}'"</span><span class='keyword'>)</span> <span class='keyword'>)</span>
<span class='keyword'>}</span></pre>
With this we can quickly skim through the file and see e.g. the different uses of `filename`.

In rmarkdown
------------

To use in `rmarkdown` you can use the `hl_hook_source` and `hl_hook_document` hooks. For example this document has these:

<pre><span class='package'>knitr</span><span class='keyword'>::</span><span class='symbol'>knit_hooks</span><span class='keyword'>$</span><span class='functioncall'>set</span><span class='keyword'>(</span>
  <span class='symbol_argument'>source</span> <span class='argument'>=</span> <span class='symbol'>hl_hook_source</span><span class='keyword'>,</span>
  <span class='symbol_argument'>document</span> <span class='argument'>=</span> <span class='symbol'>hl_hook_css</span>
<span class='keyword'>)</span></pre>
