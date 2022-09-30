
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyVirga

<!-- badges: start -->
<!-- badges: end -->

The goal of `shinyVirga` is to consolidate generalized utility functions
useful in development of shiny apps that are currently used in the
various Virga Labs repositories in one convenient location. `shinyVirga`
is intended to supply Virga Labs developers with a toolkit of
well-maintained functions and modules adaptable to various use cases
encountered likely to be encountered during shiny development with Virga
Labs.

## Installation

You can install the development version of shinyVirga like so:

``` r
virgaUtils::install_virga("shinyVirga")
```

The utility functions are divided into five categories (some functions
overlap categories):

<li><b>General:</b> <ul>
  <li>
    <code>browser_server</code>
    <span>: </span>
  </li>
  <li>
    <code>browser_ui</code>
    <span>: </span>
  </li>
  <li>
    <code>ns_find</code>
    <span>: </span>
  </li>
  <li>
    <code>path_strip_shiny</code>
    <span>: </span>
  </li>
  <li>
    <code>path_strip_to</code>
    <span>: </span>
  </li>
  <li>
    <code>tab_ns_extract</code>
    <span>: </span>
  </li>
  <li>
    <code>warn_id</code>
    <span>: </span>
  </li>
</ul></li>
<li><b>UI:</b> <ul>
  <li>
    <code>acc_list</code>
    <span>: </span>
  </li>
  <li>
    <code>box_list</code>
    <span>: </span>
  </li>
  <li>
    <code>bs4Alert</code>
    <span>: </span>
  </li>
  <li>
    <code>col_auto</code>
    <span>: </span>
  </li>
  <li>
    <code>dynamic_row</code>
    <span>: </span>
  </li>
  <li>
    <code>make_columns</code>
    <span>: </span>
  </li>
  <li>
    <code>ui_row</code>
    <span>: </span>
  </li>
  <li>
    <code>ui_tabs</code>
    <span>: </span>
  </li>
  <li>
    <code>write_col_fns</code>
    <span>: </span>
  </li>
</ul></li>
<li><b>CSS:</b> <ul>
  <li>
    <code>add_sass</code>
    <span>: </span>
  </li>
  <li>
    <code>css_props</code>
    <span>: </span>
  </li>
</ul></li>
<li><b>JS:</b> <ul>
  <li>
    <code>js_after</code>
    <span>: </span>
  </li>
  <li>
    <code>js_callback</code>
    <span>: </span>
  </li>
  <li>
    <code>js_mouseover_once</code>
    <span>: </span>
  </li>
  <li>
    <code>js_set_input_val</code>
    <span>: </span>
  </li>
</ul></li>
<li><b>Reactives:</b> <ul>
  <li>
    <code>rv</code>
    <span>: </span>
  </li>
  <li>
    <code>rv_index</code>
    <span>: </span>
  </li>
  <li>
    <code>rv_to_list</code>
    <span>: </span>
  </li>
</ul></li>
<li><b>golem:</b> <ul>
  <li>
    <code>display</code>
    <span>: </span>
  </li>
  <li>
    <code>enurl</code>
    <span>: </span>
  </li>
  <li>
    <code>is_shiny.tag</code>
    <span>: </span>
  </li>
  <li>
    <code>jq_hide</code>
    <span>: </span>
  </li>
  <li>
    <code>list_to_li</code>
    <span>: </span>
  </li>
  <li>
    <code>list_to_p</code>
    <span>: </span>
  </li>
  <li>
    <code>make_action_button</code>
    <span>: </span>
  </li>
  <li>
    <code>named_to_li</code>
    <span>: </span>
  </li>
  <li>
    <code>rep_br</code>
    <span>: </span>
  </li>
  <li>
    <code>tagRemoveAttributes</code>
    <span>: </span>
  </li>
  <li>
    <code>undisplay</code>
    <span>: </span>
  </li>
  <li>
    <code>with_red_star</code>
    <span>: </span>
  </li>
</ul></li>
