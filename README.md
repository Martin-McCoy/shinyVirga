
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
    : 
    <p>Add the observeEvent call for a hidden ui browser button.</p>

  </li>
  <li>
    <code>browser_ui</code>
    : 
    <p>Add the invisible browser button ui component</p>

  </li>
  <li>
    <code>ns_find</code>
    : 
    <p>Retrieve the <code>ns</code> function</p>

  </li>
  <li>
    <code>path_strip_shiny</code>
    : 
    <p>Useful for linking to internal files, such as with image source attributes <code style="white-space: pre;">⁠&lt;img src="[path]"&gt;⁠</code></p>

  </li>
  <li>
    <code>path_strip_to</code>
    : 
    <p>Useful for linking to internal files, such as with image source attributes <code style="white-space: pre;">⁠&lt;img src="[path]"&gt;⁠</code></p>

  </li>
  <li>
    <code>tab_ns_extract</code>
    : 
    <p>Extract the tabname to which this module instance corresponds from the ns</p>

  </li>
  <li>
    <code>warn_id</code>
    : 
    <p>Display a warning in a DOM element of choice</p>

  </li>
</ul></li>
<li><b>UI:</b> <ul>
  <li>
    <code>acc_list</code>
    : 
    <p>Create an accordion from a list of items</p>

  </li>
  <li>
    <code>box_list</code>
    : 
    <p>Create boxes around a list of shiny.tags</p>

  </li>
  <li>
    <code>bs4Alert</code>
    : 
    <p>Create a bootstrap 4 Alert box</p>

  </li>
  <li>
    <code>col_auto</code>
    : 
    <p>Render an auto-sized column</p>

  </li>
  <li>
    <code>dynamic_row</code>
    : 
    <p>Create a row with columns that dynamically resizeMust use <code>div(class = 'column', ...)</code> or <code>col_auto()</code> for internal columns to function properly.</p>

  </li>
  <li>
    <code>make_columns</code>
    : 
    <p>Make columns from assorted shiny.tag elementsSorts shiny.tags into columns based on the maximum number of columns (<code>max_cols</code>) per row</p>

  </li>
  <li>
    <code>ui_row</code>
    : 
    <p>A default full width row box.</p>

  </li>
  <li>
    <code>ui_tabs</code>
    : 
    <p>Create a <code>shiny.tag.list</code> of bs4SidebarMenuItems with an input tibble</p>

  </li>
  <li>
    <code>write_col_fns</code>
    : 
    <p>Write wrappers</p>

  </li>
</ul></li>
<li><b>CSS:</b> <ul>
  <li>
    <code>add_sass</code>
    : 
    <p>Translate inline Sass to CSS style tag</p>

  </li>
  <li>
    <code>css_props</code>
    : 
    <p>R List to CSS declarations as a string</p>

  </li>
</ul></li>
<li><b>JS:</b> <ul>
  <li>
    <code>js_after</code>
    : 
    <p>Add a temporary message after an elementMessage is added directly below element and persists for three seconds</p>

  </li>
  <li>
    <code>js_callback</code>
    : 
    <p>Create an anonymous JS function to monitor an event and bind it to a shiny input</p>

  </li>
  <li>
    <code>js_mouseover_once</code>
    : 
    <p>Make a shiny input with named ID with a logical TRUE value when the element is moused over.</p>

  </li>
  <li>
    <code>js_set_input_val</code>
    : 
    <p>Create a javascript callback that runs when shiny connects.</p>

  </li>
</ul></li>
<li><b>Reactives:</b> <ul>
  <li>
    <code>rv</code>
    : 
    <p>This function returns an object for storing reactive values. It is similar toa list, but with special capabilities for reactive programming. When you reada value from it, the calling reactive expression takes a reactive dependencyon that value, and when you write to it, it notifies any reactive functionsthat depend on that value. Note that values taken from the reactiveValuesobject are reactive, but the reactiveValues object itself is not.</p>

  </li>
  <li>
    <code>rv_index</code>
    : 
    <p>Index into a reactiveValues object and return a list</p>

  </li>
  <li>
    <code>rv_to_list</code>
    : 
    <p>Convenience wrapper around reactiveValuesToList</p>

  </li>
</ul></li>
<li><b>golem:</b> <ul>
  <li>
    <code>display</code>
    : 
  </li>
  <li>
    <code>enurl</code>
    : 
    <p>Create an url</p>

  </li>
  <li>
    <code>is_shiny.tag</code>
    : 
    <p>Is object a shiny tag or tagList</p>

  </li>
  <li>
    <code>jq_hide</code>
    : 
    <p>Hide an element by calling jquery hide on it</p>

  </li>
  <li>
    <code>list_to_li</code>
    : 
    <p>Turn an R list into an HTML list</p>

  </li>
  <li>
    <code>list_to_p</code>
    : 
    <p>Turn an R list into corresponding HTML paragraph tags</p>

  </li>
  <li>
    <code>make_action_button</code>
    : 
    <p>Make current tag behave like an action button.</p>

  </li>
  <li>
    <code>named_to_li</code>
    : 
  </li>
  <li>
    <code>rep_br</code>
    : 
    <p>Repeat tags$br</p>

  </li>
  <li>
    <code>tagRemoveAttributes</code>
    : 
    <p>Remove a tag attribute</p>

  </li>
  <li>
    <code>undisplay</code>
    : 
    <p>Hide or display a tag</p>

  </li>
  <li>
    <code>with_red_star</code>
    : 
    <p>Adds a red star at the end of the text(for example for indicating mandatory fields).</p>

  </li>
</ul></li>
