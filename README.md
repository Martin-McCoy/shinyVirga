
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyVirga

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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

<table class="table">
<thead>
<tr>
<th>Name</th>
<th>Concept</th>
<th>Title</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td>card_grid</td>
<td></td>
<td>Create a gridded card layout</td>
<td>
Creates a grid layout informed by the https://miro.com/app/board/o9J_l90QVck=/?moveToWidget=3074457361986467451&amp;cot=14design spec
</td>
</tr>
<tr>
<td>dbg_msg</td>
<td></td>
<td>Print a debug message to the R and Javascript Console</td>
<td>
Print a debug message to the R and Javascript Console
</td>
</tr>
<tr>
<td>glossarize</td>
<td></td>
<td>Add definitions to acronyms</td>
<td>
Uses the https://docs.google.com/spreadsheets/d/163ArY3cL67Vp-gzqjKSw_4r2kl-pCqMRsKDrK_zgbM0/edit#gid=0glossary to make tooltip definitions
</td>
</tr>
<tr>
<td>glossary_sync</td>
<td></td>
<td>Update the Virga Labs glossary</td>
<td>
Update the Virga Labs glossary
</td>
</tr>
<tr>
<td>grid_card</td>
<td></td>
<td>Wrap in a bs4Card with default options</td>
<td>
Wraps in a width 12 card, converts the title to an ID by replacing spaces with _. Adds a tip_icon if a tip named with the title exists in tips.
</td>
</tr>
<tr>
<td>has_card</td>
<td></td>
<td>Is a shiny.tag or shiny.tag.list have a Bootstrap card in the nesting structure?</td>
<td>
Is a shiny.tag or shiny.tag.list have a Bootstrap card in the nesting structure?
</td>
</tr>
<tr>
<td>is_card</td>
<td></td>
<td>Is a shiny.tag or shiny.tag.list a Bootstrap card?</td>
<td>
Is a shiny.tag or shiny.tag.list a Bootstrap card?
</td>
</tr>
<tr>
<td>nm_to_id</td>
<td></td>
<td>Convert a string to a valid HTML ID</td>
<td>
Convert a string to a valid HTML ID
</td>
</tr>
<tr>
<td>Re-imports</td>
<td></td>
<td>Re-imports</td>
<td>
Useful functions from other packages
</td>
</tr>
<tr>
<td>reexports</td>
<td></td>
<td>Objects exported from other packages</td>
<td>
These objects are imported from other packages. Follow the links
below to see their documentation.


  htmltoolstags

  rlang%|%, %||%

  shinyisolate, reactiveValues, reactiveValuesToList

  UU%|0|%, %|legit|%, %|try|%, %|zchar|%, write_dir_fn
</td>
</tr>
<tr>
<td>row_wrap.list</td>
<td></td>
<td>S3 Method for row_wrap list</td>
<td>
S3 Method for row_wrap list
</td>
</tr>
<tr>
<td>row_wrap.shiny.tag.list</td>
<td></td>
<td>S3 Method for row_wrap shiny.tag.list</td>
<td>
S3 Method for row_wrap shiny.tag.list
</td>
</tr>
<tr>
<td>row_wrap.shiny.tag</td>
<td></td>
<td>S3 Method for row_wrap shiny.tag</td>
<td>
S3 Method for row_wrap shiny.tag
</td>
</tr>
<tr>
<td>shinyVirga-package</td>
<td></td>
<td>shinyVirga: A Collection of R shiny utilities by Virga Labs</td>
<td>
A collection of useful utility functions for working with shiny and shiny frameworks
</td>
</tr>
<tr>
<td>simpleCard</td>
<td></td>
<td>Construct a simple Bootstrap card</td>
<td>
Construct a simple Bootstrap card
</td>
</tr>
<tr>
<td>add_sass</td>
<td>css</td>
<td>Translate inline Sass to CSS style tag</td>
<td>
Translate inline Sass to CSS style tag
</td>
</tr>
<tr>
<td>css_props</td>
<td>css</td>
<td>R List to CSS declarations as a string</td>
<td>
R List to CSS declarations as a string
</td>
</tr>
<tr>
<td>list2sass</td>
<td>css</td>
<td>Conver a list or vector to Sass variables</td>
<td>
Conver a list or vector to Sass variables
</td>
</tr>
<tr>
<td>browser_server</td>
<td>debugging</td>
<td>Add the observeEvent call for a hidden ui browser button.</td>
<td>
Add the observeEvent call for a hidden ui browser button.
</td>
</tr>
<tr>
<td>browser_ui</td>
<td>debugging</td>
<td>Add the invisible browser button ui component</td>
<td>
Add the invisible browser button ui component
</td>
</tr>
<tr>
<td>msg_mod_fun</td>
<td>debugging</td>
<td>Display the name of the parent module</td>
<td>
Stack traces often aren't available in Shiny. Use this function inside of modules to know where errors occur
</td>
</tr>
<tr>
<td>use_msg_mod_fun</td>
<td>debugging</td>
<td>Insert module debugging statements throughout module files.</td>
<td>
Insert module debugging statements throughout module files.
</td>
</tr>
<tr>
<td>deploy_stage</td>
<td>deploy</td>
<td>Run Deployment as a background task</td>
<td>
Run Deployment as a background task
</td>
</tr>
<tr>
<td>deploy_tar</td>
<td>deploy</td>
<td>Create a staging directory for deployment files</td>
<td>
Create a staging directory for deployment files
</td>
</tr>
<tr>
<td>is_shiny.tag</td>
<td>general</td>
<td>Is object a shiny tag or tagList</td>
<td>
Is object a shiny tag or tagList
</td>
</tr>
<tr>
<td>ns_custom</td>
<td>general</td>
<td>Create a custom namespace string.</td>
<td>
Useful for referencing objects nested in adjacent modules
</td>
</tr>
<tr>
<td>ns_find</td>
<td>general</td>
<td>Retrieve the ns function</td>
<td>
Designed to find the ns function from any level of nesting within a UI or Server module. htmlhttps://lifecycle.r-lib.org/articles/stages.html#experimentallifecycle-experimental.svgoptions: alt='[Experimental]'[Experimental]
</td>
</tr>
<tr>
<td>path_strip_shiny</td>
<td>general</td>
<td>Strip a file path to everything after resourcepath if shiny is running</td>
<td>
Useful for linking to internal files, such as with image source attributes &lt;img src="[path]"&gt;
</td>
</tr>
<tr>
<td>path_strip_to</td>
<td>general</td>
<td>Strip a file path to everything after resourcepath</td>
<td>
Useful for linking to internal files, such as with image source attributes &lt;img src="[path]"&gt;
</td>
</tr>
<tr>
<td>tab_ns_extract</td>
<td>general</td>
<td>Extract the tabname to which this module instance corresponds from the ns</td>
<td>
Extract the tabname to which this module instance corresponds from the ns
</td>
</tr>
<tr>
<td>warn_id</td>
<td>general</td>
<td>Display a warning in a DOM element of choice</td>
<td>
Display a warning in a DOM element of choice
</td>
</tr>
<tr>
<td>enurl</td>
<td>golem</td>
<td>Create an url</td>
<td>
Create an url
</td>
</tr>
<tr>
<td>jq_hide</td>
<td>golem</td>
<td>Hide an element by calling jquery hide on it</td>
<td>
Hide an element by calling jquery hide on it
</td>
</tr>
<tr>
<td>list_to_li</td>
<td>golem</td>
<td>Turn an R list into an HTML list</td>
<td>
Turn an R list into an HTML list
</td>
</tr>
<tr>
<td>list_to_p</td>
<td>golem</td>
<td>Turn an R list into corresponding HTML paragraph tags</td>
<td>
Turn an R list into corresponding HTML paragraph tags
</td>
</tr>
<tr>
<td>make_action_button</td>
<td>golem</td>
<td>Make the current tag behave like an action button</td>
<td>
Make current tag behave like an action button. htmlhttps://lifecycle.r-lib.org/articles/stages.html#experimentallifecycle-experimental.svgoptions: alt='[Experimental]'[Experimental]
</td>
</tr>
<tr>
<td>rep_br</td>
<td>golem</td>
<td>Repeat tags$br</td>
<td>
Repeat tags$br
</td>
</tr>
<tr>
<td>tagRemoveAttributes</td>
<td>golem</td>
<td>Remove a tag attribute</td>
<td>
Remove a tag attribute
</td>
</tr>
<tr>
<td>undisplay</td>
<td>golem</td>
<td>Hide or display a tag</td>
<td>
Hide or display a tag
</td>
</tr>
<tr>
<td>with_red_star</td>
<td>golem</td>
<td>Add a red star at the end of the text</td>
<td>
Adds a red star at the end of the text
(for example for indicating mandatory fields).
</td>
</tr>
<tr>
<td>js_after</td>
<td>JS</td>
<td>Add a temporary message after an element
Message is added directly below element and persists for three seconds</td>
<td>
Add a temporary message after an element
Message is added directly below element and persists for three seconds
</td>
</tr>
<tr>
<td>js_callback</td>
<td>JS</td>
<td>Create an anonymous JS function to monitor an event and bind it to a shiny input</td>
<td>
Create an anonymous JS function to monitor an event and bind it to a shiny input
</td>
</tr>
<tr>
<td>js_callout</td>
<td>JS</td>
<td>Create a driver.js callout
Must include shinyVirga::use_driver.js() in the head of the page.</td>
<td>
Create a driver.js callout
Must include shinyVirga::use_driver.js() in the head of the page.
</td>
</tr>
<tr>
<td>js_glow</td>
<td>JS</td>
<td>Add an animated glow to an element</td>
<td>
Add an animated glow to an element
</td>
</tr>
<tr>
<td>js_mouseover_once</td>
<td>JS</td>
<td>Make a shiny input with named ID with a logical TRUE value when the element is moused over.</td>
<td>
Make a shiny input with named ID with a logical TRUE value when the element is moused over.
</td>
</tr>
<tr>
<td>js_set_input_val</td>
<td>JS</td>
<td>Create a javascript callback that runs when shiny connects.</td>
<td>
Create a javascript callback that runs when shiny connects.
</td>
</tr>
<tr>
<td>use_driver.js</td>
<td>JS</td>
<td>Add driver.js dependency</td>
<td>
Add driver.js dependency
</td>
</tr>
<tr>
<td>rv_index</td>
<td>reactives</td>
<td>Index into a reactiveValues object and return a list</td>
<td>
Index into a reactiveValues object and return a list
</td>
</tr>
<tr>
<td>rv_to_list</td>
<td>reactives</td>
<td>Convert a reactiveValues object to a list</td>
<td>
Convenience wrapper around reactiveValuesToList
</td>
</tr>
<tr>
<td>rv</td>
<td>reactives</td>
<td>Create an object for storing reactive values</td>
<td>
This function returns an object for storing reactive values. It is similar to
a list, but with special capabilities for reactive programming. When you read
a value from it, the calling reactive expression takes a reactive dependency
on that value, and when you write to it, it notifies any reactive functions
that depend on that value. Note that values taken from the reactiveValues
object are reactive, but the reactiveValues object itself is not.
</td>
</tr>
<tr>
<td>acc_list</td>
<td>ui</td>
<td>Create an accordion from a list of items</td>
<td>
Create an accordion from a list of items
</td>
</tr>
<tr>
<td>box_list</td>
<td>ui</td>
<td>Create boxes around a list of shiny.tags</td>
<td>
Create boxes around a list of shiny.tags
</td>
</tr>
<tr>
<td>bs_extract_status</td>
<td>ui</td>
<td>Extract the bootstrap status of a Bootstrap tag that uses the status in the class</td>
<td>
Extract the bootstrap status of a Bootstrap tag that uses the status in the class
</td>
</tr>
<tr>
<td>bs_statuses</td>
<td>ui</td>
<td>Valid Bootstrap Statuses</td>
<td>
Valid Bootstrap Statuses
</td>
</tr>
<tr>
<td>bs4Alert</td>
<td>ui</td>
<td>Create a bootstrap 4 Alert box</td>
<td>
Create a bootstrap 4 Alert box
</td>
</tr>
<tr>
<td>col_1</td>
<td>ui</td>
<td>Create a column of width 1</td>
<td>
Create a column of width 1
</td>
</tr>
<tr>
<td>col_10</td>
<td>ui</td>
<td>Create a column of width 10</td>
<td>
Create a column of width 10
</td>
</tr>
<tr>
<td>col_11</td>
<td>ui</td>
<td>Create a column of width 11</td>
<td>
Create a column of width 11
</td>
</tr>
<tr>
<td>col_12</td>
<td>ui</td>
<td>Create a column of width 12</td>
<td>
Create a column of width 12
</td>
</tr>
<tr>
<td>col_2</td>
<td>ui</td>
<td>Create a column of width 2</td>
<td>
Create a column of width 2
</td>
</tr>
<tr>
<td>col_3</td>
<td>ui</td>
<td>Create a column of width 3</td>
<td>
Create a column of width 3
</td>
</tr>
<tr>
<td>col_4</td>
<td>ui</td>
<td>Create a column of width 4</td>
<td>
Create a column of width 4
</td>
</tr>
<tr>
<td>col_5</td>
<td>ui</td>
<td>Create a column of width 5</td>
<td>
Create a column of width 5
</td>
</tr>
<tr>
<td>col_6</td>
<td>ui</td>
<td>Create a column of width 6</td>
<td>
Create a column of width 6
</td>
</tr>
<tr>
<td>col_7</td>
<td>ui</td>
<td>Create a column of width 7</td>
<td>
Create a column of width 7
</td>
</tr>
<tr>
<td>col_8</td>
<td>ui</td>
<td>Create a column of width 8</td>
<td>
Create a column of width 8
</td>
</tr>
<tr>
<td>col_9</td>
<td>ui</td>
<td>Create a column of width 9</td>
<td>
Create a column of width 9
</td>
</tr>
<tr>
<td>col_auto</td>
<td>ui</td>
<td>Render an auto-sized column</td>
<td>
Render an auto-sized column
</td>
</tr>
<tr>
<td>copyright</td>
<td>ui</td>
<td>Creates an always up to date italicized copyright element.</td>
<td>
Creates an always up to date italicized copyright element.
</td>
</tr>
<tr>
<td>dynamic_row</td>
<td>ui</td>
<td>Create a row with columns that dynamically resize
Must use div(class = 'column', ...) or col_auto() for internal columns to function properly.</td>
<td>
Create a row with columns that dynamically resize
Must use div(class = 'column', ...) or col_auto() for internal columns to function properly.
</td>
</tr>
<tr>
<td>fa_arrow_icon</td>
<td>ui</td>
<td>FontAwesome Arrow Icons</td>
<td>
FontAwesome Arrow Icons
</td>
</tr>
<tr>
<td>icon_sb</td>
<td>ui</td>
<td>Wrapper for icon that supports svg images</td>
<td>
Wrapper for icon that supports svg images
</td>
</tr>
<tr>
<td>infoIcon</td>
<td>ui</td>
<td>A small info icon with a tooltip</td>
<td>
Requires use_tippy to be placed in the head of the app. See the [tippy docs]https://github.com/JohnCoene/tippy for details
</td>
</tr>
<tr>
<td>make_columns</td>
<td>ui</td>
<td>Make columns from assorted shiny.tag elements</td>
<td>
Sorts shiny.tags into columns based on the maximum number of columns (max_cols) per row. htmlhttps://lifecycle.r-lib.org/articles/stages.html#experimentallifecycle-experimental.svgoptions: alt='[Experimental]'[Experimental]
</td>
</tr>
<tr>
<td>ui_row</td>
<td>ui</td>
<td>A default full width row box.</td>
<td>
A default full width row box.
</td>
</tr>
<tr>
<td>ui_tabs</td>
<td>ui</td>
<td>Create a shiny.tag.list of bs4SidebarMenuItems with an input tibble</td>
<td>
Create a shiny.tag.list of bs4SidebarMenuItems with an input tibble
</td>
</tr>
<tr>
<td>write_col_fns</td>
<td>ui
Writes convenient wrappers around
column(12, ...), column(6, ...), column(4, ...)...
named col_1, col_2 etc.</td>
<td>Write wrappers</td>
<td>
Write wrappers
</td>
</tr>
</tbody>
</table>
