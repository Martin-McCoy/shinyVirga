/**
 * @param  {String} selector
 * @returns  {Logical}
 */
function isVisible(selector) {
    return $(selector).is(":visible");
}
function cardToggleSelector (id) {
  return `#${id} > .card-header > .card-tools > .btn-tool`;
}

function cardStatusOpen (id, immediate = false) {
  let sel = cardToggleSelector(id);
  let state_open = $(sel + ' > .fas').hasClass('fa-minus')
  if (immediate) {
    state_open = !state_open;
  }
  return(state_open)
}

function cardOpen(id, action = 'open') {
             let sel = cardToggleSelector(id);
             let state_open = $(sel + ' > .fas').hasClass('fa-minus')
             switch (action) {
              case 'open':
                if (!state_open) {
                  $(sel).click()
                }
                break;
              case 'close':
                if (state_open) {
                  $(sel).click()
                }
                break;
              case 'toggle':
                $(sel).click()
                break;
             }

}
