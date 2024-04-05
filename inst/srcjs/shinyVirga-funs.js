/**
 * @param  {String} selector
 * @returns  {Logical}
 */
function isVisible(selector) {
  var el = typeof selector == "string" ? el = $(selector) : selector
    return el.is(":visible");
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
  return state_open;
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
/**
 * @param  {String} str Character to encode
 * @returns  {String} Encoded string
 */
function encode_math(str) {
  str = str.replace('<', '&lt;');
  str = str.replace('>', '&gt;');
  return str;
}

/**
 * @param  {String} str Character to decode
 * @returns  {String} decoded string
 */
function decode_math(str) {
  str = str.replace('&lt;', '<');
  str = str.replace('&gt;', '>');
  return str;
}

/**
 * Ensure a string is an ID (#id), or remove the hash from and ID string.
* @param  {String} id An id to check for the hash (#) symbol
* @param {Boolean} rm_hash whether to remove the hash symbol if it has it
* @returns  {String} Returns an id String with or without the hash based on the `rm_hash` argument
* @example
id_check('blah')
id_check('#blah', rm_hash = true)
*/
function id_check(id, rm_hash = false) {
  let reg = new RegExp("^#");
  if (!reg.test(id) && !rm_hash) {
    id = "#" + id;
  } else if (rm_hash) {
    if (reg.test(id)) {
      id = id.substring(1);
    }
  }
  return id;
}
