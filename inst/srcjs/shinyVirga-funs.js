/**
 * @param  {String} selector
 * @returns  {Logical}
 */
function isVisible(selector) {
    return $(selector).is(":visible");
}