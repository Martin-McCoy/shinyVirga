
window["esc" + '*{id}*'] = function escClose(event) {
    if (event.keyCode === 27 && isVisible('#*{panel_id}*')) {
        $('#*{id}*').click();
      }
};
window["click" + '*{id}*'] = function clickOpen(event) {
    let click_out = !document.getElementById('*{panel_id}*').contains(event.target);
    let click_not_toggle = !document.getElementById('*{id}*').contains(event.target);
    if (click_out && click_not_toggle && isVisible('#*{panel_id}*')) {
      $('#*{id}*').click();
    }
};
document.getElementById('*{id}*').addEventListener('click', (event) => {
    if (isVisible('#*{panel_id}*')) {
        window.removeEventListener("keyup", window["esc" + '*{id}*']);
        window.removeEventListener('click', window["click" + '*{id}*']);
    } else {
        window.addEventListener("keyup", window["esc" + '*{id}*'], {passive: true});
        window.addEventListener('click', window["click" + '*{id}*'], {passive: true});
    }
});

