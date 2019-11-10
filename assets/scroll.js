window.addEventListener("hashchange", function() {
    scroll();
});

$(document).ready(function() {
    scroll();
});

function scroll() {
    var page_url = window.location.href;
    var page_id = page_url.substring(page_url.lastIndexOf("#") + 1);

    if (page_id == "about") {
        $("html, body").animate({
            scrollTop: $("#scroll-"+page_id).offset().top - 400/768 * window.screen.height
        }, "slow");
    }
}