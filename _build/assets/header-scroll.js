var hidden = false;
var header = document.querySelector("header");
var offset = window.pageYOffset;
var prevOffset = offset;
var defaultDisplay = header.style.display;
var defaultTransform = header.style.transform;

window.addEventListener("scroll", function(e) {
    change_display();
});

function change_display() {
    if (offset > prevOffset) {
        header.style.transform = "translate3d(0px,"+-window.pageYOffset+"px, 0px)";
    } else {
        header.style.transform = defaultTransform;
    }
    prevOffset = offset;
    offset = window.pageYOffset;
}