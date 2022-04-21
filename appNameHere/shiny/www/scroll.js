// 下にスクロールすると、上に戻るボタンを表示する。
window.onscroll = function() {scrollFunction()};
document.getElementsByClassName("ms-DetailsList is-horizontalConstrained root-114").onscroll = function() {scrollFunction()};

function scrollFunction() {
    if (document.body.scrollTop > 5 || document.documentElement.scrollTop > 5 || document.getElementsByClassName("ms-DetailsList is-horizontalConstrained root-114").scrollTop > 5) {
        document.getElementById("myBtn").style.display = "block";
    } else {
        document.getElementById("myBtn").style.display = "none";
    }
}

// 上にスクロールする。
function topFunction() {
    document.documentElement.scrollTop = 0;
}