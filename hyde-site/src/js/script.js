$(document).ready( function () {
    // target 属性
    $("a[href^='http']:not([href*='" + location.hostname + "'])").attr('target', '_blank');
});
