
var app = Elm.Main.fullscreen(
);

var fromApp = false;

// sha256
app.ports.askSHA.subscribe(function(string) {
    var shaObj = new jsSHA("SHA-256", "TEXT");
    shaObj.update(string);
    var hash = shaObj.getHash("HEX");
    app.ports.getSHA.send(hash);
});

app.ports.askInitNonce.subscribe(function(x) {
    app.ports.getInitNonce.send(
        [ Math.floor(Math.random() * 0xFFFFFFFF)
        , Math.floor(Math.random() * 0xFFFFFFFF)
        ]);
});

// app.ports.mathjaxRaw.subscribe(function(x) {
//     setTimeout(function() {
//         MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
//     }, 500);
// });
// app.ports.setHash.subscribe(function(path) {
//     setTimeout(function() {
//         fromApp = true;
//         history.pushState(path, "", "#" + path);
//     }, 100);
// });
// app.ports.getHashOnceRaw.subscribe(function(path) {
//     setTimeout(function() {
//         fromApp = true;
//         app.ports.getHash.send(document.location.hash.substr(1));
//     }, 100);
// });
// window.onhashchange = function(event) {
//     setTimeout(function() {
//         if (!fromApp) {
//             app.ports.getHash.send(document.location.hash.substr(1));
//         } else {
//             fromApp = false;
//         }
//     }, 100);
// };
// window.onpopstate = function(event) {
//     setTimeout(function() {
//         fromApp = true;
//         app.ports.getHash.send(event.state);
//     }, 100);
// };
