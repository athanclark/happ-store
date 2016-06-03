
var app = Elm.Main.fullscreen(
);

var fromApp = false;


// sha256
app.ports.makeSHASession.subscribe(function(xs) {
    var shaObj = new jsSHA("SHA-512", "TEXT");
    shaObj.update(xs.input);
    var hash = shaObj.getHash("HEX");
    app.ports.madeSHASession.send({
        threadId : xs.threadId,
        output   : hash
    });
});

app.ports.askInitNonce.subscribe(function(threadId) {
    app.ports.getInitNonce.send({
        seed1    : Math.floor(Math.random() * 0xFFFFFFFF),
        seed2    : Math.floor(Math.random() * 0xFFFFFFFF),
        threadId : threadId
    });
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
