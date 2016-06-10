
nacl_factory.instantiate(function(nacl) {
    var keys = nacl.crypto_box_keypair();
    var serverPk = nacl.from_hex(serverPk_);
    var conn = nacl.crypto_box_precompute(serverPk, keys.boxSk);

    // String -> { nonce : Hex, packet : Hex, publicKey : Hex }
    function encrypt(message) {
        var message_ = nacl.encode_utf8(message);
        var nonce = nacl.crypto_box_random_nonce();
        var packet = nacl.crypto_box_precomputed(message_, nonce, conn);
        return {
            "nonce"     : nacl.to_hex(nonce),
            "packet"    : nacl.to_hex(packet),
            "publicKey" : nacl.to_hex(keys.boxPk)
        };
    }

    // { nonce : Hex, packet : Hex } -> { err : String } | { ok : String }
    function decrypt(xs) {
        try {
            var nonce = nacl.from_hex(xs.nonce);
            var packet = nacl.from_hex(xs.packet);
            var message = nacl.crypto_box_open_precomputed(packet, nonce, conn);
            return {
                "ok" : nacl.decode_utf8(message)
            };
        } catch (e) {
            console.error("NaCl decoding error:",e);
            return {
                "err" : JSON.stringify(e)
            };
        }
    }

    var app = Elm.Main.fullscreen(
    );

    // sha256
    app.ports.makeSHASession.subscribe(function(xs) {
        var shaObj = new jsSHA("SHA-512", "TEXT");
        shaObj.update(xs.input);
        var hash = shaObj.getHash("B64");
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
});
