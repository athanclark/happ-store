
nacl_factory.instantiate(function(nacl) {
    var serverPk = nacl.from_hex(serverPk_);
    var keys = nacl.crypto_sign_keypair();

    // String -> { publicKey : Hex, signature : Hex }
    function sign(message_) {
        var message = nacl.encode_utf8(message_);
        return {
            "publicKey" : nacl.to_hex(keys.signPk),
            "signature" : nacl.to_hex(nacl.crypto_sign(message, keys.signSk))
        };
    }

    // Hex -> Maybe String
    function verify(signature_) {
        try {
            var signature = nacl.from_hex(signature_);
            var message = nacl.crypto_sign_open(signature, serverPk);
            if (message === null) {
                return null;
            } else {
                return nacl.decode_utf8(message);
            }
        } catch (e) {
            console.error("Decoding error:", e);
            return null;
        }
    }


    var app = Elm.Main.fullscreen(
    );

    app.ports.makeSignature.subscribe(function(xs) {
        var ys = sign(xs.payload);
        app.ports.madeSignature.send({
            "threadId" : xs.threadId,
            "payload"  : {
                "publicKey" : ys.publicKey,
                "signature" : ys.signature
            }
        });
    });

    app.ports.openSignature.subscribe(function(xs) {
        var ys = verify(xs.payload);
        app.ports.openedSignature.send({
            "threadId" : xs.threadId,
            "payload"  : ys
        });
    });
});
