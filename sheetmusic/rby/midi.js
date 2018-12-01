var rby = {

    name: "Pokemon GSC",
    piece: undefined,
    path: undefined,

    configure: function(args) {
	var regex1 = /piece=([a-zA-Z]*)&channels=([a-zA-Z,0-9]*)/;
	var tmp = regex1.exec(args);

	rby.piece = tmp[1];
	var fnam = rby.piece.toLowerCase();
	rby.path = `rby/audio/music/${fnam}.asm`;
    },


    load: function(a) {
	var b = lex(a);
	var c = parse(b);
	var d = compile(c);
	return d;
    },

}
