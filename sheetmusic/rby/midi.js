var rby = {

    name: "Pokemon RBY",
    piece: undefined,
    channels: undefined,
    path: undefined,

    configure: function(args) {
	var regex1 = /piece=([a-zA-Z]*)&channels=([a-zA-Z,0-9]*)/;
	tmp = regex1.exec(args);

	rby.piece = tmp[1];
	rby.channels = tmp[2].split(',');
	
	var fnam = rby.piece.toLowerCase();
	rby.path = `rby/audio/music/${fnam}.asm`;
    },


    load: function(asm) {
	var chan = [];
	for (var i = 0; i < rby.channels.length; i++) {
	    chan.push(rby.loadChannel(asm, i));
	}
	return {name: `Music_${rby.piece}`, chan: chan}
    },

    // to be improved...
    loadChannel: function(text, i) {
	var lines = text.split('\n');
	var state = 'looking';
	var goal = 'Music_' + rby.piece + '_' + rby.channels[i];
	var octave;
	var loop = [];
	for (var j = 0; j < lines.length; j++) {
	    if (state == 'looking' && lines[j].startsWith(goal)) {
		state = 'copying';
	    } else if (state == 'copying') {
		if (lines[j].startsWith('\toctave')) {
		    octave = Number(lines[j][1+'octave'.length+1]);
		}
		var k = chromatic.indexOf(lines[j].slice(1, 3));
		if (k != -1) {
		    var dt = Number(lines[j].split(' ')[1]);
		    loop.push([12*octave + k, dt])
		}
		if (lines[j].startsWith('\trest')) {
		    var dt = Number(lines[j].split(' ')[1]);
		    loop.push([undefined, dt]);
		}
		if (lines[j].startsWith('\tloopchannel')) {
		    break;
		}
	    }
	}
	return  {boot: [], loop: loop};
    }

}
