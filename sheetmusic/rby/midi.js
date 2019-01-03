var rby = {

    name: "Pokemon RBY",
    piece: undefined,
    channels: undefined,
    path: undefined,

    configure: function(args) {
	var regex1 = /piece=([a-zA-Z0-9]*)&channels=([a-zA-Z,0-9]*)/;
	var tmp = regex1.exec(args);

	rby.piece = tmp[1];
	rby.channels = tmp[2].split(',');
	
	var fnam = rby.piece.toLowerCase();
	rby.path = `rby/audio/music/${fnam}.asm`;
	
    },

    load: function(a) {
	var b = rby.lex(a);
	var c = rby.parse(b);
	var d = rby.compile(c);
	return d;
    },

    lex: function(a) {
	var b = a.split('\n');
	var c = [];  // nonempty lines
	for (var i = 0; i < b.length; i++) {
	    var d = rby.lexLine(b[i]);
	    if (d !== undefined) {
		c.push(d);
	    }
	}
	return c;
    },

    parse: function(a) {
	var b = [];  // blocks
	var c = undefined;  // current block;
	for (var i = 0; i < a.length; i++) {
	    if (a[i][0] == 'block') {
		if (c !== undefined) {
		    b.push(c);
		}
		c = [];
	    }
	    if (a[i][0][0].toUpperCase() === a[i][0][0]) {
		c.push(['note', a[i][0], a[i][1]]);
	    } else if (a[i][0] == 'rest') {
		c.push(['note', '__', a[i][1]]);
	    } else {
		c.push(a[i]);
	    }
	}
	if (c !== undefined) {
	    b.push(c);
	}
	return b;
    },

    compile: function(a) {
	var name = rby.piece;

	var dbw = rby.makeDBW(a);
	var chan = [];
	for (var i = 0; i < dbw.length; i++) {
	    var c = rby.compileChannel(a, dbw[i]);
	    if (c !== undefined) {
		chan.push(c);
	    }
	}

	return {
	    name: name,
	    chan: chan
	}
    },

    lexLine: function(a) {
	if (a.length == 0) {
	    return undefined;
	}
	
	var k = a.indexOf(':');
	if (k != -1) {
	    return ['block', a.slice(0, k)];
	}

	if (a[0] == '\t') {
	    var i = a.indexOf(' ');
	    var head;
	    var args;
	    if (i == -1) {
		head = a.slice(1);
		args = [];
	    }
	    else {
		head = a.slice(1, i);
		args = a.slice(i + 1).split(', ');
	    }
	    return [head].concat(args);
	}
    },

    extractLabel: function(a) {
	var b = a.slice(6);
	return b.slice(0, b.indexOf('_'));
    },

    makeDBW: function() {
	return rby.channels.map(l => `Music_${rby.piece}_${l}`);
    },

    compileChannel: function(a, id) {
	/* Model of the coding of channels: (a bit outdated) (never perfect)
	    - there is an initial block
	    - if a block ends with 'loopchannel',
	      that will determine the periodicity
	    - 'callchannel' can always be inlined
	    - nested inlining never occurs
	    - the current octave is a global variable
	*/

	var events = [];
	var outline = {};
	
	var ids = a.map(function(aj) { return aj[0][1]; });
	for (var i = ids.indexOf(id); i < a.length; i++) {
	    console.log(`chan_id=${id}, i=${i}, block_id=${ids[i]}`);
	    outline[ids[i]] = events.length;
	    for (var j = 1; j < a[i].length; j++) {
		var line = a[i][j];
		if (line[0] == 'notetype') {
		    rby.timeunit = parseInt(line[1], 10);
		}
		if (line[0] == 'octave') {
		    rby.octave = parseInt(line[1]);
		}
		if (line[0] == 'note') {
		    events.push(rby.extractNote(line));
		}
		if (line[0] == 'callchannel') {
		    var k = ids.indexOf(line[1]);
		    events = events.concat(rby.inline(a[k]));
		}
		if (line[0] == 'loopchannel') {
		    var times = parseInt(line[1]);
		    var target = line[2];
		    //console.log(line);
		    var k = outline[target];
		    //console.log(k);
		    if (k === undefined) {
			return;
		    }
		    var loop = events.slice(k);
		    //console.log(`len=${loop.length}`);
		    if (times == 0) {
			var boot = events.slice(0, k);
			return {boot: boot, loop: loop};
		    } else {
			for (var m = 1; m < times; m++) {
			    events = events.concat(loop);
			}
		    }
		}
		if (line[0] == 'endchannel') {
		    return {boot: events, loop: []};
		}
	    }
	}
    },

    chromatic: [
	'C_', 'C#', 'D_', 'D#', 'E_',
	'F_', 'F#', 'G_', 'G#', 'A_', 'A#', 'B_'
    ],

    octave: undefined,

    extractNote: function(line) {
//	console.log(line);
//	throw "break";
	var tone = line[1];
	var abs;
	if (tone != '__') {
	    abs = 12 * rby.octave;
	    abs += rby.chromatic.indexOf(tone);
	}
	var duration = parseInt(line[2]) * rby.timeunit;
	return [abs, duration];
    },

    inline: function(a) {
	var b = [];
	for (var i = 1; i < a.length; i++) {
	    var line = a[i];
	    if (line[0] == 'notetype') {
		rby.timeunit = parseInt(line[1], 10);
	    }
	    if (line[0] == 'octave') {
		rby.octave = parseInt(line[1]);
	    }
	    if (line[0] == 'note') {
		b.push(rby.extractNote(line));
	    }
	    if (line[0] == 'loopchannel') {
		var times = parseInt(line[1]);
		var target = line[2];
		if (target !== a[0][1]) return;
		var loop = b.slice(0);
		for (var m = 1; m < times; m++) {
		    b = b.concat(loop);
		}
	    }
	}
	return b;
    }

}
