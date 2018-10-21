var rby = {

    parse: function(add) {
	rby.loadLinks(['audio/headers/musicheaders1.asm',
		       'audio/headers/musicheaders2.asm',
		       'audio/headers/musicheaders3.asm'], add);
    },

    loadLinks: function(xs, add) {
	if (xs.length == 0) return;
	
	var client = new XMLHttpRequest();
	client.open('GET', xs[0]);
	client.onloadend = function() {
	    rby.parseAsm(client.responseText, add);
	    rby.loadLinks(xs.slice(1), add);
	}
	client.send();
	    
    },

    parseAsm: function(asm, add) {
	var regex1 = RegExp('Music_[a-zA-Z0-9]*::\n\taudio Music_([a-zA-Z0-9]*)((?:, Ch[0-9])*)', 'g');
	var out = [];
	var x;
	while ((x = regex1.exec(asm)) !== null) {
	    add({
		'name': x[1],
		'thumb': 'rby/thumb/' + x[1].toLowerCase() + '.png',
		'persist': `piece=${x[1]}&channels=${x[2].split(', ').slice(1)}`
	    });
	}
	return out;
    },

}
