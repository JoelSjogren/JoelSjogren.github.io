var gsc = {

    parse: function(add) {
	var client = new XMLHttpRequest();
	client.open('GET', 'gsc/audio/music_pointers.asm');
	client.onloadend = function() {
	    gsc.parseAsm(client.responseText, add);
	}
	client.send();
    },

    parseAsm: function(asm, add) {
	var regex1 = RegExp('Music_([a-zA-Z0-9]*)', 'g');
	var x;
	while ((x = regex1.exec(asm)) !== null) {
	    add({
		'name': x[1],
		'thumb': 'gsc/thumb/' + x[1].toLowerCase() + '.png',
		'persist': `piece=${x[1]}`
	    });
	}
    }

}
