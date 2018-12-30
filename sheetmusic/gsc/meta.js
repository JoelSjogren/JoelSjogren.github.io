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
	    var exceptions = {
		'Title': 'TitleScreen',
		'KantoGymLeaderBattle': 'KantoGymBattle',
		'Heal': 'HealPokemon',
		'WildVictory': 'WildPokemonVictory',
		'GymVictory': 'GymLeaderVictory',
		'PokemonTalk': 'ProfOaksPokemonTalk',
		'RivalAfter': 'AfterTheRivalFight',
		'KimonoEncounter': 'LookKimonoGirl',
		'JohtoGymLeaderBattle': 'JohtoGymBattle',
		'ProfElm': 'ElmsLab',
		'BugCatchingContestRanking': 'ContestResults',
		
	    };
	    var encounters = [
		'Hiker', 'Lass', 'Officer', 'Rival',
		'Youngster', 'Beauty', 'Rocket', 'Pokemaniac',
		'Sage'
	    ];
	    for (var i = 0; i < encounters.length; i++) {
		var e = encounters[i];
		exceptions[e + 'Encounter'] = 'Look' + e;
	    }
	    x[1] = exceptions[x[1]] || x[1];
	    add({
		'name': x[1],
		'thumb': 'gsc/thumb/' + x[1].toLowerCase() + '.png',
		'persist': `piece=${x[1]}`
	    });
	}
    }

}
