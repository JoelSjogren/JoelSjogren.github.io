var mic, fft;

setup = function() {
    // put setup code here
    makePiano();

    mic = new p5.AudioIn();
    mic.start();
    fft = new p5.FFT();
    fft.setInput(mic);
}

draw = function() {
    // put drawing code here
    ellipse(50, 170, 80, 40);

    fft.analyze();
    var [s,amp] = hearTone();
    setCurrent(s);
}

var smin = 11;
var smax = 100;
var piano;
var keys = new Array(-smin+smax);
var current = 0;

hearTone = function() {
    fft.analyze();
    var ARGMAX = 0, MAX = 0;
    for (var s = smin; s < smax; s++) {
	var VAL = fft.getEnergy(keyFreqs[-smin+s]);
	if (VAL > MAX) {
	    ARGMAX = s;
	    MAX = VAL;
	}
    }
    return [ARGMAX, MAX];
}

setCurrent = function(val) {
    keyMode(current, 0);
    current = val;
    keyMode(current, 1);
}

keyFreqs = new Array(-smin+smax);
for (var s = smin; s < smax; s++) {
    keyFreqs[-smin+s] = 440*2**((s-45)/12)
}

keyMode = function(s, play) {
    if (!(smin <= s && s < smax)) return;
    keys[-smin+s][1].color = play ? "red" : blackKey(s) ? "black" : "white"
}

blackKey = function(s) {
    return [1,3,6,8,10].indexOf(s % 12) != -1;
}

makePiano = function() {
  piano = document.getElementById("piano");
  piano.border = 1;
  
  var row = document.createElement("tr");
  piano.appendChild(row);
  for (var s = smin; s < smax; s++) {
      var cell = document.createElement("td");
      cell.bgColor = blackKey(s) ? "#000000" : "#FFFFFF";
      cell.width = "10px";
      cell.height = "60px";
      var text = document.createElement("font");
      text.color = blackKey(s) ? "#FFFFFF" : "#000000";
      text.innerText = "\u25cf";
      cell.appendChild(text);
      row.appendChild(cell);
      keys[-smin+s] = [cell,text];
      keyMode(s, 0)
  }
}
